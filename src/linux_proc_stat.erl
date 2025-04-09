%%% Parse Linux /proc/stat

% https://man7.org/linux/man-pages/man5/proc_stat.5.html

-module(linux_proc_stat).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0, start_link/1, cpu_ratios/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_INTERVAL, 1000).
-define(CALL_TIMEOUT, 1000).

%%% API

-spec cpu_ratios() -> {ok, map()} | {error, term()}.
cpu_ratios() ->
    gen_server:call(?SERVER, cpu_ratios, ?CALL_TIMEOUT).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec start_link([proplists:proplist()]) -> {ok, pid()} | ignore | {error, term()}.
start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%%% gen_server callbacks

init(Args) ->
    % ClkTck = proplists:get_value(clk_tck, Args, 100),
    SendInterval = proplists:get_value(interval, Args, ?DEFAULT_INTERVAL),
    {ok, UpdateTimer} = timer:send_interval(SendInterval, update),

    {ok, DataBin} = file:read_file("/proc/stat"),
    {ok, NewData} = parse_data(DataBin),

    State0 =
        #{update_timer => UpdateTimer,
          % clk_tck => ClkTck,
          prev_data => NewData},
    State = cpu_update(NewData, State0),
    {ok, State}.

handle_call(cpu_ratios, _From, State) ->
    Reply = {ok, maps:get(cpu, State)},
    {reply, Reply, State};
handle_call(Request, From, State) ->
    ?LOG_WARNING("Unexpected call from ~p: ~p", [From, Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?LOG_WARNING("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info(update, State0) ->
    {ok, DataBin} = file:read_file("/proc/stat"),
    {ok, CurrData} = parse_data(DataBin),

    State = cpu_update(CurrData, State0),

    {noreply, maps:put(prev_data, CurrData, State)};
handle_info(Info, State) ->
    ?LOG_WARNING("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, State) ->
    _ = timer:cancel(
            maps:get(update_timer, State)),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

cpu_update(CurrData, State) ->
    PrevData = maps:get(prev_data, State),

    PrevCpu = maps:get(cpu, PrevData, #{}),
    CurrCpu = maps:get(cpu, CurrData, #{}),
    Delta = cpu_delta(PrevCpu, CurrCpu),
    Ratios = cpu_as_ratios(Delta),
    maps:put(cpu, Ratios, State).

% Calculate difference betwen times in current and previous
-spec cpu_delta(map(), map()) -> map().
cpu_delta(Prev, Curr) ->
    lists:foldl(fun(Key, Acc) -> maps:put(Key, maps:get(Key, Curr) - maps:get(Key, Prev), Acc)
                end,
                #{},
                maps:keys(Prev)).

% Convert values into ratio of total
-spec cpu_as_ratios(map()) -> map().
cpu_as_ratios(Data) ->
    Total = maps:fold(fun(_Key, Value, Acc) -> Acc + Value end, 0.0, Data),
    if Total > 0.0 ->
           maps:map(fun(_Key, Value) -> Value / Total end, Data);
       true ->
           maps:map(fun(_Key, _Value) -> 0.0 end, Data)
    end.

-spec parse_data(binary()) -> {ok, map()}.
parse_data(Data) ->
    Lines = re:split(Data, "\r|\n|\r\n", []),
    {ok, parse_lines(Lines)}.

-spec parse_lines([binary()]) -> map().
parse_lines(Lines) ->
    lists:foldl(fun(Line, Acc) ->
                   case parse_line(Line) of
                       ok ->
                           Acc;
                       {Key, Value} ->
                           maps:put(Key, Value, Acc)
                   end
                end,
                #{},
                Lines).

% https://man7.org/linux/man-pages/man5/proc_stat.5.html
%
% user        Time spent with normal processing in user mode.
% nice        Time spent with niced processes in user mode.
% system      Time spent running in kernel mode.
% idle        Time spent in vacations twiddling thumbs.
% iowait      Time spent waiting for I/O to completed. This is considered idle
%             time too. Since 2.5.41
% irq         Time spent serving hardware interrupts. See the description of
%             the intr line for more details. Since 2.6.0
% softirq     Time spent serving software interrupts. Since 2.6.0
% steal       Time stolen by other operating systems running in a virtual
%             environment. Since 2.6.11
% guest       Time spent for running a virtual CPU or guest OS under the
%             control of the kernel. Since 2.6.24
% guest_nice  Time spent running a niced guest (virtual CPU for guest operating
%             systems under the control of the Linux kernel). Since 2.6.33

% cpu  32282702 38357 82864541 301409169352 3461306 0 261729 0 0 0
-spec parse_line(binary()) -> {atom(), map()} | ok.
parse_line(<<>>) ->
    ok;
parse_line(<<"cpu ", Data/binary>>) ->
    FieldsBin = re:split(Data, " ", []),
    [User, Nice, System, Idle, Iowait, Irq, Softirq, Steal, Guest, GuestNice | _Rest] =
        [binary_to_integer(X) || X <- FieldsBin, X =/= <<>>],
    {cpu,
     #{user => User,
       nice => Nice,
       system => System,
       idle => Idle,
       iowait => Iowait,
       irq => Irq,
       softirq => Softirq,
       steal => Steal,
       guest => Guest,
       guest_nice => GuestNice}};
parse_line(_) ->
    ok.

% -spec read_file(file:name_all()) -> {ok, binary()} | {error, atom()}.
% read_file(Path) ->
%     file:read_file(Path).
