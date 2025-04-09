![test workflow](https://github.com/cogini/linux_proc/actions/workflows/test.yml/badge.svg)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](CODE_OF_CONDUCT.md)

# linux_proc

Expose information from Linux `/proc` pseudo filesystem.

At this point, the only thing that is implemented is reading from `/proc/stat` to get CPU usage statistics.

`linux_proc_stat` is a `gen_server` that reads the file periodically (every second by default)
and store the current `cpu` information.

You can then call `linux_proc_stat:cpu_ratios/0` to get the current CPU usage statistics.
It returns a map with the following fields with the ratio of time spent in each state:

    user        Time spent with normal processing in user mode.
    nice        Time spent with niced processes in user mode.
    system      Time spent running in kernel mode.
    idle        Time spent in vacations twiddling thumbs.
    iowait      Time spent waiting for I/O to completed. This is considered idle
                time too. Since 2.5.41
    irq         Time spent serving hardware interrupts. See the description of
                the intr line for more details. Since 2.6.0
    softirq     Time spent serving software interrupts. Since 2.6.0
    steal       Time stolen by other operating systems running in a virtual
                environment. Since 2.6.11
    guest       Time spent for running a virtual CPU or guest OS under the
                control of the kernel. Since 2.6.24
    guest_nice  Time spent running a niced guest (virtual CPU for guest operating
                systems under the control of the Linux kernel). Since 2.6.33

This is most useful to get the load on the system for use in rate limiting.
For example, if the system has significant `iowait` time, then avoid reading and
writing to disks. If the system has significant `user` time, then avoid taking
on new batch jobs.

Since this module is Linux specific, it is not started by default.
You can conditionally add it to your application's supervision tree based on the OS.

Maybe check for the existence of `/proc/stat` or `{unix, linux} = os:type()`.

        %{id: linux_proc_stat, start: {linux_proc_stat, start_link, []}},

## Links

* http://linux.die.net/man/5/proc
* https://man7.org/linux/man-pages/man5/proc_stat.5.html
* https://www.idnt.net/en-US/kb/941772
* https://www.kernel.org/doc/Documentation/filesystems/proc.txt
