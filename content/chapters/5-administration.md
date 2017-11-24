+++
title = "Administration"
slug = "5-administration"
+++
# Administration

## Introduction

This chapter covers the running phase of the software lifecycle. The
deployment of the *Hunt server* is relatively easy and described in
section [section:starting] . This chapter covers the monitoring which
should accompany this phase of the life cycle.

## Reducing Memory and CPU usage

Let us assume that the size of the data to be indexed exceeds the size
of the ram which is available. Different approaches to improve the
situation are possible and we will cover some of them in detail.

Comparable to a Java application a Haskell application, like the *Hunt
server*, has a runtime system which can be parametrized Team
(2014). Changing runtime options. especially to the garbage collector,
can dramatically change the behavior of the server.

Other approaches use the system’s swap memory to increase the
available resources or store and reload the index from the disk.

### How to do it

Changing runtime systems options can be done by adding command line
options like this:

```bash
hunt-server +RTS <runtime systems options> -RTS
```

For example, one can specify the number of garbage collector
generations by setting:

```bash
hunt-server +RTS -G3 -RTS
```

In case that the index is still to large to fit into memory, the
“swappiness” of the system can be increased by writing a high value
into `/proc/sys/vm/swappiness` and adding `-I0` to the command line
arguments in order to disable the idle garbage collector:

```bash
echo 90 | sudo tee /proc/sys/vm/swappiness
hunt-server +RTS -I0 -RTS
```

Setting the “swappiness” is temporarily and can be made permanent in
`/etc/sysctl.conf`.

Now, reload the index as described in .

### How it works

There are some important options that can reduce the amount of RAM
which is needed by the *Hunt server*. One can often choose between a
lower memory footprint and a lower CPU usage.

If the index of the *Hunt search engine* is relatively stable, then
increasing the number of garbage collector generations can be a way to
reduce the CPU load of the *Hunt server*. This can be done by adding a
`-G`*generations* flag to the runtime system parameters. The number of
generations should not exceed 4 to limit the amount of unused memory
and should be at least 2, to keep the index data and requests in
different generations. This is necessary to ensure that requests can
be collected without iterating through the index.

If the memory usage of the *Hunt server* approaches the total memory
of the system, a `-c` flag can be added to reduce the memory footprint
of the last garbage collector generation. This flag tells the garbage
collector to use a compaction algorithm without copying elements in
memory. Setting this flag on the *Hunt server* for the Hayoo index
reduces the memory consumption by about 30 percent without reducing
the performance noticeably.

The garbage collector scans the heap regularly to reduce the
probability of the need for a garbage collector run while processing
client requests. The default interval between garbage collector runs
is `0.3` seconds. To increase the interval, one can set the
`-I`*seconds* flag, which can decrease the CPU usage of the *Hunt
server* on idle conditions noticeably. The Hayoo *Hunt server* uses an
interval of three seconds, which decreased the CPU usage from 30
percent to 1 percent.

Older generations will be collected, if their memory usage has
increased by a factor that can be configured by setting the flag
`-F`*factor*. Setting this factor to a value which is lower than 2
will decrease the time between garbage collector runs and thus
decrease the amount of used memory.

There are other options to control the garbage collector which are
described in the GHC manualTeam (2014). These options may also
influence the runtime characteristics of a running *Hunt server*.

The default configuration options that are used by the *Hunt server*
are these: `-c -I3 -H`. The `-H` option is suggested for programs that
create a large amount of data.

Changing the “swappiness” to a value above the default value of *60*
will increase aggressiveness of using the swap memoryRiel and Morreale
(2008) while lower values decrease the amount of swap. We also have
set the `-T` flag to *0* to disable the garbage collector in idle
conditions, because the garbage collector would scan the whole heap
for unused memory and thus need to keep the whole data in
memory. Increasing the “swappiness” of the system is limited by the
performance to initially create the index. There are two reasons for
this: First, the performance of a hard disk is orders of magnitude
worse than the performance of volatile memory, and second, inserting
new documents to an existing index requires the availability of the
complete index in memory. Tests have shown that trying to index new
data using swap memory results in a freeze of the system.

In case the memory usage of the is nearly as big as the available
memory, the index should be reloaded from file after heavily modifying
the index by inserting and deleting documents. The reason is the
Haskell runtime which leads to a decreased memory usage, if the index
was loaded from a file.

## Monitoring the Hunt Server with StatsD

There are different ways of monitoring the *Hunt server*. One can
either monitor the availability by using services like
NagiosEnterprises (2014) or gather runtime statistics. These
statistics are useful to keep track of what the *Hunt server* is
doing. For example to detect that something has gotten slower or
finding memory leaks.

We are going to send runtime statistics that are provided by the *Hunt
server* to statsDEtsy (2014), which is a daemon that listens for
statistic packages and sends them to a backend like GraphiteProject
(2014). Graphite is an application that aggregates statistics and
displays them in a web application. Graphite uses Carbon which is a
storage engine specialized for statistical data.

### How to do it

In order to show the statistics one needs to install Graphite and
StatsD. The installation of Graphite is highly dependent on the
operating system. One can find tutorials on the Internet[1] on how to
configure and run Graphite.

#### Installing StatsD

Install StatsD by cloning the Git repository:

```bash
git clone https://github.com/etsy/statsd.git
cd statsd
```

Copy the example configuration:

```bash
cp exampleConfig.js config.js
```

Edit the `config.js` file and change the host name of Graphite:

```bash
, graphiteHost: "127.0.0.1"
```

Then install *node.js*:

``` {.text}
apt-get install nodejs
```

Start StatsD by running:

```bash
nodejs stats.js config.js &
```

#### Configuring the 

In order to push statistics to StatsD, the needs to be configured and
built to support StatsD:

```bash
cabal install -fstatsD
```

We can now run Hunt by adding the `–statsd-host` parameters:

```bash
hunt-server --statsd-host=localhost 
```

Load the Hayoo index to generate some meaningful data:

```bash
hunt-server-cli eval hayoo.index.json 
```

Opening the Graphite application should provide the user with
statistics like these in figure [fig:graphiteExample]:

![Graphite example statistics](bilder/graphite)

[fig:graphiteExample]

Typical signs of an increased workload are shown in figure
[fig:graphiteExample]. The top-left diagram shows the number of runs
of the garbage collector, which increases in this example. The
top-right diagram shows statistics of the run time system (rts). The
number of bytes, that the run time system allocates increases. The
bottom-left diagram shows information about the CPU usage of the run
time system. The last diagram in the bottom-right corner shows
statistics about the time usage of the garbage collector.

### How it works

In a production environment generating statistical data may be an
essential part of using Hunt. This generated data consist of
information about the runtime, the CPU load and the amount of cycles
of the garbage collector. It generates also data about the memory
usage of the process.

It is not enough to provide the possibility to track the *Hunt
server*. A way to store and persist the data in a central location
Tibell (2014) is required to keep the data after a restart of Hunt. We
have used a chain of StatsD and Graphite to reach that goal.

### Background

Hunt is an in-memory search engine and it may happen, that a query
significantly increases the memory footprint of Hunt. It is therefore
crucial to monitor the virtual memory size of the with a monitoring
system like nagios in a production environment.

An alternative approach to generate these statistics is to parametrize
the runtime to print garbage collector statistics into the standard
output by adding `-S` to the program arguments. These generated
statistics would look like this:

```
Alloc  Copied    Live    GC    GC  TOT  TOT  Page Flts
bytes   bytes   bytes   user  elap user elap
705488  114576  161856  0.00  0.00 0.02 0.01    0    0  (Gen:  0)
565920   98264  193144  0.01  0.01 0.05 4.02    0    0  (Gen:  1)
610376   98528  738352  0.00  0.00 0.06 5.73    0    0  (Gen:  0)
560128   98432 1276976  0.00  0.00 0.06 5.74    0    0  (Gen:  0)
560128   98432 1813488  0.00  0.00 0.06 5.74    0    0  (Gen:  1)
...
```

This terminal output is comparable to the one that is transfered to
StatsD and Graphite. An overview is available at the Hackage
documentationTeam (2014).

It is also possible to use the Unix proc statistics to get the VmSize
value of `/proc/$(pidof hunt-server)/statistics`.

Another way to achieve the same goal is the output of /status/gc:

```bash
$ curl -s 'http://localhost:3000/status/gc' | jq '.["msg"]'
{
    "mutatorCpuSeconds": 7.205238303,
    "peakMegabytesAllocated": 171,
    "cumulativeBytesUsed": 254504208,
    "currentBytesSlop": 0,
    "bytesAllocated": 5640951296,
    "wallSeconds": 451.282887453,
    "bytesCopied": 381132456,
    "parMaxBytesCopied": 0,
    "currentBytesUsed": 62145296,
    "numByteUsageSamples": 9,
    "numGcs": 1524,
    "gcWallSeconds": 1.647582819,
    "mutatorWallSeconds": 449.635304634,
    "maxBytesUsed": 67190792,
    "maxBytesSlop": 11290904,
    "cpuSeconds": 8.863659734,
    "gcCpuSeconds": 1.644063206,
    "parTotBytesCopied": 0
} 
```

These are the same data as these transfered to Graphite.

Enterprises, Nagios. 2014. “Nagios.” http://www.nagios.org/. [\\url{http://www.nagios.org/}](\url{http://www.nagios.org/}).

Etsy, Inc. 2014. “statsD.” https://github.com/etsy/statsd/. [\\url{https://github.com/etsy/statsd/}](\url{https://github.com/etsy/statsd/}).

Project, Graphite. 2014. “Graphite.” http://graphite.wikidot.com/. [\\url{http://graphite.wikidot.com/}](\url{http://graphite.wikidot.com/}).

Riel, Rik van, and Peter W. Morreale. 2008. “Documentation for `/proc/sys/vm/*`.” https://www.kernel.org/doc/Documentation/sysctl/vm.txt. [\\url{https://www.kernel.org/doc/Documentation/sysctl/vm.txt}](\url{https://www.kernel.org/doc/Documentation/sysctl/vm.txt}).

Team, The GHC. 2014. “The Glorious Glasgow Haskell Compilation System User’s Guide.” https://www.haskell.org/ghc/docs/latest/html/users\_guide/using-ghc.html. [\\url{https://www.haskell.org/ghc/docs/latest/html/users\\\_guide/using-ghc.html}](\url{https://www.haskell.org/ghc/docs/latest/html/users\_guide/using-ghc.html}).

Tibell, Johan. 2014. “Ekg 0.4 Released.” http://blog.johantibell.com/2014/05/ekg-04-released.html. [\\url{http://blog.johantibell.com/2014/05/ekg-04-released.html}](\url{http://blog.johantibell.com/2014/05/ekg-04-released.html}).

[1] Installing Graphite on Ubuntu 14.04: <https://www.digitalocean.com/community/tutorials/how-to-install-and-use-graphite-on-an-ubuntu-14-04-server>

