+++
title = "Introduction"
publishdate = "2014-04-06"
slug = "1-introduction"
+++

# Preface

The *Hunt Search Engine Platform* is an open source framework to build
search engines. These search engines optimize the execution time by
keeping the index data in memory. They also provide the possibility to
index application specific documents by giving the user the freedom to
use, in addition to predefined index implementations, custom defined
implementations. Application specific documents can be for example:
book meta data, product descriptions, geo localized data or a listing
of points of interest.

The *Hunt Search Engine Platform*Sauer and Reumann (2014) has been
developed as successor of the *Holumbus Framework*Schlatt (2008)Hübel
(2008). The enhancements compared to the *Holumbus Framework* consist
of a JSON-API, online updates to existing indexes, the use of new
libraries of the Haskell ecosystem and a completion interface for user
input.

In addition to a framework for building search engines, Hunt also
contains a server with default implementations of predefined data
structures for indexing documents. Objectives of this implementation
are an easy to use interface and becoming independent of the
programming language: One can use the *Hunt server* with any
programming language on the client side.

The design decisions, implementation details and performance analysis
of Hunt are described in a previous thesis “Hunt - Implementing a
Highly Flexible Search Engine Platform in Haskell” by *Christian
Reumann* and *Ulf Sauer*Sauer and Reumann (2014). This comprehensive
manual providing problem solutions and background information aims at
users, who want to use the *Hunt Search Engine Platform*. Recipes
provide problem solutions and background information.

Chapter [Getting Started][5] covers basic tasks like installing the
*Hunt server* and setting up a development environment. Chapter
[Schema Definition][6] describes the set up of index implementations
by providing insights into the configuration of these
implementations. Chapter [Indexing][7] highlights the different
possibilities to index and replace documents. Retrieving documents is
described in chapter [Querying][8]. Chapter [Administation][9] covers
the problems, that will appear in a production environment, like
monitoring and deployment. The last chapter [Using Hunt from Haskell][10]
goes into the details of coding Haskell software that used the Hunt
framework to provide search engines.

## Required Software and Terminology

This cookbook will make extensive use of a tool called `curl` to talk
to the *Hunt server*. It can be installed on Linux by using the
software management tool. Mac OS X provides `curl` with the
installation. Using an alternative tool to talk to the *Hunt server*
requires, that the program arguments are set according to this tool.

Lots of applications use JSON as data serialization standard. JSON or
JavaScript Object Notation is an open standard defined in
RFC-4627Crockford (2006).

All recipes are tested on Linux and may work as described on
Mac OS X. The best option to follow the book on Windows is an
installation of a virtual machine running Linux. Installing the *Hunt
Search Engine Platform* has been tested, but many recipes use the
command line to demonstrate solutions. Following the book on Windows
requires the modification of bash commands to the Windows command
prompt or installing software tools using the MinGW projectMinGW.org
(2014).

## How to download the example code

This book uses example code to demonstrate problem solutions. This
example code will be available in a repository on *GitHub* or as a zip
file, which can be downloaded on <http://huntsearch.org>.

## Getting Started

### Introduction

Whoever wants to use the *Hunt Search Engine Platform* fist needs to
install programs and libraries needed for either running the *Hunt
server* or developing applications or libraries, that use the *Hunt
Search Engine Platform*. There are three different ways of how to
install the software: Install just the *Hunt server* on Debian based
operating systems by using the Debian package, install the executables
and libraries using Cabal, or cloning the git repository and using a
sandbox to develop *Hunt* itself.

The *Hunt server* will be available as a Debian package that can be
downloaded from <http://huntsearch.org>. The Debian package will
contain the `hunt-server` executable for the `x86_64` architecture.

The *Hunt Search Engine Platform* consists of multiple packages. Each
of them provide a different functionality. The `hunt-searchengine`
library implements the search engine. This library is used by the
`hunt-server` executable, which is a stand-alone application and
provides a JSON API. The `hunt-client` library implements functions to
talk to a *Hunt server*. The last components are `hunt-server-cli` and
`html-hunter`, which are command line interfaces to access a *Hunt
server* by using the `hunt-client` package.

### Installing Hunt using Cabal

Cabal is a package management tool to manage packages for the Haskell
programming language. These packages can contain executables or
libraries. Building the packages requires the existing installation of
the Haskell platform. Installing the Haskell platform depends on the
operating system.


### How to do it

The Haskell platform should be available on all modern Linux
distributions. The command to install the platform on Ubuntu 10.04 is:

``` {.text}
sudo apt-get install haskell-platform
```

The Haskell platform is also availablePlatform (2014) for Windows and
Mac OS X.

After successfully installing the Haskell platform, we can now install
the *Hunt Search Engine Platform* by executing these commands:

```bash
cabal update
cabal install -fstatsD hunt-server hunt-searchengine hunt-client
html-hunter hunt-server-cli
```

Cabal installs executables into a folder inside of the user’s home
directory. Simplify the execution of executables of the *Hunt Search
Engine Platform* by adding `~/.cabal/bin` to your `PATH` environment
variable:

```bash
export PATH="~/.cabal/bin:$PATH"
```

On Mac OS X, Cabal will install Hunt executables to
`~/Library/Haskell/bin`


### Background

To update an existing installation, execute these commands:

```bash
cabal update
cabal install --force -fstatsD hunt-server html-hunter hunt-server-cli
```

Cabal doesn’t like to upgrade library packages. If one needs to update
also the libraries, use the following recipe, that uses a Cabal
sandbox to handle conflicts between different library versions.

### Setting up the development environment

Usually one does not need to set up a development
environment. However, there are reasons to use such an environment: It
is not advisable to upgrade libraries that are dependencies of other
packages. Another reason to use a development environment is the need
for a version of the *Hunt Search Engine Platform*, that is not
currently released on HackageWell-Typed and Industrial Haskell Group
(2014).

This recipe requires the Haskell platform of the previous section .

### How to do it

If the provided Cabal version is too old to handle sandboxes, then
Cabal needs to be updated. This can be done by executing:

```bash
cabal install cabal-install cabal
```

Keep in mind, that the operating system needs to find the Cabal
executable, which will be installed to `~/.cabal/bin` or
`~/Library/Haskell/bin`. Add this folder to your `PATH` environment
variable.

Install Git and Make. This can be done on Ubuntu by executing:

```bash
sudo apt-get install git make
```

Now, clone the Git repository:

```bash
git clone https://github.com/hunt-framework/hunt.git
cd hunt
```

Initialize the Cabal sandbox and install Hunt into this sandbox by running:

```bash
make first-install
```

### Starting the Hunt Server

After the installation described in section [section:installing], we
are now ready to start the *Hunt server*.

The executable of the *Hunt server* is named `hunt-server`. Execute on
the command line this way:

```bash
hunt-server
```

A command line argument of the *Hunt server* are `--host` and `--port`
that change the host name and the port on which the *Hunt server* is
listening:

```bash
hunt-server --host=213.42.232.216 --port=3001
```

To simplify the start up, one can add `--load-index` to load a
serialized index on start up.

### Background

Per default, the *Hunt server* is listening on the local host and port
`3000`. The recipes in this book expect it to listen on these default
values.

Crockford, D. 2006. “JSON RFC-4627.” [http://www.ietf.org/rfc/rfc4627.txt][1].

Hübel, Timo B. 2008. “The Holumbus Framework. Creating Fast, Flexible
and Highly Customizable Search Engines with Haskell.” Master’s thesis,
FH-Wedel.

MinGW.org. 2014. “MinGW.” [http://www.mingw.org/][1].

Platform, Haskell. 2014. “Haskell Platform.” [http://www.haskell.org/platform/][3].

Sauer, Ulf, and Christian Reumann. 2014. “Hunt. Implementing a Highly
Flexible Search Engine Platform in Haskell.” Master’s thesis,
FH-Wedel.

Schlatt, Sebastian M. 2008. “The Holumbus Framework. Creating Scalable
and Highly Customized Crawlers and Indexers.” Master’s thesis,
FH-Wedel.

Well-Typed, and the Industrial Haskell Group. 2014. “Hackage.”
[http://hackage.haskell.org/][4].


[1]: http://www.ietf.org/rfc/rfc4627.txt
[2]: http://www.mingw.org/
[3]: http://www.haskell.org/platform/
[4]: http://hackage.haskell.org/
[5]: 
[6]: 
[7]: 
[8]: 
[9]: 
[10]: 
