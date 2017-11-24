Hunt is a fast and flexible, lightweight search platform written in
the Haskell programming language. The **Hunt framework** provides all
the tools needed to create fast and scaling customized searchengines.
Built on top of the framework, the **Hunt Server** presents an out of
the box search server.


## Features

- A powerful **query language** supports phrase queries, range queries
  and a lot more.
- Optimized algorithms and data-structures handle schematized indexing
  and search for **fulltext, numeric data, dates, geospatial data**.
- Indexing, search and configuration are easily done with a language
  independent **JSON API**.
- Extensible architecture


## Quickstart

Download and install Hunt from Github.

```bash
$ git clone https://github.com/hunt-framework/hunt.git
$ cd hunt
$ stack build --copy-bins
```

Start the Server with one simple command and try it out with one of
our sample data sets.

```bash
$ hunt-server
$ make insertJokes
```

The web interface is available at http://localhost:3000/
