+++
title = "Querying Documents"
slug = "querying"
publishdate = "2014-04-10"
+++

# Querying Documents

## Introduction

After we have built an index, we can search for documents. This
chapter will go into the details of the search commands and the query
language. First, we will cover the basics and explain how to process
the output. Later, we are going to use advanced techniques to modify
the order of results and use different context implementations. We
will also use query combinators to combine the results of more than
one query.

We are starting with an empty *Hunt server* and a set of about 20
thousand Hayoo documents as an example.

These sample documents can be downloaded from the Hayoo website.[1]
Please extract this compressed file into your current directory, as in
this chapter it is expected to be there. The documents are stored as a
single list of commands. Please execute `curl` like this to load these
documents into the :

```bash
curl -X POST -d  @hayoo.index.json http://localhost:3000/eval
```

As usual, one should get an `ok` as result:

``` {.json}
{"msg":"ok","code":0}
```

## Querying documents

The most basic way to search for documents is to search for a
word. You can either use `curl` or use the command line tool
`hunt-server-cli` that is located in the git repository. We use the
`hunt-server-cli` tool because of the automatic URL-encoding and JSON
printing. This will simplify the understanding of queries.

The provides two APIs to search for documents: using HTTP-GET requests
for simple queries and browser communication, or the `eval/` API for
complex queries or machine to machine communication.

### How to do it

We start by with the most basic search API by calling `curl` like
this:

```bash
curl 'http://localhost:3000/search/traverse'
```

This prints the complete search result for the keyword *traverse*
ranked by its score value. We can now restrict this query be fetching
at most 3 results starting at result 5:

```bash
curl 'http://localhost:3000/search/traverse/5/3/'
```

It can be seen that the maximum and offset values are encoded in the
query. We can now convert this URL into a command in JSON format:

A third way is the `hunt-server-cli` tool. Execute `hunt-server-cli`
like this to search for `traverse` and show the first 2 search
results.

```bash
hunt-server-cli search --max 2 traverse
```

The resulting JSON will look similar to this:

The document’s descriptions are truncated to reduce the amount of JSON
code here.

### How it works

Both `curl` calls performa a search for the word *traverse*
case-insensitive. The first call is the most basic one which fetches
all results. The second call limits the range of results that are
returned by the . This format of queries is a so called text-query, in
contrast to JSON-queries. Text-queries can store the same queries as
JSON-queries, but in a different format, that is optimized for simple
queries. Queries that are more complex should be executed in the
JSON-format.

Listing [lst:introQuery] shows a search command which consists of a
JSON-query for *traverse*, an integer *offset* (5) which sets the
offset of this search, and an integer *max* (3) which limits the
number of results.

The call to `hunt-server-cli` can be translated into a HTTP-GET
request to <http://localhost:3000/search/traverse/0/1>. This URL
contains the host name to the *Hunt server*, the keyword `search`, the
URL-escaped search-phrase, the result offset and the maximum number of
search results. The `hunt-server-cli` utility maps its given
parameters to the corresponding URL and performs URL-escaping.

By looking further into the details of the response in listing
[lst:queryResult], we can see that the reply consists of meta-data and
the results. The meta data contain the given maximum number of results
in line 2, the requested offset in line 3 and the actual number of
results in line 4.

Line 5 begins with the JSON-keyword `result` which contains the list
of search results.

Each search result consists of a score (line 7), the document URI
(line 8) and the description element of the document which we had
inserted into the *Hunt server*.

The score is computed by the number of hits in a single context
multiplied by the weight of a context. Then all contexts are added up.

The description element is an arbitrary JSON object and thus can also
contain JSON lists and JSON numbers.

### Background

The administration web interface which is accessible in your
browser,[2] provides a simple HTML form to insert queries, because
searching for documents is one of the important parts of the *Hunt
server* API. In addition to displaying the results, the web interface
also displays the generated JSON in a more user friendly way by
converting the results into a table. This web interface also lists
some examples of the query language to provide a simple reference as
shown in figure [fig:adminInterface].

![Administration interface of the ](bilder/admin-interface)

[fig:adminInterface]

``` {.text}
name:!map module:!Prelude package:!base
```

This query should return just one document.

## Sorting the result set

The *Hunt search engine* prints out results in a specific order:
Search results are ordered by its score value. This score value
depends on the context, the query and the documents. Usually this is
the desired order as most important results are located on top of the
result list. It might happen that search results share the same score
value. In this case, the order is arbitrary and a client may wish to
reorder the results. It may not always be the case that the origin of
search results are obvious, for example embedded in an application. In
this case, one may wish to reorder the results by their names.

In order to search results, a client library is needed, that processes
the output of a Hunt search query. JSON is language independent and
this allows the usage of any programming language capable of
processing JSON documents. Here we are going to use Python as the
client language to demonstrate the independence to Haskell and use the
library `urllib3`Petrov (2014), that provides a simple networking
functionality.

### How to do it

First, we need a query with a lot of results sharing the same
score. We are searching in Python for `map`, because `map` is
implemented in lots of different packages with the same name.

```python
>>> import urllib3, json
>>> http = urllib3.PoolManager()
>>> limitedResult = json.loads(http.request("GET",
        "http://localhost:3000/search/map/0/4").data)
>>> limitedResult
```

This should print the first four search results as Python data
types. We are not interested in neither the return code nor the
offset. Therefore we select the result list:

```python
>>> results = limitedResult['msg']['result']
>>> results 
```

The variable `results` now contains the list of results without an
additional wrapper containing the meta data. We can now sort the
results by using the `sort` function:

```python
>>> results2 = sorted(results, 
        key=lambda r: r["description"]["package"])
>>> results2
```

As we have sorted the Python array, `results2` now contains the sorted
list of results. We can now convert the new ordered results into a
printable format:

```python
>>> print "\n".join(r['description']['package'] + 
        '.' + r['description']['name'] + ' :: ' +
        r['description']['signature'] for r in results2)
EdisonAPI.map :: (a -> b) -> s a -> s b
blaze-html.map :: Html-> Html
containers.map :: (a -> b) -> Map k a -> Map k b
containers.map :: (a -> b) -> Map k a -> Map k b
```

This is a list of results that are no longer ordered by score, but by
package name.

### How it works

Line 3 executes a `GET` request to the *Hunt server* and converts the
http result into a Python data structure. Incidentally, a JSON data
structure is a subset of Python data structures Foundation (2014), and
is therefore safe to use and should not generate any exceptions.

It may happen that the *Hunt server* returns an error instead of a
`LimitedResult`Sauer and Reumann (2014), for example like this:

In this case line 5 may not execute as expected and will throw an
exception like this:

```python
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
KeyError: 'result'
```

Line 7 sorts the list of results by its package name by executing
`sort` which sorts a Python array by a user defined accessor
function. Line 9 creates a string by combining the package, the name
and the signature of each result and joins each line with a newline
character.

### Background

The decision to use Python in this receipt is to show that the usage
of Hunt is independent of Haskell as the language for implementing
Hunt clients. Instead of choosing Python, one could also use Java,
Ruby or any other functional or imperative language.

Line 9 could be roughly translated into Haskell by converting `join`
to `unlines` and the generator expression to a list comprehension like
this:

```haskell
unlines $ [(desc r) ! "package" ++ "." ++ (desc r) ! "name" 
    ++ " :: " ++ (desc r) ! "signature" | r <- results2] 
```

## Using Range Queries

A regular search may return more than one result, if the query matches
more than one document. For example, a search for a word returns also
documents, where the result contains the word as a prefix. Another
form of generating more than one result is the usage of a so-called
range query. These range queries consist, in contrast to prefix
queries, of a start element and an end element to reduce the results
to this interval.

A good example for using the range query is to search for documents in
a given date range. One could search for all Hackage packages that
were uploaded in January 2014.

This example will use a context named `uploaded` which was created
with the schema shown in listing [lst:rangeSchema]:

### How to do it

Execute this query by saving it as `range-query.json`

and then run `curl` like this:

```bash
curl -X POST -d  @range-query.json http://localhost:3000/eval
```

This query returns a list of packages which should look like this:

Another example could be the search for name ranges:

```bash
hunt-server-cli search '[mapA TO mape]'
```

Executing this returns a list of functions, where the function name is
lexically between `mapA` and `mape`.

### How it works

The *Hunt server* also accepts JSON representations of queries, as the
first example shows. The query is embedded in a command, namely
`search` (line 3), with a maximum number of results limited to two
(line 4). The query itself is a combination of two queries: The first
query starting in line 7 is a context query which reduces the embedded
query to the specified contexts in line 8. The embedded query is a
range query with an upper bound in line 11 and a lower bound in
line 12. Usually, the *Hunt server* returns a command result with the
list of search results (line 19). The search results are limited to
their upload date in January 2014.

The second example shows the usage of a range query using the text
representation. The query syntax looks like this:

`[upper TO lower]`

`upper` and `lower` are the upper and lower elements of the JSON query.

The range query works similar to a prefix search for text contexts,
but it gives more fine grained control compared to prefix searches. It
also allows fuzzy searches for context implementations like date
contexts that do not support prefix queries.

The schema definition of listing [lst:rangeSchema] uses a date context
with the `default` flag set to `false`. The default flag was explained
in section .

## Using Autocompletion to complete Queries

In many cases line input fields are used to generate search
queries. These queries can be partially completed by using the auto
complete handler of the *Hunt server*.

The result of a completion result is a list of completion results, but
these results are all possible completions for the last
word. Typically, one would expect to get a completion for complete
queries. In order to modify the resulting list of auto completion
results, a function is needed that takes a text query and a list of
completion results as parameters and returns a list of text query
completions.

Let us try this out:

### How to do it

Call `/completion` with a search term:

```bash
hunt-server-cli completion 'map fil'
```

This should print these completions:

We can now use this JavaScript function to generate completions for
complete queries:

This function should return a list like this:

These completions are suitable to be used for input fields.

### How it works

To understand the `completeQuery` function, we need to examine each
step. In line 3 we test for a successful completion result. If we
detect an error, `completeQuery` will return an empty list. The last
query element is being detected in lines 5 to 7. In line 6 the
original query string is processed by replacing delimiters by
spaces. Possible delimiters of the last word are given by the query
language. For example the colon is used to distinguish context names
and words. This last query element will be completed by the *Hunt
server*. Line 10 replaces for each line the last query element by the
completion which was provided by the parameter.

If your front-end is written in Haskell, you can use the
implementation that is provided by the . Applied on the JSON results
of the auto completion for `map fil`, `completeQueries` returns a list
of queries that look exactly like the completions returned by
`completeQuery`.

### Background

The *Hunt server* processes an auto completion request as any other
request by first parsing the given query and then executing it. But
instead of returning document descriptions Hunt returns possible word
completions.

## Building advanced Queries using the Client-Interface

When Haskell is used for implementing a client for the , one can use
the `ClientInterface` library which contains an API for building
complex queries. The `hunt-searchengine` package exports a module
named `ClientInterface` .

We are going to search for data types that are included in a given
package.

### How to do it

We can then start the interactive Haskell “Read-eval-print loop” by
executing:

```bash
cabal sandbox repl
```

The Haskell function to build our query looks like this:

```haskell
import Hunt.ClientInterface

queryTypes :: Text -> Query
queryTypes package
  = packageQuery `qAnd` typeQuery
  where
  packageQuery = qContext "package" package
  typeValues = ["data", "newtype", "type"]
  typeQuerys = (qContext "type") <$> typeValues
  typeQuery = qOrs typeQuerys
```

One can generate a printable `Query` by running `printQuery` to
convert a query into a string:

```haskell
> printQuery $ queryTypes "base"
"package:!base (type:!data OR type:!newtype OR type:!type)"
```

Instead of generating a text query, we can also generate a JSON
representation of this query:

```haskell
> putStrLn $ encodePretty $ queryTypes "base"
```

This query can now be used by `hunt-server-cli` to query the server:

```bash
hunt-server-cli search 'package:!base (type:!data OR
    type:!newtype OR type:!type)'
```

### How it works

In order to execute these steps, setting up a development environment
is required. This was done in Chapter Introduction: [subsec:devenv]

To build the query one needs to import the `ClientInterface` module
from the `hunt-searchengine` package. This exports the `Query` data
type and all used functions to build the query. Additionally, the
`ClientInterface` also exports functions to generate Hunt command like
*insert* or *delete*.

In line 7 we are declaring a function `queryTypes` of type `Text ->
Query` that will generate our query.

The query itself is being generated between line 9 and 11. For each
element of the query syntax there exists a corresponding function in
the `ClientInterface`. For example `qAnd` combines two queries into
one query by joining them with an `And` constructor.

The function `printQuery` uses a pretty-printer to generate serialized
representations of in-memory queries. These serialized queries are
deserialized by the JSON interface of the *Hunt server*.

In line 17, we are generating a JSON query. This JSON query is a to
long to be shown here in detail. It consists of sequence, context and
word queries.

We have used `hunt-server-cli` instead of `curl` to URL-escape the
query before sending it to Hunt. The equivalent `curl` command looks
like this:

```bash
curl /search/package:%21base%20%28type:
  %21data%20OR%20type:%21newtype%20OR%20type:%21type%29/0/20
```

Foundation, Python Software. 2014. “The Python Standard Library.” https://docs.python.org/2/library/json.html\#standard-compliance. [\\url{https://docs.python.org/2/library/json.html\\\#standard-compliance}](\url{https://docs.python.org/2/library/json.html\#standard-compliance}).

Petrov, Andrey. 2014. “urllib3.” http://urllib3.readthedocs.org/. [\\url{http://urllib3.readthedocs.org/}](\url{http://urllib3.readthedocs.org/}).

Sauer, Ulf, and Christian Reumann. 2014. “Hunt. Implementing a Highly Flexible Search Engine Platform in Haskell.” Master’s thesis, FH-Wedel.

[1] Tutorial Index: <http://hayoo.fh-wedel.de/download/hayoo-tutorial.tar.gz>

[2] Web interface of the on the local host: <http://localhost:3000/search>

