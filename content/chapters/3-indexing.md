+++
title = "Indexing documents"
slug = "3-indexing"
publishdate = "2014-04-08"
+++

# Indexing Documents

## Introduction

This chapter highlights the different possibilities to index and
replace documents. The beginning will be an introduction into the
basic process of inserting documents including a description of a
document. The next sections will cover the indexing of different
sources, replacing and deleting documents, loading a serialized index
at start up and using document weights to modify the sorting of search
results.

We will use Hayoo as an example throughout this chapter.

Hayoo is a search engine for Haskell source code packages which allows
to search for functions, data types and packages. Each indexed
document consists of a name, a description and, if applicable, a
function signature. It is possible, for example, to search for
function names[1], function signatures[2] and module names[3]. All
search results of Hayoo will link to the originating Hackage
documentation page. Hackage is software package repository for
Haskell.Well-Typed and Industrial Haskell Group (2014)

Hayoo uses the as its search engine and thus provided many
applications of recipes form this book.


## Indexing documents using the eval API

After configurating the and understanding schema declarations, we can
start to insert documents. Hunt accepts new documents in a specific
JSON format and we are going to use the `eval` API to index them.


### How to do it

As an example, we are indexing Hackage packages and the first document
looks like this:

Store this JSON document as `document.json` and execute
`hunt-server-cli` like this to create a simple scheme for this
document:

```bash
hunt-server-cli make-schema document.json |
hunt-server-cli eval /dev/stdin
```

This mechanism was described in section [sec:makeSchema]. Then execute
this to create an insert-command for this document:

```bash
hunt-server-cli make-insert document.json > insertDocument.json
```

The file `insertDocument.json` should look like this:

Then evaluate the insert-command by executing

```bash
hunt-server-cli eval insertDocument.json
```

If the *Hunt server* successfully evaluates the request, then you
should be able to search for it by opening
<http://localhost:3000/search/First/1/0> in your browser.

### How it works

The first step was necessary to successfully insert the documents,
because otherwise the would have rejected the command and complain
about unknown contexts like this:

The interesting line is:

```bash
mentioned context(s) are not present: ["category","name","synopsis"]
```

This is the expected error message for unknown contexts.

In listing [lst:eval-insert] in line 15, the keyword `insert`
specifies the current command and line 2 introduces the document which
will be inserted.

### Background

By using a bash pipe to link both commands together, we can simplify
the command to:

```bash
hunt-server-cli make-insert document.json |
    hunt-server-cli eval /dev/stdin
```

## Updating a document

We already know, that a document consists of an indexing part and a
description part. It is possible to update the description of a
document without touching any contexts by using the update command
without the index part of the JSON document. It is also possible to
update an index without touching any other contexts.

### How to do it

Let’s try this by first inserting a simple test document:

This example document just has a title, but no content. We are
inserting it by running

```bash
curl -X POST -d @original.json http://localhost:3000/eval
```

We are going to update this incomplete document by calling an `update`
command like this.

Here we can see, that we just provide the description part of the
document. Let us update the document by running this command:

```bash
curl -X POST -d @description.json http://localhost:3000/eval
```

## Crawling a Website

Suppose, one would like to use Hunt to provide a search function for
searching in a website. This website has a few thousand pages and a
data store, which is not easy to be indexed by the . There is a tool
called `html-hunter`, that can crawl pages, generate documents and
insert them into the *Hunt server*.

### How to do it

First, we need to execute `html-hunter` to generate documents:

```bash
html-hunter 'http://hayoo.fh-wedel.de/about' \
            'http://hayoo.fh-wedel.de/examples' \
            'http://hayoo.fh-wedel.de/' > documents.json
```

Then we need to create a schema that is suitable for these documents:

```bash
hunt-server-cli make-schema documents.json  > schema.json
```

Then we can upload both, the schema defintion and the documents, to
Hunt:

```bash
hunt-server-cli eval schema.json
hunt-server-cli eval documents.json
```

Now, we can test our crawling by searching for `holumbus`

```bash
hunt-server-cli search holumbus
```

The result should be <http://hayoo.fh-wedel.de/about>:

### How it works

The `html-hunter` is able to parse HTML pages, that are either located
on the local file system, or they can be downloaded from the
Internet. Each downloaded page will then be parsed and converted into
an insert command. The *Hunt server* does not accept documents
containing unknown contexts. Therefore we need to generate a schema
suitable for inserting these generated documents. We did that by
telling `hunt-server-cli` to generate a schema for us:

This schema is not optimal, because the title context does not have
more weight than the body context. One would usually expect that the
title context is more important than other contexts. This can be done
by adding this line after line 19:

```json
    "weight": 10,
```

### Background

HTML pages that are parsed by `html-hunter` can be split into parts of
a document, for example the title, all headlines or all text elements
of the body. These parts can be specified by XPath expressions,
although we just have used the default parts for HTML documents. An
XPath expression to gather all function names of a Hayoo search result
could look like this:

```bash
html-hunter
 -c 'headers:/html/body/div/div/div[contains(@class,'panel-heading')]/a'
 -c title 'http://hayoo.fh-wedel.de/?query=data-stringmap'
```

This is the corresponding Hunt document:

## Indexing CSV documents

Often a database table or other table like data can be exported as a
CSV file. Imagine, one would like to make this CSV file searchable by
a web service then one will discover, that using this CSV file
directly is quite slow. A better approach would be using Hunt to index
this file by converting each row into a Hunt document. For this
example, save listing [lst:csvSample] CSV document as `data.csv`

After converting the CSV file into a list of documents we are going to
create a simple schema and add it to Hunt.

### How to do it

Convert each row into a document by running the `from-csv` converter

```bash
hunt-server-cli from-csv data.cs > documents.json
```

Use the first document to generate a Hunt schema

```bash
hunt-server-cli make-schema documents.json > schema.json
```

Evaluate the schema

```bash
hunt-server-cli eval schema.json
```

Insert the documents:

```bash
hunt-server-cli insert documents.json
```

Search for a document by calling `search`

```bash
hunt-server-cli search stringmap
```

One should get this result:

### How it works

The converter uses the first line for defining the names of the
contexts as described in Shafranovich (2005). Also the column
delimiter should be a comma.

It is possible to modify the indexed data of a document, because the
documents of the are spitted into the index and the description
part. By modifying the output of `hunt-server-cli`, one can edit each
insert command. This is possible, because the converter does not
directly insert a document, but prints the document to the standard
output.

To further analyze the automatically generated documents, we will use
the first row of the CSV:

The document URI (Line 2) is generated by using the file name of the
CSV file and the current row. The document’s description equals the
document’s index, because they were both created by the row itself
without further information.

## Storing additional data to your document

Indexable data need to be represented as a string. This restriction
does not apply for the document’s description element, because it is
not being processed by the index implementation. Hunt accepts element
every JSON object in contrast to the index.

### How to do it

Save this document as `document.json`:

Create the corresponding schema:

```bash
hunt-server-cli make-schema document.json  | hunt-server-cli eval /dev/stdin
```

Insert this document into the *Hunt server*:

```bash
hunt-server-cli eval document.json
```

Search for the document and print the description by running:

```bash
hunt-server-cli search Article
```

This command prints the modified document:

### How it works

We have demonstrated, that Hunt can store arbitrary JSON values in the
description of a document.

### Background

Hunt is a search engine and not a data store. If one needs to reduce
the memory usage or prevent the duplication of documents, then it
would be a good idea to remove the description and just keep the index
part. In this case, one could use different data stores and use the
document’s URI as the key to the data.

## Replacing a set of documents

In case one would like to replace a set of related documents with
another set of documents, one needs to remove the old documents from
the index before inserting the new ones. Also deleting of documents is
unnecessary, if either the weight or the description of the document
needs to be changed. See [sec:context] for details on how to do that.

In order to replace a set of documents one can use delete-by-query to
delete all matching documents. As an example, we would like to delete
all documents, that are part of the `data-stringmap` Haskell package
and replace them with an updated set of documents.

### How to do it

First, delete all matching documents by executing

```bash
hunt-server-cli delete-by-query 'package:"data-stringmap"'
```

The following search for `data-stringmap` should not give any results
that are in the `data-stringmap` package.

```bash
hunt-server-cli search data-stringmap
```

Now, insert the new documents into Hunt by evaluating these inserts:

```bash
data-stringmap-new.json
```

We can now search for the `data-stringmap` package again:

```bash
hunt-server-cli search data-stringmap
```

This should print the new documents to the terminal.

### How it works

Deleting documents by a query works like a normal search query. But
instead of returning the list of results, the documents are deleted
from the index.

In addition to deleting documents by a query, one can also delete
single documents by its URI:

```bash
hunt-server-cli delete "http://hackage.haskell.org/package/data-
    stringmap/docs/Data-StringMap.html#v:unionWithKey"
```

This command deletes the document with the given URI.

### Common errors

Deleting documents by a query is dangerous, if the query depends on
values, that are not artificially generated by the feeder. For
example, executing this command:

```bash
hunt-server-cli delete-by-query data-stringmap
```

may also delete documents that reference `data-stringmap` in it’s
description field or may have `data-stringmap` as a dependency.

Additionally, one should use a phrase query to delete documents to
prevent deleting additional documents, where the query string is a
prefix of the context value. For example:

```bash
hunt-server-cli delete-by-query package:data-
```

deletes all documents in all packages starting with `data-`

## Perform a restart without loosing the Index

The is an in-memory search engine and because of that the index is
lost if the *Hunt server* needs to be restarted. Fortunately, one can
store and load a binary representation of the index.

### How to do it

Let us test this with some documents from Hayoo by indexing some
sample documents:

```bash
curl -X POST -d  @hayoo.json http://localhost:3000/eval
```

Then send a command to store the index into a file on the local file system:

```bash
hunt-server-cli store ./hayoo.index
```

Restart the . This will drop the current index:

```bash
killall hunt-server
```

Then start Hunt again:

```bash
hunt-server
```

The now runs with an empty index. One can verify this by retrieving
the list of all document descriptions:

```bash
curl http://localhost:3000/status/doctable
```

This command should print an empty JSON document.

The is now ready to load the index again. Send a command to load the
index from a file:

```bash
hunt-server-cli load hayoo.index
```

Depending on the amount of data this can take a few minutes. To test
the newly reloaded index, search for `data-stringmap` in the index

```bash
hunt-server-cli search data-strinmap
```

This should print the same results, as if we have added all documents
from JSON again.

### How it works

Saving the index to the file system is different form storing the JSON
documents. The stores the index in a format that is optimized for a
fast loading and re-creating the index would take much longer.

### Background

The main purpose of storing an index is a fast reload of an index
after a restart, but one can use this feature to reduce the memory
usage of the . Reducing the memory usage is described in .

To simplify the start up one can add `--load-index hayoo.index` to
load the serialized index directly after starting the :

```bash
hunt-server --load-index hayoo.index
```

## Modifying the order of results

In , we have seen how to modify the weight of a context which was
useful for example to change the weight of a title context. To get a
more fine grained control of the order of search results, a document
also consists of a weight field. In order to show the ranking
algorithms in detail, we need some real data in the .

### How to do it

Listing [lst:raningSchema] shows the schema definitions. Each of them
has a `weight` field to modify the search results:

In addition to the schema definitions, we use these three documents:

Insert both by letting the evaluate listing [lst:raningSchema] as
`schema.json` and listing [lst:raningDocuments] as `documents.json`:

```bash
hunt-server-cli eval schema.json
hunt-server-cli eval documents.json
```

These search command demonstrate different context weights and
document weights:

```bash
hunt-server-cli search map
hunt-server-cli search data
```

The first search for the word `map` prints these results:

The second search for the word `data` prints these results:

### How it works

Listing [lst:raningSchema] shows three contexts. The `title` context
has an increased weight to push hits on this context to the top of the
results. The second context, `package`, is a usual context without an
explicit weight and is per default set to `1`. The last context,
`description`, has a reduced weight to decrease the importance.

The list of documents in listing [lst:raningDocuments] contains two
package descriptions and a function description from Hackage. These
documents are a small extract of the example that is used in the next
chapter . The first document (`/data-stringmap`) has a weight of
`2.3`, a description and a name. The second document
(`/data-stringmap/map`) references the first document and shares the
same weight value. The last document has a weight of `3` and the URI
`/data-r-tree`.

We can now examine the ranging scores. A score is a value which is
computed by merging the contexts weights, the document weights, the
query weights (), the number of occurrences of the query word and a
matching value between the search phrase and the indexed word.

The first score value for `/data-r-tree` in listing [lst:rankingMap]
is mainly influenced by the weight of the `name` context, the weight
of the document `/data-r-tree` and the single occurrence of `data` in
the `description` context. The second result is similar to the first
result without the occurrence of the search phrase `data` in the
description. The score of the last search result is influenced just by
the weight of the document `/data-stringmap/map`.

Listing [lst:rankingData] is a second example, which uses the weight
of different contexts to show the difference in the resulting score
value.

Shafranovich, Y. 2005. “Common Format and MIME Type for CSV Files.”

Well-Typed, and the Industrial Haskell Group. 2014. “Hackage.” http://hackage.haskell.org/. [\\url{http://hackage.haskell.org/}](\url{http://hackage.haskell.org/}).

[1] Hayoo: search for a function name: <http://hayoo.fh-wedel.de/?query=map>

[2] Hayoo: search for a function signature: <http://hayoo.fh-wedel.de/?query=%28a-%3Eb%29-%3Ef+a-%3Ef+b>

[3] Hayoo: search for a function name: <http://hayoo.fh-wedel.de/?query=Control.Loop>

