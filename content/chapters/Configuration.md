+++
title = "Index Configuration and Schema Definition"
publishdate = "2014-04-07"
slug = "schema-definition"
+++

# Index Configuration and Schema Definition

## Introduction

The indexes documents, which are represented as JSON objects. The JSON
structure is described in chapter [appendix][1] in section. A
document consists of an URI, a description element and an index
element. The index element is a map from context names to the indexed
data. The description of this index element will be stored in memory
and is the result of search commands. Searching for documents is
described in the chapter [Querying][2].

This chapter will explain the purpose, creation and details of these
contexts and schema definitions.


## Adding a context

One critical step before we can start to index documents is to provide
a schema.

A Hunt schema is comparable to a database schema. Such a Hunt schema
consists of contexts and each context is comparable to a database
column.


### How to do it

Let us start by looking at a simple JSON example for defining two
contexts:

Save this JSON file as `schema.json` and execute the following command
to create the new context definitions.

```bash
curl -X POST -d @schema.json 'http://localhost:3000/eval'
```

Having created the contexts, we can start to index documents as it
will be done in chapter [chap:Indexing] .


### How it works

By looking at the schema Sauer and Reumann (2014), we can see, that we
are adding two contexts: a title context and a content context. Each
context consists of a content name and a schema definition. A schema
definition consists of a type and a weight.

The context name must be unique and is used to identify the context
itself. In the next chapter [chap:Indexing] , we will see, that the
name is used as the key for the document’s index.

The schema type is a keyword to choose the index
implementation. Currently, there are four indexes to choose from:
`text`, `date`, `int` and `position`. Each type can index different
types of data.

The regular expression `regex` is used to split the context of the
string to be indexed into words. Typically, you can leave it out or
use default `\\w` for splitting words by whitespace.

You can specify a context weight to change the importance of a
content. For example, you can increase the weight for title contexts
or decrease the weight for descriptions, meta informations or
normalized content.

As one can see in the example, one can create multiple contexts in one
go by concatenating them into a JSON list.


## Common Errors

If one encounters an error like this, an index with this name already
exists:

If one would like to change the index, then it is possible to either
restart Hunt by following the steps described in the previous chapter
[chap:Getting<sub>S</sub>tarted] or to delete the existing context by
saving this command as `delete.json` and then posting it to `/eval` as
described above:

It should now be possible to create the title index.


## Hiding contexts from free text searches

Search queries are looked up in every context. That means, if we want
to search for a word “traverse” the will look in every context for
occurrences of “traverse”. This may not be the desired behavior, for
example in contexts holding date, geospatial or artificially generated
data. Another reason for excluding a context from simple searches may
be a better performance.


### How to do it

Let us create a context, that is hidden for simple queries:


### How it works

Listing [lst:schemaDefaultFalse] shows an `insert-context` command,
where the `default` flag is set to `false`.

Queries without an explicit context will ignore contexts with a schema
with a `default` flag set to `false`. Typically, one will set this to
`false`, if the context is some kind of keyword or the user is not
expected to search for it.

Examples for queries that specifically use a context are given in
section [sec:advancedQueries] .

## Using a regular expression

Choosing the right regular expression for a context is important, if
you are not indexing paragraphs or texts. A typical `regex` is `.*`,
if the content is a single entity, like a position, a date or an
enumeration.


### How to do it

This is a sample schema definition with a given regular expression:


### How it works

Such a regular expression will be used to split context data into
words or chunks by matching such a regular expression to every
chunk. The default regular expression is `\\w`. It matches a single
word, which generates a chunk for every word. It is possible to store
more than one year in the `year` context by providing this regular
expression: `[0-9]+`. Each given year is then indexed independent of
other years.

## Preprocessing data

The supports a mechanism to preprocess data before it is indexed. This
is called normalizing. At this moment, the supports two normalizers:
the first one will convert all characters to upper case and the second
normalizer will convert all characters to lower case.

### How to do it

This is a `context-insert` command which uses the lower case
normalizer:

One can insert this context by calling the evaluation API of the .

### How it works

Normalizers are applied to words, that were generated from the
original data by using the regular expressions.

It is possible to apply more than one normalizer to the list. Each
normalizer is applied in the order of their appearance in the list of
normalizers.

## Indexing non-text data

Suppose, one would like to index geospatial data. Hunt has an index
implementation that is suitable for such documents.


### How to do it

One can insert position contexts like so:

Save this file as schema.json and execute the following `curl` command to add them to the :

```bash
curl -X POST -d @schema.json http://localhost:3000/eval
```

Hunt is now ready to index geospatial data.

### How it works

By specifying the type as `position`, we have changed the underlying
index to use an R-Tree Wagner and Philipp (2014). The R-Tree is a
spatial data structure similar to Quadtrees or B-Trees. Hunt is now
using it to optimize the index for fast query processing, although the
insert performance is not as fast as the inserts for the text index.

After the position index, we have also inserted a text index `name` to
be able to search for a given name in a given area. For example one
can search for all cafés in Hamburg.

Line 6 of listing [lst:rtreecontext] sets the default flag to
`false`. Search queries for position contexts are typically not
directly written by users, but created by a front-end. It is therefore
wise to set this to `false`. Otherwise users could get error messages,
if the query is not parsable as a position.

In line 14, the weight of the name index was increased to `3`. It is
important, that name or title contexts are weighted more, than other
indexes to provide better search results. The weight of a context
modifies the ranking of results by increasing or decreasing the
importance of a result. The default context weight is `1`.

## Automatic creation of context definitions

There exists a shortcut to automatically generate simple context
definitions by using the tool `hunt-server-cli`, which is a part of
the .

### How to do it

We will generate simple context definitions for this document:

Store this JSON document as `document.json` and execute
`hunt-server-cli` like this to create a simple schema:

```bash
hunt-server-cli make-schema document.json
```

The generated schema looks like this:

One can now evaluate these `insert-context` commands on the .

### How it works

Listing [lst:generatedSchema] shows three `insert-context`
commands. Each of them is a simple text index, because
`hunt-server-cli` will not generate more complex schema
definitions. If one needs more control over a schema definition, then
this can be a good starting point for modifications, like a different
context weight.

Sauer, Ulf, and Christian Reumann. 2014. “Hunt. Implementing a Highly
Flexible Search Engine Platform in Haskell.” Master’s thesis,
FH-Wedel.

Wagner, Birte, and Sebastian Philipp. 2014. “data-R-Tree” (version
0.0.3.0). http://hackage.haskell.org/package/data-r-tree.


[1]: 
[2]: 
