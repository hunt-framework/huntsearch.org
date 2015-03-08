Appendix
========

Introduction
------------

This is the reference documentation of the JSON API of the *Hunt server*.

Any request to the `/eval` API should be a valid <span>**`Command`**</span> and the answer will be a <span>**`Response`**</span>. A <span>**`Command`**</span> is, for example, an instruction add an <span>**`ApiDocument`**</span> to the index, or an instruction to store an index into the disk. A <span>**`Response`**</span> may indicate a successful execution or a list of search or completion results.

Command
-------

A <span>**`Command`**</span> is an instruction for the *Hunt server*. There are different kinds of <span>**`Command`**</span>s and each of these kinds will generate different <span>**`Response`**</span>s.

### Search

This command is a JSON Object and returns a <span>**`SearchResult`**</span> matching the <span>**`Query`**</span>. Chapter [chap:Querying] covers the details of this <span>**`Command`**</span>. A <span>**`Search`**</span> consists of a <span>**`query`**</span> containing a <span>**`Query`**</span>, an integer <span>**`offset`**</span> which sets the offset of this search, and an integer <span>**`max`**</span> which limits the number of results.

### Completion

A <span>**`Completion`**</span> is similar to a <span>**`Search`**</span>, but returns a <span>**`CompletionResult`**</span>. The <span>**`Completion`**</span> commmand has, in contrast to the <span>**`Search`**</span>, no offset parameter and query parameter is named <span>**`text`**</span>.

### Insert

The <span>**`Insert`**</span> command is used to insert <span>**`ApiDocument`**</span>s into the index and wraps a single document. This is an example:

### Update

This command is similar to the insert <span>**`Command`**</span> and updates the document’s description, but not the index. An example looks like this:

### Delete

<span>**`Delete`**</span> deletes a document from the index by its <span>**`uri`**</span>. This is an example:

### DeleteByQuery

This <span>**`Command`**</span> was described in section [section:deleteByQuery] . This command <span>**`DeleteByQuery`**</span> deletes all documents matching the <span>**`Query`**</span>. The parameter <span>**`query`**</span> is an ordinary <span>**`Query`**</span>.

### InsertContext

This command creates a new context. Its parameters are a <span>**`title`**</span> and the <span>**`schema`**</span>. The name of the new context will be the name of the title parameter. The <span>**`schema`**</span> parameter is a <span>**`Schema`**</span> description.

#### Schema

The <span>**`schema`**</span> parameter contains the schema description. Every <span>**`InsertContext`**</span> command has a schema to adjust its behavior.

The regular expression “<span>**`regexp`**</span>” splits the text into words which are then transformed by the given normalizations functions (e.g. to lower. case). The Optional <span>**`regex`**</span> parameter overrides the default given by context type. The parameter <span>**`normalizers`**</span> is a list of strings. Each normalizer is applied to the keys, that were split by the regular expression. They were described in section [sec:normalizer] . The last parameter is the <span>**`weight`**</span> to boost all results on this context. The <span>**`type`**</span> of the index (for example text, int, date, or position) specifies its internal implementation and behavior.

### StoreIx

The <span>**`StoreIx`**</span> command instructs the *Hunt server* to store the current index to the local file system. The <span>**`path`**</span> parameter of this command specifies the path to the local file.

### LoadIx

To reload an index back into the memory, one can use the <span>**`LoadIx`**</span> command. Its parameter <span>**`path`**</span> is the same as the one form the <span>**`StoreIx`**</span> command.

### Sequence

A <span>**`Sequence`**</span> is special, because its JOSN representation is not a JOSN object with a <span>**`cmd`**</span> parameter, but a list of <span>**`Commands`**</span>.

Inserting a list of documents is much more efficient, that inserting one document per <span>**`Command`**</span>

Response
--------

A <span>**`Response`**</span> is a JSON object consisting of a status code <span>**`status`**</span> indicating the success of a <span>**`Command`**</span> and payload data <span>**`msg`**</span>. This payload depends on the command and on the status code. A successful execution will reply a status code <span>**`0`**</span>.

These are example <span>**`Response`**</span>s:

SearchResult
------------

This object is returned as by a <span>**`Search`**</span> command and contains a list of <span>**`Documents`**</span> (<span>**`result`**</span>), the <span>**`maximum`**</span> and <span>**`offset`**</span> given by the <span>**`Search`**</span> command and the <span>**`count`**</span> of results of that <span>**`Search`**</span> independent of the maximum.

CompletionResult
----------------

A <span>**`CompletionResult`**</span> is a ordered list of tuples, where each tuple consists of a completion and an score value. The list id ordered by the score value.

Document
--------

A <span>**`Document`**</span> is a JSON-Object, which may contain an index and a description element. The description element is a JSON-Object with an arbitrary content. This description element is returned by the <span>**`Search`**</span> commands. The index element is a JSON-Object of string elements. The names of the elements are the context names and the values are the content, that will be indexed by the

Query
-----

The <span>**`Query`**</span> language is the mechanism to retrieve <span>**`Document`**</span>s from the index. Comparable to <span>**`Command`**</span>s, <span>**`Query`**</span>s are also available in different shapes. Each shape is distinguished from others by its <span>**`type`**</span> parameter. <span>**`QWord`**</span>, <span>**`QFullWord`**</span> and <span>**`QPhrase`**</span> require a text search type parameter of type string, called <span>**`op`**</span>. This type can be one of these: “<span>**`case`**</span>” to search case sensitive, “<span>**`nocase`**</span>” to search case insensitive, or “<span>**`fuzzy`**</span>” to search for words, that do not match exactly.

### QWord

<span>**`QWord`**</span>, specified by type <span>**`word`**</span> is a prefix search for a single word. This example searches for all words starting case sensitive with “Hunt”.

### QFullWord

This search distinguishes to <span>**`QWord`**</span> by the fact, that it searches for complete words and not for prefixes. Its type is <span>**`fullword`**</span>

### QContext

This query restricts the sub query <span>**`query`**</span> the list of <span>**`contexts`**</span>.

### QBoost

Use <span>**`boost`**</span> to change the weight of this sub query.

### QRange

Details of this <span>**`range`**</span> query in described in section . This query limits the results to the given range. Use the <span>**`upper`**</span> and <span>**`lower`**</span> parameters to set the bounds of this query.

### QSeq

A <span>**`seq`**</span>uence is a list of sub queries, that are combined by an operation <span>**`op`**</span>. This operation can be “<span>**`and`**</span>” to reduce the results to the intersection of all sub queries. It can also be “<span>**`or`**</span>” to unify all sub queries, “<span>**`and not`**</span>” to subtract all sub queries form the first sub query, “<span>**`follow 1`**</span>” to find following occurrences, or “<span>**`near 1`**</span>” to find occurrences that are in a close neighborhood.


