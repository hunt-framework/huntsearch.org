Using Hunt from Haskell
=======================

Introduction
------------

Even though Hunt is written in Haskell, it is possible to use it in every language that supports the JSON api. It was a design goal to make the usage of Hunt independent of any programming language. On the other hand, client applications written in Haskell have an advantage, that they can share common data types and client interfaces with the search engine.

In this chapter we will go into the details of using the *Hunt server* with a client written in Haskell, using Hunt directly without the need of a server and using the client interface for building queries and commands. Finally, we will extend Hunt with a new index implementation.

The following sections need a working developed environment, which was set up in the first chapter [subsec:devenv].

Building a front end web server
-------------------------------

A typical case of using the *Hunt server* is to be a back end of a web page containing a search engine. The *Hunt server* holds an index of relevant data and is accessed by a front end, which displays the search results.

This section will use the ScottyFarmer (2014) web server as an example.

### How to do it

Please create a subfolder named `clientExample` and save this file as `Main.hs`.

<!--bsp/Adding_Functionality/clientExample/Main.hs-->

Our Haskell file references an `index.html` file, which should have this content:

<!--bsp/Adding_Functionality/clientExample/index.html-->

The corresponding Cabal file `clientExample.cabal` should look like listing [lst:frontendCabal]:

<!--bsp/Adding_Functionality/clientExample/frontend.cabal-->

In order to run this example one should have a running *Hunt server* listening on port `3000`.

One should be able to start the *Hunt server* by running this command:

``` {.text}
cabal run
```

A simple input mask should appear after opening <http://localhost:8080> in a browser and a click on the submit button should return a HTML table containing the content of each returned Hunt document.

### How it works

We have built a web server that handles search requests, acts as a proxy by forwarding the search requests to the *Hunt server* and renders the resulting documents as a HTML table.

the can see that the default port of the Scotty webserver is `3000`. Therefore we need to change our port to a different value. In listing [lst:frontendmain] we have chosen port `8080` in line 15. Line 18 binds the URL path `/` to our `index.html` file. The path `/search` is bound in line 19 to a handler, which asks the *Hunt server* for documents. These documents are fetched in line 22 and renderd as HTML in line 24. The function `getQuery` in line 23 returns a data structure `LimitedResult`, which is used to hold meta informations about the result of a search request:

<!--bsp/Adding_Functionality/intro/LimitedResult.hs-->
This data structure contains the number of results, the offset of the first result, the maximum number of returned results and a list of results. The type of the results is bound in line 31 to `ApiDocument`, which contains the document’s description, the document’s URI and the document’s index. The index is empty for search results, because the *Hunt server* just returns the description.

Our `index.html` is simple. It just contains a HTML form, which calls `/search` and adds the query parameter as an HTTP-GET request.

By using Cabal, we can use the package management mechanism, which is configured in the `clientExample.cabal` file. The interesting section is the list of package dependencies, which contains dependencies to the Hunt framework.

### Background

This example is a simplified version of the Hayoo front end. One major difference is the usage of a `ReaderT` monad transformer, which is used to cache the HTTP-connection between search requests. The example to demonstrate the client interface would be a little bit longer with increased complexity. The monad transformer to store the store the HTTP-connection can be found in the Hayoo repository on GitHub[1]. In a production server, one would typically combine the `HuntConnectionT` Monad-Transformer in combination with `ActionT` Transformer from the scotty package and a self defined transformer into a single monad transformer stack to hide all the details of the communication with the behind that monad transformer stack.

The architecture of our frontend is comparable to the database importer in Section [sec:cli-importer]. We use the `hunt-client` library to talk to the *Hunt server*, because this library provides an implement of the communication protocol to the . All we had to do was to execute the `HuntConnection` Monad and process the search results.

Usually, the *Hunt server* should not be directly visible form the Internet, but instead be hidden behind a firewall or a inside a local network, because the json api is not password protected. Also, not using scotty as the frontend gives us the benefit of having a simple web server for static resources like css files or images. Figure [fig:huntExample] shows the architecture that we were using here. As one can see, the scotty is embedded between the reverse proxy and the Hunt search engine.

![Hunt example architecture](bilder/hunt-example)

[fig:huntExample]

By going through the architecture, we can see, that the reverser proxy will just pass through all relevant requests to our web application and delivers the generated HTML output to the client. The application server processes each request and generates JSON search-queries for the *Hunt server*. The *Hunt server* then answers the search with a result list and the application server processes the result and generates a response for the proxy.

As we have seen, all we had to do to convert Lighttpd to be a reverse proxy was adding a configuration block to the `lighttpd.config` file. The of the configuration are skipped here.

Providing OpenSearch suggestions
--------------------------------

Often one would like to provide OpenSearch [opensearch.org](opensearch.org) compatible autocompletion responses to get a better browser integration in modern browsers. OpenSearch is a specification for publishing search results in an accessible format. Wikipedia (2014). OpenSearch was developed by Amazon in 2005. To get suggestions in a format suitable for OpenSearch, the `ClientInterface` provides a function OpenSearch-suggestions.

We are going to extend the sample webserver from the previous section with an OpenSearch integration by adding the necessary files and URL handlers.

### How to do it

The OpenSearch specification is based on a XML file containing a description of the search engine. Such a description could look like this:

<!--bsp/Adding_Functionality/opensearch/opensearch.xml-->
Save this file as `opensearch.xml` and add this URL handler below the other ones:

``` {.haskell}
get "/opensearch.xml" (
    fileWithMime "opensearch.xml"
        "application/opensearchdescription+xml")
```

One should now be able to download the `opensearch.xml` file by opening <http://localhost:8080/opensearch.xml> in your browser. Adding this link to the `head` element of the `index.html` page will hint browsers to the OpenSearch description file:

``` {.xml}
<head>
  <link rel="search" title="Example Search"
    href="opensearch.xml"
    type="application/opensearchdescription+xml"/>
</head>
```

Now, we can add the completion handler to the list of URL handlers:

One should now get search suggestions in the browser, although one may need to configure the browser to use these suggestions.

### How it works

We added the `opensearch.xml` file to the URL handler. This was necessary, because the OpenSearch specification requires such a file to describe the functionality of the search engine. Lines 4 and 5 change the display name of the search engine, which is for example shown in the address bar. Lines 11 to 13 describe the usage of the completion handler by providing the URL with the `query` parameter and the type. The last element of the `opensearch.xml` is the description of the URL used for displaying the results in lines 14 to 16.

Two new URL handlers were added: Line 10 handles the `opensearch.xml`, which is similar to the index.html handler. The other one in line 13 handles completion requests. This handler asks for the `query` parameter and calles the hunt server for completions in line 15. The query is the parsed in line. 18 .

The completion feature of Hunt returns a list of words for the last query element. Often one would like to get completions for whole queries and not just the last query element. Fortunately, `Hunt.ClientInterface` exports a function `completeQuery` with the signature `Query -> [Text] -> [Query]`, which wraps each completion into its own query.

The last step is the serialization of the data structure to a form that is compatible to the OpenSearch specification in line 21.

### Background

The reason not to complete whole queries in the first place is the ability to generate queries in a front end where only a sub query is generated by the user. For example one could imagine a web interface to search for cars with select boxes for the brand and the model and a text input field to restrict the query further. Such a front end could build queries like `brand:audi AND model:a4 AND almost new`. The end user is not interested in completions for the generated query, but for the string in the text input field. In this case, `completeQuery` could be used to generate completions for the last element of the original query.

Another approach to get completions for complete queries without the need of a front end written in Haskell is to use the javascript function to process the autocompletion result for the *Hunt server* admin interface. It can be found in on the github page of Hunt.

Indexing a database
-------------------

Often the content of a database needs to be imported into the Hunt search engine. The Hunt framework can be combined with Yesod’s Persistance frameworkSnoyman (2013) to provide an importer to import documents into the *Hunt server*. Although not a requirement, it is wise to use the hunt-client package to communicate to the *Hunt server*. The api of the hunt-client package consists of wrappers for sending commands and using the search and the completion API.

### How to do it

This is an example program[2] to insert documents into a *Hunt server*:

<!--lst:databaseExample}{bsp/Adding_Functionality/database/Main.hs-->
We can execute this program by running

``` {.text}
cabal exec runhaskell Main.hs
```

Its output should look like this:

``` {.text}
Migrating: CREATE TABLE "person"("id" INTEGER PRIMARY KEY
    ,"name" VARCHAR NOT NULL,"age" INTEGER NOT NULL)
"{\"msg\":\"ok\",\"code\":0}"
```

Searching the documents can be done by invoking:

``` {.text}
hunt-server-cli search doe
```

As usual, this prints both documents.

### How it works

This example is based on the example for the Persistent packageSnoyman (2013). Therefore details related to Persistent are omitted.

Line 41 of listing [lst:databaseExample] creates a connection to a , which is stacked onto a connection to a Sqlite database. This monad transformer stack forces to lift all functions that are part of the persistent framework into its monad transformer, for example in line 42.

Line 47 creates a `conduit`Snoyman (2013)[3] pipe, which converts all `Entity`s into Hunt’s `ApiDocument`s. We use `selectSource` to generate a `conduit` `Source`. The `conduit` `Sink` combines all generated `Command`s into a sequence of inserts.

We use this sequence to generate a simple Hunt schema in line 53 and prepending it before the insert commands in line 54.

Finally we use the `eval` function from the `Hunt.Server.Client` module and print the result to the terminal.

### Background

In this example, we have indexed a whole database table into Hunt. A common strategy for updating an existing index, in contrast to creating a new one, is to add a table column storing time of the last successful indexing of each entity Ku<span>ć</span> (2013). By adding this kind of field, one can easily build an SQL select statement containing only the latest documents that are not currently indexed at the moment.

Instead of letting *persistent* automatically generaet database select statements, one can also use hand written ones. These custom statements allow the conversion from foreign keys to existing database entities to prevent the indexing of numerical identifiers, which do not provide useful information. Instead, foreign keys can be stored in the document’s description. This implementation is not usable without writing Haskell code. A future release could add a command line utility with an ODBC backend Weyburne (2014), which could be parameterized by the ODBC connect string and a SQL select statement to automatically index a database in Hunt.

Using Hunt without the server
-----------------------------

This section was written in Sauer and Reumann (2014) p. 54 - 55:

The Hunt framework provides a powerful interpreter abstraction. This abstraction makes it easily possible to write a custom client implementation. It is also possible to integrate the search engine as a library into existing applications. In this section, two examples are presented.

### A Command Line Client

Although a web interface is the most prevalent interface for this purpose, it is not necessary. For simple tests it might be useful to have a command line client to run test queries. This section describes how easily this can be implemented using the Hunt framework.

To keep this example simple, the index is not built from scratch, but loaded from disk into the command line application. The command line program parses text queries and performs them on the index.

<!--bsp/Adding_Functionality/terminal/Main.hs-->

The client expects the path to an existing index file as the first argument (line 3). Then, the Hunt interpreter is initialized with default values and an empty index (line 4). After that, the index is loaded from disk to memory with the `LoadIx` command (line 5). With `getQuery`, queries are read from the command line and passed to the query parser. If the query is valid, it is wrapped in the search command for the interpreter and the search results are displayed on the screen.[4]

Running Hunt inside Yesod
-------------------------

This section was written in Sauer and Reumann (2014) p. 56 - 58:

The Yesod web framework[5] is one of the most popular web frameworks in the Haskell community. There are a lot of web applications already running on this platform. This section shows, how to integrate the Hunt search engine into a Yesod application. The example could easily be transferred to other frameworks like Snap[6].

### Creating a SubSite

An abstraction provided by Yesod is the `SubSite`. A reusable web application component can be wrapped into a `SubSite` and then be used with arbitrary Yesod applications. The Hunt search engine is meant to be such a reusable component. The following part illustrated the integration of Hunt as a `SubSite`.[7]

The first line defines a wrapper type. It is the foundation of of the `SubSite` implementation. The initHuntS function helps to initialize the search engine with its default configuration. Then the `SubSite`’s typeclass is defined with the name YesodHunt. In lines 10 to 14, the `SubSite`’s web routes are defined. This is done with the help of a domain specific language. For this small example it is sufficient to create three routes for the different search options.
<!--bsp/Adding_Functionality/yesod/Handler.hs-->

Since the Yesod routes are generally exposed to the public, it would not be a good idea to expose Hunt’s administrative interface here. Listing [lst:yesodhandler] shows the implementation of the route handler.

HuntHandler is essentially an auxiliary type, to run the `SubSite` within the main Yesod application. The same way runHunt is an auxiliary function to execute Hunt commands within the application. This works the same way as the default server implementation, but runs in Yesod’s Handler monad.
<!--bsp/Adding_Functionality/yesod/HandlerImpl.hs-->

Listing [lst:yesodhandlerimpl] shows a simple request handler for the paged search route. The other two request handlers are implemented in the same way. The query string is read by calling parseQuery from Hunt’s query interface. If it is successfully parsed, the resulting Query is wrapped into a Search command and executed. Besides some helper functions and a bit of Yesod-specific code, this is all the code needed, to implement the `SubSite`.

### Creating a SubSite

Listing [lst:subsiteimpl] shows the use of the `SubSite` in a Yesod application. Since the code regarding Hunt is encapsulated within the `SubSite`, the rest of the integration is fairly trivial Yesod code.[8]

<!--bsp/Adding_Functionality/yesod/SubSiteImpl.hs-->

Creating a new index implementation
-----------------------------------

It may be the case, that no existing index implementation suits the needs for a specific use. In this situation one could, for example, index three dimensional structures, or us an existing backend as an index. Additionally, one would like to continue to use the existing Hunt-server with it’s json interface. In this case, one need to extend the current set of implementations with a new one and integrate it into the current infrastructure. In this recipe, we will use the existing demo index implementation that is using an `IntMap`.

### How to do it

First, open the index implementation that is using the `IntMap` from the `containers` Glasgow (2014) package. This index is located in the github repository in the directory `hunt-demos/extended-hunt/src/Main.hs`.

We are interested in the instance declaration for the `Index` class. It begins with

<!--bsp/Adding_Functionality/IntMap/head.hs-->

Copy this implementation and all required imports into

`hunt-searchengine/src/Index/IntMap.hs`

Additionally, one need also some other data structures, like the inverted index or a `Bijection` instance. These are also located there:

<!--bsp/Adding_Functionality/IntMap/inverted.hs-->

After copying them into `hunt-searchengine/src/Index/IntMap.hs`, one need to integrate the implementation into the searchengine by adding `cRealInt` to the list of pre defined context types located on `hunt-searchengine/src/Interpreter.hs`:

<!--bsp/Adding_Functionality/IntMap/newcontext.hs-->

then rebuild `hunt-server` by executing

``` {.text}
make install && make startServer
```

You should now be able to create a new context with this new implementation:

<!--bsp/Adding_Functionality/IntMap/context.json-->

### How it works

The interesting code lines are in listing [lst:intmapInstance] the instance of the `Index` type class. This type class uses `Int`s as index keys and an `IntMap` from the containers package as its implementation. This `IntMap` is optimized for binary operations like union and intersection and is also faster on operations like insert or delete in comparison to `Data.Map`Glasgow (2014).

Future work
===========

The current *Hunt server* is bound to a single system: It is impossible to distribute the index across multiple systems. There are different ideas and approaches of how to design a distributed Hunt platform. There will be a lot of complexity inherited by the fact, that a part of this distributed system can fail. There are existing middlewares, that can help to handle this case, for example Cloud Haskell[9] or Holumbus-Distribution[10]. On top of this middleware needs to be a system to scatter the indexes through all nodes. An interesting aspect of this concept will be the handling of complex queries across different nodes without decreasing.

Updating a context by adding or removing occurrences can be a costly operation. This cost can be reduces by caching changes and keeping snapshots of an index while modifying that cache. Such a cache should be transparent to the rest of the platform. This can especially useful for use cases with a high volume of updates to existing indexes.

There is a snapshot mechanism described in section [section:restart] . Creating a snapshot is not practical, if the index data is constantly changing. One might think of a journal between Snapshots, which tracks all insert, delete and update commands. Such a journal can be truncated, as soon as the new snapshot is written to the disk and could be as simple as appending the JSON serialization to that journal.

All current Index implementations of the *Hunt Search Engine Platform* are in-memory data structures, hence restricted by the amount of memory of the system. Adding a mutable disk-based data structure would increase the maximum number of indexed documents beyond the current limit. There is already an immutable B\*-Tree on Hackage[11].

There are two normalizers that are implemented in the : upper case and lower case. There is currently no normalizer to perfrom stamming of words. Stamming is a process of removing grammatical modifications from words. Implementing such a stammer would gratly increase the search results for free text searches.

Farmer, Andrew. 2014. “Scotty Web Server.” https://github.com/scotty-web/scotty. [\\url{https://github.com/scotty-web/scotty}](\url{https://github.com/scotty-web/scotty}).

Glasgow, The University Court of the University of. 2014. “containers” (version 0.5.5.1). http://hackage.haskell.org/package/containers.

Ku<span>ć</span>, Rafal. 2013. *Apache Solr 4 Cookbook*. Quick Answers to Common Problems. Packet Publishing.

Sauer, Ulf, and Christian Reumann. 2014. “Hunt. Implementing a Highly Flexible Search Engine Platform in Haskell.” Master’s thesis, FH-Wedel.

Snoyman, Michael. 2013. *Yesod Web Framework*. http://www.yesodweb.com/book/; O’Reilly Media. [\\url{http://www.yesodweb.com/book/}](\url{http://www.yesodweb.com/book/}).

Weyburne, Grant. 2014. “persistent-Odbc.” http://hackage.haskell.org/package/persistent-odbc-0.1.2.2. [\\url{http://hackage.haskell.org/package/persistent-odbc-0.1.2.2}](\url{http://hackage.haskell.org/package/persistent-odbc-0.1.2.2}).

Wikipedia. 2014. “OpenSearch — Wikipedia, The Free Encyclopedia.” [\\url{http://en.wikipedia.org/w/index.php?title=OpenSearch&oldid=600275839}](\url{http://en.wikipedia.org/w/index.php?title=OpenSearch&oldid=600275839}).

[1] Hayoo repository: <https://github.com/hunt-framework/hayoo>

[2] Example Program: <https://github.com/hunt-framework/hunt/blob/master/hunt-client/examples/hunt-persistent/Main.hs>

[3] conduit: <http://hackage.haskell.org/package/conduit>

[4] The example focuses on the important aspects, boilerplate code like output formatting is stripped. The complete source code for this example is available on GitHub: <https://github.com/> hunt-framework/.

[5] Yesod web framework: <http://www.yesodweb.com/>

[6] Snap: <http://snapframework.com/>

[7] More on Yesod `SubSite`s: <http://www.yesodweb.com/book/creating-a-subsite>

[8] The complete source code for this example is available on GitHub: <https://github.com/hunt-framework/.>

[9] Cloud Haskell: <http://www.haskell.org/haskellwiki/Cloud_Haskell>

[10] Holumbus-Distribution: <https://hackage.haskell.org/package/Holumbus-Distribution>

[11] Disk-based B\*-Tree: <https://hackage.haskell.org/package/b-tree>

