# Homepage for the Hunt framework

Hunt is a fast, flexible and lightweight search platform written in
Haskell. This repository provides the source of the documentation,
packaged up as a homepage, where you can learn all about it.


## Setting up

This homepage is written with the popular static site generator named
[Hugo][1]. As such, if you want to help out with this documentation,
first you need to [install][2] Hugo.

Then follow these steps and fire up your favorite editor to edit or
style this site.

```bash
$ git clone https://github.com/hunt-framework/huntsearch.org
$ cd huntsearch.org/
$ hugo server -D
```


## Organization

This repository is roughly organized around the following directories.

- *content* contains the actual content of the documentation. This is
  were you would change typos or improve the content.
- *static* contains all static files, like javascript, stylesheets or
  images.
- *layouts* contains all files, which define, *how* the files from the
  content directory should be rendered to HTML. It's powered by the
  built-in templating language from the Go programming language. You
  can read more about the layouts directory in Hugos documentation
  [here][3].

That should provide you with enough information to get started helping
out! Happy documenting.

[1]: https://gohugo.io/
[2]: https://gohugo.io/getting-started/installing/
[3]: https://gohugo.io/templates/
