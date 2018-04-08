# gerlang
_Calling Go functions from Erlang_

April 2018: This is a work in progress.

## The aim
_Allow Erlang programs to call Go functions with minimum inconvenience._

It's certainly _possible_ for an Erlang program to call a function written in Go, but it's frustratingly
difficult. You must write an Erlang [NIF](http://erlang.org/doc/tutorial/nif.html) in C,
translate Erlang terms into Golang data structures, and then use
[CGO](https://github.com/golang/go/wiki/cgo) to call the Go function.

For many projects this overhead is too big, and most developers would just re-implement the
Golang code in Erlang. The aim of this project is to make the process simple enough that calling
Go from Erlang is easy.


## How It Will Work

*   A generator program accepts a list of packages to be processed.
*   The generator produces everything that is needed to call the exported Go functions
    from Erlang:
    *   An Erlang source module containing function definitions for each exported
        function and variable in the package. 
    *   A NIF to translate between Erlang terms and Golang data structures. 
    *   A shared object containing the compiled Golang packages, plus code to interact with the NIF. 


## What Didn't Work

https://punch.photoshelter.com/image/I0000ZvleumhOmDo

### NIFs and Golang Plugins

This work is saved in branch `nif-plugin-experiment`.

* An Erlang module (`ergo`) provided various functions to call into Go.
* The Erlang module called out to a small NIF, which in turn called the Golang `Call` function.
* `Call` looked up the target function (a named function within a Golang plugin) and translated
  Erlang parameters ("terms") into the types required by the Golang function.
* The function was called, and its return value(s) converted back to Erlang terms.

This was abandoned because Golang [does not support](https://github.com/golang/go/issues/18123)
a Shared Object (`-buildmode=c-shared`) calling a Golang plugin.

### Erlang Ports

An [Erlang port](http://erlang.org/doc/tutorial/c_port.html) is a common way to call out
to external programs from Erlang, but is way too heavyweight for my purposes.

### C Nodes and JInterface

Using a technique such as [Erlang C Nodes](http://erlang.org/doc/tutorial/cnode.html) or
[JInterface](http://erlang.org/doc/apps/jinterface/jinterface_users_guide.html)  is an interesting
idea, but doesn't fit in with the lightweight approach I'm looking for.
