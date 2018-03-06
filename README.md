# gerlang
_Calling Go functions from Erlang_

April 2018: This is a work in progress.

## The aim
<span style="color: red;">Allow Erlang programs to call Go functions with minimum inconvenience</span>

It's certainly _possible_ for an Erlang program to call a function written in Go, but it's frustratingly
difficult: write an Erlang [NIF](http://erlang.org/doc/tutorial/nif.html) in C and then use
[CGO](https://github.com/golang/go/wiki/cgo) to call the Go function.

The aim of this project is to make the process simple enough that calling Go from Erlang is easy.

## How It Will Work

* An Erlang module (`ergo`) provides various functions to call into Go.
  * `ergo:call` will call a Golang function, returning the function's result.
  * Other stuff to be decided to handle Erlang mailboxes / Golang channels etc.
* The Erlang module calls out to a small NIF, which in turn calls the Golang `Call` function.
* `Call` looks up the target function (a named function within a Golang plugin) and translates
  Erlang parameters ("terms") into the types required by the Golang function.
* The function is called, and its return value(s) converted back to Erlang terms.

## Golang Plugins

Yuk.

It would be nice if you could call any Golang function in any package. TODO explain why not.

Instead the go functions must be packaged up as a [plugin](https://golang.org/pkg/plugin/).    



##
    go build  -buildmode=c-shared -o gerlang.so && erl



