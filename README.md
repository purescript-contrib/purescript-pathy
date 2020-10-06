# Pathy

[![CI](https://github.com/purescript-contrib/purescript-pathy/workflows/CI/badge.svg?branch=main)](https://github.com/purescript-contrib/purescript-pathy/actions?query=workflow%3ACI+branch%3Amain)
[![Release](https://img.shields.io/github/release/purescript-contrib/purescript-pathy.svg)](https://github.com/purescript-contrib/purescript-pathy/releases)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-pathy/badge)](https://pursuit.purescript.org/packages/purescript-pathy)
[![Maintainer: garyb](https://img.shields.io/badge/maintainer-garyb-teal.svg)](https://github.com/garyb)
[![Maintainer: thomashoneyman](https://img.shields.io/badge/maintainer-thomashoneyman-teal.svg)](https://github.com/thomashoneyman)

A type-safe abstraction for platform-independent file system paths.

## Installation

Install `pathy` with [Spago](https://github.com/purescript/spago):

```sh
spago install pathy
```

## Quick start

Applications often have to refer to file system paths in a platform-independent way.

Many path libraries provide a single abstraction to deal with file system paths. This allows easy composition of different kinds of paths, but comes at the expense of the following distinctions:

 * The distinction between relative and absolute paths.
 * The distinction between paths denoting file resources and paths denoting directories.

Pathy also uses a single abstraction for file system paths, called `Path`, but uses *phantom types* to keep track of the above distinctions.

This approach lets you write code that performs type-safe composition of relative, absolute, file, and directory paths, and makes sure you never use paths in an unsafe fashion. Bogus and insecure operations simply aren't allowed by the type system!

Many paths come from user-input or configuration data. Pathy can parse such string paths and allow you to safely resolve them to expected types.

### Paths Literals

Building path literals is easy using the following components:

 * `rootDir` – The root directory of an absolute path.
 * `currentDir` – The current directory (AKA the "working directory"), useful for building relative paths.
 * `file` – A file (in the current directory).
 * `dir` – A directory (in the current directory).
 * `(</>)` – Adds a relative path to the end of a (relative or absolute) path.
 * `(<.>)` – Sets the extension of a file path.
 * `(<..>)` – Ascends one level in a directory, then descends into the specified relative path.

All path segments (`file` / `dir`) names are required to be non-empty. This is enforced by `Name` being constructed from a `NonEmptyString`. At compile time, we can have provably non-empty strings by using `Symbol`s and a bit of type class trickery:

``` purescript
dirFoo :: Name Dir
dirFoo = dir (SProxy :: SProxy "foo")
```

Here we're using a symbol proxy (`SProxy`) and then typing it to explicitly carry the name that we want to use for our path at runtime. There is also a `dir'` and `file'` variation on the function that accepts normal `Name` values, so if you are not constructing a path at compile-time, you'd be using these instead.

Some example compile-time path constructions:

```purescript
path1 = rootDir </> dir (SProxy :: SProxy "foo") </> dir (SProxy :: SProxy "bar") </> file (SProxy :: SProxy "baz.boo")
path2 = currentDir </> dir (SProxy :: SProxy "foo")
```

Thanks to the phantom type parameters, Pathy doesn't let you create path combinations that don't make sense. The following examples will be rejected at compile time:

```purescript
rootDir </> rootDir
currentDir </> rootDir
file (SProxy :: SProxy "foo") </> file (SProxy :: SProxy "bar")
file (SProxy :: SProxy "foo") </> dir (SProxy :: SProxy "bar")
```

### The Path Type

The `Path a b` type has two type parameters:

 * `a` – This may be `Abs` or `Rel`, indicating whether the path is absolute or relative.
 * `b` – This may be `Dir` or `File`, indicating whether the path is a file or directory.

You should try to make the `Path` functions that you write as generic as possible. If you have a function that only cares if a path refers to a file, then you can write it like this:

```purescript
myFunction :: forall a. Path a File -> ...
myFunction p = ...
```

By universally quantifying over the type parameters you don't care about, you ensure your code will work with the most paths possible (you also are documenting the expectations of your function to other developers who read your code).

### Parse Paths from Strings

To parse a string into a `Path`, you can use the `parsePath` function, which expects you to handle four cases:

 * `Path Rel File`
 * `Path Abs File`
 * `Path Rel Dir`
 * `Path Abs Dir`

If you need a specific case, you can use helper functions such as `parseRelFile`, which return a `Maybe`.

The `parsePath` function also expects a `Parser` argument so that different path formats can be parsed into the common `Path` type.

### Sandboxing

Pathy makes it easy to create relative paths, even paths that ascend into parent directories of relative paths. With this power comes danger: if you parse a user string, the user may be able to escape any arbitrary directory.

Pathy solves this security problem by *disallowing* conversion from a `Path` to a `String` until the `Path` has been *sandboxed*.

To sandbox a path, you just call `sandbox` and provide the sandbox directory, as well as the path to sandbox:

```purescript
sandbox
  (rootDir </> dir (SProxy :: SProxy "foo")) -- sandbox root
  (rootDir </> dir (SProxy :: SProxy "foo") </> dir (SProxy :: SProxy "bar")) -- path to sandbox
```

This returns a `Maybe`, which is `Nothing` if the tainted path escapes the sandbox.

After you have sandboxed a foreign path, you may call `printPath` on it, which will print the path absolutely.

There is also the option to `unsafePrintPath`. This is labelled as being unsafe as it may be depending on how it is used - for example, if a path was sandboxed against some path other than the current working directory, but then used when launching a command in the current working directory, it may still refer to a location that it should not have access to.

### Renaming, Transforming, Etc.

There are many other functions available to you for renaming files, renaming directories, getting parent directories, etc.

These are all documented [on Pursuit](http://pursuit.purescript.org/packages/purescript-pathy), and you can find [test usages](/test/Main.purs) for most of them.

## Documentation

`pathy` documentation is stored in a few places:

1. Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-pathy).
2. Usage examples can be found in [the test suite](./test).

If you get stuck, there are several ways to get help:

- [Open an issue](https://github.com/purescript-contrib/purescript-pathy/issues) if you have encountered a bug or problem.
- [Search or start a thread on the PureScript Discourse](https://discourse.purescript.org) if you have general questions. You can also ask questions in the `#purescript` and `#purescript-beginners` channels on the [Functional Programming Slack](https://functionalprogramming.slack.com) ([invite link](https://fpchat-invite.herokuapp.com/)).

## Contributing

You can contribute to `pathy` in several ways:

1. If you encounter a problem or have a question, please [open an issue](https://github.com/purescript-contrib/purescript-pathy/issues). We'll do our best to work with you to resolve or answer it.

2. If you would like to contribute code, tests, or documentation, please [read the contributor guide](./CONTRIBUTING.md). It's a short, helpful introduction to contributing to this library, including development instructions.

3. If you have written a library, tutorial, guide, or other resource based on this package, please share it on the [PureScript Discourse](https://discourse.purescript.org)! Writing libraries and learning resources are a great way to help this library succeed.
