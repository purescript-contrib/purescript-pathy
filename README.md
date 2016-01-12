# purescript-pathy

A type-safe abstraction for platform-independent file system paths.

# Example

```purescript
fullPath = rootDir </> dir "baz" </> file "foo.png"
```

See the [examples file](/examples/src/Examples.purs) for more.

# Getting Started

## Installation

```bash
bower install purescript-pathy
```

```purescript
import Data.Path.Pathy
```

## Introduction

Applications often have to refer to file system paths in a platform-independent way. 

Many path libraries provide a single abstraction to deal with file system paths. This allows easy composition of different kinds of paths, but comes at the expense of the following distinctions:

 * The distinction between relative and absolute paths.
 * The distinction between paths denoting file resources and paths denoting directories.
 * The distinction between paths that are secure (sandboxed to some location in the file system) and those that are insecure.

*Pathy* also uses a single abstraction for file system paths, called `Path`, but uses *phantom types* to keep track of the above distinctions.

This approach lets you write code that performs type-safe composition of relative, absolute, file, and directory paths, and makes sure you never use paths in an unsafe fashion. Bogus and insecure operations simply aren't allowed by the type system!

Many paths come from user-input or configuration data. Pathy can parse such string paths and allow you to safely resolve them to expected types.

### Paths Literals

Building path liberals is easy. You will typically build path literals from the following components:

 * `rootDir`    &mdash; The root directory of an absolute path.
 * `currentDir` &mdash; The current directory (AKA the "working directory"), useful for building relative paths.
 * `file`       &mdash; A file (in the current directory).
 * `dir`        &mdash; A directory (in the current directory).
 * `(</>)`      &mdash; Adds a relative path to the end of a (relative or absolute) path.
 * `(<.>)`      &mdash; Sets the extension of a file path.
 * `(<..>)`     &mdash; Ascends one level in a directory, then descends into the specified relative path.

For example:

```purescript
let 
  path1 = rootDir    </> dir "foo" </> dir "bar" </> file "baz.boo"
  path2 = currentDir </> dir "foo"
in do
  trace $ show $ printPath path1
  trace $ show $ printPath path2
```

Pathy doesn't let you create combinators that don't make sense, such as:

```purescript
rootDir    </> rootDir
currentDir </> rootDir
file "foo" </> file "bar"
file "foo" </> dir "bar"
```

All these combinations will be disallowed at compile time!

### The Path Type

The `Path a b s` type has three type parameters:

 * `a` &mdash; This may be `Abs` or `Rel`, indicating whether the path is absolute or relative.
 * `b` &mdash; This may be `Dir` or `File`, indicating whether the path is a file or directory.
 * `s` &mdash; This may be `Sandboxed` or `Unsandboxed`, indicating whether the path has been sandboxed yet or not.

You should try to make the `Path` functions that you write as generic as possible. If you have a function that only cares if a path refers to a file, then you can write it like this:

```purescript
myFunction :: forall a s. Path a File s -> ...
myFunction p = ...
```

By universally quantifying over the type parameters you don't care about, you ensure your code will work with the most paths possible (you also are documenting the expectations of your function to other developers who read your code).

### Parse Paths from Strings

To parse a string into a `Path`, you can use the `parsePath` function, which expects you to handle four cases:

 * `Path Rel File Unsandboxed`
 * `Path Abs File Unsandboxed`
 * `Path Rel Dir Unsandboxed`
 * `Path Abs Dir Unsandboxed`

If you need a specific case, you can use helper functions such as `parseRelFile`, which return a `Maybe`.

### Print Paths to Strings

You can print any path as a `String` by calling the `printPath` function.

For security reasons, you can only perform this operation if you have *sandboxed* the path. Sandboxing a path ensures that users cannot escape a sandbox directory that you specify; it's the right thing to do!

### Sandboxing

Pathy makes it easy to create relative paths, even paths that ascend into parent directories of relative paths.

With this power comes danger: if you parse a user string, the user may be able to escape any arbitrary directory.

Pathy solves this security problem by *disallowing* conversion from a `Path` to a `String` until the `Path` has been *sandboxed*.

To sandbox a path, you just call `sandbox` and provide the sandbox directory, as well as the path to sandbox:

```purescript
sandbox (rootDir </> dir "foo") (rootDir </> dir "foo" </> dir "bar")
```

This returns a `Maybe`, which is either equal to `Nothing` if the tainted path escapes the sandbox, or `Just p`, where `p` is the tainted path, relative to the sandbox path.

After you have sandboxed a foreign path, you may call `printPath` on it. There's no need to remember this rule because it's enforced at compile-time by phantom types!

All the path literals you build by hand are automatically sandboxed, unless you call `parentDir'` on them.

### Renaming, Transforming, Etc.

There are many other functions available to you for renaming files, renaming directories, getting parent directories, etc.

These are all documented in [MODULES.md](MODULES.md), and you can find [examples](/examples/Examples.purs) for most of them.

# API Docs

For complete documentation on all functions and types, see [MODULES.md](MODULES.md).
