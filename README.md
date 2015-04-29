# purescript-pathy

A type-safe abstraction for platform-independent file system paths.

# Example

```purescript
fullPath = rootDir </> dir "baz" </> file "foo.png"
```

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
 * `currentDir` &mdash; The current directory (AKA the "working directory"), useful for describing relative paths.
 * `file`       &mdash; A file (in the current directory).
 * `dir`        &mdash; A directory (in the current directory).
 * `(</>)`      &mdash; Combines two paths into one, if the composition makes sense!
 * `(<.>)`      &mdash; Sets the extension of a file path.

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

Pathy also carries information on whether a path is a file or directory, and whether it's been sandboxed to some known directory.

### Paths from Strings

`parsePath`

### Paths to Strings

`printPath`

### Basic Path Manipulation

`parentDir`

`parentDir'`

`sandbox`


# API Docs

[MODULES.md](MODULES.md)
