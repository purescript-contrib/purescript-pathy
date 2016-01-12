## Module Data.Path.Pathy

#### `Rel`

``` purescript
data Rel :: *
```

The (phantom) type of relative paths.

#### `Abs`

``` purescript
data Abs :: *
```

The (phantom) type of absolute paths.

#### `File`

``` purescript
data File :: *
```

The (phantom) type of files.

#### `Dir`

``` purescript
data Dir :: *
```

The (phantom) type of directories.

#### `Unsandboxed`

``` purescript
data Unsandboxed :: *
```

The (phantom) type of unsandboxed paths.

#### `Sandboxed`

``` purescript
data Sandboxed :: *
```

The (phantom) type of sandboxed paths.

#### `FileName`

``` purescript
newtype FileName
  = FileName String
```

A newtype around a file name.

##### Instances
``` purescript
Show FileName
Eq FileName
Ord FileName
```

#### `runFileName`

``` purescript
runFileName :: FileName -> String
```

Unwraps the `FileName` newtype.

#### `DirName`

``` purescript
newtype DirName
  = DirName String
```

A newtype around a directory name.

##### Instances
``` purescript
Show DirName
Eq DirName
Ord DirName
```

#### `runDirName`

``` purescript
runDirName :: DirName -> String
```

Unwraps the `DirName` newtype.

#### `Path`

``` purescript
data Path a b s
```

A type that describes a Path. All flavors of paths are described by this
type, whether they are absolute or relative paths, whether they
refer to files or directories, whether they are sandboxed or not.

* The type parameter `a` describes whether the path is `Rel` or `Abs`.
* The type parameter `b` describes whether the path is `File` or `Dir`.
* The type parameter `s` describes whether the path is `Sandboxed` or `Unsandboxed`.

To ensure type safety, there is no way for users to create a value of
this type directly. Instead, helpers should be used, such as `rootDir`,
`currentDir`, `file`, `dir`,  `(</>)`, and `parsePath`.

This ADT allows invalid paths (e.g. paths inside files), but there is no
possible way for such paths to be constructed by user-land code. The only
"invalid path" that may be constructed is using the `parentDir'` function, e.g.
`parentDir' rootDir`, or by parsing an equivalent string such as `/../`,
but such paths are marked as unsandboxed, and may not be rendered to strings
until they are first sandboxed to some directory.

##### Instances
``` purescript
Show (Path a b s)
Eq (Path a b s)
```

#### `RelFile`

``` purescript
type RelFile s = Path Rel File s
```

A type describing a file whose location is given relative to some other,
unspecified directory (referred to as the "current directory").

#### `AbsFile`

``` purescript
type AbsFile s = Path Abs File s
```

A type describing a file whose location is absolutely specified.

#### `RelDir`

``` purescript
type RelDir s = Path Rel Dir s
```

A type describing a directory whose location is given relative to some
other, unspecified directory (referred to as the "current directory").

#### `AbsDir`

``` purescript
type AbsDir s = Path Abs Dir s
```

A type describing a directory whose location is absolutely specified.

#### `Escaper`

``` purescript
newtype Escaper
  = Escaper (String -> String)
```

Escapers encode segments or characters which have reserved meaning.

#### `runEscaper`

``` purescript
runEscaper :: Escaper -> String -> String
```

Given an escaper and a segment to encode, returns the encoded segment.

#### `posixEscaper`

``` purescript
posixEscaper :: Escaper
```

An escaper that removes all slashes, converts ".." into "$dot$dot", and
converts "." into "$dot".

#### `file`

``` purescript
file :: forall s. String -> Path Rel File s
```

Creates a path which points to a relative file of the specified name.

#### `file'`

``` purescript
file' :: forall s. FileName -> Path Rel File s
```

Creates a path which points to a relative file of the specified name.

#### `fileName`

``` purescript
fileName :: forall a s. Path a File s -> FileName
```

Retrieves the name of a file path.

#### `extension`

``` purescript
extension :: FileName -> String
```

Retrieves the extension of a file name.

#### `dropExtension`

``` purescript
dropExtension :: FileName -> FileName
```

Drops the extension on a file name.

#### `changeExtension`

``` purescript
changeExtension :: (String -> String) -> FileName -> FileName
```

Changes the extension on a file name.

#### `dir`

``` purescript
dir :: forall s. String -> Path Rel Dir s
```

Creates a path which points to a relative directory of the specified name.

#### `dir'`

``` purescript
dir' :: forall s. DirName -> Path Rel Dir s
```

Creates a path which points to a relative directory of the specified name.

#### `dirName`

``` purescript
dirName :: forall a s. Path a Dir s -> Maybe DirName
```

Retrieves the name of a directory path. Not all paths have such a name,
for example, the root or current directory.

#### `(</>)`

``` purescript
(</>) :: forall a b s. Path a Dir s -> Path Rel b s -> Path a b s
```

_left-associative / precedence 6_

Given a directory path, appends either a file or directory to the path.

#### `(<.>)`

``` purescript
(<.>) :: forall a s. Path a File s -> String -> Path a File s
```

_left-associative / precedence 6_

Sets the extension of the file to the specified extension.

```purescript
file "image" <.> "png"
```

#### `(<..>)`

``` purescript
(<..>) :: forall a b s s'. Path a Dir s -> Path Rel b s' -> Path a b Unsandboxed
```

_left-associative / precedence 6_

Ascends into the parent of the specified directory, then descends into
the specified path. The result is always unsandboxed because it may escape
its previous sandbox.

#### `isAbsolute`

``` purescript
isAbsolute :: forall a b s. Path a b s -> Boolean
```

Determines if this path is absolutely located.

#### `isRelative`

``` purescript
isRelative :: forall a b s. Path a b s -> Boolean
```

Determines if this path is relatively located.

#### `peel`

``` purescript
peel :: forall a b s. Path a b s -> Maybe (Tuple (Path a Dir s) (Either DirName FileName))
```

Peels off the last directory and the terminal file or directory name
from the path. Returns `Nothing` if there is no such pair (for example,
if the last path segment is root directory, current directory, or parent
directory).

#### `maybeDir`

``` purescript
maybeDir :: forall a b s. Path a b s -> Maybe (Path a Dir s)
```

Determines if the path refers to a directory.

#### `maybeFile`

``` purescript
maybeFile :: forall a b s. Path a b s -> Maybe (Path a File s)
```

Determines if the path refers to a file.

#### `maybeRel`

``` purescript
maybeRel :: forall a b s. Path a b s -> Maybe (Path Rel b s)
```

Determines if the path is relatively specified.

#### `maybeAbs`

``` purescript
maybeAbs :: forall a b s. Path a b s -> Maybe (Path Rel b s)
```

Determines if the path is absolutely specified.

#### `depth`

``` purescript
depth :: forall a b s. Path a b s -> Int
```

Returns the depth of the path. This may be negative in some cases, e.g.
`./../../../` has depth `-3`.

#### `parentDir`

``` purescript
parentDir :: forall a b s. Path a b s -> Maybe (Path a Dir s)
```

Attempts to extract out the parent directory of the specified path. If the
function would have to use a relative path in the return value, the function will
instead return `Nothing`.

#### `unsandbox`

``` purescript
unsandbox :: forall a b s. Path a b s -> Path a b Unsandboxed
```

Unsandboxes any path (whether sandboxed or not).

#### `parentDir'`

``` purescript
parentDir' :: forall a b s. Path a b s -> Path a Dir Unsandboxed
```

Creates a path that points to the parent directory of the specified path.
This function always unsandboxes the path.

#### `currentDir`

``` purescript
currentDir :: forall s. Path Rel Dir s
```

The "current directory", which can be used to define relatively-located resources.

#### `rootDir`

``` purescript
rootDir :: forall s. Path Abs Dir s
```

The root directory, which can be used to define absolutely-located resources.

#### `renameFile`

``` purescript
renameFile :: forall a s. (FileName -> FileName) -> Path a File s -> Path a File s
```

Renames a file path.

#### `renameDir`

``` purescript
renameDir :: forall a s. (DirName -> DirName) -> Path a Dir s -> Path a Dir s
```

Renames a directory path. Note: This is a simple rename of the terminal
directory name, not a "move".

#### `canonicalize`

``` purescript
canonicalize :: forall a b s. Path a b s -> Path a b s
```

Canonicalizes a path, by reducing things in the form `/x/../` to just `/x/`.

#### `unsafePrintPath'`

``` purescript
unsafePrintPath' :: forall a b s. Escaper -> Path a b s -> String
```

#### `unsafePrintPath`

``` purescript
unsafePrintPath :: forall a b s. Path a b s -> String
```

#### `printPath`

``` purescript
printPath :: forall a b. Path a b Sandboxed -> String
```

Prints a `Path` into its canonical `String` representation. For security
reasons, the path must be sandboxed before it can be rendered to a string.

#### `printPath'`

``` purescript
printPath' :: forall a b. Escaper -> Path a b Sandboxed -> String
```

Prints a `Path` into its canonical `String` representation, using the
specified escaper to escape special characters in path segments. For
security reasons, the path must be sandboxed before rendering to string.

#### `identicalPath`

``` purescript
identicalPath :: forall a a' b b' s s'. Path a b s -> Path a' b' s' -> Boolean
```

Determines if two paths have the exact same representation. Note that
two paths may represent the same path even if they have different
representations!

#### `relativeTo`

``` purescript
relativeTo :: forall a b s s'. Path a b s -> Path a Dir s' -> Maybe (Path Rel b s')
```

Makes one path relative to another reference path, if possible, otherwise
returns `Nothing`. The returned path inherits the sandbox settings of the
reference path.

Note there are some cases this function cannot handle.

#### `sandbox`

``` purescript
sandbox :: forall a b s. Path a Dir Sandboxed -> Path a b s -> Maybe (Path Rel b Sandboxed)
```

Attempts to sandbox a path relative to some directory. If successful, the sandboxed
directory will be returned relative to the sandbox directory (although this can easily
be converted into an absolute path using `</>`).

This combinator can be used to ensure that paths which originate from user-code
cannot access data outside a given directory.

#### `refine`

``` purescript
refine :: forall a b s. (FileName -> FileName) -> (DirName -> DirName) -> Path a b s -> Path a b s
```

Refines path segments but does not change anything else.

#### `parsePath`

``` purescript
parsePath :: forall z. (RelFile Unsandboxed -> z) -> (AbsFile Unsandboxed -> z) -> (RelDir Unsandboxed -> z) -> (AbsDir Unsandboxed -> z) -> String -> z
```

Parses a canonical `String` representation of a path into a `Path` value.
Note that in order to be unambiguous, trailing directories should be
marked with a trailing slash character (`'/'`).

#### `parseRelFile`

``` purescript
parseRelFile :: String -> Maybe (RelFile Unsandboxed)
```

Attempts to parse a relative file from a string.

#### `parseAbsFile`

``` purescript
parseAbsFile :: String -> Maybe (AbsFile Unsandboxed)
```

Attempts to parse an absolute file from a string.

#### `parseRelDir`

``` purescript
parseRelDir :: String -> Maybe (RelDir Unsandboxed)
```

Attempts to parse a relative directory from a string.

#### `parseAbsDir`

``` purescript
parseAbsDir :: String -> Maybe (AbsDir Unsandboxed)
```

Attempts to parse an absolute directory from a string.

#### `fold`

``` purescript
fold :: forall c a b s. (FileName -> c -> c) -> (DirName -> c -> c) -> c -> Path a b s -> c
```

Folds a `Path` into a value using two specified functions and a provided
default value.

The first function folds a `FileName` into our value, and the second
function folds a `DirName` into our value.


