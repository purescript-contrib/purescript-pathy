# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:
- Update project and deps to PureScript v0.15.0 (#50 by @JordanMartinez)

New features:

Bugfixes:

Other improvements:
- Added `purs-tidy` formatter (#49 by @thomashoneyman)

## [v8.1.0](https://github.com/purescript-contrib/purescript-pathy/releases/tag/v8.1.0) - 2021-05-06

New features:
- Exported `escape` implemented by @safareli in #33 (#46 by @JordanMartinez)

Other improvements:
- Fixed warnings revealed by v0.14.1 PS release (#46 by @JordanMartinez)
- Installed missing dependencies used in source code (#46 by @JordanMartinez)

## [v8.0.0](https://github.com/purescript-contrib/purescript-pathy/releases/tag/v8.0.0) - 2021-02-26

Breaking changes:
- Added support for PureScript 0.14 and dropped support for all previous versions (#42)

New features:
- Added roles declarations to forbid unsafe coercions (#43)

Bugfixes:

Other improvements:
- Changed default branch to `main` from `master`
- Updated to comply with Contributors library guidelines by adding new issue and pull request templates, updating documentation, and migrating to Spago for local development and CI (#41)

## [v7.0.1](https://github.com/purescript-contrib/purescript-pathy/releases/tag/v7.0.1) - 2019-06-04

- Updated for PureScript 0.13

## [v7.0.0](https://github.com/purescript-contrib/purescript-pathy/releases/tag/v7.0.0) - 2019-03-04

- Updated dependencies

## [v6.0.0](https://github.com/purescript-contrib/purescript-pathy/releases/tag/v6.0.0) - 2018-06-28

- Updates for PureScript 0.12

## [v5.0.0](https://github.com/purescript-contrib/purescript-pathy/releases/tag/v5.0.0) - 2018-03-20

- Removed `Sandboxed` slot from `Pathy` type.
- Added better way of Sandboxing.
- Used `NonEmptyString` for segment names.
- Made runtime representation of Pathy simpler.
- Many more: see #33 (@safareli, @garyb)

## [v4.1.0](https://github.com/purescript-contrib/purescript-pathy/releases/tag/v4.1.0) - 2017-12-21

- Added `Gen` module - (#31 by @safareli)

## [v4.0.0](https://github.com/purescript-contrib/purescript-pathy/releases/tag/v4.0.0) - 2017-04-04

- Updated for PureScript 0.11
- Removed deprecated `Data.Generic` instances.

## [v3.0.2](https://github.com/purescript-contrib/purescript-pathy/releases/tag/v3.0.2) - 2016-12-21

- Fixed shadowed name warning
- Fixed license identifier for Pursuit publishing

## [v3.0.1](https://github.com/purescript-contrib/purescript-pathy/releases/tag/v3.0.1) - 2016-11-14

- Fixed `Ord` instance

## [v3.0.0](https://github.com/purescript-contrib/purescript-pathy/releases/tag/v3.0.0) - 2016-10-28

- Updated dependencies for PureScript 0.10

## [v2.0.0](https://github.com/purescript-contrib/purescript-pathy/releases/tag/v2.0.0) - 2016-07-28

- The `Escaper` is no longer ignored when printing paths
- Directories are consistently on the left and files consistently on the right in `Either`s and function arguments now

## [v0.3.3](https://github.com/purescript-contrib/purescript-pathy/releases/tag/v0.3.3) - 2016-04-21

- Fixed the exports for v0.3.2

## [v0.3.2](https://github.com/purescript-contrib/purescript-pathy/releases/tag/v0.3.2) - 2016-04-21

- Added synonyms for where a path is either a directory or file.
- Added `pathName` to extract the name of a directory or file.

## [v0.3.1](https://github.com/purescript-contrib/purescript-pathy/releases/tag/v0.3.1) - 2016-01-21

- Fixed various warnings raised by psc 0.7.6.1

## [v0.2.1](https://github.com/purescript-contrib/purescript-pathy/releases/tag/v0.2.1) - 2015-08-14

- Functionally identical to v0.2.0, this update is to allow publishing on http://pursuit.purescript.org/

## [v0.1.0](https://github.com/purescript-contrib/purescript-pathy/releases/tag/v0.1.0) - 2015-04-29

- Initial release
