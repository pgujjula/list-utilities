# list-duplicate
_list-duplicate_ is a library for working with duplicates in lists.

## Features
_list-duplicate_ is a new project, but the following features have been implemented
and extensively tested:
  * `group`, `groupBy`: Group duplicate elements
  * `groupAdj`, `groupAdj`: Group duplicate adjacent elements in a list. Also
    useful for grouping the elements of a sorted list.
  * `deleteDups`, `deleteDupsBy`: Delete duplicates from a list.
  * `deleteAdjDups`, `deleteAdjDupsBy`: Delete adjacent duplicates from a list.
    Also useful for deleting duplicates from a sorted list.

Full documentation can be built with Haddock (see instructions below), and is
also available on Hackage.

## Installation
Build and install with [Stack](https://www.haskellstack.org):

```
git clone https://github.com/pgujjula/list-utilities
cd list-utilities/list-duplicate
stack build list-duplicate           # build the project
stack haddock list-duplicate --open  # build and view documentation
stack test                           # run the test suite
```

## Contact
If you want to report a bug, request a feature, or suggest improvements, feel
free to email me at preetham (dot) gujjula (at) protonmail (dot) com!

-- Preetham
