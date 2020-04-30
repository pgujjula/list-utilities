# tsil
_tsil_ is a list utilities library in Haskell (tsil is "list" backwards).

## Features
_tsil_ is a new project, but the following features have been implemented and extensively tested:
* `Data.List.Digit`: Convert integers to a list of digits, and vice versa.
* `Data.List.Transform`: Filter lists in new ways, group elements, delete duplicates, and rotate lists by an integer offset.
* `Data.List.Ordered.Transform`: Diff, union, and intersect ordered lists. Also perform complicated merges, including **merging infinite lists of infinite lists**.
* `Data.List.Predicate`: Predicates on lists, such as `palindrome`, `allEqual`, `sorted`, and `allUnique`.

## Installation
Build and install with [Stack](https://www.haskellstack.org):

```
git clone https://github.com/pgujjula/tsil
cd tsil
stack build                 # build the project
stack haddock tsil --open   # build and view documentation
stack test                  # run the test suite
```

## Bug Reports/Feature Requests
Feel free to email me at preetham (dot) gujjula (at) protonmail (dot) com!

-- Preetham