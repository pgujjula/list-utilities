# list-utilies
The _list-utilities_ project is a group of utility packages for lists in Haskell.

## Features
_list-utilities_ is a new project, but the following packages have been implemented and extensively tested:
* `list-predicate`: Predicates (True/False) queries on lists.
* `list-filter`: Supplement the filters available in the standard library.
* `list-duplicate`: Work with duplicates in lists.

See the individual package READMEs and hackage for more documentation.

## Installation
Build and install with [Stack](https://www.haskellstack.org):

```
git clone https://github.com/pgujjula/list-utilities
cd list-utilities
stack build                           # build the project
stack haddock list-utilities --open   # build and view documentation
stack test                            # run the test suite
```

## Bug Reports/Feature Requests
Feel free to email me at preetham (dot) gujjula (at) protonmail (dot) com!

-- Preetham
