# list-filter
_list-filter_ is a library for more filters than are available in the standard
libraries

## Features
_list-filter_ is a new project, but the following features have been implemented
and extensively tested:
  * `takeEvery`, `dropEvery`: Take or drop every nth element
  * `takeUntil`, `dropUntil`: Like `takeWhile`, `dropWhile`, but keep the
    element that satisfies/falsifies the predicate.

Full documentation can be built with Haddock (see instructions below), and is
also available on Hackage.

## Installation
Build and install with [Stack](https://www.haskellstack.org):

```
git clone https://github.com/pgujjula/list-utilities
cd list-utilities/list-filter
stack build list-filter           # build the project
stack haddock list-filter --open  # build and view documentation
stack test                        # run the test suite
```

## Contact
If you want to report a bug, request a feature, or suggest improvements, feel
free to email me at preetham (dot) gujjula (at) protonmail (dot) com!

-- Preetham

