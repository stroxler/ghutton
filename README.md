# ghutton


This repo exists for me to work through two resources at the same time, as part
of learning Haskell (and refreshing myself on the basics):
 - some of the more interesting examples from Graham Hutton's *Programming in Haskell*
 - a bunch of resources for learning testing in haskell, focused on both hspec and
   quickcheck but with more of an eye toward quickcheck. In particular:
   - [A John Hughes talk from Lambda Days '19'](https://www.youtube.com/watch?v=NcJOiQlzlXQ)
     on expansions to QuickCheck to build on developer intuitions


## Bootstrap

To create the project:
```
stack new ghutton
```

Then, add `hspec` and `uickcheck` to the test dependencies in `package.yaml`.

To run the tests:
```
stack test --file-watch
```

To get a test-enabled interactive ghci session:
```
stack ghci --test --main-is test
```

In production, you probably want to set the seed and you may want to increase the
number of test cases (default is just 100) by using the flag (note that it's one flag
at the bash level, which a subparser splits into two)
```
--test-arguments="--seed=666 --qc-max-successes=1000"
```

For the most part the only reason to use ghci on regular specs is to get type information
for debugging, but it can make a lot of sense to run quickcheck manually for a single test
(and also potentially play around in the repl with various tools), e.g.:
```
> quickCheck prop_whatever
> sample myGen
>
```


Once that is working, we can rename `Lib` to `Countdown` and start adding additional
library / test modules as we please(this doesn't require changing any config files).


## Automate wiring tests

By default, when you write haskell unit and property tests you usually have to


I did a bit of digging on ho
