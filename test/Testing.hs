{-# LANGUAGE ScopedTypeVariables #-}
module Testing where

-- Module that allows wrapping QuickCheck with HSpec and vice-versa.
--
-- The goal of these utilities - which are pretty hacky - is to be able to
--  - write test modules without needing to worry about importing them into
--    a central place. HSpec makes this pretty easy with the
--    `hspec-discover` GHC options
--  - write property tests as top-level module functions without needing to
--    worry about explicitly running them. This is possible via QuickCheck's
--    $quickcheckAll, but then it cannot be composed with the HSpec functionality
--    described above.
--
-- The recommended workflow is:
--  - write normal Specs in any way you want to (all in one AST, or as separate
--    objects brought together manually)
--  - write property tests that you want to embed in a Spec in one of two ways
--    - inline them (as lambdas or via let/while) into the Specs to put them
--      alongside the specs
--    - use a name that is *not* of the form `prop_.*` and explicitly call them
--      in the Spec if you want to keep them alongside the specs but as separate
--      functions. I'd suggest using `specprop_.*` as the prefix in this case.
--  - For all other property tests, just use `prop_.*` as the naming convention,
--    and include the following snippet at the bottom of your test module:
--    ```
--    return []  -- needed by TemplateHaskell
--    moduleProps :: QuickCheckProps
--    moduleProps = $allProperties
--    ```
--    and then
--    ```
--    spec :: Spec
--    spec = hspec $ do
--      testAllProps moduleProps
--      ...any other specs to run...
--    ```
--
-- Note that if you don't explicitly need hspec functionality, you can use no-arg
-- Testable instances (either Property or Boolean) to do unit tests as well as
-- property tests, so in many cases it might make sense to use HSpec exclusively
-- for generating the top-level test suite. This workflow, having all tests be
-- module-level property declarations or functions, is similar to pytest in that
-- it's lightweight and based around just functions plus naming conventions.
--
-- Are there better options? Probably yes, there are a couple of testing frameworks
-- that try to sit on top of the various libraries. But most of these frameworks
-- seem much less widely used than HSpec and QuickCheck, so maybe using

import Control.Monad (forM_)
import Control.Exception (handle, SomeException)
import Test.Hspec (hspec, describe, it, Spec)
import Test.QuickCheck (Property, withMaxSuccess)
import Test.QuickCheck.Monadic (monadicIO, run)



-- type returned by quickcheck's $allProperties
type QuickCheckProps = [(String, Property)]

-- given a list of (name, property) pairs (e.g. the output of
-- quickcheck's $allProperties), 
testProps :: String -> QuickCheckProps -> Spec
testProps name props = describe name $ do
  forM_ props runPropertyTest
  where runPropertyTest (n, p) = it n p


-- special case of testProperties where the name is set reasonably
-- for passing in $allProperties
testAllProps :: QuickCheckProps -> Spec
testAllProps props = testProps "All quickcheck props" props


-- tool to take an hspec unit test suite and run it as a
-- QuickCHeck property. This is almost certainly a bad idea, it incurs
-- a lot of overhead, but I thought it was a cool thing to have figured out
-- so I'm keeping it for reference, event though it isn't useful.
propFromSpec :: Spec -> Property
propFromSpec spec = withMaxSuccess 1 $ monadicIO $ do
  let indentedSpec = describe "" $ describe "" $ spec
  run $ handle
    (\(_ :: SomeException) -> pure False)
    ((const True) <$> (hspec indentedSpec)) 

