{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module TestingSpec where
import Testing
import Test.Hspec
import Test.QuickCheck


-- A normal spec
test_addition :: Spec
test_addition = describe "addition" $ do
  it "should work properly in unit tests" $ do
    (1 :: Int) + 1 `shouldBe` 2
    (1 :: Int) + 4 `shouldBe` 5
  it "should have a property check" $ property specprop_addition


specprop_addition :: Int -> Int -> Property
specprop_addition x y = x + y === y + x


-- this property won't get picked up

-- some typical stand-alone properties, both as Boolean and as Property
--
-- The first two take no args, so they are effectively like unit tests
--
-- The next two do take args, so they are more typical quickcheck properties

prop_addition_unitTestBool :: Bool
prop_addition_unitTestBool = (1 :: Int) + 1 == 2

prop_addition_unitTestProperty :: Property
prop_addition_unitTestProperty = (1 :: Int) + 1 === 2

prop_addition_shouldWorkWithBool :: Int -> Int -> Bool
prop_addition_shouldWorkWithBool x y = x + y == y + x

prop_addition_shouldWorkWithProperty :: Int -> Int -> Property
prop_addition_shouldWorkWithProperty x y = x + y === y + x


-- Check that we can convert a Spec into a Property
-- ... this is probably a bad idea, I recommend avoiding it
prop_run_multiplication_spec :: Property
prop_run_multiplication_spec = propFromSpec $ describe "multiplication" $ do
  it "should work properly in unit tests" $ do
    (1 :: Int) * 1 `shouldBe` 1
    (1 :: Int) * 3 `shouldBe` 3


-- Standard end-of-file boilerplate --------------------------------------------------------

return []  -- needed by TemplateHaskell
moduleProps :: QuickCheckProps
moduleProps = $allProperties


spec :: Spec
spec = do
 test_addition
 -- run all property tests with hspec logging
 testAllProps moduleProps
