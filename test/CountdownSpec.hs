{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module CountdownSpec where
import qualified Testing
import Test.Hspec
import Test.QuickCheck
import Countdown


-- simple examples of arbitrary

-- Note that these examples actually cover about 90% of the
-- things you would do in arbitrary:
--  - use monadic or applicative interfaces to bootstrap
--  - use `elements`
--  - use `suchThat` to restrict conditions

instance Arbitrary Op where
  arbitrary = elements [Add, Mul, Sub, Div]

positiveInt :: Gen Int
positiveInt = suchThat arbitrary (> 0)

data OpTestCase = OpTestCase Op Int Int
  deriving (Show, Eq)

instance Arbitrary OpTestCase where
  arbitrary = OpTestCase <$> arbitrary <*> positiveInt <*> positiveInt

prop_valid_seemsToWork :: OpTestCase -> Property
prop_valid_seemsToWork (OpTestCase o x y) =
  let isValid = valid o x y
  in case o of
    Add -> isValid === True
    Mul -> isValid === True
    Sub -> isValid === (x - y > 0)
    Div -> isValid === (x `rem` y == 0)



newtype ListTestCase a = ListTestCase [a]
  deriving (Show, Eq)

instance (Arbitrary a) => Arbitrary (ListTestCase a) where
  arbitrary = ListTestCase <$> arbitrary `suchThat` ((< 6) . length)


-- Using property tests as unit tests
-- ... overall I think I prefer normal specs

prop_evalSanityCheck_Val :: Property
prop_evalSanityCheck_Val = eval (Val 15) === Just 15

prop_evalSanityCheck_Appl :: Property
prop_evalSanityCheck_Appl = eval (Apply Add (Val 4) (Val 15)) === Just 19

prop_evalSanityCheck_Appl2 :: Property
prop_evalSanityCheck_Appl2 = eval (Apply Add (Apply Sub (Val 4) (Val 2)) (Val 15)) === Just 17


instance Arbitrary Expr where
  arbitrary = oneof [ Val <$> positiveInt
                    , Apply <$> arbitrary <*> arbitrary <*> arbitrary]
  -- this shrink implementation probably isn't very good, but I tried
  -- to preserve the invariants. If I exposed `apply` I could probably do better
  --
  -- That said, I actually seem to be getting pretty good results... try deleting
  -- changing the x0 `div` x1 in prop_evalAndMathLineUp to x0 `div` x0 and you'll
  -- probably notice if you delete this line and paste it back, you pretty reliably
  -- get Apply Div (Val 1) (Val 2) when this shrink is live, and less-friendly examples
  -- when it's removed
  shrink (Val x) = [Val xx | xx <- shrink x, xx > 0]
  shrink (Apply op x y) =
    [x, y] ++ [Apply op xx yy | (xx, yy) <- shrink (x, y)]


prop_evalAndMathLineUp :: Expr -> Property
prop_evalAndMathLineUp expr =
  eval expr === expected expr  -- I get a parse error if I don't make expected a function /shrug
  where
    expected e = case e of
      Val n ->
        if n > 0
        then Just n
        else Nothing
      Apply o e0 e1 ->
        case (eval e0, eval e1) of
          (Just x0, Just x1) ->
              if x > 0 then Just x else Nothing
              where
                x = case o of
                  Add -> x0 + x1
                  Sub -> x0 - x1
                  Mul -> x0 * x1
                  Div -> x0 `div` x1
          _ -> Nothing


-- One "unit" type test and a bunch of sanity checks of the subsequences funciton.
-- This is a nicer example of where property tests work, although it's a bit tricky
-- to write great tests without using Set

prop_subsequences_passesSanityChecks :: Property
prop_subsequences_passesSanityChecks = subsequences ([] :: [Int]) === [[]]

prop_subsequences_passesCardinalityCheck :: ListTestCase Int -> Property
prop_subsequences_passesCardinalityCheck (ListTestCase xs) = length(subsequences xs) === 2 ^ (length xs)

prop_subsequences_containsEmpty :: ListTestCase Int -> Bool
prop_subsequences_containsEmpty (ListTestCase xs) = [] `elem` (subsequences xs)

prop_subsequences_containsArg :: ListTestCase Int -> Bool
prop_subsequences_containsArg (ListTestCase xs) = xs `elem` (subsequences xs)

prop_subsequences_containsSingletons :: ListTestCase Int -> Bool
prop_subsequences_containsSingletons (ListTestCase xs) =
  let xss = subsequences xs
  in all (\x -> [x] `elem` xss) xs

-- This last test case uses a filter; the `forAll` function spits out a
-- new Property given a Gen and a function consuming the output to produce a
-- Property; the Gen controls the input which can effectively act as a filter (although
-- other use cases do exist, e.g. you could make a Gen that's more likely to hit edge cases)
prop_subsequences_containsPairs :: Property
prop_subsequences_containsPairs = (forAll atLeastTwo) containsFirstTwo
  where
    atLeastTwo :: (Arbitrary a) => Gen (ListTestCase a)
    atLeastTwo = arbitrary `suchThat` (\(ListTestCase x) -> length x >= 2)
    containsFirstTwo :: (ListTestCase Int) -> Bool
    containsFirstTwo (ListTestCase xs) = case xs of
        (x0:x1:_) -> [x0, x1] `elem` (subsequences xs)
        _ -> False


prop_values_additive :: Expr -> Property
prop_values_additive e@(Val n) = values e === [n]
prop_values_additive e@(Apply _ x y) = length (values e) === length (values x) + length (values y)

prop_values_preservesFirst :: Expr -> Property
prop_values_preservesFirst e@(Val n) = values e === [n]
prop_values_preservesFirst e@(Apply _ x y) =
  head (values e) === head (values x)
  .&&. head (values y) `elem` values e


prop_allInserts_sanityCheck :: Property
prop_allInserts_sanityCheck = (allInserts (5 :: Int) [2, 3]) === [[5, 2, 3], [2, 5, 3], [2, 3, 5]]

prop_allInserts_length :: Int -> ListTestCase Int -> Property
prop_allInserts_length x (ListTestCase xs) = length (allInserts x xs) === 1 + length xs

prop_allInserts_hasX :: Int -> ListTestCase Int -> Bool
prop_allInserts_hasX x (ListTestCase xs) = (all (\xs_ -> x `elem` xs_) (allInserts x xs))


prop_permutations_sanityCheck :: Property
prop_permutations_sanityCheck = permutations [1 :: Int, 2] === [[1, 2], [2, 1]]

prop_permutations_containsOriginal :: ListTestCase Int -> Bool
prop_permutations_containsOriginal (ListTestCase (xs :: [Int])) = xs `elem` (permutations xs)

prop_permutations_containsReversed :: ListTestCase Int -> Bool
prop_permutations_containsReversed (ListTestCase (xs :: [Int])) = (reverse xs) `elem` (permutations xs)

prop_permutations_containsRotated :: Property
prop_permutations_containsRotated = (forAll atLeastOne) containsRotatedOriginal
  where
    atLeastOne :: (Arbitrary a) => Gen (ListTestCase a)
    atLeastOne = arbitrary `suchThat` (\(ListTestCase x) -> length x >= 1)
    containsRotatedOriginal (ListTestCase (x:xs)) = xs ++ [(x :: Int)] `elem` (permutations (x:xs))
    containsRotatedOriginal _ = False

return []
moduleProps :: Testing.QuickCheckProps
moduleProps = $allProperties


spec :: Spec
spec = do
  describe "valid" $ do
    it "passes sanity checks" $ do
      valid Add 3 4 `shouldBe` True
      valid Mul 3 4 `shouldBe` True
      valid Sub 3 4 `shouldBe` False
      valid Sub 4 3 `shouldBe` True
      valid Div 3 4 `shouldBe` False
      valid Div 4 2 `shouldBe` True
    it "Is always valid for 2 1 as args" $ property $
      -- Note that you can use `shouldBe` in Properties!
      -- This is probably pretty idiomatic in the case when most tests are HSpec-based
      \o -> valid o 2 1 `shouldBe` True
  -- NOTE: for all of these functions, I ran out of steam to use property testing and
  -- decided to just finish the chapter with some trivial unit tests. The sad thing is that
  -- this is actually where the property testing might have been more fun; the earlier functions
  -- were hard to test and actually easier to verify on concrete test cases for the most part
  describe "choices" $ do
    it "should pass basic smoke tests" $ do
      choices ([] :: [Int]) `shouldBe` [[]]
      length (choices [(1 :: Int), 2]) `shouldBe` 5
  describe "isSolution" $ do
    it "should pan out on basic Vals" $ do
      isSolution (Val 3) [3] 3 `shouldBe` True
      isSolution (Val 3) [3] 4 `shouldBe` False
      isSolution (Val 3) [4] 3 `shouldBe` False
      isSolution (Val (-1)) [4] (-1) `shouldBe` False
    it "should pan out on somewhat more complex Vals" $ do
      isSolution (Apply Add (Val 1) (Val 2)) [1, 2] 3 `shouldBe` True
      isSolution (Apply Add (Apply Sub (Val 3) (Val 2)) (Val 1)) [1, 2, 3] 2 `shouldBe` True
      isSolution (Apply Add (Apply Sub (Val 2) (Val 1)) (Val 3)) [1, 2, 3] 4 `shouldBe` True
      isSolution (Apply Add (Apply Sub (Val 1) (Val 2)) (Val 3)) [1, 2, 3] 4 `shouldBe` False
      isSolution (Apply Add (Val 2) (Val 2)) [1, 2, 3] 4 `shouldBe` False
  describe "splits" $ do
    it "should do the right thing on some small lists" $ do
      splits ['a', 'b', 'c'] `shouldBe` [(['a'], ['b', 'c']), (['a', 'b'], ['c'])]
  describe "solutions" $ do
    it "should work" $ property $
      \(ListTestCase xs, target) ->
        if (all (>0) xs && target > 0)
        then all (\e -> isSolution e xs target) (solutions xs target)
        else True

  -- run all property tests with hspec logging
  Testing.testAllProps moduleProps
