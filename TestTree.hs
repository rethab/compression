{-# LANGUAGE TemplateHaskell #-}

import Control.Monad (liftM, liftM2)
import Test.QuickCheck
import Test.QuickCheck.All

import Tree

runTests :: IO Bool
runTests = $quickCheckAll

main = runTests >>= \passed -> if passed then putStrLn "All tests passed"
                                         else putStrLn "Some tests failed"

-- equivalence function for floating point based on relative error
infix 4 ~=~
(~=~) :: (Ord a, Floating a) => a -> a -> Bool
(~=~) 0 b = (abs (b - 0)) < 0.001
(~=~) a b = (abs (b - a)) / a < 0.001

newtype Leaves a = Leaves [Tree a] deriving (Show)

instance (Arbitrary a) => Arbitrary (Leaves a) where
    arbitrary = liftM Leaves kids
        where kids = listOf $ liftM2 Leaf arbitrary positiveWeight
              positiveWeight = suchThat arbitrary (\i -> i > 0)

listweight = foldl (\acc k -> weight k + acc) 0

prop_preseve_weight_once (Leaves kids) =
    listweight (combineSmallest kids) ~=~ listweight kids

prop_preserve_weight_total (Leaves kids) = not (null kids) ==> 
    weight (combineAll kids) ~=~ listweight kids

prop_one_node_less (Leaves kids) = length kids > 1 ==>
    length (combineSmallest kids) == (length kids - 1)

prop_combined_leaves_are_smallest (Leaves kids) = length kids > 1 ==>
    let combined = combineSmallest kids
        Node r l = head $ filter (not . isLeaf) combined
        -- bigger leaf of the two is still smaller than all others
        bigger = if weight r > weight l then weight r else weight l
    in all (\l -> weight l >= bigger) combined
