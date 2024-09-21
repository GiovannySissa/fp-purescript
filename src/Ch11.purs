module Ch11 where

import Prelude hiding (max)
import Prelude (Unit, show, type (~>), ($), (<>))

import Data.Generic.Rep (class Generic)
import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.List (List(..), (:), singleton)
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Semiring (class Semiring, zero)
import Data.Show.Generic (genericShow)
-- import Data.Semigroup.Foldable (foldl1)
import Effect (Effect)
import Effect.Console (log)

reverse :: List ~> List
reverse = foldl (\lr next -> next : lr) Nil
reverseTest :: Effect Unit
reverseTest = do
  log "========================================="
  log "Reverse"
  log "========================================="
  log $show $reverse (10 : 20 : 30 : Nil)
  log $show $max (-1) 99
  log $show $max "aa" "z"

max :: ∀ a. Ord a => a -> a -> a
max x y 
  | x > y = x
  | otherwise = y


findMax' :: ∀ a. Ord a => a -> List a -> a
findMax' m Nil = m
findMax' m (x : xs) = findMax' (max x m) xs

findMax'' :: ∀ a. Ord a => List a -> Maybe a
findMax'' Nil = Nothing
findMax'' l@(first : _) = Just $ go first l where
  go mx Nil = mx
  go mx (x : xs) = go (max x mx) xs
-- findMax'' (x : xs) = max (Just x) (findMax'' xs) this is not tail recursive

findMax :: ∀ a. Ord a => List a -> Maybe a
findMax Nil = Nothing
-- findMax l@(first : xs) = Just $ foldl(\mx next -> max mx next) first xs without eta reduction
findMax l@(first : _) = Just $ foldl max first l 
findMax l@(first : _) = Just $ foldl max first l 

findMaxTest :: Effect Unit 
findMaxTest = do 
  log "========================================="
  log "FindMax"
  log "========================================="
  log $show $findMax' 0 (37 : 311 : -1 : 2 : 84 : Nil)  
  log $show $findMax' "" ("a" : "bbb" : "c" : Nil)
  log $show $findMax (37 : 311 : -1 : 2 : 84 : Nil)  
  log $show $findMax ("a" : "bbb" : "c" : Nil)
  log $show $findMax ("a0": Nil)

foldl1 :: ∀ a f. Foldable f => (a -> a -> a) -> NonEmpty f a -> a
foldl1 f (x :| xs) = foldl f x xs


findMaxNE :: ∀ a. Ord a => NonEmptyList a -> a
-- findMaxNE (NonEmptyList (NonEmpty x xs)) = foldl max x xs 
findMaxNE (NonEmptyList ne) = foldl1 max ne 


findMaxNETest :: Effect Unit
findMaxNETest = do 
  log "========================================="
  log "FindMaxNE"
  log "========================================="
  log $show $findMaxNE (NonEmptyList $ 37 :| (311 : -1 : 2 : 84 : Nil))
  log $show $findMaxNE (NonEmptyList $ "a" :| ("bbb" : "c" : Nil))


sum' :: List Int -> Int
sum' Nil = 0
sum' (x : xs) = x + sum' xs

sum'' :: List Int -> Int
sum'' l = go 0 l where
  go sl Nil = sl
  go sp (x : xs ) = go (sp + x) xs

sum''' :: List Int -> Int
sum''' = foldl(+) 0

sum :: ∀ f a. Foldable f => Semiring a => f a -> a
sum = foldl(+) zero

sumTest :: Effect Unit
sumTest = do
  log "========================================="
  log "Sum"
  log "========================================="
  log $ show $ sum' (1 : 2 : 3 : Nil)
  log $ show $ sum'' (4 : 2 : 3 : Nil)
  log $ show $ sum''' (40 : 1232 : 23 : Nil)
  log $ show $ sum (4.9 : 0.1: Nil)
  log $ show $ sum (4 : 6: Nil)
  log $ show $ sum [4, 6]

data Tree e = Leaf e | Node (Tree e) (Tree e)
derive instance genericTree :: Generic (Tree a) _
instance showTree :: Show a =>  Show (Tree a) where
  show = genericShow


foldableTreeTest :: Effect Unit
foldableTreeTest = do
  log "========================================="
  log "Foldable Tree"
  log "========================================="
  log $ show $ toList'(Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14) ) ) (Leaf 99))
  log $ show $ sum(Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14) ) ) (Leaf 99))
  

toList' :: ∀ a. Tree a -> List a
-- toList :: Tree ~> List
toList' (Leaf x) = singleton x
toList' (Node lt rt) = toList' lt <> toList' rt

instance foldableTree :: Foldable Tree where
  foldr f acc = foldr f acc <<< toList'
  foldl f acc = foldl f acc <<< toList'
  foldMap f = foldMap f <<< toList'
 
step1 :: String -> String
step1  s = "step1" <> ", " <> s

step2 :: String -> String
step2 s = "step2" <> ", " <> s

step3 :: String -> String
step3 s = "step3" <> ", " <> s

steps :: String -> String
steps x = 
  step3
  $ step2  
  $ step1 x

steps' :: String -> String
steps' x = x 
  # step1
  # step2
  # step3

class ToList f where
  toList :: ∀ a. f a -> List a


newtype RightFirstTree a = RightFirstTree(Tree a)

instance  toListRightFirstTree :: ToList RightFirstTree where
  toList (RightFirstTree (Leaf a)) = singleton a
  toList (RightFirstTree (Node lt rt)) = toList (RightFirstTree rt) <> toList (RightFirstTree lt)


instance foldableRightFirstTree :: Foldable RightFirstTree where
  foldr f b = foldr f b <<< toList
  foldl f b = foldl f b <<< toList
  foldMap f = foldMap f <<< toList

newtype LeftFirstTree a = LeftFirstTree(Tree a)
instance  toListLeftFirstTree :: ToList LeftFirstTree where
  toList (LeftFirstTree (Leaf a)) = singleton a
  toList (LeftFirstTree (Node lt rt)) = toList (LeftFirstTree lt) <> toList (LeftFirstTree rt)


instance foldableLeftFirstTree :: Foldable LeftFirstTree where
  foldr f b = foldr f b <<< toList
  foldl f b = foldl f b <<< toList
  foldMap f = foldMap f <<< toList


treeTest :: Effect Unit
treeTest = do
  log "========================================="
  log "Right Tree"
  log "========================================="
  log $ show $ toList $ RightFirstTree(Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14) ) ) (Leaf 99))
  log $ show $ sum $ RightFirstTree(Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14) ) ) (Leaf 99))
  log "========================================="
  log "Left Tree"
  log "========================================="
  log $ show $ toList $ LeftFirstTree(Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14) ) ) (Leaf 99))
  log $ show $ sum $ LeftFirstTree(Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14) ) ) (Leaf 99))


testFlipAp:: Effect Unit 
testFlipAp = do 
  log $ steps "sample"
  log $ steps' "sample"



test :: Effect Unit
test = do
  reverseTest
  findMaxTest
  findMaxNETest
  sumTest
  foldableTreeTest
  testFlipAp
  treeTest