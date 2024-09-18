module Ch10 where

import Prelude

import Data.List (List(..), (:))


class Foldable f where
  foldr :: ∀ a b. (a -> b -> b) -> b -> f a -> b
  foldl :: ∀ a b. (b -> a -> b) -> b -> f a -> b
  foldMap :: ∀ a m. Monoid m => (a -> m) -> f a -> m


instance foldableList :: Foldable List where
  foldr _ acc Nil = acc
  foldr f acc (x : xs) = f x (foldr f acc xs)

  foldl _ acc Nil = acc
  foldl f acc (x : xs) = foldl f (f acc x) xs

  foldMap _ Nil = mempty
  foldMap f l = foldl (\acc x -> acc <> f x) mempty l


length :: ∀ a. List a -> Int
length = go 0 where
  go acc Nil = acc
  go acc (x : xs) = go (1 + acc) xs 

lengthF :: ∀ a. List a -> Int
lengthF = foldl (\acc _ -> 1 + acc ) 0 