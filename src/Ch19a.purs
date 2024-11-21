module Ch19a where

import Prelude

import Data.Tuple (Tuple(..))
import Data.Show.Generic (genericShow)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)

newtype Writer w a = Writer (Tuple a w)
derive  instance newtypeWriter :: Newtype(Writer w a) _

derive instance genericWriter :: Generic(Writer w a) _

instance showWriter :: (Show w, Show a) => Show (Writer w a) where
  show = genericShow


instance functorWriter :: Functor(Writer a) where
  map f (Writer(Tuple x l)) = Writer (Tuple (f $ x) l)

instance applyWriter :: Monoid a => Apply (Writer a) where
  apply = ap

instance applicativeWriter :: Monoid a => Applicative (Writer a) where
  pure x = Writer(Tuple x mempty)

instance bind0Writer :: Monoid w => Bind (Writer w) where
  bind (Writer (Tuple a e)) mf = 
    mf a # \(Writer (Tuple a1 e1)) -> Writer(Tuple a1 (e <> e1))

instance monadWriter :: Monoid w => Monad(Writer w)