module Parser where

import Prelude

import Data.Unfoldable (class Unfoldable,replicate, none)
import Data.Traversable (sequence, class Traversable)

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (uncons, fromCharArray)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

import Effect (Effect)
import Effect.Console (log)

type ParserState a = Tuple String a

-- other ways to model it! 
-- data ParserState a = ParserState String a
-- newtype ParserState a = ParserState(Tuple String a) 

class ParserError (e :: Type) where
  eof :: e

-- data MyError = EOF | InvalidThing
-- instance parserErrorMyError :: ParserError MyError where
--   eof = EOF

data PError = EOF
derive instance genericPError :: Generic PError _

instance showPError :: Show PError where
  show  = genericShow

instance parserErrorPError :: ParserError PError where
  eof = EOF

type ParseFunction e a = ParserError e =>  String -> Either e (ParserState a)
newtype Parser e a = Parser (ParseFunction e a)

{-type classes needed for applicative parser-}

instance functorParser :: Functor (Parser e) where
  map:: ∀ a b. (a -> b) -> Parser e a -> Parser e b 
  map f g = Parser \s ->  map f <$> parse g s


{-
  Parser f :: Parser(e (a->b))
  Parser g :: Parser(e a)

  f :: String -> Either e (ParseState (a->b))
  g :: String -> Either e (ParseState a)

-}
instance applyParser :: Apply (Parser e) where
  apply :: ∀ a b. Parser e (a -> b) -> Parser e a -> Parser e b
  apply p1 p2 = 
    Parser \s -> case parse p1 s of
      Left err -> Left err
      Right (Tuple st fst) -> case parse p2 st of
        Left err -> Left err
        Right(Tuple st2 aa) -> Right (Tuple st2 $fst aa)



instance applicative :: Applicative (Parser e) where
  pure:: ∀ a. a -> Parser e a
  pure a = Parser \s -> pure $ Tuple s a

-- Example of usage
parse :: ∀ a e. Parser e a -> ParseFunction e a
parse (Parser f) = f 

char :: ∀ e. Parser e Char
char = Parser \s -> case uncons s of
  Nothing -> Left eof
  Just{ head, tail } ->  Right $ Tuple tail head

twoChars :: ∀ e. Parser e (Tuple Char Char)
twoChars = Tuple <$> char <*> char
-- case uncons s of
--   Nothing -> Left eof
--   Just{ head, tail } ->  Right $ Tuple tail (Tuple head head)

threeChars :: ∀ e. Parser e String
threeChars = (\ c1 c2 c3 -> fromCharArray [c1, c2, c3]) <$> char <*> char <*> char

parse' :: ∀ a. Parser PError a -> ParseFunction PError a
parse' = parse
testParser:: Effect Unit 
testParser = do 
  log "============= Test Parser =============="
  log $ show $ parse' char "ABC"
  log $ show $ parse' twoChars "ABC"
  log $ show $ parse' threeChars "ABC"
  log $ show $ parse' threeChars "AB"
  log $ show $ parse' (fromCharArray <$> (count 3 char)) "XYZ" 

-- Specific implementantion just for Arrays
count' :: ∀ e a. Int -> Parser e a -> (Parser e (Array a))
count' n p
  | n < 0 = pure []
  | otherwise = sequence (replicate n p)

-- more general implementation
count :: ∀ e a f
  . Traversable f 
  => Unfoldable f
  => Int 
  -> Parser e a 
  -> Parser e (f a)
count n p
  | n < 0 = pure none
  | otherwise = sequence (replicate n p)


test :: Effect Unit
test = do
  testParser