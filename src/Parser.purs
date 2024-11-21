module Parser where

import Prelude

import Control.Lazy (class Lazy, defer)
import Control.Monad (ap)
import Control.Alt (class Alt, (<|>))
import Data.Array ((:))
import Data.Unfoldable (class Unfoldable,replicate, none)
import Data.Traversable (sequence, class Traversable)

import Data.String.CodePoints (codePointFromChar)
import Data.CodePoint.Unicode (isDecDigit, isAlpha, isAlphaNum)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (NonEmpty(..), (:|), fromNonEmpty)
import Data.String.CodeUnits (uncons, fromCharArray, singleton)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))

import Effect (Effect)
import Effect.Console (log)

type ParserState a = Tuple String a

-- other ways to model it! 
-- data ParserState a = ParserState String a
-- newtype ParserState a = ParserState(Tuple String a) 

class ParserError (e :: Type) where
  eof :: e
  invalidChar :: String -> e

-- data MyError = EOF | InvalidThing
-- instance parserErrorMyError :: ParserError MyError where
--   eof = EOF

data PError 
  = EOF
  | InvalidChar String

derive instance genericPError :: Generic PError _

instance showPError :: Show PError where
  show  = genericShow

instance parserErrorPError :: ParserError PError where
  eof = EOF
  invalidChar = InvalidChar 



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
  apply = ap
  -- apply p1 p2 = Parser \s ->
  --   do
  --     Tuple st fst <- parse p1 s
  --     Tuple st2 aa <- parse p2 st
  --     pure $ Tuple st2 $fst aa
  -- we can change above code for the ap helper
    -- Parser \s -> case parse p1 s of
    --   Left err -> Left err
    --   Right (Tuple st fst) -> case parse p2 st of
    --     Left err -> Left err
    --     Right(Tuple st2 aa) -> Right (Tuple st2 $fst aa)



instance applicative :: Applicative (Parser e) where
  pure:: ∀ a. a -> Parser e a
  pure a = Parser \s -> pure $ Tuple s a
 
instance bindParser :: Bind (Parser e) where
  bind :: ∀ a b . (Parser e a) -> (a -> Parser e b) -> Parser e b
  bind f g =
     Parser \s -> do 
      (Tuple err a) <- parse f s       
      parse (g a) err

instance monadParser :: Monad (Parser a)

instance altParser :: Alt (Parser a) where
  alt p1 p2 = Parser \s -> case parse p1 s of
    Right x -> Right x
    Left _ -> parse p2 s
-- Example of usage
parse :: ∀ a e. Parser e a -> ParseFunction e a
parse (Parser f) = f 

char :: ∀ e. Parser e Char
char = Parser \s -> case uncons s of
  Nothing -> Left eof
  Just{ head, tail } ->  Right $ Tuple tail head

twoChars :: ∀ e. Parser e (Tuple Char Char)
twoChars = do
  c1 <- char 
  c2 <- char 
  pure $Tuple c1 c2
  
twoCharsB :: ∀ e. Parser e (Tuple Char Char)
twoCharsB = char >>= \c1 -> char >>= \c2 -> pure $Tuple c1 c2 

twoCharsA :: ∀ e. Parser e (Tuple Char Char)
twoCharsA = Tuple <$> char <*> char

threeChars :: ∀ e. Parser e String
threeChars = do
  c1 <- char
  c2 <- char
  c3 <- char
  pure $fromCharArray [c1, c2, c3]

  
threeCharsA :: ∀ e. Parser e String
threeCharsA = (\ c1 c2 c3 -> fromCharArray [c1, c2, c3]) <$> char <*> char <*> char

digit :: ∀ e. ParserError e => Parser e Char
digit = satisfy "digit" (isDecDigit <<< codePointFromChar)

letter :: ∀ e. ParserError e => Parser e Char 
letter = satisfy "leter" (isAlpha <<< codePointFromChar)

alphaNum :: ∀ e. ParserError e => Parser e Char
-- alphaNum = Parser \s -> case (parse letter s :: Either PError _) of
--   Right x -> Right x
--   Left _ -> case parse digit s of
--     Right x1 -> Right x1
--     Left err -> Left err  
alphaNum = letter <|> digit <|> fail (invalidChar "alphaNum")

parse' :: ∀ a. Parser PError a -> ParseFunction PError a
parse' = parse

satisfy :: ∀ e 
  . ParserError e 
  => String
  -> (Char -> Boolean)
  -> Parser e Char
satisfy expected p = char >>= \c -> if p c then pure c else fail $ invalidChar expected

fail :: ∀ e a. ParserError e => e -> Parser e a
fail e = Parser $ const $ Left e

newtype Month = Month Int
derive instance genericMonth :: Generic Month _
instance showMonth :: Show Month where
  show = genericShow

newtype Day = Day Int 

derive  instance genericDay :: Generic Day _
instance showDay :: Show Day where
  show = genericShow

newtype Year = Year Int

derive instance genericYear :: Generic Year _
instance showYear :: Show Year where
  show = genericShow


data DateFormat  = YearFirst | MonthFirst

derive instance genericDateFormat :: Generic DateFormat _
instance showDateFormat :: Show DateFormat where
  show = genericShow

type DateParts = 
  { year  :: Year
  , month :: Month
  , day   :: Day
  , format :: DateFormat
  }


atMost :: ∀ e a f. 
  Unfoldable f 
  => (a -> f a -> f a)
  -> Int
  -> Parser e a 
  -> Parser e (f a)
atMost cons n p 
  | n <= 0 = pure none
  | otherwise = optional none $ p >>= \c -> cons c <$> atMost cons (n - 1) p

atMostArr :: ∀ e a. Int -> Parser e a -> Parser e (Array a)
atMostArr n p 
  | n <= 0 = pure []
  | otherwise = optional [] $ p >>= \c -> (c : _) <$> atMost (:) (n - 1) p
  
atMost' :: ∀ e. Int -> Parser e Char -> Parser e String
atMost' n p = fromCharArray <$> atMost (:) n p

optional :: ∀ e a. a -> Parser e a -> Parser e a
optional default p = p <|> pure default

range :: ∀ e a f.
  Semigroup (f a)
  => Traversable f
  => Unfoldable f 
  => (a -> f a -> f a)
  -> Int 
  -> Int 
  -> Parser e a 
  -> Parser e (f a)
range cons min max p
  | min < 0 || max <= 0 || max < min = pure none
  | otherwise = count min p >>= \cs -> (cs <> _) <$> atMost cons (max - min) p

range' :: ∀ e. Int -> Int -> Parser e Char -> Parser e String
range' min max p = fromCharArray <$> range (:) min max p

constChar :: ∀ e. ParserError e => Char -> Parser e Unit
constChar = void <<< constChar'

constChar' :: ∀ e. ParserError e => Char -> Parser e Char
constChar' c = satisfy (singleton c) (_ == c)

digitsToNum :: String -> Int
digitsToNum = fromMaybe 0 <<< fromString
yearFirst :: ∀ e. ParserError e => Parser e DateParts
yearFirst = do
  year  <- Year  <<< digitsToNum<$> count' 4 digit
  constChar '-'
  month <- Month <<< digitsToNum <$> range' 1 2 digit
  constChar '-'
  day   <- Day   <<< digitsToNum <$> range' 1 2 digit
  pure $ {year, month, day, format : YearFirst }

monthFirst :: ∀ e. ParserError e => Parser e DateParts
monthFirst = do
  month <- Month <<< digitsToNum <$> range' 1 2 digit
  constChar '/'
  day   <- Day   <<< digitsToNum <$> range' 1 2 digit
  constChar '/'
  year  <- Year  <<< digitsToNum<$> count' 4 digit
  pure $ {year, month, day, format : MonthFirst }

date :: ∀ e. ParserError e => Parser e DateParts
date = yearFirst <|> monthFirst <|> fail (invalidChar "date")


instance lazyParser :: Lazy (Parser e a) where
  defer :: (Unit -> Parser e a) -> Parser e a
  defer f = Parser \s -> parse(f unit) s

some :: ∀ f m a.  
  Unfoldable f
  => Alt m
  => Applicative m
  => Lazy (m (f a))   
  => (a -> f a -> f a) 
  -> m a 
  -> m (NonEmpty f a)
some cons p = (:|) <$> p <*> defer \_ -> many cons p 

some' :: ∀ e. Parser e Char -> Parser e String
some' p =  fromCharArray <<< fromNonEmpty (:) <$> some (:) p

many :: ∀  a f m .
  Unfoldable f
  => Alt m
  => Applicative m
  => Lazy (m (f a))    
  => (a -> f a -> f a)
  -> m a -> m (f a)
many cons p = fromNonEmpty cons <$> some cons p <|> pure none

many' :: ∀ e. Parser e Char -> Parser e String
many' p = fromCharArray <$> many (:) p


digits :: ∀ e. ParserError e => Parser e String
digits = some' digit

ugly :: ∀ e. ParserError e => Parser e (Array String)
ugly = do
  nums <- range' 1 4 digit
  constChar ','
  constChar ' '
  alp  <- some' (letter <|> constChar' ' ')
  lnum <- many' digit
  pure [nums, alp, lnum]

testParser:: Effect Unit 
testParser = do 
  log "============= Test Parser =============="
  log $ show $ parse' char "ABC"
  log $ show $ parse' twoChars "ABC"
  log $ show $ parse' threeChars "ABC"
  log $ show $ parse' threeChars "AB"
  log $ show $ parse' threeChars "AB"
  log $ show $ parse' threeChars "ABCD"
  log $ show $ parse' (fromCharArray <$> (count 3 char)) "XYZ" 
  log ""
  log "--Count--"
  log $ show $ parse' (count' 3 digit) "123456"
  log $ show $ parse' (count' 3 digit) "123abc"
  log $ show $ parse' (count' 4 letter) "Freddy"
  log $ show $ parse' (count' 10 alphaNum) "a1b2c3d4e5"
  log $ show $ parse' (count' 10 alphaNum) "#########"
  log ""
  log "AtMost Parser --"
  log $ show $ parse' (atMost' (-2) alphaNum) "a1b2c3"
  log $ show $ parse' (atMost' (2) alphaNum) "$_$"
  log $ show $ parse' (atMost' (2) alphaNum) "a1b2c3"
  log ""
  log "Date parser"
  log $ show $ parse' yearFirst "2024-11-20"
  log $ show $ parse' monthFirst "11/20/2024"
  log $ show $ parse' date "11/20/2024"
  log $ show $ parse' date "2024-11-20"
  log $ show $ parse' date "2024/11-20"
  log ""
  log "Some and Many parser"
  log $ show $ parse' (some' digit) "2343423423abc"
  log $ show $ parse' (many' digit) "_2343423423abc"
  log $ show $ parse' (some' digit) "_2343423423abc"
  

-- Specific implementantion just for Arrays
countAr :: ∀ e a. Int -> Parser e a -> (Parser e (Array a))
countAr n p
  | n < 0 = pure []
  | otherwise = sequence (replicate n p)

-- more general implementation
count' :: ∀ e.
   Int 
  -> Parser e Char
  -> Parser e String
count' n p = fromCharArray <$> count n p

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