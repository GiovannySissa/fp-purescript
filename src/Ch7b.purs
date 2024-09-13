module Ch7b where

import Prelude (class Eq, class Show, Unit, show, discard, otherwise, ($), (#), (<>), (==))


import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (Pattern(..), split)
import Data.Show.Generic (genericShow)

import Effect
import Effect.Console (log)

-- because we don't want mix up any old String in th repr
newtype CSV = CSV String
derive instance newtypeCSV :: Newtype CSV _
derive newtype instance eqCSV :: Eq CSV
derive newtype instance showCSV :: Show CSV

class ToCSV a where
  toCSV :: a -> CSV

class FromCSV a where
  fromCSV :: CSV -> Maybe a

instance fromCSVPerson :: FromCSV Person where
  fromCSV (CSV csvPerson) = 
    case split (Pattern ",") csvPerson of 
      [name, age, ocupation] -> 
        case fromString age of
          Just age' -> case toOcupation ocupation of 
            Just ocupation' -> Just $ Person {name: FullName name, age: Age age', ocupation: ocupation'}
            _ -> Nothing
          _ -> Nothing
      _ -> Nothing

newtype FullName = FullName String
instance showFullName :: Show FullName where
  show (FullName fullName) = fullName 
derive newtype instance eqFullName :: Eq FullName
-- derive newtype instance showFullName :: Show FullName


newtype Age = Age Int
derive instance newtypeAge :: Newtype Age _
derive newtype instance showAge :: Show Age
derive newtype instance eqAge :: Eq Age


data Ocupation = Doctor | Dentist | Lawyer | Unemployed
derive instance genericOcupation :: Generic Ocupation _
instance showOcupation :: Show Ocupation where
  show ocupation = genericShow ocupation
derive instance eqOcupation :: Eq Ocupation 

toOcupation :: String -> Maybe Ocupation
toOcupation  = case _ of
    "Doctor" -> Just Doctor
    "Dentist" -> Just Dentist
    "Lawyer" -> Just Lawyer
    "Unemployed" -> Just Unemployed
    _ -> Nothing
  


data Person = Person {
  name :: FullName,
  age :: Age,
  ocupation :: Ocupation
}

instance toCSVPerson :: ToCSV Person where
  toCSV (Person {name, age, ocupation}) = 
    CSV $ (show name) <> "," <> show age <> "," <> show ocupation 

derive instance genericPerson :: Generic Person _
instance showPerson :: Show Person where
  show person = genericShow person

derive instance eqPerson :: Eq Person

test :: Effect Unit
test = do
  let person = Person { 
        name: FullName "J A",
        age: Age 32,
        ocupation: Unemployed 
      }
  log $show $ toCSV person == CSV "J A,32,Unemployed"
  log $show $ toCSV person
  log $show person
  log "--------From  CSV -------"
  log $show $ (fromCSV $ CSV "J A,32,Unemployed" :: Maybe Person)  
  log $show $ (fromCSV $ CSV "J A,32,Unemployed") == Just person
  log $show $ (toCSV person # fromCSV ) == Just person