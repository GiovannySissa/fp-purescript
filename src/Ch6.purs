module Ch6 where

import Prelude ((<<<), (&&), (<>), (*), (+))
import Data.Newtype (class Newtype, unwrap)



-- Typeclases


data Address =
  Address {
    street1 :: String,
    street2 :: String,
    city :: String,
    state :: String,
    zip :: String 
  }

type Directions = String
getDirections :: Address -> Directions
getDirections (Address address) = address.city -- details implementation does not matter :) 

data Person = Person {
  name :: String,
  age :: Int,
  address :: Address
}

data Company = Company {
  name :: String,
  address :: Address
}

data Residence = Home Address | Facility Address


getPersonDirections :: Person -> Directions
getPersonDirections (Person person) = getDirections person.address

getCompanyDirections :: Company -> Directions
getCompanyDirections (Company company) = getDirections company.address

getResidenceDirections :: Residence -> Directions
getResidenceDirections = getDirections <<< go where
  go(Home address) = address
  go(Facility address) = address

  -- "Generalized" solution not using typeclasses

data HasAddress' 
  = PersonAddress Person
  | CompanyAddress Company
  | ResidenceAddress Residence

getDirectionsTo:: HasAddress' -> Directions
getDirectionsTo = getDirections <<< case _ of
  PersonAddress (Person person) -> person.address
  CompanyAddress (Company company) -> company.address
  ResidenceAddress (Home address) -> address
  ResidenceAddress (Facility address) -> address


  -- Typeclass Solution

class HasAddress a where
  getAddress :: a -> Address

instance hasAddressPerson :: HasAddress Person where
  getAddress (Person person) = person.address

instance hasAddressCompany :: HasAddress Company where
  getAddress (Company company) = company.address

instance hasAddressResidence :: HasAddress Residence where
  getAddress (Home address) = address
  getAddress (Facility address) = address

-- using tc

-- getDirections' :: ∀ a. HasAddress a => a -> Directions
-- getDirections' hasAddr = 
--   let address = getAddress hasAddr in
--   hasAddress address

class Eq a where
  eq :: a -> a -> Boolean

infix 4 eq as ==

-- Eq instances 

instance eqPerson :: Eq Person where
  eq (Person p1) (Person p2) = p1.name == p2.name && p1.age == p2.age && p1.address == p2.address


instance eqAddress :: Eq Address where
  eq (Address a1) (Address a2) = a1.street1 == a2.street1  && a2.street2 == a2.street2 && a1.city == a2.city && a1.state == a2.state && a1.zip == a2.zip

instance eqInt :: Eq Int where
  eq s1 s2 = s1 == s2

instance eqString :: Eq String where
  eq s1 s2 = s1 == s2



-- instance eqAddress :: Eq Address where
--   eq (Address a1) (Address a2) = a1 ==

-- class Ord Eq a <= Ord a where
--   compare :: a -> a -> Ordering

-- data Ordering = LT | GT | EQ

-- instance ordUnit Ord Unit where
--   compare _ _ = EQ



newtype FirstName  = FirstName String
derive instance newtypeFirstName :: Newtype FirstName _ 

newtype LastName = LastName String
derive instance newtypeLastName :: Newtype LastName _

fullName :: FirstName -> LastName -> String
fullName (FirstName first) (LastName last) = first <> " " <> last

fullName' :: FirstName -> LastName -> String
fullName' first last = unwrap first <> " " <> unwrap last

-- Overlaping instances

class Combine a where
  combine :: a -> a -> a

-- instance combineAddInt :: Combine Int where
--   combine = (+)

-- instance combineMultInt :: Combine Int where
--   combine = (*)

-- we had two instances same of type compiler wont't know which use

newtype MultInt = MultInt Int
newtype AddInt = AddInt Int

instance combineMultInt :: Combine MultInt where
  combine (MultInt a1) (MultInt a2) = MultInt(a1 * a2)

instance combineAddInt :: Combine AddInt where
  combine (AddInt a1) (AddInt a2) = AddInt(a1 + a2)

-- Polymorphic example


class IsRecord a where
  isRecord :: a -> Boolean

instance isRecordRecord :: IsRecord (Record a) where
  isRecord _ = true
else instance isRecordOther :: IsRecord a where
  isRecord _ = false

-- Orphaned Instances

