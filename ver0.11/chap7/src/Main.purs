module Main where

import Control.Applicative
import Data.AddressBook
import Data.List
import Data.Maybe
import Data.String
import Data.String.Regex
import Data.Validation.Semigroup 
import Prelude

import Control.Apply (lift2, (<*>))
import Data.AddressBook (fullName)
import Data.Either (Either(..))
import Data.Functor ((<$>))
import Data.Int (radix)
import Data.String.CodePoints (length)
import Data.String.Regex.Flags (RegexFlags(..), noFlags)
import Partial.Unsafe (unsafePartial)
import Data.Traversable(traverse)

withError :: (Maybe String) -> String -> (Either String String)
withError Nothing err = Left err
withError (Just a) _ = Right a

fullNameEither :: Maybe String -> Maybe String -> Maybe String -> Either String String
fullNameEither first middle last =
  fullName <$> (withError first "No First Name")
           <*> (withError middle "No Middle Name")
           <*> (withError last "No Last Name")

combineList :: forall f a. Applicative f => List (f a) -> f (List a)
combineList Nil = pure Nil
combineList (Cons x xs) = apply (map Cons x) (combineList xs)

-- 演習7.8-1
optionalAdd :: forall f. Apply f => f Int -> f Int -> f Int
optionalAdd a b = apply (map add a) b

-- 演習7.8-3
combineMaybe :: forall a f. (Applicative f) => Maybe (f a) -> f (Maybe a)
combineMaybe (Just f) = Just <$> f
combineMaybe Nothing = pure Nothing

type Errors = Array String

lengthIs :: String -> Int -> String -> V Errors Unit
lengthIs field len value | length value /= len =
  invalid ["Field '" <> field <> "' must have length " <> show len]
lengthIs _ _ _ =
  pure unit

phoneNumberRegex :: Regex
phoneNumberRegex =
  unsafePartial
    case regex "^\\d{3}-\\d{3}-\\d{4}$" (RegexFlags { unicode: false, sticky: false, multiline: false, ignoreCase: false, global: false}) of
      Right r -> r

matches :: String -> Regex -> String -> V Errors Unit
matches _ regex value | test regex value =
  pure unit
matches field _ _ =
  invalid ["Field '" <> field <> "' did not match the required format"]

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber (PhoneNumber o) =
  phoneNumber <$> pure o."type"
              <*> (matches "Number" phoneNumberRegex o.number *> pure o.number)

-- 演習7.10-1
stateRegex :: Regex
stateRegex = 
  unsafePartial
    case regex "^[a-zA-Z]{2}$" noFlags of
      Right r -> r

validateState :: Address -> V Errors Address
validateState (Address o) =
  address <$> pure o.street
          <*> pure o.city
          <*> (matches "State" stateRegex o.state *> pure o.state)

-- 演習7.10-2
blankRegex :: Regex
blankRegex = 
  unsafePartial
    case regex "^\\S+$" noFlags of
      Right r -> r

nonEmpty :: String -> String -> V Errors Unit
nonEmpty field "" = invalid ["Field '" <> field <> "' cannot be empty"]
nonEmpty _     _  = pure unit

validateAddress :: Address -> V Errors Address
validateAddress (Address o) =
  address <$> (matches "Street" blankRegex o.street  *> pure o.street)
          <*> (matches "City" blankRegex   o.city    *> pure o.city)
          <*> (matches "State" stateRegex o.state *> pure o.state)

arrayNonEmpty :: forall a. String -> Array a -> V Errors Unit
arrayNonEmpty field [] =
  invalid ["Field '" <> field <> "' must contain at least one value"]
arrayNonEmpty _ _ =
  pure unit

-- validatePerson :: Person -> V Errors Person
-- validatePerson (Person o) =
--   person <$> (nonEmpty "First Name" o.firstName *>
--               pure o.firstName)
--          <*> (nonEmpty "Last Name" o.lastName *>
--               pure o.lastName)
--          <*> validateAddress o.address
--          <*> (arrayNonEmpty "Phone Numbers" o.phones *>
--               traverse validatePhoneNumber o.phones)

-- 演習7.11-1
data Tree a = Leaf | Branch (Tree a) a (Tree a)

--演習7.11-2
--personの定義諸々変更。addressでMaybeを指定できるようにした。
--これで正しいか自身はない。
validatePerson :: Person -> V Errors Person
validatePerson (Person o) =
  person <$> (nonEmpty "First Name" o.firstName *>
              pure o.firstName)
         <*> (nonEmpty "Last Name" o.lastName *>
              pure o.lastName)
         <*> traverse validateAddress o.address
         <*> (arrayNonEmpty "Phone Numbers" o.phones *>
              traverse validatePhoneNumber o.phones)