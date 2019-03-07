module Main where

import Control.Applicative
import Data.List
import Data.Maybe
import Prelude

import Control.Apply (lift2, (<*>))
import Data.AddressBook (fullName)
import Data.Either (Either(..))
import Data.Functor ((<$>))

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