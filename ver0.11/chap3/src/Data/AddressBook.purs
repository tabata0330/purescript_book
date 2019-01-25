module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, null, nubBy)
import Data.Maybe (Maybe)


type Address = { street :: String, city :: String, state :: String }

type Entry = { firstName :: String, lastName :: String, address :: Address, telephone :: String }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <> addr.city <> ", " <> addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <> entry.firstName <> ": " <> showAddress entry.address <> ": " <> entry.telephone

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry entry book = Cons entry book

-- filter関数は判定する関数を第一引数に、判定されるリストを第二引数にもつ
-- JavaでもComparator定義したりとか似たようなのあったな
findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName book = head $ filter filterEntry book
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

findEntryTel :: String -> AddressBook -> Maybe Entry
findEntryTel telephone book = head $ filter filterEntryTel book
  where
    filterEntryTel :: Entry -> Boolean
    filterEntryTel entry = entry.telephone == telephone

isNotContainEntry :: String -> String -> AddressBook -> Boolean
isNotContainEntry firstName lastName book = null $ filter filterEntry book
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

isContainEntry :: String -> String -> AddressBook -> Boolean
isContainEntry firstName lastName book = not isNotContainEntry firstName lastName book

compareName :: Entry -> Entry -> Boolean
compareName entry1 entry2 = entry1.firstName == entry2.firstName && entry1.lastName == entry2.lastName 

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates book = nubBy compareName book

printBook :: AddressBook -> List String
printBook book = map showEntry book

printEntry firstName lastName book = map showEntry (findEntry firstName lastName book)
printEntryTel telephone book = map showEntry (findEntryTel telephone book)