module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.AddressBook (AddressBook, Entry, emptyBook, insertEntry, printEntry, printEntryTel, isContainEntry, removeDuplicates, printBook)

entry1 :: Entry
entry1 =
  { firstName: "John"
  , lastName: "Smith"
  , address: { street: "123 Fake St."
             , city: "Faketown"
             , state: "CA"
             }
  , telephone: "080-1388-4963"
  }
entry2 :: Entry
entry2 =
  { firstName: "John"
  , lastName: "Hank"
  , address: { street: "345 Fake St."
             , city: "Faketown"
             , state: "CA"
             }
  , telephone: "080-4735-9926"
  }
entry3 :: Entry
entry3 =
  { firstName: "John"
  , lastName: "Smith"
  , address: { street: "23 Fake St."
             , city: "Faketown"
             , state: "CA"
             }
  , telephone: "080-1388-4932"
  }

book0 :: AddressBook
book0 = emptyBook

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let book1 = insertEntry entry3 $ insertEntry entry2 $ insertEntry entry1 emptyBook

  logShow "Expected: Nothing, some_data"
  logShow $ printEntry "John" "Smith" book0
  logShow $ printEntry "John" "Smith" book1
  logShow "Expected: Nothing, some_data"
  logShow $ printEntryTel "080-1388-4963" book0
  logShow $ printEntryTel "080-1388-4963" book1
  logShow "Expected: false, true"
  logShow $ isContainEntry "John" "Smith" book0
  logShow $ isContainEntry "John" "Smith" book1
  logShow "---Before remove duplicates---"
  logShow $ printBook book1
  let book1_af = removeDuplicates book1
  logShow "---After remove duplicates---"
  logShow "Expected: John Smith exists only one"
  logShow $ printBook book1_af