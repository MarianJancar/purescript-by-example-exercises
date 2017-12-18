module Main where

import Prelude

import Control.Biapply (biapplySecond)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)

import Data.Foldable
import Data.List
import Data.Monoid

data Entry = Entry {
    name::String
   ,surname::String
   ,address:: Address
}

data Address = Address {
    street::String
   ,city::String
   ,state::String
}

--data AddressBook = ACons Entry AddressBook | ANil
--data AddressBookF = AddressBook (List Entry)
data AddressBook = AddressBook (List Entry)
data AddressBookF a = ACons a (AddressBookF a) | ANil

instance showEntry:: Show Entry where
    show (Entry e) = e.name <> " " <>
                     e.surname <> ", " <>
                     show e.address

instance showAddress:: Show Address where
    show (Address a) = a.street <> ", " <>
                       a.city <> ", " <>
                       a.state

instance foldAddressBook:: Foldable AddressBookF where
    foldl f = go
        where
        go b = case _ of
            ANil -> b
            ACons a as -> go (f b a) as
    foldr f b = foldl (flip f) b <<< rev
        where
        rev = foldl (flip ACons) ANil
    foldMap f = foldl (\acc -> append acc <<< f) mempty



-- x :: forall a b f. Foldable f => (b -> a -> b) -> b -> f a -> b
-- x = foldl

instance showAddressBook:: Show AddressBook where
    show (AddressBook Nil) = ""
    show (AddressBook (b:bs)) = foldl append (show b) bs
        where
        append s e = s <> "\n" <> show e

e1 = Entry {name:"n1",surname:"sn1",address:Address{street:"st1",city:"c1",state:"a1"}}
e2 = Entry {name:"n2",surname:"sn2",address:Address{street:"st2",city:"c2",state:"a2"}}
b1 = AddressBook (e1:e2:Nil)

main :: ∀ e. Eff (console :: CONSOLE | e) Unit
main = do
    log "Hello sailor!"
    logShow b1
