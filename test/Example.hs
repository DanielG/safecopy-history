{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
import Data.SafeCopy
import Data.KeepHistory

type Name     = String
type Address  = String
type Phone    = String

keepHistory [d|
  data Contact = Contact {
               name    :: Name
             , address :: Address
             , phone   :: Phone
             }
  data Contacts = Contacts [Contact]
 |]

instance Migrate Contacts where
    type MigrateFrom Contacts = Contacts_v0
    migrate (Contacts_v0 contacts) = Contacts
      [ Contact {
          name    = name
        , address = address
        , phone   = ""
        }
      | (name, address) <- contacts ]

main = return ()
