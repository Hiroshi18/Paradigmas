module Agenda where

    data Contact = Contact {ctName :: String, 
                            ctEmail :: String, 
                            ctPhone :: String 
                            }

    class Pessoa contact where
        contact :: c -> Contact
    
    instance Pessoa where
        contact = "Joao" "joao@joao.com" "4565"