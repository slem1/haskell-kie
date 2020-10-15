{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
--{-# LANGUAGE RecordDotSyntax #-}

module Record (
    render,
    example,
    example',
    example'',
    Person(name, admin), 
    makePerson,
    example_,
    render',
    render''
) where

data Person = Person { name :: String, admin :: Bool} deriving Show

example :: Person
example = Person { name = "John Doe", admin = True}

--with NameFieldPuns
example' :: Person
example' = Person {name, admin}
    where
        name = "John Doe"
        admin = True

--with RecordWildCards
example'' :: Person
example'' = Person {..}
    where
        name = "John Doe"
        admin = True


render :: Person -> String
render person = name person ++ suffix
    where
        suffix = if admin person then "-Admin" else ""

render' :: Person -> String
render' Person { name = name, admin = admin} = name ++ suffix
    where
        suffix = if admin then "-Admin" else ""

render'' :: Person -> String
render'' Person { name, admin } = name ++ suffix
    where
        suffix = if admin then "-Admin" else ""

render''' :: Person -> String
render''' Person { .. } = name ++ suffix
    where
        suffix = if admin then "-Admin" else ""

{-- 
render_ :: Person -> String
render_ person = person.name ++ suffix
    where
        suffix = if person.admin then "-Admin" else ""
        --}

makePerson :: String -> Person
makePerson name = Person { name = name, admin = False}

example_ = (makePerson "John Doe") { admin = True}

example__ = (makePerson "John Doe") { admin}
    where
        admin=True
