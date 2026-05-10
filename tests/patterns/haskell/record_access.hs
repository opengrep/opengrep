module RecordAccess where

--ERROR:
getName p = p.name

--ERROR:
x = person.name

noAccess p = age p
