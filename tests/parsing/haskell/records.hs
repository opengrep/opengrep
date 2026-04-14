module Records where

-- Record declaration
data Person = Person
  { name :: String
  , age  :: Int
  } deriving (Show, Eq)

-- Record construction
alice :: Person
alice = Person { name = "Alice", age = 30 }

-- Record update
olderAlice :: Person
olderAlice = alice { age = 31 }

-- Record field access via projection function
aliceName :: String
aliceName = name alice

-- Nested record
data Address = Address
  { street :: String
  , city   :: String
  } deriving (Show)

data Employee = Employee
  { person  :: Person
  , address :: Address
  } deriving (Show)

emp :: Employee
emp = Employee
  { person  = alice
  , address = Address { street = "1 Main St", city = "Springfield" }
  }

empCity :: String
empCity = city (address emp)
