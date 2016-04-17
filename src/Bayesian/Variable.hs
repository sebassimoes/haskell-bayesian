module Bayesian.Variable(
  Domain,
  Variable,
  VariableSet,
  VariableInstantiation,
  createVariable,
  instantiateVariable
) where

import qualified Data.Set

-- | Synonym for a set of Strings.
type Domain = Data.Set.Set String
-- | Synonym for a set of Variables.
type VariableSet = Data.Set.Set Variable

-- | Discrete variable on a network.
data Variable = V {
  label :: String,
  domain :: Domain
} deriving (Show, Eq, Ord)

-- | Instantiated variable.
data VariableInstantiation = I {
  var :: Variable,
  value :: String
} deriving Show

-- | Creates a variable, given its label and domain values.
createVariable :: String -> Domain -> Variable
createVariable = V

-- | Creates a variable instantiantion, given the variable and the corresponding value.
instantiateVariable :: String -> Variable -> Either String VariableInstantiation
instantiateVariable value variable =
  if Data.Set.member value $ domain variable then
    Right $ I variable value
  else
    Left $ "Value " ++ value ++ " is not valid for the variable " ++ show variable
