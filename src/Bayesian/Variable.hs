module Bayesian.Variable(
  Domain,
  Variable,
  VariableSet,
  VariableInstantiation,
  VariableInstantiationSet,
  createVariable,
  instantiateVariable,
  instantiatedVariable,
  instantiation,
  variableLabel,
  variableSize,
  variableSetSize,
  variableInstantiations,
  variableSetAllInstantiations
) where

import Data.List
import Bayesian.CheckedError

-- | Synonym for a set of Strings.
type Domain = [String]
-- | Synonym for a set of Variables.
type VariableSet = [Variable]
-- | Synonym for a set of Variables Instantiations.
type VariableInstantiationSet = [VariableInstantiation]

-- | Discrete variable on a network.
data Variable = V {
  variableLabel :: String,
  variableDomain :: Domain
} deriving (Eq, Ord)

instance Show Variable where
  show = variableLabel

-- | Instantiated variable.
data VariableInstantiation = I {
  instantiatedVariable :: Variable,
  instantiation :: String
} deriving (Eq, Ord)

instance Show VariableInstantiation where
  show vi =
    show (instantiatedVariable vi) ++ "=" ++ instantiation vi

-- | Creates a variable, given its label and domain values.
createVariable :: String -> Domain -> Variable
createVariable label domain = V label (nub domain)

-- | Creates a variable instantiantion, given the variable and the corresponding value.
instantiateVariable :: String -> Variable -> Checked VariableInstantiation
instantiateVariable value variable =
  if value `elem` variableDomain variable then
    Right $ I variable value
  else
    Left InvalidVariableInstantiation

-- | Get the set of valid instantiations for a given variable.
variableInstantiations :: Variable -> VariableInstantiationSet
variableInstantiations v = map (I v) $ variableDomain v

-- | Gets the variable domain size (the number of valid instantiations)
variableSize :: Variable -> Int
variableSize = length.variableDomain

-- | Gets the size of a variable set, which is the product of all variables sizes.
variableSetSize :: VariableSet -> Int
variableSetSize = foldr ((*) .variableSize) 1

variableSetAllInstantiations :: [Variable] -> [Domain]
variableSetAllInstantiations = mapM variableDomain
