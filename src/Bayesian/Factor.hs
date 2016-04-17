module Bayesian.Factor(
  Factor,
  buildFactor,
  buildFactorValue,
  multiply,
  summingOut
) where

import Bayesian.Variable
import Control.Monad.State

-- | Represents a Factor over a set of variables. For each possible instantiation a number is assigned.
data Factor = MFactor {
  factorLabel :: String,
  factorVars :: VariableSet,
  factorValues :: [FactorValue]
} deriving Show

-- | Represents an instantiation of the factor variables and the corresponding number value.
data FactorValue = MFv {
  variablesInstantiation :: [VariableInstantiation],
  value :: Double
} deriving Show

-- handy synonym.
type FactorBuilder = State Factor ()

-- | Builds a factor over a set of variables.
buildFactor :: String -> VariableSet -> FactorBuilder -> Factor
buildFactor label vars builder = execState builder (MFactor label vars [])

-- | Adds a factor value to the factor building process.
buildFactorValue :: [VariableInstantiation] -> Double -> FactorBuilder
buildFactorValue vsInstantiation value =
  -- TODO sebas.simoes: add validations -> all the set variables must be instantiated!
  state (\f -> ((), addFactorValue (MFv vsInstantiation value) f))

addFactorValue :: FactorValue -> Factor -> Factor
addFactorValue fv f = MFactor (factorLabel f) (factorVars f) (fv:factorValues f)

-- | Multiplies two factors, computing a new one.
multiply :: Factor -> Factor -> Factor
multiply f1 f2 = f1

-- | Sums out the set of variables from a factor.
summingOut :: VariableSet -> Factor -> Factor
summingOut vs f = f
