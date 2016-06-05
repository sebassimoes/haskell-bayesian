module Bayesian.Factor(
  Factor,
  factorVars,
  factorValues,
  createFactorValue,
  buildFactor,
  buildFactorValue,
  buildFactorValueFromInstantiation,
  multiply,
  summingOut
) where

import Bayesian.Variable
import Bayesian.CheckedError
import Control.Monad.State
import Control.Monad.Except
import Data.List

-- | Represents a Factor over a set of variables. For each possible instantiation a number is assigned.
data Factor = MFactor {
  factorLabel :: String,
  factorVars :: VariableSet,
  factorValues :: [FactorValue]
} deriving Eq

instance Show Factor where
  show f = "Factor(" ++ factorLabel f ++ "): " ++ show (factorValues f)

-- | Represents an instantiation of the factor variables and the corresponding number value.
data FactorValue = MFv {
  variablesInstantiation :: VariableInstantiationSet,
  value :: Double
} deriving Eq

instance Show FactorValue where
  show fv = showVariablesInstantiation (variablesInstantiation fv) ++ " := " ++ show (value fv)
    where showVariablesInstantiation = unwords.map show

-- handy synonyms.
type FactorState a = ExceptT CheckedError (State Factor) a

-- | Creates a factor value, given a instnatiation and the corresponding value.
createFactorValue :: VariableInstantiationSet -> Double -> FactorValue
createFactorValue = MFv

-- | Builds a factor over a set of variables.
buildFactor :: String -> VariableSet -> FactorState () -> Checked Factor
buildFactor label vars fstate =
  let initialState = MFactor label (nub vars) []
      factorStateResult = runExceptT fstate
      (value, state) = runState factorStateResult initialState
  in either Left (\_ -> buildFactor' state) value

buildFactor' :: Factor -> Checked Factor
buildFactor' factor
  | length (factorValues factor) == variableSetSize (factorVars factor) =
      Right factor
  | otherwise = Left IncompleteFactor


-- | Build a factor value and add it to the "builder", represented by a State Monad.
buildFactorValue :: VariableInstantiationSet -> Double -> FactorState ()
buildFactorValue instantiation value = do
  factor <- lift get
  factor <- checkVariableInstantiationIntegrity instantiation factor
  checkVariableInstantiationUniqueness instantiation factor
  addFactorValue $ createFactorValue instantiation value

-- | Function that simplifies the process of creating an instantiation with value on a factor.
buildFactorValueFromInstantiation :: Domain -> Double -> FactorState ()
buildFactorValueFromInstantiation domainValues value = do
  factor <- lift get
  let a = createInstantiation factor domainValues
  case a of
    Left e -> throwError e
    Right ins -> buildFactorValue ins value

createInstantiation :: Factor -> Domain -> Checked [VariableInstantiation]
createInstantiation f values =
  let vars = factorVars f
      zippedInstantiations = zip vars values
      instantiationList = map (\(v, i) -> instantiateVariable i v) zippedInstantiations
  in sequence instantiationList

checkVariableInstantiationIntegrity :: VariableInstantiationSet -> Factor -> FactorState Factor
checkVariableInstantiationIntegrity instantiation factor
  | factorVars factor == map instantiatedVariable instantiation =
      lift $ return factor
  | otherwise = throwError InconsistentVariableInstantiation

checkVariableInstantiationUniqueness :: VariableInstantiationSet -> Factor -> FactorState Factor
checkVariableInstantiationUniqueness instantiation factor =
  let variablesInstantiations = map variablesInstantiation $ factorValues factor
  in
    if instantiation `elem` variablesInstantiations then
      throwError DuplicatedVariableInstantiation
    else
      lift $ return factor

addFactorValue :: FactorValue -> FactorState ()
addFactorValue factorValue = lift $ state (\s -> ((), addFactorValue' factorValue s))

addFactorValue' :: FactorValue -> Factor -> Factor
addFactorValue' fv f = MFactor (factorLabel f) (factorVars f) (fv:factorValues f)

-- | Multiplies two factors, computing a new one. Both factors are consistent and valid,
-- | so there's no need to wrap the result on a Checked Type, as it is consisten and valid
-- | as well.
multiply :: Factor -> Factor -> Factor
multiply f1 f2 = MFactor flabel fvars multipliedFactorValues
  where flabel = factorLabel f1 ++ "." ++ factorLabel f2
        fvars = factorVars f1 `union` factorVars f2
        multipliedFactorValues =
          let allVariablesInstantiation = mapM variableInstantiations fvars
          in map (\newInst ->
                     createFactorValue newInst $ getInstantiationValue newInst f1 *
                                                 getInstantiationValue newInst f2) allVariablesInstantiation


-- | Sums out the set of variables from a factor.
summingOut :: VariableSet -> Factor -> Factor
summingOut vs f = MFactor flabel fvars remainingFactorValues
  where flabel = "(" ++ factorLabel f ++ "\\{" ++ show vs ++ "})"
        fvars = factorVars f \\ vs
        remainingFactorValues =
          let oldFactorValues = factorValues f
              allVariablesInstantiation = mapM variableInstantiations fvars
          in map (createSummedOutFactorValue oldFactorValues) allVariablesInstantiation

-- Private function, so there's no need to return a Maybe. The input is assumed to be valid.
getInstantiationValue :: VariableInstantiationSet -> Factor -> Double
getInstantiationValue instantiation factor =
  value.head.filter
    (\fvalue -> isSubsetOf (variablesInstantiation fvalue) instantiation) $ factorValues factor

createSummedOutFactorValue :: [FactorValue] -> VariableInstantiationSet -> FactorValue
createSummedOutFactorValue fvs vins =
  let v = foldr ((+).value) 0 $ filter (isSubsetOf vins.variablesInstantiation) fvs
  in createFactorValue vins v

isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf sublist list = all (`elem` list) sublist
