module Bayesian.Inference.VariableElimination(
  vePr1
) where

import Bayesian.Factor
import Bayesian.Variable
import Bayesian.CheckedError
import Data.List

vePr1 :: [Variable] -> [Factor] -> Factor
vePr1 eliminationOrder =
  foldr1 multiply.vePr1' eliminationOrder
    where vePr1' eliminationOrder factors =
            foldl (flip vePr1EliminateVariable) factors eliminationOrder

vePr1EliminateVariable :: Variable -> [Factor] -> [Factor]
vePr1EliminateVariable variable factors =
  let (factorsToEliminate, remainingFactors) = partition (elem variable.factorVars) factors
  in if null factorsToEliminate then
       remainingFactors
     else
       (summingOut [variable].foldr1 multiply $ factorsToEliminate) : remainingFactors
