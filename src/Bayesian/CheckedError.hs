module Bayesian.CheckedError where

data CheckedError = InconsistentVariableInstantiation
                  | InvalidVariableInstantiation
                  | IncompleteFactor
                  | DuplicatedVariableInstantiation
                  deriving (Show, Eq)

type Checked a = Either CheckedError a
