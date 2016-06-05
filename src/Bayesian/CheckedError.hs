module Bayesian.CheckedError where

data CheckedError = InconsistentVariableInstantiation
                  | InvalidVariableInstantiation
                  | IncompleteFactor
                  | DuplicatedVariableInstantiation
                  deriving (Show, Eq)

type Checked a = Either CheckedError a

isError :: Checked a -> Bool
isError (Left _) = True
isError _ = False

getError :: Checked a -> CheckedError
getError (Left e) = e

getValue :: Checked a -> a
getValue (Right v) = v
