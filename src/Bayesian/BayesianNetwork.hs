module Bayesian.BayesianNetwork(
  BayesianNetwork,
  BayesianNode,
  BNBuilder,
  nodeVariable,
  nodeCPT,
  buildBN,
  variable,
  cpt,
  p
) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.List
import Control.Monad.State
import Control.Monad.Except
import Bayesian.Factor
import Bayesian.Variable
import Bayesian.CheckedError

-- | Each node of the bayesian network DAG represents a domain variable. Each variable
-- | has its own CPT (represented as a factor over the target variable).
data BayesianNode = Bn {
  nodeVariable :: Variable,
  nodeCPT :: Factor
}

-- | The bayesian network is a graph with labelled nodes and non-labelled edges.
type BayesianNetwork = Gr BayesianNode ()

instance Show BayesianNode where
  show bn = show $ nodeVariable bn

instance Eq BayesianNode where
  (==) bn1 bn2 = nodeVariable bn1 == nodeVariable bn2

-- | State Monad that simplifies the creation of bayesian networks. The state monad is itself
-- | wrapped on a ExceptT monad transformer, so that error handling is transparent.
type BNBuilder a = ExceptT CheckedError (State BayesianNetwork) a

-- | Creates a bayesian network. If no errors are found on the building process,
-- | the bayesian network is returned.
buildBN :: BNBuilder a -> Checked BayesianNetwork
buildBN builder =
  let disconnectedBN = runBN builder empty
   in either Left (connectBN.snd) disconnectedBN

runBN :: BNBuilder a -> BayesianNetwork -> Checked (a, BayesianNetwork)
runBN st bn = case runState (runExceptT st) bn of
                (Left err, _) -> Left err
                (Right r, bn) -> Right (r, bn)

-- | Adds a variable to the bayesian network builder.
variable :: String -> Domain -> BNBuilder Variable
variable label domain =
  return $ createVariable label domain

-- | Adds a variable CPT to the builder. Internally, a factor will be generated with
-- | the target variable and the parents variables.
cpt :: Variable -> [Variable] -> [Double] -> BNBuilder ()
cpt v parents probs =
   let factorVariables = v:parents
       allValuesWithProbs = zip (variableSetAllInstantiations factorVariables) probs
       factorValues = mapM_ (uncurry buildFactorValueFromInstantiation) allValuesWithProbs
       factor = buildFactor (variableLabel v) factorVariables factorValues
   in if isError factor then
        throwError $ getError factor
      else
        do builderState <- lift get
           put $ createNode builderState v (getValue factor)

-- | Shorthand for cpt, when the target variable is not conditioned.
p :: Variable -> [Double] -> BNBuilder ()
p variable = cpt variable []

createNode :: BayesianNetwork -> Variable -> Factor -> BayesianNetwork
createNode bn variable factor = insNode (noNodes bn, bayesianNode variable factor) bn

-- TODO sebas: this functions should check that the bayesian network is a valid DAG.
connectBN :: BayesianNetwork -> Checked BayesianNetwork
connectBN bn =
  let allNodes = labNodes bn
      edges = concatMap (getParentEdges bn) allNodes
   in Right $ insEdges edges bn

getParentEdges :: BayesianNetwork -> LNode BayesianNode -> [LEdge ()]
getParentEdges bn node =
  let variables = factorVars (nodeCPT $ snd node) \\ [nodeVariable $ snd node]
   in [(fst n, fst node, ()) | n <- labNodes bn, nodeVariable (snd n) `elem` variables]

bayesianNode :: Variable -> Factor -> BayesianNode
bayesianNode = Bn


bn =   runBN (do a <- variable "A" ["0", "1"]
                 b <- variable "B" ["0", "1"]
                 cpt a [b] [0.5, 0.3, 0.5, 0.7]
                 cpt b [] [0.3, 0.7]
                 --p b [0.7, 0.3]
                 ) empty

bnet = buildBN $ do a <- variable "A" ["0", "1"]
                    b <- variable "B" ["0", "1"]
                    cpt a [b] [0.5, 0.3, 0.5, 0.7]
                    p b [0.3, 0.7]
