import qualified Data.Graph
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.List
import Bayesian.BayesianNetwork

main :: IO ()
main = do
  --print $ mapM id [["1", "2"], ["true", "false"], ["A", "B", "C", "D"], ["0", "1", "2", "3", "4", "5"]]
  print fglTestNode
  --print fglGraph1
  --print fglDisconnected

myGraph :: Data.Graph.Graph
myGraph =  Data.Graph.buildG bounds edges
  where
    bounds = (1, 3)
    edges = [(1, 2), (2, 3)]

adjencyListGraph :: Data.Graph.Graph
adjencyListGraph = fst $ Data.Graph.graphFromEdges' [("A", 1, [2]), ("B", 2, [3]), ("C", 3, [])]

type Factor = Int
type Variable = (String, Factor)

fglTestNode :: LNode Variable
fglTestNode = (1, ("A", 2))

fglGraph1 :: Gr String String
fglGraph1 = ([("B -> C", 2)], 3, "c", []) & (([("A -> B",1)], 2, "b", []) & (([],1,"a",[]) & empty))

fglDisconnected :: Gr Char ()
fglDisconnected = ([], 1, 'B', []) & (([], 0, 'A', [((), 3)]) & empty)
