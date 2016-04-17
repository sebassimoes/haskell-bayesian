import           Bayesian.Variable
import           Bayesian.Factor
import           Data.Set
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = do
  --defaultMain unitTest
  print instantiateAOk
  print instantiateANothing
  print createFactorTest
--main =
--  print $ "graph edges: " ++ (show.edges) adjencyListGraph

unitTest = testGroup "Sums"
  [ testCase "Addition" $ 1 + 1 @?= 2
  , testCase "Multiplication" $ 2 * 2 @?= 4
  ]

testMultiplicationZero :: TestTree
testMultiplicationZero = testCase "Multiplication" $ 2 * 0 @?= 0

variableA :: Variable
variableA = createVariable "A" $ (insert "2".insert "1") empty

variableB :: Variable
variableB = createVariable "B" $ (insert "3".insert "2".insert "1") empty

instantiateAOk :: Either String VariableInstantiation
instantiateAOk = instantiateVariable "1" variableA

instantiateANothing :: Either String VariableInstantiation
instantiateANothing = instantiateVariable "0" variableA

createFactorTest :: Factor
createFactorTest =
  let vars = (insert variableA.insert variableB) empty
      Right instantiation1 = sequence [instantiateVariable "1" variableA, instantiateVariable "2" variableB]
      Right instantiation2 = sequence [instantiateVariable "1" variableA, instantiateVariable "1" variableB]
      Right instantiation3 = sequence [instantiateVariable "2" variableA, instantiateVariable "1" variableB]
      Right instantiation4 = sequence [instantiateVariable "2" variableA, instantiateVariable "2" variableB]
  in  buildFactor "A" vars (buildFactorValue instantiation1 0.3 >>
                            buildFactorValue instantiation2 0.7 >>
                            buildFactorValue instantiation3 0.8 >>
                            buildFactorValue instantiation4 0.2)
