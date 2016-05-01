import           Bayesian.CheckedError
import           Bayesian.Variable
import           Bayesian.Factor
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.Either

main :: IO ()
main = do
  --defaultMain unitTest
  --print instantiateAOk
  --print instantiateANothing
  --print createFactorTest
  print $ 0.2 * 0.7
  print multiplyFactorTest
  print summingOutTest
--main =
--  print $ "graph edges: " ++ (show.edges) adjencyListGraph

unitTest = testGroup "Sums"
  [ testCase "Addition" $ 1 + 1 @?= 2
  , testCase "Multiplication" $ 2 * 2 @?= 4
  ]

testMultiplicationZero :: TestTree
testMultiplicationZero = testCase "Multiplication" $ 2 * 0 @?= 0

variableA :: Variable
variableA = createVariable "A" ["1", "2"]

variableA' :: Variable
variableA' = createVariable "A" ["false", "true"]

variableB :: Variable
variableB = createVariable "B" ["1", "2", "3"]

variableB' :: Variable
variableB' = createVariable "B" ["M", "F"]


variableC :: Variable
variableC = createVariable "C" ["false", "true"]

instantiateAOk :: Either CheckedError VariableInstantiation
instantiateAOk = instantiateVariable "1" variableA

instantiateANothing :: Either CheckedError VariableInstantiation
instantiateANothing = instantiateVariable "0" variableA

createFactorTest :: Either CheckedError Factor
createFactorTest =
  let vars = [variableA, variableB]
      instantiation1 = rights [instantiateVariable "1" variableA, instantiateVariable "1" variableB]
      instantiation2 = rights [instantiateVariable "1" variableA, instantiateVariable "2" variableB]
      instantiation3 = rights [instantiateVariable "2" variableA, instantiateVariable "1" variableB]
      instantiation4 = rights [instantiateVariable "2" variableA, instantiateVariable "2" variableB]
      instantiation5 = rights [instantiateVariable "1" variableA, instantiateVariable "3" variableB]
      instantiation6 = rights [instantiateVariable "2" variableA, instantiateVariable "3" variableB]
  in  buildFactor "A" vars (buildFactorValue instantiation1 0.3 >>
                            buildFactorValue instantiation2 0.7 >>
                            buildFactorValue instantiation3 0.8 >>
                            buildFactorValue instantiation4 0.2 >>
                            buildFactorValue instantiation5 0.1 >>
                            buildFactorValue instantiation6 0.5)

factorA = buildFactor "A" vars factorValues
  where vars = [variableA]
        factorValues = do
          buildFactorValueFromInstantiation ["1"] 0.4
          buildFactorValueFromInstantiation ["2"] 0.6

factorB = buildFactor "B" vars factorValues
  where vars = [variableA, variableB]
        factorValues = do
          buildFactorValueFromInstantiation ["1", "1"] 0.4
          buildFactorValueFromInstantiation ["1", "2"] 0.3
          buildFactorValueFromInstantiation ["1", "3"] 0.3
          buildFactorValueFromInstantiation ["2", "1"] 0.1
          buildFactorValueFromInstantiation ["2", "2"] 0.3
          buildFactorValueFromInstantiation ["2", "3"] 0.6

factorA' = buildFactor "A" vars factorValues
  where vars = [variableA', variableB']
        factorValues = do
          buildFactorValueFromInstantiation ["true", "M"] 0.8
          buildFactorValueFromInstantiation ["true", "F"] 0.6
          buildFactorValueFromInstantiation ["false", "M"] 0.2
          buildFactorValueFromInstantiation ["false", "F"] 0.4

factorC = buildFactor "C" vars factorValues
  where vars = [variableC]
        factorValues = do
          buildFactorValueFromInstantiation ["true"] 0.3
          buildFactorValueFromInstantiation ["false"] 0.7


multiplyFactorTest :: Either CheckedError Factor
multiplyFactorTest =
  do fa <- factorA'
     fc <- factorC
     return $ multiply fa fc

summingOutTest :: Either CheckedError Factor
summingOutTest =
  do fa <- factorA'
     return $ summingOut [variableB'] fa
