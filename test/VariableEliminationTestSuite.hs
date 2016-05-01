import Bayesian.CheckedError
import Bayesian.Factor
import Bayesian.Variable
import Bayesian.Inference.VariableElimination
import Data.Either
import Data.List

main :: IO ()
main = do
  print vePr1Test1
  print vePr1Test2
  print vePr1Test3
  print vePr1Test4

variableA :: Variable
variableA = createVariable "A" ["0", "1"]

variableB :: Variable
variableB = createVariable "B" ["0", "1"]

variableC :: Variable
variableC = createVariable "C" ["0", "1"]

factorA :: Checked Factor
factorA = buildFactor "A" vars factorValues
  where vars = [variableA]
        factorValues = do
          buildFactorValueFromInstantiation ["1"] 0.6
          buildFactorValueFromInstantiation ["0"] 0.4

factorB :: Checked Factor
factorB = buildFactor "B" vars factorValues
  where vars = [variableA, variableB]
        factorValues = do
          buildFactorValueFromInstantiation ["1", "1"] 0.9
          buildFactorValueFromInstantiation ["1", "0"] 0.1
          buildFactorValueFromInstantiation ["0", "1"] 0.2
          buildFactorValueFromInstantiation ["0", "0"] 0.8

factorC :: Checked Factor
factorC = buildFactor "C" vars factorValues
  where vars = [variableB, variableC]
        factorValues = do
          buildFactorValueFromInstantiation ["1", "1"] 0.3
          buildFactorValueFromInstantiation ["1", "0"] 0.7
          buildFactorValueFromInstantiation ["0", "1"] 0.5
          buildFactorValueFromInstantiation ["0", "0"] 0.5

vePr1Test1 = vePr1 [variableB, variableA] $ rights [factorA, factorB, factorC]
vePr1Test2 = vePr1 [variableA, variableB] $ rights [factorA, factorB, factorC]
vePr1Test3 = vePr1 [variableC, variableB] $ rights [factorA, factorB, factorC]
vePr1Test4 = vePr1 [variableA, variableC] $ rights [factorA, factorB, factorC]
