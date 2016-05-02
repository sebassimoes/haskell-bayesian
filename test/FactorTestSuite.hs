import           Bayesian.CheckedError
import           Bayesian.Variable
import           Bayesian.Factor
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.Either

main :: IO ()
main = defaultMain $ testGroup "Factor Tests" [
         testInstantiation,
         testCreateFactor,
         testMultiplyFactor,
         testSummingOut
       ]


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

createFactor1 :: Either CheckedError Factor
createFactor1 =
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


-- tests --
testInstantiation = testGroup "Variable instantiation" [
    testCase "Valid instantiation" $ assertBool "isRight was expected" (isRight instantiateAOk),
    testCase "Invalid instantiation" $
      assertEqual "InvalidVariableInstantiation was expected" instantiateANothing (Left InvalidVariableInstantiation)
  ]

testCreateFactor =
  testGroup "Factor creation" [
     testCaseCreateFactorValid,
     testCaseCreateFactorVars,
     testCaseCreateFactorInconsistentVariableInstantiation,
     testCaseCreateIncompleteFactor,
     testCaseDuplicatedVariableInstantiation
   ]

testMultiplyFactor =
  testGroup "Factor multiplication" [
      testCaseFactorMultiplicationValid,
      testCaseFactorMultiplicationVars,
      testCaseFactorMultiplicationFactorValues
    ]

testSummingOut =
  testGroup "Summing out variables from factor" [
      testCaseFactorSummingOutVariable,
      testCaseFactorSummingOutNonExistentVariable,
      testeCaseFactorSummingOutValue
    ]

testCaseCreateFactorValid =
  testCase "Valid Factor" $
    assertBool "isRight was expected" (isRight createFactor1)

testCaseCreateFactorVars =
  let (Right factor) = createFactor1
  in testCase "Factor Vars" $
       assertEqual "" [variableA, variableB] (factorVars factor)

testCaseCreateFactorInconsistentVariableInstantiation =
  let instantiation1 = rights [instantiateVariable "1" variableA]
      checkedFactor =
        buildFactor "Invalid" [variableA', variableB'] (buildFactorValue instantiation1 0.6)
  in testCase "Variable instantiation inconsistent with factor variables" $
       assertEqual "" (Left InconsistentVariableInstantiation) checkedFactor

testCaseCreateIncompleteFactor =
  let checkedFactor = buildFactor "Invalid" [variableB'] (buildFactorValueFromInstantiation ["M"] 0.8)
  in testCase "Incomplete factor" $
       assertEqual "" (Left IncompleteFactor) checkedFactor

testCaseDuplicatedVariableInstantiation =
  let checkedFactor = buildFactor "Invalid" [variableB'] $
                        buildFactorValueFromInstantiation ["M"] 0.8 >>
                        buildFactorValueFromInstantiation ["M"] 0.3
  in testCase "Duplicated instantiation" $
       assertEqual "" (Left DuplicatedVariableInstantiation) checkedFactor


testCaseFactorMultiplicationValid =
  let checkedFactor = multiplyFactorTest
  in testCase "Multiplication returns valid factor" $
       assertBool "" $ isRight checkedFactor

testCaseFactorMultiplicationVars =
  let Right factor = multiplyFactorTest
  in testCase "Multiplication returns union of factors vars" $
       assertEqual "" [variableA', variableB', variableC] $ factorVars factor

testCaseFactorMultiplicationFactorValues =
  let Right factor = multiplyFactorTest
      expectedFactorValue =
        createFactorValue (rights [instantiateVariable "true" variableA',
                                   instantiateVariable "M" variableB',
                                   instantiateVariable "true" variableC]) 0.24
  in testCase "Contains instantiation" $
       assertBool ("The factor value " ++ show expectedFactorValue ++ " was expected to be found") $
         expectedFactorValue `elem` factorValues factor

testCaseFactorSummingOutVariable =
  let Right factor = factorA'
      summedOutB = summingOut [variableB'] factor
  in testCase "Summed variable is out of the returned factor" $
       assertEqual "" [variableA'] $ factorVars summedOutB

testCaseFactorSummingOutNonExistentVariable =
  let Right factor = factorA'
      summedOutB = summingOut [variableB] factor
  in testCase "Summing out a variable not belonging to the factor" $
       assertEqual "" [variableA', variableB'] $ factorVars summedOutB

testeCaseFactorSummingOutValue =
  let Right factor = factorA'
      summedOutB = summingOut [variableB'] factor
      expectedFactorValue = createFactorValue (rights [instantiateVariable "true" variableA']) 1.4
  in testCase "Contains instantiation" $
       assertBool ("The factor value " ++ show expectedFactorValue ++ " was expected to be found") $
         expectedFactorValue `elem` factorValues summedOutB
