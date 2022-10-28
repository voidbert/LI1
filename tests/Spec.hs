{-
   Copyright 2022 Humberto Gomes, José Lopes

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}

module Main where

import Tarefa1_2022li1g012_Spec
import Tarefa2_2022li1g012_Spec
import Tarefa3_2022li1g012_Spec
import Tarefa4_2022li1g012_Spec
import Test.HUnit

runTestsT1 = runTestTT testsT1

runTestsT2 = runTestTT testsT2

runTestsT3 = runTestTT testsT3

runTestsT4 = runTestTT testsT4

main = runTestTTAndExit $ TestList [testsT1, testsT2, testsT3, testsT4]
