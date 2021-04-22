import Data.List
import System.IO

calcSwaps :: [Int] -> [Int] -> Int -> Int

calcSwaps carriages lastLeftCarriageList inputLength = do
  if (length carriages) == 1 then 0
  else do
    let firstElement = carriages !! 0
    let secondElement = carriages !! 1

    if firstElement < secondElement then do
      let leftCarriageList = lastLeftCarriageList ++ [firstElement]
      calcSwaps (tail carriages) [] (inputLength - 1)
    else do
      let rightCarriageList = [firstElement] ++ (tail (tail carriages))
      let leftCarriageList = lastLeftCarriageList ++ [secondElement]

      if (length leftCarriageList) == (inputLength - 1) then
        1 + (calcSwaps rightCarriageList leftCarriageList inputLength) + (calcSwaps leftCarriageList [] (inputLength - 1))
      else
        1 + (calcSwaps rightCarriageList leftCarriageList inputLength)

main = do
  let input1 = [1, 3, 2]
  let input2 = [4, 3, 2, 1]
  let input3 = [2, 1]

  print "Optimal number of swaps: "
  print (calcSwaps input1 [] 3)

  print "Optimal number of swaps: "
  print (calcSwaps input2 [] 4)

  print "Optimal number of swaps: "
  print (calcSwaps input3 [] 2)
