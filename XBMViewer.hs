import Data.Char
import Data.List
import Data.Maybe
import System.Environment
import Control.Monad

main = do
    x <- getArgs
    mapM_ showXBMFile x

showXBMFile fname = do
    content <- readFile fname
    let (spec, validContent) = span ((=='#').head) $ lines content
    let (w,h) = loadSpec spec
    putStrLn $ "[Info] Filename = " ++ fname ++ " (W,H) = " ++ show (w,h)
    let needW = ceiling (fromIntegral w / 8)
    let bitData = map read $ map (take 4) $ words $  takeWhile (/='}') $ tail $ dropWhile (/='{') $ concat validContent :: [Int]
    let groupedBitData = takeRepeat needW bitData 
    let convertedData = map (take w) $ map (concatMap (reverse.padStr.intToBin)) groupedBitData
    mapM_ putStrLn $ map (map changeRep) convertedData


changeRep '0' = ' '
changeRep '1' = 'x'

takeRepeat len [] = []
takeRepeat len xs = a : takeRepeat len b
    where
        (a,b) = splitAt len xs

convertSpec :: String -> (Char, Int)
convertSpec specStr = if field == "width"
    then ('W', read $ ws !! 2)
    else ('H', read $ ws !! 2)
    where
        ws = words specStr
        field = tail $ dropWhile (/='_') $ ws !! 1

loadSpec :: [String] -> (Int, Int)
loadSpec xs = (w,h)
    where
        ((_,w),(_,h)) = (head $ filter ((=='W').fst) specs, head $ filter ((=='H').fst) specs)
        specs = map convertSpec xs

intToBin n = if n < 2
    then [chToBin n]
    else intToBin (n `div` 2) ++ [chToBin $ n `mod` 2]
    where
        chToBin x = chr $ ord '0' + x

padStr str = if length str < 8 then replicate (8-length str) '0' ++ str else str
