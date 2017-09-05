import Data.List (intercalate, intersperse)

main :: IO ()
main = putStrLn $ triangle 4
  where
    triangle n = intercalate "\n" $ map line [1..n]
    line i = intersperse ' ' $ take i $ drop (i `mod` 2) $ zeroOnes
    zeroOnes = '0' : '1' : zeroOnes
