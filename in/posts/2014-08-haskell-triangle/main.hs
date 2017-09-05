import Data.List (intercalate)

main :: IO ()
main = putStrLn $ triangle 4
  where
    -- a triangle with n rows
    triangle n = intercalate "\n" $ map line [1..n]
    -- the i^th line of a triangle
    line i = intercalate " " $ map show $ take i $ alternation i
    -- list of 0 and 1, starting with i mod 2
    alternation i = map (`mod` 2) [i..]
