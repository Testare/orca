type Matrix a = [[a]]

--main = do
    --putStrLn "Ur mom"

main = interact wordCount
    where wordCount input = foldr (\x y-> x ++ ('\n':y)) [] $ map reverse $ lines $ input