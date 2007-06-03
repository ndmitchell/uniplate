

count file = readFile file >>= print . length . lexList

lexList x = case lex x of
    [("",_)] -> []
    [(x,y)] -> x : lexList y

count2 file = readFile file >>= putStrLn . unlines . map f . group . lines
    where
        group ("":xs) = group xs
        group (x:"":xs) = x : group xs
        group (x:y :xs) = group ((x ++ "\n" ++ y) : xs)
        group x = x

        f x | null lst = "(blank)"
            | otherwise = head lst ++ " = " ++ show (length lst)
            where lst = lexList x
