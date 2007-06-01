

count file = readFile file >>= print . length . lexList

lexList x = case lex x of
    [("",_)] -> []
    [(x,y)] -> x : lexList y
