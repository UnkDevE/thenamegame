import Data.Char 

isVowel :: Char -> Bool
isVowel c  
    | char == 'A' = True
    | char == 'E' = True
    | char == 'I' = True
    | char == 'O' = True
    | char == 'U' = True
    | otherwise = False
    where char = toUpper c 

shorten :: String -> String
shorten name  
    | isVowel $ head name = map toLower name
    | otherwise = map toLower $ tail name

theNameGame :: String -> String
theNameGame name =
    name ++ ", " ++ name ++ ", bo-b" ++ shorten name ++ "\n" ++
    "Banana-fana fo-f" ++ shorten name ++ "\n" ++ 
    "Fee-fi-mo-m" ++ shorten name ++ "\n" ++ 
    name ++ "!\n" 
    
main = do
    mapM_ (putStrLn . theNameGame) ["Gary", "Earl", "Billy", "Felix", "Mike", "Steve"]