import qualified Text.Parsec as Parsec

parse rule text = Parsec.parse rule ""  text

frontItem :: String -> Either Parsec.ParseError [Char]
frontItem text = parse (Parsec.many Parsec.letter) text

backItem :: String -> String
backItem [x] = "none"
backItem (x:xs)  = do
    if x == ':' 
        then xs
        else backItem xs

main = do
    let item1 = frontItem "test:param" 
    case item1 of
        Right c -> putStrLn (show c)
        Left err -> putStrLn (show err)
    print $ backItem "test:param"
