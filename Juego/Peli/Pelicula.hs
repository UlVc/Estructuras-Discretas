module Main(main) where

    import Control.Monad 
    import System.IO
    import System.Random
    import Emojis
    import Data.Emoji

    main :: IO ()
    main = do
     menu

    menu :: IO ()
    menu = do
     putStrLn " [1] Jugar \n [2] Instrucciones \n [3] Salir"
     x <- getChar
     case x of 
        '1' -> opcion1
        '2' -> opcion2
        '3' -> putStrLn "Ok. Ciao"
        otherwise -> menu

    opcion2 :: IO ()
    opcion2 = do 
     putStrLn "Instrucciones: \n\nLorem ipsum dolor sit amet, at est odio corpora invidunt, ornatus voluptatum ei eos. Id quo partem sapientem gubergren, vim ei dico quidam aperiri. Natum exerci appellantur ne vix, ut mei utamur disputationi. Ut graeci eruditi mea. Minim elitr apeirian ei his, ut falli temporibus vel. Cu ius solum fugit sapientem. Iriure nusquam at eum, mel dicam efficiendi neglegentur te, eum munere complectitur no. \n\nPresiona intro para continuar"
     x <- getChar
     c <- getChar
     if c == '\n'
        then do
            main
        else
            opcion2

    opcion1 :: IO ()
    opcion1 = do
        x <- randomRIO (0,120) :: IO Int
        let a = rand x
        let fa = fst a
        putStrLn $ snd a ++ "  " ++ fst a
        b <- getLine
        u <- getLine
        case u of 
            fa -> putStrLn "GENIAL"
            otherwise -> putStrLn "Nope"
        -- if a /= 130
        --     then do
        --         putStrLn "Hola"
        --     else
        --         putStrLn "Adios"
    --let a = elem $ [("","")]
    --putStrLn "" ++ a


   -- busca :: IO () 
   -- busca = do 
   --     let x = randomRIO (0,86)
   --     a <- lectura
       
