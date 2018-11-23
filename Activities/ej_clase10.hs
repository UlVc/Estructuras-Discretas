module Main(main) where

    main :: IO ()
    main = do
      putStrLn "Hello World"
      line <- getLine
      putStrLn line