main :: IO ()
main = putStrLn "Welcome to the interpreter! You can now run your expressions."

{-main = do
  putStrLn "Welcome to the interpreter! You can now run your expressions."
  putStr ">>> "
  hFlush stdout
  input <- getLine
  let sexpr = parseExpr input
      expr = parse sexpr
      val  = interp expr primEnv
  putStrLn $ "Result: " ++ serialize val-}