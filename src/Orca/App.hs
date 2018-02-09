module Orca.App (selectOp) where
type AppOp = (String, IO ())

selectOp :: [AppOp] -> IO ()

selectOp ops = do
    putStrLn "/* Choose op */"
    putStrLn $ displayOps ops
    integer <- readLn
    snd $ ops !! integer

displayOps :: [AppOp] -> String
displayOps = disp 0 
    where disp n [] = ""
          disp n (x:xs) = (show n) ++ ". " ++ (fst x) ++ "\n" ++ (disp (succ n) xs)