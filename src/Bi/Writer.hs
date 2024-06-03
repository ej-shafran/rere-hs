module Bi.Writer (
    blobFieldW,
    intFieldW,
) where

blobFieldW :: String -> String -> String
blobFieldW name value = ":b " ++ name ++ " " ++ show (length value) ++ "\n" ++ value ++ "\n"

intFieldW :: String -> Int -> String
intFieldW name value = ":i " ++ name ++ " " ++ show value ++ "\n"
