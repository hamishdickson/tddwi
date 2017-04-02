module Main

import Data.Vect

data VectUnknown : Type -> Type where
     MkVect : (len : Nat) -> Vect len a -> VectUnknown a

{-
readVect1 : IO (VectUnknown String)
readVect1 = do x <- getLine
              if (x == "")
                 then pure (MkVect _ [])
                 else do MkVect _ xs <- readVect1
                         pure (MkVect _ (x :: xs))
-}

readVect : IO (len ** Vect len String)
readVect = do x <- getLine
              if (x == "")
                then pure (_ ** [])
                else do (_ ** xs) <- readVect
                        pure (_ ** x :: xs)

readVectLen : (len : Nat) -> IO (Vect len String)
readVectLen Z = pure []
readVectLen (S k) = do x <- getLine
                       xs <- readVectLen k
                       pure (x :: xs)

anyVect : (n : Nat ** Vect n String)
anyVect = (3 ** ["a", "b", "c"])


zipInputs : IO ()
zipInputs = do putStrLn "Enter the first vector (blank line to end):"
               (len1 ** vec1) <- readVect
               putStrLn "Enter the second vector (blank line to end):"
               (len2 ** vec2) <- readVect
               case exactLength len1 vec2 of
                    Nothing => putStrLn "vects of different lengths"
                    Just vec2' => printLn (zip vec1 vec2')
