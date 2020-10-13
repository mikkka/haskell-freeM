{-# language DeriveFunctor #-}
module Game where

import Control.Monad
import Control.Monad.Free

data Direction = Left | Up | Right | Down | Forward | Back
type Image = [Char] 

data Interaction next =
    Look Direction (Image -> next)
  | Fire Direction next
  | ReadLine (String -> next)
  | WriteLine String (Bool -> next) deriving (Functor)


type Program = Free Interaction

look :: Direction -> Program Image
look dir = liftF (Look dir id)

fire :: Direction -> Program ()
fire dir = liftF (Fire dir ())

readLine :: Program String
readLine = liftF (ReadLine id)

writeLine :: String -> Program Bool
writeLine s = liftF (WriteLine s id)

easyToAnger :: Program a
easyToAnger = forever $ do
    str <- readLine
    when (str == "No") $ do
        fire Forward
        -- Ignore the Bool returned by writeLine
        _ <- writeLine "Take that!"
        return ()
