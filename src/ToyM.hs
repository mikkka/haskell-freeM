module ToyM where

import Control.Exception
import Free

data Toy b next =
    Output b next
  | Bell next
  | Done deriving Show

instance Functor (Toy a) where
  fmap f (Output x next) = Output x (f next)
  fmap f (Bell     next) = Bell     (f next)
  fmap _ Done            = Done


data Fix f = Fix (f (Fix f))

toy1 = Output 'A' Done

toy2 = Bell (Output 'A' Done)

fixToy1 = Fix (Output 'A' (Fix Done))

fixToy2 = Fix (Bell (Fix (Output 'A' (Fix Done))))



output x = liftF (Output x ())
bell     = liftF (Bell     ())
done     = liftF  Done


program = do
    output 'A'
    bell
    done

showI :: (Show a, Show r) => Free (Toy a) r -> String
showI (Free (Output a x)) =
    "output " ++ show a ++ "\n" ++ showI x
showI (Free (Bell x)) =
    "bell\n" ++ showI x
showI (Free Done) =
    "done\n"
showI (Pure r) =
    "return " ++ show r ++ "\n"

pretty :: (Show a, Show r) => Free (Toy a) r -> IO ()
pretty = putStr . showI

ringBell :: IO ()
ringBell = print "RING"

ioI :: (Show b) => Free (Toy b) r -> IO ()
ioI (Free (Output b next)) = print b  >> ioI next
ioI (Free (Bell     next)) = ringBell >> ioI next
ioI (Free Done           ) = return ()
ioI (Pure _) = throwIO (userError "improper termination")