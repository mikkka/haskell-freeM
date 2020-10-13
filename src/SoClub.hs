{-# LANGUAGE DeriveFunctor, TypeOperators, RankNTypes #-}

module SoClub where

import Control.Monad
import Control.Monad.Free
import Data.List
import Data.Functor.Sum

---- HELPERZ ------

infixr 0 ~>
type f ~> g = forall x. f x -> g x

freeM :: (Functor f, Functor g) => f ~> g -> Free f ~> Free g
freeM phi (Pure x) = Pure x
freeM phi (Free fx) = Free $ phi (freeM phi <$> fx)

monad :: Monad m => Free m ~> m
monad (Pure x) = pure x
monad (Free mfx) = do
  fx <- mfx
  monad fx

interp :: (Functor f, Monad m) => f ~> m -> Free f ~> m
interp phi = monad . freeM phi

---- SUMZ ----

left :: (Functor f, Functor g) => Free f ~> Free (Sum f g)
left = freeM InL

right :: (Functor f, Functor g) => Free g ~> Free (Sum f g)
right = freeM InR

sumNat :: (f ~> t) -> (g ~> t) -> (Sum f g) ~> t
sumNat phi _   (InL x) = phi x
sumNat _   psi (InR x) = psi x

---- HELPERZ ------

----- IO Lang -----

data KeyValF a
  = GetKey String (Maybe String -> a)
  | PutKey String String a
  deriving (Functor)

data ConsoleF a
  = PutStrLn String a
  | GetLine (String -> a)
  deriving (Functor)

type Console = Free ConsoleF
type KeyVal = Free KeyValF

getKey :: String -> KeyVal (Maybe String)
getKey k = liftF (GetKey k id)

putKey :: String -> String -> KeyVal ()
putKey k v = liftF (PutKey k v ())

putStrLnC :: String -> Console ()
putStrLnC s = liftF (PutStrLn s ())

getLineC :: Console String
getLineC = liftF (GetLine id)

----- IO Lang -----

---- BUZ Lang -----

data ClubF a
  = GetClubMembers String (Maybe [String] -> a)
  | GetMemberClubs String (Maybe [String] -> a)
  | GetInput (String -> a)
  | Display String a
  deriving (Functor)

type Club = Free ClubF

getClubMembers :: String -> Club (Maybe [String])
getClubMembers s = liftF (GetClubMembers s id)

getMemberClubs :: String -> Club (Maybe [String])
getMemberClubs s = liftF (GetMemberClubs s id)

getInput :: Club String
getInput = liftF (GetInput id)

display :: String -> Club ()
display s = liftF (Display s ())

---- BUZ Lang -----

---- PROGRAM ------

showClubSiblings :: Club ()
showClubSiblings = do
  display "Enter club Id:"
  clubId <- getInput
  mmembers <- getClubMembers clubId
  case mmembers of
    Nothing -> display "Sorry, that club does not exist!"
    Just members -> do
      r <- sequence <$> traverse getMemberClubs members
      case r of
        Nothing -> display "Error getting club members."
        Just clubIdGroups -> do
          let siblings = nub $ concat clubIdGroups
          display $ "Here are the siblings of club " ++ clubId ++ ":"
          display (intercalate ", " siblings)

---- PROGRAM ------

---- INTERP IO ----

consoleIO :: ConsoleF ~> IO
consoleIO (PutStrLn s v) = do
  putStrLn s
  pure v
consoleIO (GetLine cb) = do
  s <- getLine
  pure (cb s)

-- KeyValue in IO via Redis.
keyValIO :: KeyValF ~> IO
keyValIO (GetKey k cb) = do
  putStrLn $ "redis lookup: " ++ k
  r <- getLine 
  pure (cb $ Just r)

keyValIO (PutKey k v n) = do
  putStrLn $ "redis put: " ++ k ++ " " ++ v
  pure n

---- INTERP IO ----

---- INTERP BUZ ---

clubI :: ClubF ~> (Free (Sum ConsoleF KeyValF))
clubI (GetClubMembers clubId next) = do
  r <- right $ getKey ("clubs." ++ clubId ++ ".members")
  pure $ next (words <$> r)
clubI (GetMemberClubs memberId next) = do
  r <- right $ getKey ("users." ++ memberId ++ ".clubs")
  pure $ next (words <$> r)
clubI (GetInput next) = do
  r <- left getLineC
  pure $ next r
clubI (Display o next) = do
  left $ putStrLnC o
  pure next

---- INTERP BUZ ---
sumInterp :: Free (Sum ConsoleF KeyValF) x -> IO x
sumInterp = interp (sumNat consoleIO keyValIO) 

clubInterp :: Free ClubF x -> Free (Sum ConsoleF KeyValF) x
clubInterp = interp clubI

compoundInterp :: Free ClubF x -> IO x
compoundInterp = sumInterp . clubInterp

io = compoundInterp showClubSiblings