module Thread where

-- import Free
import Control.Monad.Free

thread1 :: Free IO ()
thread1 = do
    liftF $ print 1
    liftF $ print 2

thread2 :: Free IO ()
thread2 = do
    str <- liftF $ getLine
    liftF $ putStrLn str

interleave :: (Monad m) => Free m r -> Free m r -> Free m r
interleave (Free m1) (Free m2) = do
    next1 <- liftF m1
    next2 <- liftF m2
    interleave next1 next2
interleave t1 (Pure _) = t1
interleave (Pure _) t2 = t2

runThread :: (Monad m) => Free m r -> m r
runThread (Free m) = m >>= runThread
runThread (Pure r) = return r