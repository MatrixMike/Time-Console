{-# OPTIONS_GHC -fwarn-missing-signatures #-}

import Control.Applicative
import Control.Monad
import Data.Time
import System.Environment
import System.IO
import System.Process
import Text.Printf

sequenceWhileTrue :: Monad m => [m Bool] -> m ()
sequenceWhileTrue [] = return ()
sequenceWhileTrue (m:ms) = do
  r <- m
  if r
    then m >> sequenceWhileTrue ms
    else return ()

main :: IO ()
main = do
  (x:xs) <- getArgs
  putStrLn  x
--  putStrLn $ xs
  (_, Just hout, _, _) <- createProcess (proc x xs) {std_out = CreatePipe}
  sequenceWhileTrue
    (flip      --flip f takes its (first) two arguments in the reverse order of f
       map
       [1 ..]
       (\x -> do
          ((more, line0), time0) <-
            timeAction $ do
              more <- not <$> hIsEOF hout
              if more
                then do
                  hout' <- hGetLine hout
                  return (more, hout')
                else return (more, "")
          when
            more
            (putStrLn $ show x ++ ":" ++ printf "%f" time0 ++ ":" ++ line0)
          return more))

timeAction :: IO a -> IO (a, Float)
timeAction action = do
  t1 <- getCurrentTime
  g <- action
  t2 <- getCurrentTime
  let timeInUnits = realToFrac $ diffUTCTime t2 t1 :: Float
  return (g, timeInUnits)
