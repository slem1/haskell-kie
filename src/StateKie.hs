module StateKie (
    rollTwice,
    rollTwice'
) where

import Control.Monad.State.Lazy
import System.Random

rollTwice :: (Int, Int)
rollTwice = 
    let range = (1,6) :: (Int, Int) 
        (n, g') = randomR range (mkStdGen 0)
        (m, _) = randomR range g' in
    (n,m)

rollDice :: State StdGen Int 
rollDice = state (randomR (1,6))

rollTwice' :: (Int,Int)
rollTwice' = let (r, _) = runState rollTwice'' (mkStdGen 0)
    in r

rollTwice'' :: State StdGen (Int,Int)
rollTwice'' = do
    n <- rollDice
    m <- rollDice
    return (n, m)