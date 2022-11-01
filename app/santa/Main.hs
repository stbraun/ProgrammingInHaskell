-- | Santa example for STM (Software Transactional Memory) from 'Beautiful Code' chapter 'Beautiful Concurrency' by Simon Peyton Jones.


import Control.Concurrent.STM
import Control.Concurrent
import System.Random


main :: IO ()
main = do
    elf_group <- newGroup 3
    sequence_ [ elf elf_group n | n <- [1..10] ]
    rein_group <- newGroup 5
    sequence_ [ reindeer rein_group n | n <- [1..9] ]
    forever (santa elf_group rein_group)


santa :: Group -> Group -> IO ()
santa elf_gp rein_gp = do
    putStr "----------\n"
    (task, (in_gate, out_gate)) <- atomically (orElse
        (chooseGroup rein_gp "deliver toys")
        (chooseGroup elf_gp "meet in study"))
    putStr ("Ho! Ho! Ho! Let's " ++ task ++ "\n")
    operateGate in_gate
    -- now the helpers do their task
    operateGate out_gate
  where
    chooseGroup :: Group -> String -> STM (String, (Gate, Gate))
    chooseGroup gp task = do
        gates <- awaitGroup gp
        return (task, gates)


-- Fork off an elf thread looping indefinitely.
elf :: Group -> Int -> IO ThreadId
elf gp id = forkIO (forever (do
    elf1 gp id
    randomDelay))


-- Fork off a reindeer thread looping indefinitely.
reindeer :: Group -> Int -> IO ThreadId
reindeer gp id = forkIO (forever (do
    reindeer1 gp id
    randomDelay))


-- Repeatedly perform the action.
forever :: IO () -> IO ()
forever act = do
    act
    forever act


-- Delay for a random time between 1 and 1,000,000 microseconds.
randomDelay = do
    waitTime <- getStdRandom (randomR (1, 1000000))
    threadDelay waitTime


-- The elf action.
meetInStudy :: Int -> IO ()
meetInStudy id = putStr ("Elf " ++ show id ++ " meeting in the study.\n")


-- The reindeer action.
deliverToys :: Int -> IO ()
deliverToys id = putStr ("Reindeer " ++ show id ++ " delivering toys.\n")


-- Generalized for elves and reindeers
helper1 :: Group -> IO () -> IO ()
helper1 group do_task = do
    (in_gate, out_gate) <- joinGroup group
    passGate in_gate
    do_task
    passGate out_gate


elf1, reindeer1 :: Group -> Int -> IO ()
elf1 group id = helper1 group (meetInStudy id)
reindeer1 group id = helper1 group (deliverToys id)


-- A gate has a fixed capacity and a remaining capacity.
data Gate = MkGate Int (TVar Int)


-- A new gate is created with remaining capacity 0.
newGate :: Int -> STM Gate
newGate capacity = do
    tv <- newTVar 0
    return (MkGate capacity tv)


-- A gate can be passed if the remaining capacity is greater than 0. Otherwise it will block.
-- When successfully passed the remaining quantity is decremetned.
passGate :: Gate -> IO ()
passGate (MkGate cap tv) = atomically (do
  remaining <- readTVar tv
  check (remaining > 0)
  writeTVar tv (remaining - 1))


-- A gate is operated by pre-setting the remaining capacity to the fixed capacity of the gate.
-- Then the function waits until the remaining capacity is 0.
operateGate :: Gate -> IO ()
operateGate (MkGate capacity tv) = do
    atomically (writeTVar tv capacity)
    atomically ( do
        remaining <- readTVar tv
        check (remaining == 0))


-- A group has a capacity, a remaining capacity, and in_gate and out_gate.
data Group = MkGroup Int (TVar (Int, Gate, Gate))


newGroup :: Int -> IO Group
newGroup n = atomically (do
    g1 <- newGate n
    g2 <- newGate n
    tv <- newTVar (n, g1, g2)
    return (MkGroup n tv))


joinGroup :: Group -> IO (Gate, Gate)
joinGroup (MkGroup n tv) = atomically (do
    (remaining, g1, g2) <- readTVar tv
    check (remaining > 0)
    writeTVar tv (remaining - 1, g1, g2)
    return (g1, g2))


-- Wait for the group to be full, then immediately reinitialize with fresh gates.
awaitGroup :: Group -> STM (Gate, Gate)
awaitGroup (MkGroup n tv) = do
    (remaining, g1, g2) <- readTVar tv
    check (remaining == 0)
    new_g1 <- newGate n
    new_g2 <- newGate n
    writeTVar tv (n, new_g1, new_g2)
    return (g1, g2)


