module Vocabulary where
import Char
import Directory
import Control.Monad
import System.IO 
import System.Random

-- (v-dictionary, d-direction)
practice v d = do
    -- Select random word
    randomNumber <- randomRIO (0::Int,((length v)-1)::Int)
    randomTuple  <- return (v !! randomNumber)
    
    -- Ask for translation
    putStr "\nTranslate: "
    if d then putStr (show (fst randomTuple)) else putStr (show (snd randomTuple))
    putStr " (type k to quit)\n"
    
    x <- getLine
    
    -- Are we finished?
    if x == "k" 
        then do 
             putStr "\nGood bye! The answer was "
           
             -- Display correct answer
             if d then putStr (show (snd randomTuple)) else putStr (show (fst randomTuple))
        else do 
             -- Check if the answer is correct
             if d 
                then
                    if (snd randomTuple) == x then putStr "\nWell done!\n" else putStr "\nIncorrect answer\n"
                else
                    if (fst randomTuple) == x then putStr "\nWell done!\n" else putStr "\nIncorrect answer\n"
                
             practice v d
             
-- Lets user select dictionary file
getCustomVocabulary = do
    -- Get files in current dir
    currentDir <- getCurrentDirectory
    directoryListing <- getDirectoryContents currentDir
    
    -- Select only dictionary files
    vocabularies <- return [x | x <- directoryListing, isVocabularyFilePath x] 
    
    putStrLn ""
    
    -- Print listing
    vocabularyNames <- return (zipWith vocabularyName [0..] vocabularies)
    putStrLn (concat vocabularyNames)
    putStrLn "Select dictionary by entering its number:\n"
    
    -- Get selected number
    x <- getLine
                     
    -- Return path to selected dictionary file
    path <- return (vocabularies !! (read x))
    
    result <- getVocabulary path
    return result

-- Reads dictionary file based on path
getVocabulary str = do
    handle <- openFile str ReadMode  
    
    -- Load file
    contents <- hGetContents handle
    
    items <- return (lines contents) 
    
    -- Return items in the form of tuples
    vocabulary <- return (map getWords items)
    
    return vocabulary

-- Asks user in which direction he wants to translate
getDirection = do
    putStr "\nDo you want to keep original direction for translation (yes) or to reverse it (no):\n"
    x <- getLine
    if(x == "yes") then return True else return False

-- Get tuple for words in one line
getWords :: [Char] -> ([Char],[Char])
getWords x = (firstWord x, secondWord x)

-- Returns first word
firstWord :: [Char] -> [Char]
firstWord x = if (last x) == ';' then init x else firstWord (init x)

-- Returns second word
secondWord :: [Char] -> [Char]
secondWord (s:x) = if s == ';' then x else (secondWord x)

-- Checks whether file is a dictionary file (based on .txt extension)
isVocabularyFilePath str = if (drop ((length str)-4) str) == ".txt" then True else False

-- Prints vocabulary name and number for selection
vocabularyName number str = (show number) ++ ": " ++ (take ((length str)-4) str) ++ "\n"