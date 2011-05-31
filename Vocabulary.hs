module Vocabulary where
import Char
import Directory
import Control.Monad
import System.IO 
import System.Random

-- Smycka pro procvicovani slovicek (v-slovnik, d-smer)
practice v d = do
    -- Vyberu nahodne slovo
    randomNumber <- randomRIO (0::Int,((length v)-1)::Int)
    randomTuple  <- return (v !! randomNumber)
    
    -- Zeptam se na preklad
    putStr "\nPrelozte: "
    if d then putStr (show (fst randomTuple)) else putStr (show (snd randomTuple))
    putStr " (k pro konec)\n"
    
    x <- getLine
    
    -- Konec?
    if x == "k" 
        then do 
             putStr "\nNashledanou! Spravna odpoved byla "
           
             -- Na zaver zobrazim jeste spravnou odpoved
             if d then putStr (show (snd randomTuple)) else putStr (show (fst randomTuple))
        else do 
             -- Podle smeru prekladu zkontroluju odpoved
             if d 
                then
                    if (snd randomTuple) == x then putStr "\nDobre\n" else putStr "\nSpatne\n"
                else
                    if (fst randomTuple) == x then putStr "\nDobre\n" else putStr "\nSpatne\n"
                
             -- Zavolam sebe sama - smycka
             practice v d
             
-- Dame uzivateli na vyber z dostupnych slovniku, vratime cestu k souboru slovniku ktery si vybere
getCustomVocabulary = do
    -- Zjistim jake soubory jsou v nasi slozce
    currentDir <- getCurrentDirectory
    directoryListing <- getDirectoryContents currentDir
    
    -- Necham pouze .txt soubory, ktere identifikuji slovniky
    vocabularies <- return [x | x <- directoryListing, isVocabularyFilePath x] 
    
    putStrLn ""
    
    -- Dam uzivateli na vyber
    vocabularyNames <- return (zipWith vocabularyName [0..] vocabularies)
    putStrLn (concat vocabularyNames)
    putStrLn "Zadejte cislo slovniku, ktery chcete nacist:\n"
    
    -- Vstup od uzivatele
    x <- getLine
                     
    -- Vratim cestu k souboru, ktery se ma nacist
    path <- return (vocabularies !! (read x))
    
    result <- getVocabulary path
    return result

-- Na zaklade cesty k souboru dany soubor nacte
getVocabulary str = do
    handle <- openFile str ReadMode  
    
    -- Nactu obsah souboru
    contents <- hGetContents handle
    
    -- Vytvorim si pole radku - zaznamu
    items <- return (lines contents) 
    
    -- Prevedu kazdy zaznam na tuple
    vocabulary <- return (map getWords items)
    
    return vocabulary

-- Funkce direction se zepta uzivatele zda chce odpovidat na slovo v beznem "smeru" nebo chce poradi slov prevratit
getDirection = do
    putStr "\nChcete zachovat standardni smer prekladu (ano) nebo ho obratit (ne):\n"
    x <- getLine
    if(x == "ano") then return True else return False

-- Vrati nam tuple slov na radku
getWords :: [Char] -> ([Char],[Char])
getWords x = (firstWord x, secondWord x)

-- Vrati nam prvni slovo v radku (pred strednikem)
firstWord :: [Char] -> [Char]
firstWord x = if (last x) == ';' then init x else firstWord (init x)

-- Vrati nam druhe slovo v radku (za strednikem)
secondWord :: [Char] -> [Char]
secondWord (s:x) = if s == ';' then x else (secondWord x)

-- Funkce, ktera urci zda nechat v seznamu dany soubor (necha pouze .txt soubory, ktere identifikuji slovnik)
isVocabularyFilePath str = if (drop ((length str)-4) str) == ".txt" then True else False

-- Funkce pro vytvoreni vyberu slovniku - zbavi nas pripony souboru a prida cislo
vocabularyName number str = (show number) ++ ": " ++ (take ((length str)-4) str) ++ "\n"