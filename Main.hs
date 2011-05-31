module Main where
import Vocabulary
	     
main :: IO ()
main = do
       -- Dam uzivateli vybrat slovnik  
	   vocab <- getCustomVocabulary
	   
	   -- Dam uzivateli vybrat smer prekladu
	   direction <- getDirection
	   
	   -- Spustim hlavni smycku
	   practice vocab direction