module Main where
import Vocabulary
	     
main :: IO ()
main = do
        -- Let the user choose dictionary
        vocab <- getCustomVocabulary
	   
        -- Let the user choose direction for translation
        direction <- getDirection
	   
        -- Run main loop
        practice vocab direction