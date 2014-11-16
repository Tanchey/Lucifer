import qualified Lucifer as Luc
import Control.Monad
import Data.Maybe
 
main :: IO ()
main = loop (Just ((Luc.Digit 0), (Luc.Digit 0)))

loop :: Maybe (Luc.Digit, Luc.Digit) -> IO()
loop Nothing = putStrLn "Whoooops"
loop (Just (work, broken)) = do
        foo <- putStrLn "Okay, google. What does traffic light say?"  
        digitString <- getLine  
        let (candidates, settings) = settingsTuple digitString work broken
        putStrLn ("Aha! " ++ show candidates ++ "\n" ++ show settings)
        loop settings

settingsTuple :: String -> Luc.Digit -> Luc.Digit -> (Maybe [Luc.Digit], Maybe (Luc.Digit, Luc.Digit))
settingsTuple digitString w b | isNothing maybeDigit = (Nothing, Nothing)
                              | isNothing details = (Nothing, Nothing)
                              | otherwise = reformat $ fromJust details
                                    where maybeDigit = Luc.fromString digitString
                                          details = Luc.realWorldDetailedCandidates (fromJust maybeDigit) w b
                                          reformat (a, b, c) = (Just a, Just (b, c))

