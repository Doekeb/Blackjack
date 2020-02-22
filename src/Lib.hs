module Lib where

import Blackjack
import Data.Char
import Data.List
import System.Random
import System.Exit


-- New deck

freshDeck :: IO Deck
freshDeck = do
  g <- newStdGen
  return $ shuffle (randoms g :: [Int]) fullDeck


-- Draw a card from a deck

draw :: Deck -> IO (Card, Deck)
draw [] = do
  putStrLn "Shuffling a new deck."
  newDeck <- freshDeck
  draw newDeck
draw (c:cs) = do
  return (c, cs)


-- Hit the given hand

hitHand :: Hand -> Deck -> IO (Hand, Deck)
hitHand h d = do
  (newCard, newDeck) <- draw d
  return (h ++ [newCard], newDeck)


-- Deal cards

deal :: Deck -> IO (Hand, Deck)
deal d = do
  (first, d') <- draw d
  (second, d'') <- draw d'
  return ([first, second], d'')


-- Pretty printing for a hand

prettyPrint :: Hand -> String
prettyPrint h = printAll h
                ++ "("
                ++ plasticity -- a string, either "hard" or "soft"
                ++ " "
                ++ show (handValue h)
                ++ ")" where
  printAll :: Hand -> String
  printAll [] = ""
  printAll (x:xs) = show x ++ " " ++ printAll xs
  plasticity
    | hard (handScore h) < handValue h = "soft"
    | otherwise                        = "hard"


-- General prompt function

prompt :: String              -- query
       -> String              -- help
       -> (String -> Maybe a) -- parser
       -> (a -> IO b)         -- action on input
       -> IO b                -- result
prompt query help parse act = do
  putStrLn query
  input <- getLine
  case input of
    "quit" -> do
      putStrLn "Better luck next time!"
      exitSuccess
    "help" -> do
      putStrLn help
      prompt query help parse act
    _ -> do
      case parse input of
        Nothing -> do
          putStrLn "I didn't understand that."
          putStrLn help
          prompt query help parse act
        Just x -> do
          act x


ioId :: a -> IO a
ioId x = do
  return x

-- Just prints help on "help", quits on "quit", and does nothing otherwise.
dumbPrompt :: String -- query
           -> String -- help
           -> IO ()
dumbPrompt query help = do
  prompt query help alwaysEmpty ioId where
    alwaysEmpty :: String -> Maybe ()
    alwaysEmpty x = Just ()


-- enumeration type for a move: hitting or standing
data Move = Hit | Stand | Surrender
  deriving (Show, Read)

-- maps the first element of the list by the given function
mapFirst :: (a -> a) -> [a] -> [a]
mapFirst _ []     = []
mapFirst f (x:xs) = (f x) : xs

-- capitalizes the first letter of the string
capitalize :: String -> String
capitalize x = mapFirst toUpper x

-- try to understand the move. "hit" or "Hit" or "stand" or "Stand" are all
-- accepted, as well as "surrender" or "Surrender", as long as the first
-- argument is "True".
safeReadMove :: Bool -> String -> Maybe Move
safeReadMove b x
  | isSafe (capitalize x) = Just $ read $ capitalize x
  | otherwise = Nothing
  where isSafe x
          | b         = elem x ["Hit", "Stand", "Surrender"]
          | otherwise = elem x ["Hit", "Stand"]

playerTurn :: Hand -> Deck -> IO (Hand, Deck)
playerTurn h d = do
  case (compare (handValue h) 21, length h) of
    (GT, _) -> do
      return (h, d)
    (EQ, 2) -> do
      return (h, d)
    _ -> do
      let surrAvailable = length h == 2
      let surrString
            | surrAvailable = "\"surrender\", "
            | otherwise     = ""
      prompt ("Your hand is " ++ (prettyPrint h) ++ ". What do you do?")
             ("You can \"hit\", \"stand\", \"quit\", " ++ surrString
              ++ "or get \"help\".")
             (safeReadMove surrAvailable)
             doMove where
               doMove :: Move -> IO (Hand, Deck)
               doMove m = do
                 case m of
                   Hit       -> do
                     (h', d') <- hitHand h d
                     playerTurn h' d'
                   Stand     -> do
                     return (h, d)
                   Surrender -> do
                     return ([], d)


-- Take the dealer's turn

dealerTurn :: Hand -> Deck -> IO (Hand, Deck)
dealerTurn h d = do
  if handValue h > 16 then do
    return (h, d)
  else do
    (h', d') <- hitHand h d
    dealerTurn h' d'


-- some other useful stuff

data Result = PlayerBust | PlayerBlackjack | DealerBust | PlayerWin | DealerWin | Push | PlayerSurrender
  deriving (Show, Eq)


-- First int is the max wager
safeReadWager :: Int -> String -> Maybe Int
safeReadWager n x
  | isSafe x && read x >= 0 && read x <= n = Just $ read $ x
  | otherwise                              = Nothing
  where isSafe :: String -> Bool
        isSafe ""     = False
        isSafe [x]    = isDigit x
        isSafe (x:xs) = isDigit x && isSafe xs
