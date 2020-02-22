module Main where

import Lib
import Blackjack

startingWallet = 500


-- Welcome and start game loop

main :: IO ()
main = do
  putStrLn "Welcome to Blackjack!"
  d <- freshDeck
  gameLoop d startingWallet



-- Main game loop

gameLoop :: Deck -> Int -> IO ()
gameLoop d0 w0 = do
  -- If the player has $0, tell them they lose and ask if they want to try again.
  if w0 <= 0 then do
    dumbPrompt ("You have $0. You lose! Press Enter to play again, "
                ++ "or type \"quit\" to quit.")
               "Press Enter to play again, or you can \"quit\" or get \"help\"."
    d <- freshDeck
    gameLoop d startingWallet
  else do
    return ()

  -- Check to see if the player is ready.
  dumbPrompt "Are you ready? (Press Enter to continue.)"
             $ "Press Enter when you are ready. At any point, you can type "
               ++ "\"quit\" to quit or \"help\" to get help."

  -- Get the player's wager.
  wager <- prompt ("You have $" ++ show w0 ++ ". What is your wager?")
                  ("Enter an amount between 0 and " ++ show w0
                   ++ ", or you can \"quit\" or get \"help\".")
                  (safeReadWager w0)
                  (ioId)

  (dealerStart, d1) <- deal d0 -- Dealer's first card, and updated deck
  putStrLn $ "The dealer's first card is " ++ show (head dealerStart) ++ "."
  (playerStart, d2) <- deal d1 -- Player's hand and updated deck
  (playerEnd, d3)   <- playerTurn playerStart d2 -- Do the player's turn
  putStrLn "" -- I just want a new line after the player's turn is over.
  (result, d4)      <- -- Only do the dealer's turn if the player isn't bust.
    case (handValue playerEnd, length playerEnd) of
      (0, _)          -> do
        return (PlayerSurrender, d3)
      (x, _) | x > 21  -> do
               return (PlayerBust, d3)
             | x == 21 -> do
               return (PlayerBlackjack, d3)
      _               -> do
        (dealerEnd, d4) <- dealerTurn dealerStart d3
        putStrLn $ "The dealer reveals the hand " ++ prettyPrint dealerEnd ++ "."
        let result
              | handValue dealerEnd > 21                  = DealerBust
              | handValue dealerEnd > handValue playerEnd
                ||
                (handValue dealerEnd == 21
                 && length dealerEnd == 2
                 && length playerEnd > 2)                 = DealerWin
              | handValue playerEnd > handValue dealerEnd
                ||
                (handValue playerEnd == 21
                 && length playerEnd == 2
                 && length dealerEnd > 2)                 = PlayerWin
              | otherwise                                 = Push
        return (result, d4)
  -- print result, repeat the gameloop with updated deck and wallet.
  case result of
    PlayerSurrender -> do
      let tipDealer
            | wager `mod` 2 == 1 = " You tip the dealer $1."
            | otherwise          = ""
      putStrLn $ "You've surrendered." ++ tipDealer
      gameLoop d4 (w0 - ((wager+1) `div` 2))
    PlayerBust      -> do
      putStrLn $ "You bust! Your hand is "
               ++ prettyPrint playerEnd
               ++ ". The house wins.\n"
      gameLoop d4 (w0-wager)
    PlayerBlackjack -> do
      putStrLn $ "Blackjack! Your hand is "
               ++ prettyPrint playerEnd
               ++ ". You win!\n"
      gameLoop d4 (w0 + ((wager+1) `div` 2))
    DealerBust      -> do
      putStrLn "Dealer busts! You win!\n"
      gameLoop d4 (w0+wager)
    PlayerWin       -> do
      putStrLn "You win!\n"
      gameLoop d4 (w0+wager)
    DealerWin       -> do
      putStrLn "The house wins.\n"
      gameLoop d4 (w0-wager)
    Push            -> do
      putStrLn "It's a push.\n"
      gameLoop d4 w0
