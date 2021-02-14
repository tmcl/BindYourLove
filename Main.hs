module Main where

import Prelude ((>>=), (==), (++), (+), (-), div, flip)
import qualified Prelude as IO (IO, readFile, putStrLn, print, getLine)
import Flow
import qualified Data.Map as Map
import qualified Data.Char as Char
import qualified Data.Int as Int
import qualified Data.Word as Word
import Data.String as String
import Data.Maybe
import Data.Bool
import qualified Data.List as List
import Data.Bits as Bits
import Data.Monoid
import Control.Monad
import Control.Applicative
import Text.Read (readMaybe)

data TextEncoding = Ascii | Unicode

type Font = Map.Map Char.Char [Word.Word8]

-- Because we use the Flow package, repl.it has some problems with 
-- this. So in the shell tab, run these commands to get it to work:
--      cabal new-update
--      cabal new-run (or cabal new-repl)
-- Then, it should run in the Shell tab just fine. The Console tab
-- is flaky and after you've run `cabal update`, maybe it works or
-- maybe not.

main :: IO.IO ()
main = 
  readFont
    >>= withValidatedFont runMessageOfLove

readFont :: IO.IO (Maybe Font)
readFont = 
  IO.readFile "font.spec"
    |> fmap parseFont

readOneLetter :: [String] -> Maybe (Char.Char, [Word.Word8])
readOneLetter strings = 
  case strings of
      (charLine : numbers) -> 
        case charLine of
          (char : []) -> parseNumbers numbers >>= ((,) char .> Just)
          _ -> Nothing
      _ -> Nothing

parseNumbers :: [String] -> Maybe [Word.Word8]
parseNumbers =
  List.map readMaybe .> sequence

readManyLetters :: [String] -> Maybe Font
readManyLetters =
  splitBy (== "") 
    .> List.map readOneLetter 
    .> sequence
    .> fmap Map.fromList

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy predicate list = first:continue
  where
     (first, rest) = List.break predicate list
     continue = 
      case rest of
        _:list' -> splitBy predicate list'
        _ -> []

parseFont :: String -> Maybe Font
parseFont =
  lines .> readManyLetters

withValidatedFont :: (Font -> IO.IO ()) -> Maybe Font -> IO.IO ()
withValidatedFont continuation mfont = 
  case mfont of
    Nothing -> IO.putStrLn "Error: font file corrupt"
    Just font -> continuation font

runMessageOfLove :: Font -> IO.IO ()
runMessageOfLove font =
  boxedMessageOfLove font 
           <$> inputEncoding 
           <*> inputWith "Enter the first name:"
           <*> inputWith "Enter the second name:"
       >>= IO.putStrLn        

inputWith :: String.String -> IO.IO String.String
inputWith msg = IO.putStrLn msg >> IO.getLine

byteToLine :: TextEncoding -> Word.Word8 -> String.String
byteToLine encoding byte = helper (Bits.finiteBitSize byte) ""
  where
    helper :: Int.Int -> String.String -> String.String
    helper (-1) acc = acc
    helper n acc = helper (n-1) <| (if Bits.testBit byte n then letterChar encoding else ' ') : acc

letterChar :: TextEncoding -> Char.Char
letterChar Unicode = '█'
letterChar Ascii = '#'
  
charToLines :: TextEncoding -> Font -> Char.Char -> Maybe [String.String]
charToLines encoding them it = 
  Map.lookup it them 
  |> fmap (fmap (byteToLine encoding) .> ("        " :))



asciiArtWord :: TextEncoding -> Font -> String.String -> [String.String]
asciiArtWord encoding font name1 = 
  charToLines encoding font
    |> flip fmap name1 
    |> Data.Maybe.catMaybes
    |> mconcat
    |> List.transpose
    |> List.reverse

centreLine :: Int.Int -> String.String -> String.String
centreLine paddedLength content = 
  let 
    contentLength = List.length content
    shortfall = (paddedLength - contentLength)
    neededAtStart = shortfall `div` 2
    neededAtEnd = shortfall - neededAtStart
    padding len = List.repeat ' ' |> List.take len 
  in 
    padding neededAtStart ++ content ++ padding neededAtEnd

maxLength :: [[a]] -> Int.Int
maxLength = fmap List.length .> List.maximum

centreLines :: [String.String] -> [String.String]
centreLines eachLine = 
     maxLength eachLine
     |> centreLine
     |> flip fmap eachLine

boxDrawing :: TextEncoding -> (Char.Char, Char.Char, Char.Char, Char.Char, Char.Char, Char.Char)
boxDrawing Unicode = ('╔', '═',  '╗',  '║', '╝', '╚')
boxDrawing Ascii = ('#', '#',  '#',  '#', '#', '#')

box :: TextEncoding -> [String.String] -> [String.String]
box encoding eachLine = 
  let 
    len = maxLength eachLine
    toptail = List.take (len + 4) <| List.repeat top :: String.String
    content = eachLine |> fmap (\s -> left ++ s ++ right) :: [String.String]
    (topLeft, top, topRight, side, bottomRight, bottomLeft) =
       boxDrawing encoding
    left = side : "  "
    right = "  " ++ pure side
  in
    [pure topLeft ++ toptail ++ pure topRight] 
    ++ content 
    ++ [pure bottomLeft ++ toptail ++ pure bottomRight]
 
boxedMessageOfLove :: Font -> TextEncoding -> String.String -> String.String -> String.String
boxedMessageOfLove font encoding name1 name2 = 
   asciiArtWord encoding font name1 ++ heart encoding ++ asciiArtWord encoding font name2 
   |> centreLines 
   |> box encoding
   |> String.unlines

inputEncoding :: IO.IO TextEncoding
inputEncoding = do
  IO.putStrLn "Would you like ASCII (a) or Unicode (u)?"
  answer <- IO.getLine
  case fmap Char.toLower answer of 
    "a" -> return Ascii
    "u" -> return Unicode
    _ -> inputEncoding



heart :: TextEncoding -> [String.String]
heart Unicode = 
   [ "      ♥♥♥♥♥♥♥♥♥♥♥                  ♥♥♥♥♥♥♥♥♥♥♥      "
   , "   ♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥            ♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥   "
   , " ♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥        ♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥ "
   , "♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥      ♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥"
   , "♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥    ♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥"
   , "♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥  ♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥"
   , " ♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥ "
   , "  ♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥  "
   , "    ♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥    "
   , "      ♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥      "
   , "         ♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥         "
   , "           ♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥           "
   , "              ♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥              "
   , "                ♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥♥                "
   , "                   ♥♥♥♥♥♥♥♥♥♥♥♥♥♥                   "
   , "                     ♥♥♥♥♥♥♥♥♥♥                     "
   , "                       ♥♥♥♥♥♥                       "
   , "                         ♥♥                         "
   ]
heart Ascii = 
   [ "        ******       ******          ",
     "       **********   **********       ",
     "     ************* *************     ",
     "    *****************************    ",
     "    *****************************    ",
     "    *****************************    ",
     "     ***************************     ",
     "       ***********************       ",
     "         *******************         ",
     "           ***************           ",
     "             ***********             ",
     "               *******               ",
     "                 ***                 ",
     "                  *                  "
   ]

