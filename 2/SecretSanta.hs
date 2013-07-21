{-
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

import Data.Char
import Data.Function
import Data.Maybe
import Data.List
import System.Random
import System.Environment
import System.IO
import System.Exit
import Control.Monad.State
import Text.ParserCombinators.Parsec
import Network.Mail.SMTP
import qualified Network.Mail.Mime as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

data Entry = Entry {firstName :: String, familyName :: String,
                    emailAddress :: String} deriving Show

data Email = Email {from :: String, to :: String, subject :: String,
                    text :: String} deriving Show

entriesParser :: CharParser st [Entry]
entriesParser = flip sepEndBy (many1 newline) $
  do skipMany space
     first   <- many1 . satisfy $ not . isSpace
     skipMany1 space
     family  <- many1 . satisfy $ not . isSpace
     skipMany1 space
     address <- between (char '<') (char '>') . many1 $ noneOf ">"
     skipMany .satisfy $ \x -> x /= '\n' && isSpace x
     return $ Entry first family address

choices :: Entry -> [Entry] -> [Entry]
choices = filter . ((/= ) `on` familyName)

getChoice :: RandomGen g => g -> [Entry] -> (Maybe Entry, g)
getChoice g l
  | len == 0  = (Nothing, g)
  | otherwise = (Just $ l !! n, g')
  where (n, g') = randomR (0, len - 1) g
        len     = length l

getChoices :: RandomGen g => g -> [Entry] -> [Maybe Entry]
getChoices r l = flip evalState r $ mapM makeChoice l
  where makeChoice e =
          do g <- get
             let (e', g') = getChoice g $ choices e l
             put g'
             return e'

makeMail :: String -> Entry -> Entry -> Email
makeMail f e s = Email f (emailAddress e)
                 "Your secret santa has been selected" $
                 "Hi, " ++ (firstName e) ++ " " ++ (familyName e) ++
                 ".\n\nYour secret santa is " ++ (firstName s) ++ " " ++
                 (familyName s) ++ "!"

makeMails :: RandomGen g => g -> String -> [Entry] -> Either [String] [Email]
makeMails g f es
  | null errors = Right $ zipWith zipFunction es $ getChoices g es
  | otherwise   = Left errors
  where errors  = map (\e -> firstName e ++ " " ++ familyName e ++
                             " don't have any valid choice") $
                  filter (null . flip choices es) es
        zipFunction e c = makeMail f e $ fromJust c

generateMails :: RandomGen g => g -> String -> String -> Either [String] [Email]
generateMails g f = evaluate . parse entriesParser ""
  where evaluate (Left  err) = Left [show err]
        evaluate (Right es)  = makeMails g f es

getMail :: Email -> M.Mail
getMail e = simpleMail from' [to'] [] [] subject' [text']
  where from'    = Address Nothing $ T.pack $ from e
        to'      = Address Nothing $ T.pack $ to   e
        subject' = T.pack $ subject e
        text'    = plainTextPart $ TL.pack $ text e

sendMail :: Email -> IO ()
sendMail email = do putStrLn $ "Sending:\n\t" ++ show email
                    renderSendMail $ getMail email

main :: IO ()
main = do args <- getArgs
          when (length args < 1) $
            err "The first parameter have to be the from address"
          gen   <- getStdGen
          input <- getContents
          either (err . intercalate "\n") (mapM_ Main.sendMail) $
            generateMails gen (head args) input
  where err s = hPutStrLn stderr s >> exitFailure
