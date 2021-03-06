{- |
   Module      :  Act
   Description :  Defines acts such as look, comment, uncomment.

   Copyright   :  (c) Dominik Schrempf 2017
   License     :  GPL-3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  non-portable (not tested)

Creation date: Sun Oct  1 20:52:46 2017.

Defines possible actions such as looking for the next relevant comment, comment
or uncomment.

* Changelog

TODO: Use Parsec to find stuff (targets, commands).

TODO: Move towards using stateful computations. I.e., the parsing depends on the
comment string. The actions depend on source and destination.

-}

module Act where

import           Control.Monad.Trans.State
import           Data.List                 (find)
import           Data.Maybe                (fromMaybe)
import           Data.Traversable          (traverse)
import           Parse

-- | Actions; either search for a command (i.e., don't act but watch out);
-- comment or uncomment.
data Action = Search | Comment | Uncomment
  deriving (Eq, Show)

-- | The state of 'QuText'. We have to know the source (where do we convert
-- from), the destination (where do we convert to) and a list of targets, so
-- that we can determine the result of some commands that involve more than one
-- target. Including the action 'Action' in here, would build the basis for a
-- stateful processing.
data QuTextState = QuTextState
                   { qtsTargets     :: [Target]
                   , qtsSource      :: Source
                   , qtsDest        :: Dest
                   , qtsAction      :: Action
                   , qtsCommentChar :: Char }
  deriving (Eq, Show)

type QuText a = State QuTextState a

-- | Process a file for a (possibly given) destination 'Dest'.
processFile :: Maybe Dest -> String -> String
processFile mbd f = unlines $ evalState (traverse processLine lns) qts
  where ts = searchTargets f
        s = searchSource f
        d = fromMaybe (getDest s ts) mbd
        lns = lines f
        -- TODO: Allow different file types (comment types).
        qts = QuTextState ts s d Search '#'

testCmd :: Command -> Either a Instruction -> Bool
testCmd c (Right i) = c == instrCmd i
testCmd _ _         = False

-- | Search for the 'Source'.
searchSource :: String -> Source
searchSource f = toSource $ find (testCmd Current) $ map instructionParser (lines f)
  where toSource (Just (Right (Instruction Current tgs))) = head tgs
        toSource _ = error "Source could not be determined."

searchTargets :: String -> [Target]
searchTargets f = toTargets $ find (testCmd Targets) $ map instructionParser (lines f)
  where toTargets (Just (Right (Instruction Targets tgs))) = tgs
        toTargets _ = error "Targets not specified."

-- | Search for a destination 'Dest' in a file. The destination can only be
-- found if the total number of targets is two and the source is known.
getDest :: Source -> [Target] -> Dest
getDest s [x, y]
  | s == x = y
  | s == y = x
  | otherwise = error $ "Source " ++ s ++ "differs from both targets " ++ x ++ " and " ++ y ++ "."
getDest _ _ = error "Destination could not be determined."

-- -- TODO: Allow more complicated conversions.
processLine :: String -> QuText String
processLine ln = do
  st@(QuTextState _ _ d a _) <- get
  case a of
       Search -> case instructionParser ln of
         Right (Instruction Current _) -> do
           put $ st {qtsAction = Search}
           return $ "# ..current.. " ++ "=" ++ d ++ "="
         Right (Instruction Only tgs) ->
           if d `elem` tgs
           then do
             put $ st {qtsAction = Uncomment}
             return ln
           else do
             put $ st {qtsAction = Comment}
             return ln
         _ -> do
           put st {qtsAction = Search}
           return ln
       Comment -> do
         put st {qtsAction = Search}
         comment ln
       Uncomment -> do
         put st {qtsAction = Search}
         uncomment ln
       -- _ -> error $ "Unknown action: " ++ show a

-- TODO: More sophisticated comments.
comment :: String -> QuText String
comment ln = do
  s <- get
  let c = qtsCommentChar s
  if head ln == c
    then return ln
    else return $ c : ln

uncomment :: String -> QuText String
uncomment ln = do
  s <- get
  if head ln == qtsCommentChar s
    then return $ tail ln
    else return ln
