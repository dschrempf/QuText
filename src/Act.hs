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

import           Data.List  (find, mapAccumL, stripPrefix)
import           Data.Maybe (fromMaybe)
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
                   { qtsTargets :: [Target]
                   , qtsSource  :: Source
                   , qtsDest    :: Dest
                   , qtsAction  :: Action }
  deriving (Eq, Show)

-- | Process a file for a (possibly given) destination 'Dest'.
processFile :: Maybe Dest -> String -> String
processFile mbd f = unlines . processLines qts . lines $ f
  where ts = searchTargets f
        s = searchSource f
        d = fromMaybe (getDest s ts) mbd
        qts = QuTextState ts s d Search

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

-- TODO: Allow more complicated conversions.
processLines :: QuTextState -> [String] -> [String]
processLines state lns = snd $ mapAccumL processLine state lns

processLine :: QuTextState -> String -> (QuTextState, String)
processLine st@(QuTextState _ _ d a) ln =
  case a of
       Search -> case instructionParser ln of
         Right (Instruction Current _) ->
           let st' = st {qtsAction = Search}
               ln' = "# ..current.. " ++ "=" ++ d ++ "="
               in (st', ln')
         Right (Instruction Only tgs) ->
           if d `elem` tgs
           then (st{qtsAction = Uncomment}, ln)
           else (st{qtsAction = Comment}, ln)
         _ -> (st{qtsAction = Search}, ln)
       Comment -> (st{qtsAction = Search}, comment ln)
       Uncomment -> (st{qtsAction = Search}, uncomment ln)
       -- _ -> error $ "Unknown action: " ++ show a

-- TODO: More sophisticated comments.
-- TODO: Allow different file types.
comment :: String -> String
comment ln = if head ln == '#'
             then ln
             else ("#" ++) ln

uncomment :: String -> String
uncomment ln = if head ln == '#'
               then tail ln
               else ln
