{- |
   Module      :  QuText
   Description :  Map a quantum text file to its fixed target.
   Copyright   :  (c) Dominik Schrempf 2017
   License     :  GPL-3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  non-portable (not tested)

Creation date: Sat Sep 30 13:18:09 2017.

For a general description, see the .cabal file.

A TARGET is a 'Source' or a destination 'Dest', i.e., a host that the file is
tailored for at the moment or a host that is should be tailored for. All targets
must be specified with

  -- ..targets.. =TargetA= =TargetB=

Here, the current target has name TargetA and the commenting style for Haskell
has been used (but of course this is language dependent).

The source that the file is tailored for has to be given in a comment BEFORE the
first polymorphic line in the following way.

  -- ..current.. =TargetA=

Then, lines after comments of the form

  -- ..only.. =TargetA=

Will be commented out. Commenting characters in the lines following comments of
the form

  -- ..only.. =TargetB=

will be removed (i.e., the line will be commented in). If the line is not a
comment, an error will be returned.

* Changelog

TODO: Improve usage, e.g., allow specification of areas for targets.

-}

module Main where

import Options.Applicative
import System.IO
import System.IO.Temp (getCanonicalTemporaryDirectory)
import System.Directory (copyFile, removeFile)
import System.Posix.Files (getFileStatus, fileMode, setFileMode)

import Act
import Parse

main :: IO ()
main = do
  args <- parseQuTextArgs
  let fn = fileName args
      fnBak = fn ++ ".bak"
      d = dest args
  if noBackup args
    then putStrLn "Omitting backup."
    else do putStrLn $ "Creating backup: " ++ fn ++ " -> " ++ fnBak
            copyFile fn fnBak
  f <- readFile fn
  tmpDir <- getCanonicalTemporaryDirectory
  (tmpFn, tmpFh) <- openTempFile tmpDir fn
  hPutStr tmpFh (processFile d f)
  _ <- hClose tmpFh
  -- Replace original file preserving permissions.
  fs <- getFileStatus fn
  copyFile tmpFn fn
  setFileMode fn (fileMode fs)
  removeFile tmpFn
  return ()

parseQuTextArgs :: IO QuTextArgs
parseQuTextArgs = execParser $ info (helper <*> quTextOptions)
  ( fullDesc
    `mappend` progDesc "Map a quantum text file to its fixed target."
    `mappend` header "QuText - quantum text mapper." )

data QuTextArgs = QuTextArgs
  { fileName :: FilePath
  , dest     :: Maybe Dest
  , noBackup :: Bool
  }

quTextOptions :: Parser QuTextArgs
quTextOptions = QuTextArgs
  <$> fileNameOpt
  <*> targetOpt
  <*> noBackupOpt

fileNameOpt :: Parser String
fileNameOpt = strArgument
  ( metavar "FILEPATH"
    `mappend` help "Read QuText file from FILEPATH.")

targetOpt :: Parser (Maybe Dest)
targetOpt = optional $ strOption
  ( long "destination"
    `mappend` short 'd'
    `mappend` metavar "DESTINATION"
    `mappend` help "Specify a target the file should be tailored for (precedes inferred destination)")

noBackupOpt :: Parser Bool
noBackupOpt = flag False True
  ( long "no-backup"
    `mappend` short 'n'
    `mappend` help "Suppress file backup" )
