{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Exception (Exception, throwIO)
import Control.Monad (guard, unless)
import Data.Aeson (FromJSON (..), decodeStrict)
import qualified Data.Aeson as Aeson
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HM
import Data.Scientific (Scientific, toRealFloat)
import qualified Data.Text as Text

import qualified Options.Applicative as Opt

import Dino.AST
import Dino.AST.Diff

bsLines :: ByteString -> [ByteString]
bsLines = filter ((> 0) . BS.length) . BS.split (fromIntegral $ ord '\n')

-- | Wrapper type for approximate equality
newtype Approx a = Approx {unApprox :: a}
  deriving (Num, Functor)

-- | Values that differ less than 1e-7 are considered equal
instance (Fractional a, Ord a) => Eq (Approx a) where
  Approx a == Approx b
    -- Don't do relative comparison for small numbers. These cases also prevent
    -- division by 0.
    | abs a < 1e-12 = abs b < 1e-12
    | abs b < 1e-12 = abs a < 1e-12

    -- For non-small numbers, check that their ratio is close to 1:
    | otherwise = abs (1 - a/b) < 1e-7

-- | Show without the constructor
instance Show a => Show (Approx a) where
  show = show . unApprox

instance FromJSON (AST Scientific) where
  parseJSON Aeson.Null       = return $ App Tuple []
  parseJSON (Aeson.Bool b)   = return $ App (bool "False" "True" b) []
  parseJSON (Aeson.Number n) = return $ Number n
  parseJSON (Aeson.String s) = return $ Text s
  parseJSON (Aeson.Array a)  = App List . toList <$> traverse parseJSON a
  parseJSON (Aeson.Object o) = Record . Mapping Unimportant . HM.fromList <$>
    sequence [(Field $ Text.unpack k, ) <$> parseJSON v | (k, v) <- HM.toList o]

data MergeError
  = NotAList
  | ElementNotARecord
  | ClashingKeys [Field]
  deriving (Show)

instance Exception MergeError

-- | Merge a list of records into a single record
--
-- The function throws a 'MergeError' unless the argument is a list where all
-- elements are records with no clashing keys.
mergeRecords :: AST n -> Either MergeError (AST n)
mergeRecords (App List as) = do
  rs <- mapM getRec as
  let rec = foldr (HM.unionWith (++)) HM.empty (map (fmap pure) rs)
      clashes =
        HM.keys $ HM.mapMaybe (\bs -> guard (length bs >= 2) >> return bs) rec
  unless (null clashes) $ Left $ ClashingKeys clashes
  let rec' = flip HM.mapMaybe rec $ \bs -> case bs of
        [b] -> Just b
        _   -> Nothing
  return $ Record $ Mapping Unimportant rec'
  where
    getRec (Record (Mapping _ r)) = return r
    getRec _ = Left ElementNotARecord
mergeRecords _ = Left NotAList

data Options = Options
  { readLines :: Bool
  , mergeRecs :: Bool
  , exact     :: Bool
  , file1     :: FilePath
  , file2     :: FilePath
  } deriving (Show)

optParser :: Opt.Parser Options
optParser = Options
  <$> Opt.switch
      (  Opt.long  "lines"
      <> Opt.short 'l'
      <> Opt.help  "Treat the input as list of single-line JSON blobs"
      )
  <*> Opt.switch
      (  Opt.long  "merge"
      <> Opt.short 'm'
      <> Opt.help  "Merge a top-level list of records into a single record (assumes no clashing keys)"
      )
  <*> Opt.switch
      (  Opt.long  "exact"
      <> Opt.short 'e'
      <> Opt.help  "Exact comparison of numbers"
      )
  <*> Opt.argument Opt.str (Opt.metavar "FILE1")
  <*> Opt.argument Opt.str (Opt.metavar "FILE2")

toApprox :: Scientific -> Approx Double
toApprox = Approx . toRealFloat

main :: IO ()
main = do
  Options {..} <- Opt.execParser opts
  let parser s
        | readLines = do
            as <- maybe noParse return $ traverse decodeStrict $ bsLines s
            if mergeRecs
              then either throwIO return $ mergeRecords $ App List as
              else return $ App List as
        | otherwise = maybe noParse return $ decodeStrict s
  a1 <- parser =<< BS.readFile file1
  a2 <- parser =<< BS.readFile file2
  if exact
    then case diff a1 a2 of
      Nothing -> return ()
      Just e -> printEdit e
    else case diff (toApprox <$> a1) (toApprox <$> a2) of
      Nothing -> return ()
      Just e -> printEdit e
  where
    noParse = fail "cannot parse JSON"
    opts = Opt.info (Opt.helper <*> optParser)
      (  Opt.fullDesc
      <> Opt.progDesc "Show differences between two JSON files"
      )
