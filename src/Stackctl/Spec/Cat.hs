module Stackctl.Spec.Cat
  ( CatOptions(..)
  , runCatOptions
  , runCat
  ) where

import Stackctl.Prelude

import Blammo.Logging.Logger (flushLogger)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import Data.Aeson.Lens
import qualified Data.HashMap.Strict as HashMap
import Data.List (sort, sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Yaml as Yaml
import Options.Applicative
import Stackctl.AWS
import Stackctl.AWS.Scope
import Stackctl.Colors
import Stackctl.DirectoryOption (HasDirectoryOption(..))
import Stackctl.FilterOption (HasFilterOption)
import Stackctl.Spec.Discover
import Stackctl.StackSpec
import Stackctl.StackSpecPath
import Stackctl.StackSpecYaml

data CatOptions = CatOptions
  { sctoNoStacks :: Bool
  , sctoNoTemplates :: Bool
  , sctoBrief :: Bool
  }

-- brittany-disable-next-binding

runCatOptions :: Parser CatOptions
runCatOptions = CatOptions
  <$> switch
    (  long "no-stacks"
    <> help "Only show templates/"
    )
  <*> switch
    (  long "no-templates"
    <> help "Only show stacks/"
    )
  <*> switch
    (  short 'b'
    <> long "brief"
    <> help "Don't show file contents, only paths"
    )

runCat
  :: ( MonadMask m
     , MonadResource m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
     , HasAwsScope env
     , HasDirectoryOption env
     , HasFilterOption env
     , HasColorOption env
     )
  => CatOptions
  -> m ()
runCat CatOptions {..} = do
  dir <- view directoryOptionL
  colors@Colors {..} <- getColorsStdout
  tree <- specTree <$> discoverSpecs

  let
    putStack n x = if sctoNoStacks then pure () else put n x
    putStackBody n x =
      if sctoNoStacks || sctoBrief then pure () else putBoxed n x
    putTemplate n x = if sctoNoTemplates then pure () else put n x
    putTemplateBody n x =
      if sctoNoTemplates || sctoBrief then pure () else putBoxed n x

  flushLogger

  put 0 $ fromString dir <> "/"
  putStack 2 "stacks/"
  templates <- for tree $ \((accountId, accountName), regions) -> do
    putStack 4 $ magenta (unAccountId accountId) <> "." <> accountName <> "/"

    for regions $ \(region, specs) -> do
      putStack 6 $ magenta (toText region) <> "/"

      let sorted = sortOn (stackSpecPathBasePath . stackSpecSpecPath) specs
      for sorted $ \spec -> do
        let
          base = stackSpecPathBasePath $ stackSpecSpecPath spec
          body = stackSpecSpecBody spec
          name = stackSpecStackName spec
          yaml = prettyPrintStackSpecYaml colors name body

        putStack 8 $ magenta (fromString base)
        putStackBody 10 yaml
        pure $ ssyTemplate body

  putTemplate 2 "templates/"
  for_ (sort $ concat $ concat templates) $ \template -> do
    val <- Yaml.decodeFileThrow @_ @Value $ dir </> "templates" </> template

    putTemplate 4 $ green $ fromString template
    putTemplateBody 6 $ prettyPrintTemplate colors val

specTree :: [StackSpec] -> [((AccountId, Text), [(Region, [StackSpec])])]
specTree = map (second groupRegion) . groupAccount
 where
  groupRegion :: [StackSpec] -> [(Region, [StackSpec])]
  groupRegion = groupTo (stackSpecPathRegion . stackSpecSpecPath)

  groupAccount :: [StackSpec] -> [((AccountId, Text), [StackSpec])]
  groupAccount = groupTo
    ((stackSpecPathAccountId &&& stackSpecPathAccountName) . stackSpecSpecPath)

groupTo :: Ord b => (a -> b) -> [a] -> [(b, [a])]
groupTo f = map (f . NE.head &&& NE.toList) . NE.groupAllWith f

prettyPrintStackSpecYaml :: Colors -> StackName -> StackSpecYaml -> [Text]
prettyPrintStackSpecYaml Colors {..} name StackSpecYaml {..} = concat
  [ [cyan "Name" <> ": " <> green (unStackName name)]
  , [cyan "Template" <> ": " <> green (pack ssyTemplate)]
  , ppList
    "Parameters"
    (ppParameters . map unParameterYaml . unParametersYaml)
    ssyParameters
  , ppList "Capabilities" ppCapabilities ssyCapabilities
  , ppList "Tags" (ppTags . map unTagYaml . unTagsYaml) ssyTags
  ]
 where
  ppList :: Text -> (a -> [Text]) -> Maybe a -> [Text]
  ppList label f = maybe [] (((cyan label <> ":") :) . f)

  ppParameters = concatMap $ \p ->
    [ "  - " <> cyan "ParameterKey" <> ": " <> maybe
      ""
      green
      (p ^. parameter_parameterKey)
    , "    " <> cyan "ParameterValue" <> ": " <> maybe
      ""
      toText
      (p ^. parameter_parameterValue)
    ]

  ppCapabilities = map (("  - " <>) . green . toText)

  ppTags = concatMap $ \tg ->
    [ "  - " <> cyan "Key" <> ": " <> green (tg ^. tag_key)
    , "    " <> cyan "Value" <> ": " <> (tg ^. tag_value)
    ]

prettyPrintTemplate :: Colors -> Value -> [Text]
prettyPrintTemplate Colors {..} val = concat
  [ displayTextProperty "Description"
  , displayObjectProperty "Parameters"
  , displayObjectProperty "Resources"
  , displayObjectProperty "Outputs"
  ]
 where
  displayTextProperty :: Text -> [Text]
  displayTextProperty = displayPropertyWith
    $ \v -> let tp = T.dropWhileEnd (== '\n') $ pack v in ["  " <> green tp]

  displayObjectProperty :: Text -> [Text]
  displayObjectProperty =
    displayPropertyWith @(HashMap Text Value)
      $ map (("  - " <>) . green)
      . sort
      . HashMap.keys

  displayPropertyWith
    :: (FromJSON a, ToJSON a) => (a -> [Text]) -> Text -> [Text]
  displayPropertyWith f k = cyan k <> ": " : fromMaybe [] displayValue
    where displayValue = val ^? key (Key.fromText k) . _JSON . to f

putBoxed :: MonadIO m => Int -> [Text] -> m ()
putBoxed n xs = do
  traverse_ (put n . ("│ " <>)) xs
  put n "└──────────"
  put 0 ""

put :: MonadIO m => Int -> Text -> m ()
put n = liftIO . T.putStrLn . (indent <>)
  where indent = mconcat $ replicate n " "
