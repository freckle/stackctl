module Stackctl.Spec.Cat
  ( CatOptions(..)
  , runCatOptions
  , runCat
  ) where

import Stackctl.Prelude

import Data.Aeson
import Data.Aeson.Lens
import qualified Data.Text.IO as T
import qualified Data.Yaml as Yaml
import Options.Applicative
import qualified RIO.HashMap as HashMap
import RIO.List (sort, sortOn)
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T
import Stackctl.AWS
import Stackctl.ColorOption (HasColorOption)
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
  :: ( MonadResource m
     , MonadReader env m
     , HasLogFunc env
     , HasAwsEnv env
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

  put 0 $ fromString dir <> "/"
  putStack 2 "stacks/"
  templates <- for tree $ \((accountId, accountName), regions) -> do
    putStack 4
      $ magenta (display accountId)
      <> "."
      <> display accountName
      <> "/"

    for regions $ \(region, specs) -> do
      putStack 6 $ magenta (display region) <> "/"

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

prettyPrintStackSpecYaml
  :: Colors -> StackName -> StackSpecYaml -> [Utf8Builder]
prettyPrintStackSpecYaml Colors {..} name StackSpecYaml {..} = concat
  [ [cyan "Name" <> ": " <> green (display name)]
  , [cyan "Template" <> ": " <> green (fromString ssyTemplate)]
  , ppList "Parameters" (ppParameters . map unParameterYaml) ssyParameters
  , ppList "Capabilities" ppCapabilities ssyCapabilities
  , ppList "Tags" (ppTags . map unTagYaml) ssyTags
  ]
 where
  ppList :: Utf8Builder -> (a -> [Utf8Builder]) -> Maybe a -> [Utf8Builder]
  ppList label f = maybe [] (((cyan label <> ":") :) . f)

  ppParameters = concatMap $ \p ->
    [ "  - " <> cyan "ParameterKey" <> ": " <> maybe
      ""
      (green . display)
      (p ^. parameter_parameterKey)
    , "    " <> cyan "ParameterValue" <> ": " <> maybe
      ""
      display
      (p ^. parameter_parameterValue)
    ]

  ppCapabilities = map (("  - " <>) . green . display)

  ppTags = concatMap $ \t ->
    [ "  - " <> cyan "Key" <> ": " <> green (display $ t ^. tag_key)
    , "    " <> cyan "Value" <> ": " <> display (t ^. tag_value)
    ]

prettyPrintTemplate :: Colors -> Value -> [Utf8Builder]
prettyPrintTemplate Colors {..} val = concat
  [ displayTextProperty "Description"
  , displayObjectProperty "Parameters"
  , displayObjectProperty "Resources"
  , displayObjectProperty "Outputs"
  ]
 where
  displayTextProperty :: Text -> [Utf8Builder]
  displayTextProperty = displayPropertyWith $ \v ->
    let t = T.dropWhileEnd (== '\n') $ pack v in ["  " <> green (display t)]

  displayObjectProperty :: Text -> [Utf8Builder]
  displayObjectProperty =
    displayPropertyWith @(HashMap Text Value)
      $ map (("  - " <>) . green . display)
      . sort
      . HashMap.keys

  displayPropertyWith
    :: (FromJSON a, ToJSON a) => (a -> [Utf8Builder]) -> Text -> [Utf8Builder]
  displayPropertyWith f k =
    cyan (display k) <> ": " : fromMaybe [] displayValue
    where displayValue = val ^? key k . _JSON . to f

putBoxed :: MonadIO m => Int -> [Utf8Builder] -> m ()
putBoxed n xs = do
  traverse_ (put n . ("│ " <>)) xs
  put n "└──────────"
  put 0 ""

put :: MonadIO m => Int -> Utf8Builder -> m ()
put n = liftIO . T.putStrLn . utf8BuilderToText . (indent <>)
  where indent = mconcat $ replicate n " "
