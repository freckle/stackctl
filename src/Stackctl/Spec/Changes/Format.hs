module Stackctl.Spec.Changes.Format
  ( Format(..)
  , formatOption
  , OmitFull(..)
  , omitFullOption
  , formatChangeSet
  , formatRemovedStack
  , formatTTY
  ) where

import Stackctl.Prelude

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Options.Applicative hiding (action)
import Stackctl.AWS
import Stackctl.Colors

data Format
  = FormatTTY
  | FormatPullRequest

data OmitFull
  = OmitFull
  | IncludeFull

formatOption :: Parser Format
formatOption = option (eitherReader readFormat) $ mconcat
  [ short 'f'
  , long "format"
  , help "Format to output changes in"
  , value FormatTTY
  , showDefaultWith showFormat
  ]

readFormat :: String -> Either String Format
readFormat = \case
  "tty" -> Right FormatTTY
  "pr" -> Right FormatPullRequest
  x -> Left $ "Invalid format: " <> x

showFormat :: Format -> String
showFormat = \case
  FormatTTY -> "tty"
  FormatPullRequest -> "pr"

-- brittany-disable-next-binding

omitFullOption :: Parser OmitFull
omitFullOption = flag IncludeFull OmitFull
  (  long "no-include-full"
  <> help "Don't include full ChangeSet JSON details"
  )

formatChangeSet
  :: Colors -> OmitFull -> Text -> Format -> Maybe ChangeSet -> Text
formatChangeSet colors omitFull name = \case
  FormatTTY -> formatTTY colors name
  FormatPullRequest -> formatPullRequest omitFull name

formatRemovedStack :: Colors -> Format -> Stack -> Text
formatRemovedStack Colors {..} format stack = case format of
  FormatTTY -> red "DELETE" <> " stack " <> cyan name
  FormatPullRequest -> ":x: This PR will **delete** the stack `" <> name <> "`"
  where name = stack ^. stack_stackName

formatTTY :: Colors -> Text -> Maybe ChangeSet -> Text
formatTTY colors@Colors {..} name mChangeSet = case (mChangeSet, rChanges) of
  (Nothing, _) -> "No changes for " <> name
  (_, Nothing) -> "Metadata only changes (e.g. Tags or Outputs)"
  (_, Just rcs) ->
    ("\n" <>) $ (<> "\n") $ mconcat $ ("Changes for " <> cyan name <> ":") : map
      (("\n  " <>) . formatResourceChange)
      (NE.toList rcs)
 where
  rChanges = do
    cs <- mChangeSet
    changes <- csChanges cs
    NE.nonEmpty $ mapMaybe resourceChange changes

  formatResourceChange ResourceChange' {..} =
    maybe "" colorAction action
      <> " "
      <> maybe "" toText logicalResourceId
      <> " ("
      <> maybe "" cyan resourceType
      <> ")"
      <> maybe "" ((" " <>) . magenta) physicalResourceId
      <> maybe "" (("\n    Replacement: " <>) . colorReplacement) replacement
      <> maybe "" (("\n    Scope: " <>) . T.intercalate ", " . map toText) scope
      <> maybe "" (("\n    Details:" <>) . formatDetails) details

  formatDetails =
    mconcat . map ("\n      * " <>) . mapMaybe (formatDetail colors)

  colorAction = \case
    x@ChangeAction_Add -> green (toText x)
    x@ChangeAction_Modify -> yellow (toText x)
    x@ChangeAction_Remove -> red (toText x)
    ChangeAction' x -> x

  colorReplacement = \case
    x@Replacement_True -> red (toText x)
    x@Replacement_False -> green (toText x)
    x@Replacement_Conditional -> yellow (toText x)
    Replacement' x -> x

formatPullRequest :: OmitFull -> Text -> Maybe ChangeSet -> Text
formatPullRequest omitFull name mChangeSet =
  emoji
    <> " This PR generates "
    <> description
    <> " for `"
    <> name
    <> "`."
    <> fromMaybe "" (commentBody omitFull <$> mChangeSet <*> rChanges)
    <> "\n"
 where
  emoji = case (mChangeSet, nChanges) of
    (Nothing, _) -> ":heavy_check_mark:"
    (_, Nothing) -> ":book:"
    (_, Just _) -> ":warning:"

  description = case (mChangeSet, nChanges) of
    (Nothing, _) -> "no changes"
    (_, Nothing) -> "only metadata changes (Tags, Outputs, etc)"
    (_, Just 1) -> "**1** change"
    (_, Just n) -> "**" <> pack (show n) <> "** changes"

  nChanges = length <$> rChanges

  rChanges = do
    cs <- mChangeSet
    changes <- csChanges cs
    NE.nonEmpty $ mapMaybe resourceChange changes

commentBody :: OmitFull -> ChangeSet -> NonEmpty ResourceChange -> Text
commentBody omitFull cs rcs =
  mconcat
    $ [ "\n"
      , "\n| Action | Logical Id | Physical Id | Type | Replacement | Scope | Details |"
      , "\n| ---    | ---        | ---         | ---  | ---         | ---   | ---     |"
      ]
    <> map commentTableRow (NE.toList rcs)
    <> case omitFull of
         OmitFull -> []
         IncludeFull ->
           [ "\n"
           , "\n<details>"
           , "\n<summary>Full changes</summary>"
           , "\n"
           , "\n```json"
           , "\n" <> changeSetJSON cs
           , "\n```"
           , "\n"
           , "\n</details>"
           ]

commentTableRow :: ResourceChange -> Text
commentTableRow ResourceChange' {..} = mconcat
  [ "\n"
  , "| " <> maybe "" toText action <> " "
  , "| " <> maybe "" toText logicalResourceId <> " "
  , "| " <> maybe "" toText physicalResourceId <> " "
  , "| " <> maybe "" toText resourceType <> " "
  , "| " <> maybe "" toText replacement <> " "
  , "| " <> maybe "" (T.intercalate ", " . map toText) scope <> " "
  , "| " <> maybe "" (mdList . mapMaybe (formatDetail noColors)) details <> " "
  , "|"
  ]

mdList :: [Text] -> Text
mdList xs =
  "<ul>" <> mconcat (map (\x -> "<li>" <> x <> "</li>") xs) <> "</ul>"

formatDetail :: Colors -> ResourceChangeDetail -> Maybe Text
formatDetail Colors {..} ResourceChangeDetail' {..} = do
  c <- changeSource
  t <- target

  let
    attr = attribute t
    n = name t
    rr = requiresRecreation t

  pure
    $ toText c
    <> maybe "" ((" in " <>) . toText) attr
    <> maybe "" (\x -> " (" <> magenta (toText x) <> ")") n
    <> maybe "" ((", recreation " <>) . formatRR) rr
    <> maybe "" ((", caused by " <>) . toText) causingEntity
 where
  formatRR = \case
    x@RequiresRecreation_Always -> red (toText x)
    x@RequiresRecreation_Never -> green (toText x)
    x@RequiresRecreation_Conditionally -> yellow (toText x)
    RequiresRecreation' x -> x
