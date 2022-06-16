module Stackctl.Spec.Changes.Format
  ( Format(..)
  , formatOption
  , formatChangeSet
  , formatTTY
  ) where

import Stackctl.Prelude

import Options.Applicative hiding (action)
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T
import Stackctl.AWS
import Stackctl.Colors

data Format
  = FormatTTY
  | FormatPullRequest

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

formatChangeSet :: Colors -> Text -> Format -> Maybe ChangeSet -> Text
formatChangeSet colors name = \case
  FormatTTY -> formatTTY colors name
  FormatPullRequest -> formatPullRequest name

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

formatPullRequest :: Text -> Maybe ChangeSet -> Text
formatPullRequest name mChangeSet =
  emoji
    <> " This PR generates "
    <> description
    <> " for `"
    <> name
    <> "`."
    <> fromMaybe "" (commentBody <$> mChangeSet <*> rChanges)
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

commentBody :: ChangeSet -> NonEmpty ResourceChange -> Text
commentBody cs rcs =
  mconcat
    $ [ "\n"
      , "\n| Action | Logical Id | Physical Id | Type | Replacement | Scope | Details |"
      , "\n| ---    | ---        | ---         | ---  | ---         | ---   | ---     |"
      ]
    <> map commentTableRow (NE.toList rcs)
    <> [ "\n"
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
