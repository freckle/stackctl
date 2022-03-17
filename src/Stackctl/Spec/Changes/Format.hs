module Stackctl.Spec.Changes.Format
  ( Format(..)
  , formatOption
  , formatChangeSet
  , formatTTY
  ) where

import Stackctl.Prelude

import Stackctl.AWS
import Stackctl.Colors
import Options.Applicative hiding (action)
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T

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

formatChangeSet
  :: Display name => Colors -> name -> Format -> Maybe ChangeSet -> Utf8Builder
formatChangeSet colors name = \case
  FormatTTY -> formatTTY colors name
  FormatPullRequest -> formatPullRequest name

formatTTY :: Display name => Colors -> name -> Maybe ChangeSet -> Utf8Builder
formatTTY colors@Colors {..} name mChangeSet = case (mChangeSet, rChanges) of
  (Nothing, _) -> "No changes for " <> display name
  (_, Nothing) -> "Metadata only changes (e.g. Tags or Outputs)"
  (_, Just rcs) ->
    ("\n" <>)
      $ (<> "\n")
      $ mconcat
      $ ("Changes for " <> cyan (display name) <> ":")
      : map (("\n  " <>) . formatResourceChange) (NE.toList rcs)
 where
  rChanges = do
    cs <- mChangeSet
    changes <- csChanges cs
    NE.nonEmpty $ mapMaybe resourceChange changes

  formatResourceChange ResourceChange' {..} =
    maybe "" colorAction action
      <> " "
      <> maybe "" display logicalResourceId
      <> " ("
      <> maybe "" (cyan . display) resourceType
      <> ")"
      <> maybe "" ((" " <>) . magenta . display) physicalResourceId
      <> maybe "" (("\n    Replacement: " <>) . colorReplacement) replacement
      <> maybe
           ""
           (("\n    Scope: " <>) . display . T.intercalate ", " . map toText)
           scope
      <> maybe "" (("\n    Details:" <>) . formatDetails) details

  formatDetails =
    mconcat . map ("\n      * " <>) . mapMaybe (formatDetail colors)

  colorAction = \case
    x@ChangeAction_Add -> green (display x)
    x@ChangeAction_Modify -> yellow (display x)
    x@ChangeAction_Remove -> red (display x)
    ChangeAction' x -> display x

  colorReplacement = \case
    x@Replacement_True -> red (display x)
    x@Replacement_False -> green (display x)
    x@Replacement_Conditional -> yellow (display x)
    Replacement' x -> display x

formatPullRequest :: Display name => name -> Maybe ChangeSet -> Utf8Builder
formatPullRequest name mChangeSet =
  emoji
    <> " This PR generates "
    <> description
    <> " for `"
    <> display name
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
    (_, Just n) -> "**" <> displayShow n <> "** changes"

  nChanges = length <$> rChanges

  rChanges = do
    cs <- mChangeSet
    changes <- csChanges cs
    NE.nonEmpty $ mapMaybe resourceChange changes

commentBody :: ChangeSet -> NonEmpty ResourceChange -> Utf8Builder
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
       , "\n" <> display (changeSetJSON cs)
       , "\n```"
       , "\n"
       , "\n</details>"
       ]

commentTableRow :: ResourceChange -> Utf8Builder
commentTableRow ResourceChange' {..} = mconcat
  [ "\n"
  , "| " <> maybe "" display action <> " "
  , "| " <> maybe "" display logicalResourceId <> " "
  , "| " <> maybe "" display physicalResourceId <> " "
  , "| " <> maybe "" display resourceType <> " "
  , "| " <> maybe "" display replacement <> " "
  , "| " <> maybe "" (display . T.intercalate ", " . map toText) scope <> " "
  , "| " <> maybe "" (mdList . mapMaybe (formatDetail noColors)) details <> " "
  , "|"
  ]

mdList :: [Utf8Builder] -> Utf8Builder
mdList xs =
  "<ul>" <> mconcat (map (\x -> "<li>" <> x <> "</li>") xs) <> "</ul>"

formatDetail :: Colors -> ResourceChangeDetail -> Maybe Utf8Builder
formatDetail Colors {..} ResourceChangeDetail' {..} = do
  c <- changeSource
  t <- target

  let
    attr = attribute t
    n = name t
    rr = requiresRecreation t

  pure
    $ display c
    <> maybe "" ((" in " <>) . display) attr
    <> maybe "" (\x -> " (" <> magenta (display x) <> ")") n
    <> maybe "" ((", recreation " <>) . formatRR) rr
    <> maybe "" ((", caused by " <>) . display) causingEntity
 where
  formatRR = \case
    x@RequiresRecreation_Always -> red $ display x
    x@RequiresRecreation_Never -> green $ display x
    x@RequiresRecreation_Conditionally -> yellow $ display x
    RequiresRecreation' x -> display x
