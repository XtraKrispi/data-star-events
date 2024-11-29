{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module DataStar.Events (
  -- * Fragments
  Selector,
  MergeMode (..),
  MergeFragmentConfig (..),
  defaultMergeFragmentConfig,
  mergeFragments,
  mergeFragments',
  removeFragments,

  -- * Signals
  SignalPath,
  mergeSignals,
  removeSignals,

  -- * Scripts
  Attribute,
  Script,
  Url,
  executeScripts,
  redirect,
  redirectScript,
) where

import Data.Aeson (ToJSON, encode)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import Data.List (intersperse)
import Data.Maybe (catMaybes)

type Selector = ByteString

-- | The merge mode to use when merging in HTML fragments
data MergeMode
  = -- | Merges the fragment using Idiomorph.
    Morph
  | -- | Replaces the target’s innerHTML with the fragment.
    Inner
  | -- | Replaces the target’s outerHTML with the fragment.
    Outer
  | -- | Prepends the fragment to the target’s children.
    Prepend
  | -- | Appends the fragment to the target’s children.
    Append
  | -- | Inserts the fragment before the target as a sibling.
    Before
  | -- | Inserts the fragment after the target as a sibling.
    After
  | -- | Merges attributes from the fragment into the target – useful for updating a store.
    UpsertAttributes
  deriving (Enum, Bounded)

instance Show MergeMode where
  show :: MergeMode -> String
  show Morph = "morph"
  show Inner = "inner"
  show Outer = "outer"
  show Prepend = "prepend"
  show Append = "append"
  show Before = "before"
  show After = "after"
  show UpsertAttributes = "upsertAttributes"

data MergeFragmentConfig = MergeFragmentConfig
  { mergeFragmentConfigSelector :: Maybe Selector
  -- ^ Selects the target element of the merge process using a CSS selector.
  , mergeFragmentConfigMergeMode :: MergeMode
  -- ^ The merge strategy for merging the fragments into the DOM
  , mergeFragmentConfigSettleDurationInMs :: Int
  -- ^ Settles the element after 1000ms, useful for transitions.
  , mergeFragmentConfigUseViewTransition :: Bool
  -- ^ Whether to use view transitions when merging into the DOM.
  }

{- | Default configuration for merging fragments into the DOM:

@
'MergeFragmentConfig'
{ 'mergeFragmentConfigSelector'              = Nothing
, 'mergeFragmentConfigMergeMode'             = 'Morph'
, 'mergeFragmentConfigSettleDurationInMs'    = 300
, 'mergeFragmentConfigUseViewTransition'     = False
}
@
-}
defaultMergeFragmentConfig :: MergeFragmentConfig
defaultMergeFragmentConfig =
  MergeFragmentConfig
    { mergeFragmentConfigSelector = Nothing
    , mergeFragmentConfigMergeMode = Morph
    , mergeFragmentConfigSettleDurationInMs = 300
    , mergeFragmentConfigUseViewTransition = False
    }

{- | Construct an event that will send a series of fragment updates to the frontend to update the DOM.

This version takes in a mapping function from any data type 'a' to 'ByteString', and a list of 'a' which will then be converted into the 'ByteString' representation.
-}
mergeFragments ::
  -- | Configuration for how to send this event to the frontend
  MergeFragmentConfig ->
  -- | Convert your chosen HTML type to ByteString
  (a -> ByteString) ->
  -- | A list of fragments to merge into the DOM
  [a] ->
  -- | The event.  This can be used within your backend of choice to send to the frontend.
  ByteString
mergeFragments config toByteString = mergeFragments' config . fmap toByteString

{- | Construct an event that will send a series of fragment updates to the frontend to update the DOM.

This version takes in a list of 'ByteString's already assumed to be HTML.
-}
mergeFragments' ::
  -- | Configuration for how to send this event to the frontend
  MergeFragmentConfig ->
  -- | A list of fragments to merge into the DOM
  [ByteString] ->
  -- | The event.  This can be used within your backend of choice to send to the frontend.
  ByteString
mergeFragments' config fragments =
  let eventLine = Builder.byteString "event: datastar-merge-fragments"
      fragments' = "data: fragments " <> mconcat (Builder.lazyByteString <$> fragments)
      configLines =
        catMaybes
          [ (\s -> "data: selector " <> Builder.lazyByteString s) <$> mergeFragmentConfigSelector config
          , Just $
              Builder.lazyByteString "data: mergeMode "
                <> Builder.lazyByteString (pack (show (mergeFragmentConfigMergeMode config)))
          , Just $ Builder.lazyByteString "data: settleDuration " <> Builder.lazyByteString (pack (show (mergeFragmentConfigSettleDurationInMs config)))
          , Just $ Builder.lazyByteString "data: useViewTransition " <> Builder.lazyByteString (if mergeFragmentConfigUseViewTransition config then "true" else "false")
          ]
      allLines =
        [ eventLine
        , fragments'
        ]
          <> configLines
          <> [newline]
   in Builder.toLazyByteString $ mconcat $ intersperse newline allLines

-- | Construct an event that will send a signal update to the frontend to update the client store
mergeSignals ::
  (ToJSON a) =>
  -- | Only if missing: determines whether to update the store with new values only if the key does not exist
  Bool ->
  -- | The data to send to the frontend for updating
  a ->
  -- | The event to send to the frontend
  ByteString
mergeSignals onlyIfMissing dataToUpdate =
  Builder.toLazyByteString $
    mconcat $
      intersperse
        newline
        [ Builder.lazyByteString "event: datastar-merge-signals"
        , Builder.lazyByteString $ "data: onlyIfMissing " <> if onlyIfMissing then "true" else "false"
        , Builder.lazyByteString $ "data: signals " <> encode dataToUpdate
        , newline
        ]

-- | Removes HTML fragments from the DOM that match the provided selector
removeFragments :: Selector -> ByteString
removeFragments selector =
  Builder.toLazyByteString $
    mconcat $
      intersperse
        newline
        [ Builder.lazyByteString "event: datastar-remove-fragments"
        , Builder.lazyByteString "data: selector " <> Builder.lazyByteString selector
        , newline
        ]

type SignalPath = ByteString

{- | Removes matching signal paths from the store.

For example:

@foo.bar@

@1234@

@abc@
-}
removeSignals ::
  -- | Signal Paths to remove from the store.
  [SignalPath] ->
  -- | The event to send to the frontend
  ByteString
removeSignals paths =
  Builder.toLazyByteString $
    mconcat $
      intersperse
        newline
        [ Builder.lazyByteString "event: datastar-remove-signals"
        , Builder.lazyByteString "data: paths "
            <> mconcat (Builder.lazyByteString <$> intersperse " " paths)
        , newline
        ]

type Attribute = (ByteString, ByteString)
type Script = ByteString
type Url = ByteString

{- | Executes scripts on the frontend.

For example:

@executeScripts True [("type", "module"), ("defer", "true")] ["console.log('Hello, world!')"]@

Would effectively render the following @script@ element:

@
\<script type="module" defer="true"\>
  console.log('Hello, world!')
\</script\>
@

NOTE: There is no sanitation of this script before sending it to the frontend. Be careful.
-}
executeScripts ::
  -- | AutoRemove: Determines whether to remove the script after execution
  Bool ->
  -- | The attributes to attach to the @script@ element
  [Attribute] ->
  -- | The individual scripts to run within the @script@ element
  [Script] ->
  -- | The event to send to the front end
  ByteString
executeScripts autoRemove attributes scripts =
  Builder.toLazyByteString $
    mconcat $
      intersperse
        newline
        ( [ Builder.lazyByteString "event: datastar-execute-script"
          , Builder.lazyByteString "data: autoRemove " <> if autoRemove then "true" else "false"
          ]
            <> (buildAttribute <$> attributes)
            <> (buildScript <$> scripts)
            <> [ newline
               ]
        )
 where
  buildAttribute :: Attribute -> Builder.Builder
  buildAttribute (key, value) =
    "data: attributes "
      <> Builder.lazyByteString key
      <> " "
      <> Builder.lazyByteString value
  buildScript :: Script -> Builder.Builder
  buildScript script = "data: script " <> Builder.lazyByteString script

-- | Creates the entire event for redirecting to a url, in case that's all you want to do
redirect :: Url -> ByteString
redirect url = executeScripts True [] [redirectScript url]

-- | Sets up a redirect script to be used with 'executeScripts'
redirectScript :: Url -> Script
redirectScript url = "window.location = \"" <> url <> "\""

newline :: Builder.Builder
newline = Builder.byteString "\n"