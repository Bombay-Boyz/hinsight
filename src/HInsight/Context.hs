-- |
-- Module      : HInsight.Context
-- Description : Where in the program a finding sits.
module HInsight.Context
  ( Context (..),
    renderContext,
  )
where

import Data.Text (Text)
import HInsight.Identifier (Identifier, unIdentifier)
import HInsight.Source (Span, renderPosition, spanStart)
import HInsight.TypeText (TypeText, unTypeText)

-- | Where in the program a finding sits: the top-level binding around it and
-- what that binding declares. This is a statement of fact about the source,
-- not a claim that the declared type caused anything.
--
-- Closed by design (Standard 0.2): adding a case is a compile error at every
-- site that renders one.
data Context
  = -- | Inside a binding that has a type signature: its name, where the
    -- signature starts, and the type the signature declares.
    InSignedBinding !Identifier !Span !TypeText
  | -- | Inside a binding with no type signature, so GHC inferred its type.
    InUnsignedBinding !Identifier
  | -- | Not inside any top-level function or variable binding.
    OutsideBinding
  deriving stock (Eq, Show)

-- | The lines that describe a context: one line, or none for 'OutsideBinding'.
renderContext :: Context -> [Text]
renderContext = \case
  InSignedBinding name sp declared ->
    [ "  context: in "
        <> unIdentifier name
        <> ", declared at "
        <> renderPosition (spanStart sp)
        <> " as "
        <> unTypeText declared
    ]
  InUnsignedBinding name ->
    ["  context: in " <> unIdentifier name <> ", which has no type signature"]
  OutsideBinding -> []
