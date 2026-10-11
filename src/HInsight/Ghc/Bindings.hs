-- |
-- Module      : HInsight.Ghc.Bindings
-- Description : Which top-level binding a source location is in, and what it declares.
--
-- Works on the parsed module, so it needs no type information and still works
-- when type checking fails, which is exactly when it is wanted.
--
-- Limitation: only top-level function and variable bindings are considered.
-- An error inside an instance method, a pattern binding or a class declaration
-- has no enclosing binding here and gets 'OutsideBinding'.
module HInsight.Ghc.Bindings
  ( Binding,
    collectBindings,
    contextAt,
  )
where

import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import GHC.Hs (GhcPs, HsBindLR (..), HsDecl (..), LHsDecl, Sig (..), hsSigWcType)
import GHC.Parser.Annotation (locA)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.SrcLoc (GenLocated (..), RealSrcSpan, SrcSpan (..), containsSpan, unLoc)
import GHC.Utils.Outputable (ppr)
import HInsight.Context (Context (..))
import HInsight.Error (InsightError)
import HInsight.Ghc.Convert (convertSpan, convertText, fromDomain, renderGhc)
import HInsight.Identifier (mkIdentifier)

-- | A type signature as written: where it is and the type it declares.
data Signature = Signature
  { signatureSpan :: !RealSrcSpan,
    signatureType :: !String
  }

-- | A top-level function or variable binding.
data Binding = Binding
  { bindingName :: !String,
    -- | Everything the binding covers, all of its equations included.
    bindingSpan :: !RealSrcSpan,
    bindingSignature :: !(Maybe Signature)
  }

-- | The top-level bindings of a parsed module, each paired with its signature
-- when there is one. Signatures are matched by name in a 'Map', so a module
-- with many declarations is not scanned once per binding.
collectBindings :: [LHsDecl GhcPs] -> [Binding]
collectBindings decls =
  [ Binding name rs (Map.lookup name signatures)
  | L loc (ValD _ FunBind {fun_id = L _ rdr}) <- decls,
    let name = occNameString (rdrNameOcc rdr),
    RealSrcSpan rs _ <- [locA loc]
  ]
  where
    signatures :: Map String Signature
    signatures =
      Map.fromList
        [ (occNameString (rdrNameOcc rdr), Signature rs (renderGhc (ppr (unLoc (hsSigWcType ty)))))
        | L loc (SigD _ (TypeSig _ names ty)) <- decls,
          RealSrcSpan rs _ <- [locA loc],
          L _ rdr <- names
        ]

-- | The context of a source span: the top-level binding that contains it.
contextAt :: FilePath -> [Binding] -> RealSrcSpan -> Either InsightError Context
contextAt file bindings rs =
  maybe (Right OutsideBinding) (contextOf file) (find covers bindings)
  where
    covers :: Binding -> Bool
    covers b = bindingSpan b `containsSpan` rs

contextOf :: FilePath -> Binding -> Either InsightError Context
contextOf file b = do
  name <- fromDomain file (mkIdentifier (T.pack (bindingName b)))
  case bindingSignature b of
    Nothing -> Right (InUnsignedBinding name)
    Just s ->
      InSignedBinding name
        <$> convertSpan file (signatureSpan s)
        <*> convertText file (signatureType s)
