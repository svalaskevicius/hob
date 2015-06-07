{-# LANGUAGE DeriveDataTypeable #-}

module Hob.Ui.Editor.Fancy.Types (
  FancyEditor(..),
  FunctionDef,
  FunctionCache,
  VariableDependency(..),
  VariableUsage(..),
  ScopedVariableDependencies,
  SourceData(..),
  SourceChangeEvent(..),
  Point(..),
  PointD,
  PointI,
  Block(..),
  Blocks,
  EditorDrawingOptions(..),
  DrawableLine(..),
  EditorDrawingData(..),
  CursorHead(..),
  TextShapeCache,
  ColouredRange(..),
  ColourGroup(..),
  debugPrint,
  tracePrint,
  tracePrintOther
) where

import qualified Control.Monad.State.Lazy            as S
import           Data.Generics
import           Data.Graph
import           Data.Map                            (Map)
import           Data.Text                           (Text)

import qualified Data.Vector                         as V
import           Debug.Trace
import           Graphics.UI.Gtk                     hiding (Point)
import qualified Language.Haskell.Exts.Annotated     as P

import qualified IPPrint
import qualified Language.Haskell.HsColour           as HsColour
import qualified Language.Haskell.HsColour.Colourise as HsColour
import qualified Language.Haskell.HsColour.Output    as HsColour


data Block a = Block (Point a) (Point a) deriving Show
type Blocks a = Forest (Block a)


data VariableUsage = VariableUsage
                                [P.Name P.SrcSpanInfo] -- ^ defined var, list for destructuring matches | target <- usages
                                [P.Name P.SrcSpanInfo] -- ^ "depending on" variable usages
                                deriving Show

data VariableDependency = VariableDependency
                                (P.Name P.SrcSpanInfo) -- ^ definition of the "depending on" variable | source -> usages
                                [VariableUsage]        -- ^ usages of the definition
                                deriving Show

type ScopedVariableDependencies = Map (P.Match P.SrcSpanInfo) [VariableDependency]
type FunctionDef = (P.Match P.SrcSpanInfo, P.Name P.SrcSpanInfo, [P.Pat P.SrcSpanInfo], P.Rhs P.SrcSpanInfo, [P.Decl P.SrcSpanInfo])
type FunctionCache = Map (P.Decl P.SrcSpanInfo) [FunctionDef] -- this is still slow to lookup - find a better key for cache, maybe just an int tag for decls (e.g. generation int + srcspan)

data ColourGroup = DefaultColourGroup | ColourGroup Int deriving (Show, Eq)

data ColouredRange = ColouredRange
                        Int -- ^ pos
                        Int -- ^ length
                        ColourGroup -- ^ colour group
                    deriving (Show, Eq)

data SourceChangeEvent = InsertText (Point Int) [Text] -- ^ Point (col, line) before the inserted text lines
                       | DeleteText (Point Int) [Text] -- ^ Point (col, line) before the deleted text lines
        deriving (Show)

data SourceData = SourceData {
    isModified                :: Bool,
    textLines                 :: [Text],
    textHighlights            :: [[(Int, Int)]],
    ast                       :: Maybe (P.Module P.SrcSpanInfo, [P.Comment]),
    parseErrorMessage         :: Maybe (Point Int, String),
    functionDefCache          :: FunctionCache,
    sourceBlocks              :: Blocks Int,
    varDeps                   :: ScopedVariableDependencies,
    varDepGraph               :: Graph,
    varDepGraphLookupByVertex :: Vertex -> (P.Name P.SrcSpanInfo, Int, [Int]),
    varDepGraphLookupByKey    :: Int -> Maybe Vertex,
    history                   :: [SourceChangeEvent],
    undoneHistory             :: [SourceChangeEvent]
}

-- | Point x y
data Point a = Point a a deriving (Show, Eq)
type PointD = Point Double
type PointI = Point Int

--data DrawableG
-- | DrawableLine (y position) (x position of each char) [(width of the glyph, colour group, glyph to draw)]
data DrawableLine = DrawableLine Double [Double] [(Double, ColourGroup, GlyphItem)]

type TextShapeCache = Map String [(GlyphItem, (PangoRectangle, PangoRectangle), [Double])]

data EditorDrawingData = EditorDrawingData {
    drawableLines      :: [DrawableLine],
    textToShapeCache   :: TextShapeCache,
    boundingRect       :: (PointD, PointD),
    selectionContour   :: [PointD],
    cursorPosition     :: PointD,
    errorPosition      :: Maybe PointD,
    backgroundPaths    :: Forest [PointD],
    colourGroupToRgb   :: V.Vector (Double, Double, Double),
    sourceErrorMessage :: Maybe String
}

data EditorDrawingOptions = EditorDrawingOptions {
    lineHeight  :: Double,
    fontAscent  :: Double,
    fontDescent :: Double,
    reportError :: Maybe String -> IO ()
}

data CursorHead = CursorHead Int Int Int -- ^ X, Y, X_{navigation}

instance Eq CursorHead where
    (CursorHead x1 y1 _) == (CursorHead x2 y2 _) = (x1 == x2) && (y1 == y2)

instance Ord CursorHead where
    (CursorHead x1 y1 _) <= (CursorHead x2 y2 _) = (y1 < y2) || ((y2 == y1) && (x1 <= x2))

data FancyEditor = FancyEditor {
    sourceData              :: SourceData,
    cursorHead              :: CursorHead,
    selectionHead           :: Maybe CursorHead,
    drawingOptions          :: EditorDrawingOptions,
    drawingData             :: EditorDrawingData,
    editorApiId             :: Int,
    emitInternalEditorEvent :: String -> IO(),
    emitEditorEvent         :: String -> IO(),
    drawingAreaWidget       :: DrawingArea,
    redraw                  :: S.StateT FancyEditor IO ()
} deriving Typeable


debugColourPrefs :: HsColour.ColourPrefs
debugColourPrefs = HsColour.defaultColourPrefs { HsColour.conid = [HsColour.Foreground HsColour.Yellow, HsColour.Bold], HsColour.conop = [HsColour.Foreground HsColour.Yellow], HsColour.string = [HsColour.Foreground HsColour.Green], HsColour.char = [HsColour.Foreground HsColour.Cyan], HsColour.number = [HsColour.Foreground HsColour.Red, HsColour.Bold], HsColour.layout = [HsColour.Foreground HsColour.White], HsColour.keyglyph = [HsColour.Foreground HsColour.White] }

debugPrint :: Show a => a -> IO()
debugPrint = putStrLn . HsColour.hscolour (HsColour.TTYg HsColour.XTerm256Compatible) debugColourPrefs False False "" False . IPPrint.pshow

tracePrint :: Show a => a -> a
tracePrint v = trace (HsColour.hscolour (HsColour.TTYg HsColour.XTerm256Compatible) debugColourPrefs False False "" False . IPPrint.pshow $ v) v

tracePrintOther :: Show a => a -> b -> b
tracePrintOther p = trace (HsColour.hscolour (HsColour.TTYg HsColour.XTerm256Compatible) debugColourPrefs False False "" False . IPPrint.pshow $ p)

