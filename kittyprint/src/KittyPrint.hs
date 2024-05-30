{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module KittyPrint (
  printQDiagram,
  prettyPrint,
  prettyPrintBy,
  PrintBehaviour,
  SPrintBehaviour (..),
  PrintCellBehaviour,
  SPrintCellBehaviour (..),
) where

import Codec.Picture (encodePng)
import Data.ByteString.Lazy qualified as L
import Data.Foldable (toList)
import Data.IntMap.Strict qualified as IntMap
import Data.List
import Data.Map.Strict qualified as Map
import Data.Monoid (Any)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Typeable
import Diagrams hiding (V)
import Diagrams.Backend.Rasterific
import GHC.IsList (IsList (..))
import GHC.TypeLits (KnownNat, KnownSymbol, Symbol, natVal, symbolVal)
import Graphics.Rendering.Chart.Backend.Diagrams qualified as Chart
import Graphics.Rendering.Chart.Easy qualified as Chart
import System.Process.Typed as Proc
import Text.Layout.Table (columnHeaderTableS, def, rowsG, tableLines, titlesH, unicodeS)
import Text.Pretty.Simple (pPrint)

printQDiagram :: (RealFloat n, Typeable n, Read n, Monoid m) => QDiagram Rasterific V2 n m -> IO ()
printQDiagram dia = do
  imgSize <- getImageSize
  let bs = encodePng (renderDia Rasterific (RasterificOptions imgSize) dia)
  !_ <-
    runProcess
      . setStdin (byteStringInput bs)
      $ "kitty +kitten icat --stdin=yes --background=white"
  pure ()

getImageSize :: (Read n, RealFloat n, Typeable n) => IO (SizeSpec V2 n)
getImageSize = do
  (ExitSuccess, sizeBs) <- readProcessStdout "kitty +kitten icat --print-window-size"
  let !(w, h) = case map (read . TL.unpack . decodeUtf8) (L.split 120 {- x -} sizeBs) of
        [w_, h_] -> (w_, h_)
        _ -> error "could not get window size from kitty"
  pure (dims2D (w * 0.8) (min (w * 0.8 * (3 / 4)) (h * 0.6)))

-- | Helper closed type family to allow 'easyPrint' to have decidable instances
type family PrintBehaviour a where
  PrintBehaviour (QDiagram _ _ Float _) = PrintDiagramInferFloat
  PrintBehaviour (QDiagram _ _ _ _) = PrintDiagramInferDouble
  PrintBehaviour (Chart.EC _ _) = PrintDiagramInferDouble
  PrintBehaviour (Map.Map k v) = PrintMapAsTable
  PrintBehaviour (IntMap.IntMap v) = PrintMapAsTable
  PrintBehaviour [(k, v)] = PrintListAsTable "fst" (PrintCellBehaviour k) "snd" (PrintCellBehaviour v)
  -- PrintBehaviour (V n String, [V n a]) = PrintVectors (PrintCellBehaviour a)
  -- PrintBehaviour [V n a] = PrintVectors (PrintCellBehaviour a)
  PrintBehaviour [v] = PrintList "Value" (PrintCellBehaviour v)
  PrintBehaviour _ = PrintOpen

-- | Enum of possible print behaviours
data SPrintBehaviour
  = PrintDiagramInferFloat
  | PrintDiagramInferDouble
  | PrintMapAsTable
  | PrintListAsTable Symbol SPrintCellBehaviour Symbol SPrintCellBehaviour
  | PrintList Symbol SPrintCellBehaviour
  | PrintTable [Symbol]
  | PrintVectors SPrintCellBehaviour
  | PrintRecord
  | PrintOpen

prettyPrint :: forall a behaviour. (PrettyPrint a behaviour) => a -> IO ()
prettyPrint = prettyPrintBy (Proxy :: Proxy behaviour)

type PrettyPrint a behaviour = (PrettyPrintBy a behaviour, behaviour ~ PrintBehaviour a)

class PrettyPrintBy a (behaviour :: SPrintBehaviour) where
  prettyPrintBy :: Proxy behaviour -> a -> IO ()

instance (b ~ B, n ~ Float, m ~ Any, v ~ V2) => PrettyPrintBy (QDiagram b v n m) PrintDiagramInferFloat where
  prettyPrintBy _ = printQDiagram

instance (b ~ B, n ~ Double, m ~ Any, v ~ V2) => PrettyPrintBy (QDiagram b v n m) PrintDiagramInferDouble where
  prettyPrintBy _ = printQDiagram

runChartEC :: (Chart.Default r, Chart.ToRenderable r) => Chart.EC r a -> IO (QDiagram B V2 Double Any)
runChartEC chart = do
  let (dw, dh) = (500, 500)
  diaenv <- Chart.defaultEnv Chart.vectorAlignmentFns dw dh
  let (diachart, _) = Chart.runBackend diaenv (Chart.render (Chart.toRenderable (Chart.execEC chart)) (dw, dh))
  pure diachart

instance (Chart.Default r, Chart.ToRenderable r) => PrettyPrintBy (Chart.EC r a) PrintDiagramInferDouble where
  prettyPrintBy _ chart = prettyPrintBy (Proxy :: Proxy PrintDiagramInferDouble) =<< runChartEC chart

instance (Show a, PrintBehaviour a ~ PrintOpen) => PrettyPrintBy a PrintOpen where
  prettyPrintBy _ = pPrint

instance (PrettyPrintCell k kb, PrettyPrintCell a ab) => PrettyPrintBy (Map.Map k a) PrintMapAsTable where
  prettyPrintBy _ = prettyPrintBy (Proxy :: Proxy (PrintListAsTable "Key" kb "Value" ab)) . Map.toList

instance (PrettyPrintCell a ab) => PrettyPrintBy (IntMap.IntMap a) PrintMapAsTable where
  prettyPrintBy _ = prettyPrintBy (Proxy :: Proxy (PrintListAsTable "Key" PrintCellShow "Value" ab)) . IntMap.toList

-- instance (KnownNat n, PrettyPrintCell a ab, Show a, s ~ String) => PrettyPrintBy (V n s, [V n a]) (PrintVectors ab) where
--   prettyPrintBy _ (titles, list) =
--     mapM_ putStrLn
--       . tableLines
--       $ columnHeaderTableS
--         (map (const def) (toList titles))
--         unicodeS
--         (titlesH (toList titles))
--         [ rowsG (map (map (prettyPrintCellBy (Proxy :: Proxy ab)) . toList) list)
--         ]

-- instance (KnownNat n, PrettyPrintCell a ab, Show a) => PrettyPrintBy [V n a] (PrintVectors ab) where
--   prettyPrintBy _ list = prettyPrintBy (Proxy :: Proxy (PrintVectors ab)) (titles, list)
--     where
--       titles :: V n String
--       titles =
--         V.fromVector (fromList (map show [0 .. natVal (Proxy :: Proxy n) - 1]))
--           ^?! _Just

instance
  ( KnownSymbol klabel
  , KnownSymbol alabel
  , PrettyPrintCell k kb
  , PrettyPrintCell a ab
  )
  => PrettyPrintBy [(k, a)] (PrintListAsTable klabel kb alabel ab)
  where
  prettyPrintBy _ list =
    mapM_ putStrLn
      . tableLines
      $ columnHeaderTableS
        [def, def]
        unicodeS
        (titlesH [symbolVal (Proxy :: Proxy klabel), symbolVal (Proxy :: Proxy alabel)])
        [ rowsG
            [ [prettyPrintCellBy (Proxy :: Proxy kb) k, prettyPrintCellBy (Proxy :: Proxy ab) a]
            | (k, a) <- list
            ]
        ]

instance (PrettyPrintCell a ab, KnownSymbol label) => PrettyPrintBy [a] (PrintList label ab) where
  prettyPrintBy _ list =
    mapM_ putStrLn
      . tableLines
      $ columnHeaderTableS
        [def]
        unicodeS
        (titlesH [symbolVal (Proxy :: Proxy label)])
        [ rowsG
            [ [prettyPrintCellBy (Proxy :: Proxy ab) a]
            | a <- list
            ]
        ]

--------------------------------------------------------------------------------

type PrettyPrintCell a behaviour = (PrettyPrintCellBy a behaviour, behaviour ~ PrintCellBehaviour a)

data SPrintCellBehaviour
  = PrintCellShow
  | PrintCellString

type family PrintCellBehaviour a where
  PrintCellBehaviour String = 'PrintCellString
  PrintCellBehaviour T.Text = 'PrintCellString
  PrintCellBehaviour TL.Text = 'PrintCellString
  PrintCellBehaviour a = 'PrintCellShow

class PrettyPrintCellBy a (behaviour :: SPrintCellBehaviour) where
  prettyPrintCellBy :: Proxy behaviour -> a -> String

instance (Show a) => PrettyPrintCellBy a 'PrintCellShow where
  prettyPrintCellBy _ = show

instance PrettyPrintCellBy String 'PrintCellString where
  prettyPrintCellBy _ = id

instance PrettyPrintCellBy T.Text 'PrintCellString where
  prettyPrintCellBy _ = T.unpack

instance PrettyPrintCellBy TL.Text 'PrintCellString where
  prettyPrintCellBy _ = TL.unpack
