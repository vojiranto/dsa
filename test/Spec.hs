
import Shift
import Control.Monad
import Graphics.UI.GLUT hiding ( initialize )
import Lib

import Gr
import System.Clock
import Data.Time
import Data.Graph
import GrBuild
import Data.IORef
import Quar
import qualified Data as D

main = do
    putStrLn ""
    ok <- foldM (\a f -> f >>= \x -> pure (a && x)) True
        [zonePointTest, distanceTest]
    when ok $ putStrLn "Ok."
    testF

zonePointTest :: IO Bool
zonePointTest = do
    -- zonePoint  :: Point -> Bool
    let test = all zonePoint
            [ D.Point (1/2) (1/2)
            , D.Point (1/3) (1/3)
            , D.Point 0.9 0]
    unless test $ print "zonePointEror"
    return test

distanceTest :: IO Bool
distanceTest = do
    -- distance   :: Point -> Point -> Double
    let test = and
            [ distance (D.Point 2 0) (D.Point 2 0) == 0
            , distance (D.Point 0 0) (D.Point 2 0) == 2
            , distance (D.Point 0 3) (D.Point 0 0) == 3
            ]
    unless test $ print "distanceTestEror"
    return test

testF = grInit $ \win -> do
    displayCallback $= testDisplay :: IO ()
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just keyboardMouseHandler

testDisplay = do
    fx:fy:_ <- Gr.form
    let f p        = D.Point (Gr.shift fx p) (Gr.shift fy p)
    st <- newIORef $ do
        let pt = D.Patch $ localizate f 16 testLine
        renderPt $ pt
        print $ D.size pt
        return []
    display $ st

testLine = [D.Point (-1) (-1), D.Point 1 1]

-- patch      :: F Point -> [Point] -> IterateNumber -> [Point]
-- localizate :: F Point -> [Point] -> [Point]
-- patchStep  :: F Point -> [Point] -> [Point]
-- toChain    :: F Point -> Point -> [Point] -> [Link]
-- toPatch    :: F Point -> Link -> [Point]

-- 31 875 661 | 748
-- TimeSpec {sec = 32, nsec = 14683505}
-- TimeSpec {sec = 2, nsec = 626595795}

