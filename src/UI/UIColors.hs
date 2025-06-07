module UI.UIColors where

import qualified Graphics.Vty as V

cBlack       :: V.Color
cBlack       = V.black

cWhite       :: V.Color
cWhite       = V.white

cYellow      :: V.Color
cYellow      = V.rgbColor 228 232 100

cDarkRed     :: V.Color
cDarkRed     = V.rgbColor 87 0 0

cDarkBlue    :: V.Color
cDarkBlue    = V.rgbColor 0 0 95

cBrightGreen :: V.Color
cBrightGreen = V.rgbColor 128 219 123

-- rgb(86, 87, 6)
--rgb(4, 87, 4)
cDarkGreen :: V.Color
cDarkGreen = V.rgbColor 205 49 49