{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE PatternSynonyms #-}
module Graphics.Datoviz.Colormaps where

import Foreign.Ptr
import Foreign.C.Types

#include <datoviz/datoviz.h>

newtype DvzColorMap = DvzColorMap { getDvzColorMap :: CInt }

-- Yes, this was a vim macro.
pattern DVZ_CMAP_BINARY :: DvzColorMap
pattern DVZ_CMAP_BINARY = DvzColorMap #{const DVZ_CMAP_BINARY}
pattern DVZ_CMAP_HSV :: DvzColorMap
pattern DVZ_CMAP_HSV = DvzColorMap #{const DVZ_CMAP_HSV}
pattern DVZ_CMAP_CIVIDIS :: DvzColorMap
pattern DVZ_CMAP_CIVIDIS = DvzColorMap #{const DVZ_CMAP_CIVIDIS}
pattern DVZ_CMAP_INFERNO :: DvzColorMap
pattern DVZ_CMAP_INFERNO = DvzColorMap #{const DVZ_CMAP_INFERNO}
pattern DVZ_CMAP_MAGMA :: DvzColorMap
pattern DVZ_CMAP_MAGMA = DvzColorMap #{const DVZ_CMAP_MAGMA}
pattern DVZ_CMAP_PLASMA :: DvzColorMap
pattern DVZ_CMAP_PLASMA = DvzColorMap #{const DVZ_CMAP_PLASMA}
pattern DVZ_CMAP_VIRIDIS :: DvzColorMap
pattern DVZ_CMAP_VIRIDIS = DvzColorMap #{const DVZ_CMAP_VIRIDIS}
pattern DVZ_CMAP_BLUES :: DvzColorMap
pattern DVZ_CMAP_BLUES = DvzColorMap #{const DVZ_CMAP_BLUES}
pattern DVZ_CMAP_BUGN :: DvzColorMap
pattern DVZ_CMAP_BUGN = DvzColorMap #{const DVZ_CMAP_BUGN}
pattern DVZ_CMAP_BUPU :: DvzColorMap
pattern DVZ_CMAP_BUPU = DvzColorMap #{const DVZ_CMAP_BUPU}
pattern DVZ_CMAP_GNBU :: DvzColorMap
pattern DVZ_CMAP_GNBU = DvzColorMap #{const DVZ_CMAP_GNBU}
pattern DVZ_CMAP_GREENS :: DvzColorMap
pattern DVZ_CMAP_GREENS = DvzColorMap #{const DVZ_CMAP_GREENS}
pattern DVZ_CMAP_GREYS :: DvzColorMap
pattern DVZ_CMAP_GREYS = DvzColorMap #{const DVZ_CMAP_GREYS }
pattern DVZ_CMAP_ORANGES :: DvzColorMap
pattern DVZ_CMAP_ORANGES = DvzColorMap #{const DVZ_CMAP_ORANGES}
pattern DVZ_CMAP_ORRD :: DvzColorMap
pattern DVZ_CMAP_ORRD = DvzColorMap #{const DVZ_CMAP_ORRD}
pattern DVZ_CMAP_PUBU :: DvzColorMap
pattern DVZ_CMAP_PUBU = DvzColorMap #{const DVZ_CMAP_PUBU}
pattern DVZ_CMAP_PUBUGN :: DvzColorMap
pattern DVZ_CMAP_PUBUGN = DvzColorMap #{const DVZ_CMAP_PUBUGN}
pattern DVZ_CMAP_PURPLES :: DvzColorMap
pattern DVZ_CMAP_PURPLES = DvzColorMap #{const DVZ_CMAP_PURPLES }
pattern DVZ_CMAP_RDPU :: DvzColorMap
pattern DVZ_CMAP_RDPU = DvzColorMap #{const DVZ_CMAP_RDPU}
pattern DVZ_CMAP_REDS :: DvzColorMap
pattern DVZ_CMAP_REDS = DvzColorMap #{const DVZ_CMAP_REDS}
pattern DVZ_CMAP_YLGN :: DvzColorMap
pattern DVZ_CMAP_YLGN = DvzColorMap #{const DVZ_CMAP_YLGN}
pattern DVZ_CMAP_YLGNBU :: DvzColorMap
pattern DVZ_CMAP_YLGNBU = DvzColorMap #{const DVZ_CMAP_YLGNBU}
pattern DVZ_CMAP_YLORBR :: DvzColorMap
pattern DVZ_CMAP_YLORBR = DvzColorMap #{const DVZ_CMAP_YLORBR}
pattern DVZ_CMAP_YLORRD :: DvzColorMap
pattern DVZ_CMAP_YLORRD = DvzColorMap #{const DVZ_CMAP_YLORRD}
pattern DVZ_CMAP_AFMHOT :: DvzColorMap
pattern DVZ_CMAP_AFMHOT = DvzColorMap #{const DVZ_CMAP_AFMHOT}
pattern DVZ_CMAP_AUTUMN :: DvzColorMap
pattern DVZ_CMAP_AUTUMN = DvzColorMap #{const DVZ_CMAP_AUTUMN}
pattern DVZ_CMAP_BONE :: DvzColorMap
pattern DVZ_CMAP_BONE = DvzColorMap #{const DVZ_CMAP_BONE}
pattern DVZ_CMAP_COOL :: DvzColorMap
pattern DVZ_CMAP_COOL = DvzColorMap #{const DVZ_CMAP_COOL}
pattern DVZ_CMAP_COPPER :: DvzColorMap
pattern DVZ_CMAP_COPPER = DvzColorMap #{const DVZ_CMAP_COPPER}
pattern DVZ_CMAP_GIST_HEAT :: DvzColorMap
pattern DVZ_CMAP_GIST_HEAT = DvzColorMap #{const DVZ_CMAP_GIST_HEAT}
pattern DVZ_CMAP_GRAY :: DvzColorMap
pattern DVZ_CMAP_GRAY = DvzColorMap #{const DVZ_CMAP_GRAY}
pattern DVZ_CMAP_HOT :: DvzColorMap
pattern DVZ_CMAP_HOT = DvzColorMap #{const DVZ_CMAP_HOT}
pattern DVZ_CMAP_PINK :: DvzColorMap
pattern DVZ_CMAP_PINK = DvzColorMap #{const DVZ_CMAP_PINK}
pattern DVZ_CMAP_SPRING :: DvzColorMap
pattern DVZ_CMAP_SPRING = DvzColorMap #{const DVZ_CMAP_SPRING}
pattern DVZ_CMAP_SUMMER :: DvzColorMap
pattern DVZ_CMAP_SUMMER = DvzColorMap #{const DVZ_CMAP_SUMMER}
pattern DVZ_CMAP_WINTER :: DvzColorMap
pattern DVZ_CMAP_WINTER = DvzColorMap #{const DVZ_CMAP_WINTER}
pattern DVZ_CMAP_WISTIA :: DvzColorMap
pattern DVZ_CMAP_WISTIA = DvzColorMap #{const DVZ_CMAP_WISTIA}
pattern DVZ_CMAP_BRBG :: DvzColorMap
pattern DVZ_CMAP_BRBG = DvzColorMap #{const DVZ_CMAP_BRBG}
pattern DVZ_CMAP_BWR :: DvzColorMap
pattern DVZ_CMAP_BWR = DvzColorMap #{const DVZ_CMAP_BWR}
pattern DVZ_CMAP_COOLWARM :: DvzColorMap
pattern DVZ_CMAP_COOLWARM = DvzColorMap #{const DVZ_CMAP_COOLWARM}
pattern DVZ_CMAP_PIYG :: DvzColorMap
pattern DVZ_CMAP_PIYG = DvzColorMap #{const DVZ_CMAP_PIYG}
pattern DVZ_CMAP_PRGN :: DvzColorMap
pattern DVZ_CMAP_PRGN = DvzColorMap #{const DVZ_CMAP_PRGN}
pattern DVZ_CMAP_PUOR :: DvzColorMap
pattern DVZ_CMAP_PUOR = DvzColorMap #{const DVZ_CMAP_PUOR}
pattern DVZ_CMAP_RDBU :: DvzColorMap
pattern DVZ_CMAP_RDBU = DvzColorMap #{const DVZ_CMAP_RDBU}
pattern DVZ_CMAP_RDGY :: DvzColorMap
pattern DVZ_CMAP_RDGY = DvzColorMap #{const DVZ_CMAP_RDGY}
pattern DVZ_CMAP_RDYLBU :: DvzColorMap
pattern DVZ_CMAP_RDYLBU = DvzColorMap #{const DVZ_CMAP_RDYLBU}
pattern DVZ_CMAP_RDYLGN :: DvzColorMap
pattern DVZ_CMAP_RDYLGN = DvzColorMap #{const DVZ_CMAP_RDYLGN}
pattern DVZ_CMAP_SEISMIC :: DvzColorMap
pattern DVZ_CMAP_SEISMIC = DvzColorMap #{const DVZ_CMAP_SEISMIC}
pattern DVZ_CMAP_SPECTRAL :: DvzColorMap
pattern DVZ_CMAP_SPECTRAL = DvzColorMap #{const DVZ_CMAP_SPECTRAL}
pattern DVZ_CMAP_TWILIGHT_SHIFTED :: DvzColorMap
pattern DVZ_CMAP_TWILIGHT_SHIFTED = DvzColorMap #{const DVZ_CMAP_TWILIGHT_SHIFTED}
pattern DVZ_CMAP_TWILIGHT :: DvzColorMap
pattern DVZ_CMAP_TWILIGHT = DvzColorMap #{const DVZ_CMAP_TWILIGHT}
pattern DVZ_CMAP_BRG :: DvzColorMap
pattern DVZ_CMAP_BRG = DvzColorMap #{const DVZ_CMAP_BRG}
pattern DVZ_CMAP_CMRMAP :: DvzColorMap
pattern DVZ_CMAP_CMRMAP = DvzColorMap #{const DVZ_CMAP_CMRMAP}
pattern DVZ_CMAP_CUBEHELIX :: DvzColorMap
pattern DVZ_CMAP_CUBEHELIX = DvzColorMap #{const DVZ_CMAP_CUBEHELIX}
pattern DVZ_CMAP_FLAG :: DvzColorMap
pattern DVZ_CMAP_FLAG = DvzColorMap #{const DVZ_CMAP_FLAG}
pattern DVZ_CMAP_GIST_EARTH :: DvzColorMap
pattern DVZ_CMAP_GIST_EARTH = DvzColorMap #{const DVZ_CMAP_GIST_EARTH}
pattern DVZ_CMAP_GIST_NCAR :: DvzColorMap
pattern DVZ_CMAP_GIST_NCAR = DvzColorMap #{const DVZ_CMAP_GIST_NCAR}
pattern DVZ_CMAP_GIST_RAINBOW :: DvzColorMap
pattern DVZ_CMAP_GIST_RAINBOW = DvzColorMap #{const DVZ_CMAP_GIST_RAINBOW}
pattern DVZ_CMAP_GIST_STERN :: DvzColorMap
pattern DVZ_CMAP_GIST_STERN = DvzColorMap #{const DVZ_CMAP_GIST_STERN}
pattern DVZ_CMAP_GNUPLOT2 :: DvzColorMap
pattern DVZ_CMAP_GNUPLOT2 = DvzColorMap #{const DVZ_CMAP_GNUPLOT2}
pattern DVZ_CMAP_GNUPLOT :: DvzColorMap
pattern DVZ_CMAP_GNUPLOT = DvzColorMap #{const DVZ_CMAP_GNUPLOT}
pattern DVZ_CMAP_JET :: DvzColorMap
pattern DVZ_CMAP_JET = DvzColorMap #{const DVZ_CMAP_JET}
pattern DVZ_CMAP_NIPY_SPECTRAL :: DvzColorMap
pattern DVZ_CMAP_NIPY_SPECTRAL = DvzColorMap #{const DVZ_CMAP_NIPY_SPECTRAL}
pattern DVZ_CMAP_OCEAN :: DvzColorMap
pattern DVZ_CMAP_OCEAN = DvzColorMap #{const DVZ_CMAP_OCEAN}
pattern DVZ_CMAP_PRISM :: DvzColorMap
pattern DVZ_CMAP_PRISM = DvzColorMap #{const DVZ_CMAP_PRISM}
pattern DVZ_CMAP_RAINBOW :: DvzColorMap
pattern DVZ_CMAP_RAINBOW = DvzColorMap #{const DVZ_CMAP_RAINBOW}
pattern DVZ_CMAP_TERRAIN :: DvzColorMap
pattern DVZ_CMAP_TERRAIN = DvzColorMap #{const DVZ_CMAP_TERRAIN}
pattern DVZ_CMAP_BKR :: DvzColorMap
pattern DVZ_CMAP_BKR = DvzColorMap #{const DVZ_CMAP_BKR}
pattern DVZ_CMAP_BKY :: DvzColorMap
pattern DVZ_CMAP_BKY = DvzColorMap #{const DVZ_CMAP_BKY}
pattern DVZ_CMAP_CET_D10 :: DvzColorMap
pattern DVZ_CMAP_CET_D10 = DvzColorMap #{const DVZ_CMAP_CET_D10}
pattern DVZ_CMAP_CET_D11 :: DvzColorMap
pattern DVZ_CMAP_CET_D11 = DvzColorMap #{const DVZ_CMAP_CET_D11}
pattern DVZ_CMAP_CET_D8 :: DvzColorMap
pattern DVZ_CMAP_CET_D8 = DvzColorMap #{const DVZ_CMAP_CET_D8}
pattern DVZ_CMAP_CET_D13 :: DvzColorMap
pattern DVZ_CMAP_CET_D13 = DvzColorMap #{const DVZ_CMAP_CET_D13}
pattern DVZ_CMAP_CET_D3 :: DvzColorMap
pattern DVZ_CMAP_CET_D3 = DvzColorMap #{const DVZ_CMAP_CET_D3}
pattern DVZ_CMAP_CET_D1A :: DvzColorMap
pattern DVZ_CMAP_CET_D1A = DvzColorMap #{const DVZ_CMAP_CET_D1A}
pattern DVZ_CMAP_BJY :: DvzColorMap
pattern DVZ_CMAP_BJY = DvzColorMap #{const DVZ_CMAP_BJY}
pattern DVZ_CMAP_GWV :: DvzColorMap
pattern DVZ_CMAP_GWV = DvzColorMap #{const DVZ_CMAP_GWV}
pattern DVZ_CMAP_BWY :: DvzColorMap
pattern DVZ_CMAP_BWY = DvzColorMap #{const DVZ_CMAP_BWY}
pattern DVZ_CMAP_CET_D12 :: DvzColorMap
pattern DVZ_CMAP_CET_D12 = DvzColorMap #{const DVZ_CMAP_CET_D12}
pattern DVZ_CMAP_CET_R3 :: DvzColorMap
pattern DVZ_CMAP_CET_R3 = DvzColorMap #{const DVZ_CMAP_CET_R3}
pattern DVZ_CMAP_CET_D9 :: DvzColorMap
pattern DVZ_CMAP_CET_D9 = DvzColorMap #{const DVZ_CMAP_CET_D9}
pattern DVZ_CMAP_CWR :: DvzColorMap
pattern DVZ_CMAP_CWR = DvzColorMap #{const DVZ_CMAP_CWR}
pattern DVZ_CMAP_CET_CBC1 :: DvzColorMap
pattern DVZ_CMAP_CET_CBC1 = DvzColorMap #{const DVZ_CMAP_CET_CBC1}
pattern DVZ_CMAP_CET_CBC2 :: DvzColorMap
pattern DVZ_CMAP_CET_CBC2 = DvzColorMap #{const DVZ_CMAP_CET_CBC2}
pattern DVZ_CMAP_CET_CBL1 :: DvzColorMap
pattern DVZ_CMAP_CET_CBL1 = DvzColorMap #{const DVZ_CMAP_CET_CBL1}
pattern DVZ_CMAP_CET_CBL2 :: DvzColorMap
pattern DVZ_CMAP_CET_CBL2 = DvzColorMap #{const DVZ_CMAP_CET_CBL2}
pattern DVZ_CMAP_CET_CBTC1 :: DvzColorMap
pattern DVZ_CMAP_CET_CBTC1 = DvzColorMap #{const DVZ_CMAP_CET_CBTC1}
pattern DVZ_CMAP_CET_CBTC2 :: DvzColorMap
pattern DVZ_CMAP_CET_CBTC2 = DvzColorMap #{const DVZ_CMAP_CET_CBTC2}
pattern DVZ_CMAP_CET_CBTL1 :: DvzColorMap
pattern DVZ_CMAP_CET_CBTL1 = DvzColorMap #{const DVZ_CMAP_CET_CBTL1}
pattern DVZ_CMAP_BGY :: DvzColorMap
pattern DVZ_CMAP_BGY = DvzColorMap #{const DVZ_CMAP_BGY}
pattern DVZ_CMAP_BGYW :: DvzColorMap
pattern DVZ_CMAP_BGYW = DvzColorMap #{const DVZ_CMAP_BGYW}
pattern DVZ_CMAP_BMW :: DvzColorMap
pattern DVZ_CMAP_BMW = DvzColorMap #{const DVZ_CMAP_BMW}
pattern DVZ_CMAP_CET_C1 :: DvzColorMap
pattern DVZ_CMAP_CET_C1 = DvzColorMap #{const DVZ_CMAP_CET_C1}
pattern DVZ_CMAP_CET_C1S :: DvzColorMap
pattern DVZ_CMAP_CET_C1S = DvzColorMap #{const DVZ_CMAP_CET_C1S}
pattern DVZ_CMAP_CET_C2 :: DvzColorMap
pattern DVZ_CMAP_CET_C2 = DvzColorMap #{const DVZ_CMAP_CET_C2}
pattern DVZ_CMAP_CET_C4 :: DvzColorMap
pattern DVZ_CMAP_CET_C4 = DvzColorMap #{const DVZ_CMAP_CET_C4}
pattern DVZ_CMAP_CET_C4S :: DvzColorMap
pattern DVZ_CMAP_CET_C4S = DvzColorMap #{const DVZ_CMAP_CET_C4S}
pattern DVZ_CMAP_CET_C5 :: DvzColorMap
pattern DVZ_CMAP_CET_C5 = DvzColorMap #{const DVZ_CMAP_CET_C5}
pattern DVZ_CMAP_CET_I1 :: DvzColorMap
pattern DVZ_CMAP_CET_I1 = DvzColorMap #{const DVZ_CMAP_CET_I1}
pattern DVZ_CMAP_CET_I3 :: DvzColorMap
pattern DVZ_CMAP_CET_I3 = DvzColorMap #{const DVZ_CMAP_CET_I3}
pattern DVZ_CMAP_CET_L10 :: DvzColorMap
pattern DVZ_CMAP_CET_L10 = DvzColorMap #{const DVZ_CMAP_CET_L10}
pattern DVZ_CMAP_CET_L11 :: DvzColorMap
pattern DVZ_CMAP_CET_L11 = DvzColorMap #{const DVZ_CMAP_CET_L11}
pattern DVZ_CMAP_CET_L12 :: DvzColorMap
pattern DVZ_CMAP_CET_L12 = DvzColorMap #{const DVZ_CMAP_CET_L12}
pattern DVZ_CMAP_CET_L16 :: DvzColorMap
pattern DVZ_CMAP_CET_L16 = DvzColorMap #{const DVZ_CMAP_CET_L16}
pattern DVZ_CMAP_CET_L17 :: DvzColorMap
pattern DVZ_CMAP_CET_L17 = DvzColorMap #{const DVZ_CMAP_CET_L17}
pattern DVZ_CMAP_CET_L18 :: DvzColorMap
pattern DVZ_CMAP_CET_L18 = DvzColorMap #{const DVZ_CMAP_CET_L18}
pattern DVZ_CMAP_CET_L19 :: DvzColorMap
pattern DVZ_CMAP_CET_L19 = DvzColorMap #{const DVZ_CMAP_CET_L19}
pattern DVZ_CMAP_CET_L4 :: DvzColorMap
pattern DVZ_CMAP_CET_L4 = DvzColorMap #{const DVZ_CMAP_CET_L4}
pattern DVZ_CMAP_CET_L7 :: DvzColorMap
pattern DVZ_CMAP_CET_L7 = DvzColorMap #{const DVZ_CMAP_CET_L7}
pattern DVZ_CMAP_CET_L8 :: DvzColorMap
pattern DVZ_CMAP_CET_L8 = DvzColorMap #{const DVZ_CMAP_CET_L8}
pattern DVZ_CMAP_CET_L9 :: DvzColorMap
pattern DVZ_CMAP_CET_L9 = DvzColorMap #{const DVZ_CMAP_CET_L9}
pattern DVZ_CMAP_CET_R1 :: DvzColorMap
pattern DVZ_CMAP_CET_R1 = DvzColorMap #{const DVZ_CMAP_CET_R1}
pattern DVZ_CMAP_CET_R2 :: DvzColorMap
pattern DVZ_CMAP_CET_R2 = DvzColorMap #{const DVZ_CMAP_CET_R2}
pattern DVZ_CMAP_COLORWHEEL :: DvzColorMap
pattern DVZ_CMAP_COLORWHEEL = DvzColorMap #{const DVZ_CMAP_COLORWHEEL}
pattern DVZ_CMAP_FIRE :: DvzColorMap
pattern DVZ_CMAP_FIRE = DvzColorMap #{const DVZ_CMAP_FIRE}
pattern DVZ_CMAP_ISOLUM :: DvzColorMap
pattern DVZ_CMAP_ISOLUM = DvzColorMap #{const DVZ_CMAP_ISOLUM}
pattern DVZ_CMAP_KB :: DvzColorMap
pattern DVZ_CMAP_KB = DvzColorMap #{const DVZ_CMAP_KB}
pattern DVZ_CMAP_KBC :: DvzColorMap
pattern DVZ_CMAP_KBC = DvzColorMap #{const DVZ_CMAP_KBC}
pattern DVZ_CMAP_KG :: DvzColorMap
pattern DVZ_CMAP_KG = DvzColorMap #{const DVZ_CMAP_KG}
pattern DVZ_CMAP_KGY :: DvzColorMap
pattern DVZ_CMAP_KGY = DvzColorMap #{const DVZ_CMAP_KGY}
pattern DVZ_CMAP_KR :: DvzColorMap
pattern DVZ_CMAP_KR = DvzColorMap #{const DVZ_CMAP_KR}
pattern DVZ_CMAP_BLACK_BODY :: DvzColorMap
pattern DVZ_CMAP_BLACK_BODY = DvzColorMap #{const DVZ_CMAP_BLACK_BODY}
pattern DVZ_CMAP_KINDLMANN :: DvzColorMap
pattern DVZ_CMAP_KINDLMANN = DvzColorMap #{const DVZ_CMAP_KINDLMANN}
pattern DVZ_CMAP_EXTENDED_KINDLMANN :: DvzColorMap
pattern DVZ_CMAP_EXTENDED_KINDLMANN = DvzColorMap #{const DVZ_CMAP_EXTENDED_KINDLMANN}
pattern DVZ_CPAL256_GLASBEY_COOL :: DvzColorMap
pattern DVZ_CPAL256_GLASBEY_COOL = DvzColorMap #{const DVZ_CPAL256_GLASBEY_COOL}
pattern DVZ_CPAL256_GLASBEY_DARK :: DvzColorMap
pattern DVZ_CPAL256_GLASBEY_DARK = DvzColorMap #{const DVZ_CPAL256_GLASBEY_DARK}
pattern DVZ_CPAL256_GLASBEY_HV :: DvzColorMap
pattern DVZ_CPAL256_GLASBEY_HV = DvzColorMap #{const DVZ_CPAL256_GLASBEY_HV}
pattern DVZ_CPAL256_GLASBEY_LIGHT :: DvzColorMap
pattern DVZ_CPAL256_GLASBEY_LIGHT = DvzColorMap #{const DVZ_CPAL256_GLASBEY_LIGHT}
pattern DVZ_CPAL256_GLASBEY_WARM :: DvzColorMap
pattern DVZ_CPAL256_GLASBEY_WARM = DvzColorMap #{const DVZ_CPAL256_GLASBEY_WARM}
pattern DVZ_CPAL032_DARK2 :: DvzColorMap
pattern DVZ_CPAL032_DARK2 = DvzColorMap #{const DVZ_CPAL032_DARK2}
pattern DVZ_CPAL032_PAIRED :: DvzColorMap
pattern DVZ_CPAL032_PAIRED = DvzColorMap #{const DVZ_CPAL032_PAIRED}
pattern DVZ_CPAL032_PASTEL1 :: DvzColorMap
pattern DVZ_CPAL032_PASTEL1 = DvzColorMap #{const DVZ_CPAL032_PASTEL1}
pattern DVZ_CPAL032_PASTEL2 :: DvzColorMap
pattern DVZ_CPAL032_PASTEL2 = DvzColorMap #{const DVZ_CPAL032_PASTEL2}
pattern DVZ_CPAL032_SET1 :: DvzColorMap
pattern DVZ_CPAL032_SET1 = DvzColorMap #{const DVZ_CPAL032_SET1}
pattern DVZ_CPAL032_SET2 :: DvzColorMap
pattern DVZ_CPAL032_SET2 = DvzColorMap #{const DVZ_CPAL032_SET2}
pattern DVZ_CPAL032_SET3 :: DvzColorMap
pattern DVZ_CPAL032_SET3 = DvzColorMap #{const DVZ_CPAL032_SET3}
pattern DVZ_CPAL032_TAB10 :: DvzColorMap
pattern DVZ_CPAL032_TAB10 = DvzColorMap #{const DVZ_CPAL032_TAB10}
pattern DVZ_CPAL032_TAB20 :: DvzColorMap
pattern DVZ_CPAL032_TAB20 = DvzColorMap #{const DVZ_CPAL032_TAB20}
pattern DVZ_CPAL032_TAB20B :: DvzColorMap
pattern DVZ_CPAL032_TAB20B = DvzColorMap #{const DVZ_CPAL032_TAB20B}
pattern DVZ_CPAL032_TAB20C :: DvzColorMap
pattern DVZ_CPAL032_TAB20C = DvzColorMap #{const DVZ_CPAL032_TAB20C}
pattern DVZ_CPAL032_CATEGORY10_10 :: DvzColorMap
pattern DVZ_CPAL032_CATEGORY10_10 = DvzColorMap #{const DVZ_CPAL032_CATEGORY10_10}
pattern DVZ_CPAL032_CATEGORY20_20 :: DvzColorMap
pattern DVZ_CPAL032_CATEGORY20_20 = DvzColorMap #{const DVZ_CPAL032_CATEGORY20_20}
pattern DVZ_CPAL032_CATEGORY20B_20 :: DvzColorMap
pattern DVZ_CPAL032_CATEGORY20B_20 = DvzColorMap #{const DVZ_CPAL032_CATEGORY20B_20}
pattern DVZ_CPAL032_CATEGORY20C_20 :: DvzColorMap
pattern DVZ_CPAL032_CATEGORY20C_20 = DvzColorMap #{const DVZ_CPAL032_CATEGORY20C_20}
pattern DVZ_CPAL032_COLORBLIND8:: DvzColorMap
pattern DVZ_CPAL032_COLORBLIND8 = DvzColorMap #{const DVZ_CPAL032_COLORBLIND8}

{-# COMPLETE
    DVZ_CMAP_BINARY,
    DVZ_CMAP_HSV,
    DVZ_CMAP_CIVIDIS,
    DVZ_CMAP_INFERNO,
    DVZ_CMAP_MAGMA,
    DVZ_CMAP_PLASMA,
    DVZ_CMAP_VIRIDIS,
    DVZ_CMAP_BLUES,
    DVZ_CMAP_BUGN,
    DVZ_CMAP_BUPU,
    DVZ_CMAP_GNBU,
    DVZ_CMAP_GREENS,
    DVZ_CMAP_GREYS,
    DVZ_CMAP_ORANGES,
    DVZ_CMAP_ORRD,
    DVZ_CMAP_PUBU,
    DVZ_CMAP_PUBUGN,
    DVZ_CMAP_PURPLES,
    DVZ_CMAP_RDPU,
    DVZ_CMAP_REDS,
    DVZ_CMAP_YLGN,
    DVZ_CMAP_YLGNBU,
    DVZ_CMAP_YLORBR,
    DVZ_CMAP_YLORRD,
    DVZ_CMAP_AFMHOT,
    DVZ_CMAP_AUTUMN,
    DVZ_CMAP_BONE,
    DVZ_CMAP_COOL,
    DVZ_CMAP_COPPER,
    DVZ_CMAP_GIST_HEAT,
    DVZ_CMAP_GRAY,
    DVZ_CMAP_HOT,
    DVZ_CMAP_PINK,
    DVZ_CMAP_SPRING,
    DVZ_CMAP_SUMMER,
    DVZ_CMAP_WINTER,
    DVZ_CMAP_WISTIA,
    DVZ_CMAP_BRBG,
    DVZ_CMAP_BWR,
    DVZ_CMAP_COOLWARM,
    DVZ_CMAP_PIYG,
    DVZ_CMAP_PRGN,
    DVZ_CMAP_PUOR,
    DVZ_CMAP_RDBU,
    DVZ_CMAP_RDGY,
    DVZ_CMAP_RDYLBU,
    DVZ_CMAP_RDYLGN,
    DVZ_CMAP_SEISMIC,
    DVZ_CMAP_SPECTRAL,
    DVZ_CMAP_TWILIGHT_SHIFTED,
    DVZ_CMAP_TWILIGHT,
    DVZ_CMAP_BRG,
    DVZ_CMAP_CMRMAP,
    DVZ_CMAP_CUBEHELIX,
    DVZ_CMAP_FLAG,
    DVZ_CMAP_GIST_EARTH,
    DVZ_CMAP_GIST_NCAR,
    DVZ_CMAP_GIST_RAINBOW,
    DVZ_CMAP_GIST_STERN,
    DVZ_CMAP_GNUPLOT2,
    DVZ_CMAP_GNUPLOT,
    DVZ_CMAP_JET,
    DVZ_CMAP_NIPY_SPECTRAL,
    DVZ_CMAP_OCEAN,
    DVZ_CMAP_PRISM,
    DVZ_CMAP_RAINBOW,
    DVZ_CMAP_TERRAIN,
    DVZ_CMAP_BKR,
    DVZ_CMAP_BKY,
    DVZ_CMAP_CET_D10,
    DVZ_CMAP_CET_D11,
    DVZ_CMAP_CET_D8,
    DVZ_CMAP_CET_D13,
    DVZ_CMAP_CET_D3,
    DVZ_CMAP_CET_D1A,
    DVZ_CMAP_BJY,
    DVZ_CMAP_GWV,
    DVZ_CMAP_BWY,
    DVZ_CMAP_CET_D12,
    DVZ_CMAP_CET_R3,
    DVZ_CMAP_CET_D9,
    DVZ_CMAP_CWR,
    DVZ_CMAP_CET_CBC1,
    DVZ_CMAP_CET_CBC2,
    DVZ_CMAP_CET_CBL1,
    DVZ_CMAP_CET_CBL2,
    DVZ_CMAP_CET_CBTC1,
    DVZ_CMAP_CET_CBTC2,
    DVZ_CMAP_CET_CBTL1,
    DVZ_CMAP_BGY,
    DVZ_CMAP_BGYW,
    DVZ_CMAP_BMW,
    DVZ_CMAP_CET_C1,
    DVZ_CMAP_CET_C1S,
    DVZ_CMAP_CET_C2,
    DVZ_CMAP_CET_C4,
    DVZ_CMAP_CET_C4S,
    DVZ_CMAP_CET_C5,
    DVZ_CMAP_CET_I1,
    DVZ_CMAP_CET_I3,
    DVZ_CMAP_CET_L10,
    DVZ_CMAP_CET_L11,
    DVZ_CMAP_CET_L12,
    DVZ_CMAP_CET_L16,
    DVZ_CMAP_CET_L17,
    DVZ_CMAP_CET_L18,
    DVZ_CMAP_CET_L19,
    DVZ_CMAP_CET_L4,
    DVZ_CMAP_CET_L7,
    DVZ_CMAP_CET_L8,
    DVZ_CMAP_CET_L9,
    DVZ_CMAP_CET_R1,
    DVZ_CMAP_CET_R2,
    DVZ_CMAP_COLORWHEEL,
    DVZ_CMAP_FIRE,
    DVZ_CMAP_ISOLUM,
    DVZ_CMAP_KB,
    DVZ_CMAP_KBC,
    DVZ_CMAP_KG,
    DVZ_CMAP_KGY,
    DVZ_CMAP_KR,
    DVZ_CMAP_BLACK_BODY,
    DVZ_CMAP_KINDLMANN,
    DVZ_CMAP_EXTENDED_KINDLMANN,
    DVZ_CPAL256_GLASBEY_COOL,
    DVZ_CPAL256_GLASBEY_DARK,
    DVZ_CPAL256_GLASBEY_HV,
    DVZ_CPAL256_GLASBEY_LIGHT,
    DVZ_CPAL256_GLASBEY_WARM,
    DVZ_CPAL032_DARK2,
    DVZ_CPAL032_PAIRED,
    DVZ_CPAL032_PASTEL1,
    DVZ_CPAL032_PASTEL2,
    DVZ_CPAL032_SET1,
    DVZ_CPAL032_SET2,
    DVZ_CPAL032_SET3,
    DVZ_CPAL032_TAB10,
    DVZ_CPAL032_TAB20,
    DVZ_CPAL032_TAB20B,
    DVZ_CPAL032_TAB20C,
    DVZ_CPAL032_CATEGORY10_10,
    DVZ_CPAL032_CATEGORY20_20,
    DVZ_CPAL032_CATEGORY20B_20,
    DVZ_CPAL032_CATEGORY20C_20,
    DVZ_CPAL032_COLORBLIND8
    #-}

foreign import capi safe "datoviz/datoviz.h dvz_colormap_scale" c_dvz_colormap_scale
    :: DvzColorMap -> Double -> Double -> Double -> Ptr () -> IO ()

