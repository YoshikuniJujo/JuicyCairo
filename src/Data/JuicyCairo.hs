{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.JuicyCairo (
	cairoToJuicy, juicyToCairo,
	cairoArgb32ToJuicyRGBA8, juicyRGBA8ToCairoArgb32,
	cairoRgb24ToJuicyRGB8, juicyRGB8ToCairoRgb24,
	cairoA8ToJuicyY8, juicyY8ToCairoA8,
	cairoA1ToJuicyY8, juicyY8ToCairoA1 ) where

import Control.Arrow
import Data.Bool
import Data.Maybe
import Data.Word

import qualified Data.CairoImage.Internal as C
import qualified Codec.Picture as J

cairoToJuicy :: (C.Image i, J.Pixel p) => (C.Pixel i -> p) -> i -> J.Image p
cairoToJuicy f i = (uncurry . J.generateImage)
	(\x y -> f . fromJust $ C.pixelAt i (fromIntegral x) (fromIntegral y))
	(fromIntegral *** fromIntegral $ C.imageSize i)

pixelArgb32ToPixelRGBA8 :: C.PixelArgb32 -> J.PixelRGBA8
pixelArgb32ToPixelRGBA8 (C.PixelArgb32Straight a r g b) = J.PixelRGBA8 r g b a

cairoArgb32ToJuicyRGBA8 :: C.Argb32 -> J.Image J.PixelRGBA8
cairoArgb32ToJuicyRGBA8 = cairoToJuicy pixelArgb32ToPixelRGBA8

pixelRgb24ToPixelRGB8 :: C.PixelRgb24 -> J.PixelRGB8
pixelRgb24ToPixelRGB8 (C.PixelRgb24 r g b) = J.PixelRGB8 r g b

cairoRgb24ToJuicyRGB8 :: C.Rgb24 -> J.Image J.PixelRGB8
cairoRgb24ToJuicyRGB8 = cairoToJuicy pixelRgb24ToPixelRGB8

sample1 :: C.Argb32
sample1 = C.generateImage 256 256 \x y -> C.PixelArgb32Straight
	255
	(fromIntegral x)
	(fromIntegral y)
	(fromIntegral $ 255 - x `div` 2 - y `div` 2)

sample1Png :: IO ()
sample1Png = J.writePng "sample1.png" $ cairoArgb32ToJuicyRGBA8 sample1

juicyToCairo :: (J.Pixel p, C.Image i) => (p -> C.Pixel i) -> J.Image p -> i
juicyToCairo f i = C.generateImage
	(fromIntegral $ J.imageWidth i)
	(fromIntegral $ J.imageHeight i)
	(\x y -> f $ J.pixelAt i (fromIntegral x) (fromIntegral y))

pixelRGBA8ToPixelArgb32 :: J.PixelRGBA8 -> C.PixelArgb32
pixelRGBA8ToPixelArgb32 (J.PixelRGBA8 r g b a) = C.PixelArgb32Straight a r g b

juicyRGBA8ToCairoArgb32 :: J.Image J.PixelRGBA8 -> C.Argb32
juicyRGBA8ToCairoArgb32 = juicyToCairo pixelRGBA8ToPixelArgb32

pixelRGB8ToPixelRgb24 :: J.PixelRGB8 -> C.PixelRgb24
pixelRGB8ToPixelRgb24 (J.PixelRGB8 r g b) = C.PixelRgb24 r g b

juicyRGB8ToCairoRgb24 :: J.Image J.PixelRGB8 -> C.Rgb24
juicyRGB8ToCairoRgb24 = juicyToCairo pixelRGB8ToPixelRgb24

pixel8ToPixelA8 :: J.Pixel8 -> C.PixelA8
pixel8ToPixelA8 b = C.PixelA8 b

juicyY8ToCairoA8 :: J.Image J.Pixel8 -> C.A8
juicyY8ToCairoA8 = juicyToCairo pixel8ToPixelA8

pixelA8ToPixel8 :: C.PixelA8 -> J.Pixel8
pixelA8ToPixel8 (C.PixelA8 b) = b

cairoA8ToJuicyY8 :: C.A8 -> J.Image J.Pixel8
cairoA8ToJuicyY8 = cairoToJuicy pixelA8ToPixel8

pixel8ToPixelA1 :: Word8 -> J.Pixel8 -> C.PixelA1
pixel8ToPixelA1 t = C.PixelA1 . bool C.O C.I . (t <)

juicyY8ToCairoA1 :: Word8 -> J.Image J.Pixel8 -> C.A1
juicyY8ToCairoA1 = juicyToCairo . pixel8ToPixelA1

pixelA1ToPixel8 :: C.PixelA1 -> J.Pixel8
pixelA1ToPixel8 = \case C.PixelA1 C.O -> 0x00; C.PixelA1 C.I -> 0xff

cairoA1ToJuicyY8 :: C.A1 -> J.Image J.Pixel8
cairoA1ToJuicyY8 = cairoToJuicy pixelA1ToPixel8
