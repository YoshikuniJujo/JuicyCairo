{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.JuicyCairo (cairoArgb32ToJuicyRGBA8, juicyRGBA8ToCairoArgb32) where

import Control.Arrow
import Data.Maybe

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
