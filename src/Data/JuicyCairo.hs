{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.JuicyCairo (
	-- * Immutable
	-- ** Cairo Image =\> JuicyPixels Image
	cairoToJuicy,
	cairoArgb32ToJuicyRGBA8, cairoRgb24ToJuicyRGB8,
	cairoA8ToJuicyY8, cairoA1ToJuicyY8,
	cairoRgb16_565ToJuicyRGB8, cairoRgb30ToJuicyRGB16,
	-- ** JuicyPixels Image =\> Cairo Image
	juicyToCairo,
	juicyRGBA8ToCairoArgb32, juicyRGB8ToCairoRgb24,
	juicyY8ToCairoA8, juicyY8ToCairoA1,
	juicyRGB8ToCairoRgb16_565, juicyRGB16ToCairoRgb30,

	-- * Mutable
	-- ** Cairo Mutable Image =\> JuicyPixels Image
	cairoMutToJuicy,
	cairoArgb32MutToJuicyRGBA8, cairoRgb24MutToJuicyRGB8,
	cairoA8MutToJuicyY8, cairoA1MutToJuicyY8,
	cairoRgb16_565MutToJuicyRGB8, cairoRgb30MutToJuicyRGB16,
	-- ** JuicyPixels Image =\> Cairo Mutable Image
	juicyToCairoMut,
	juicyRGBA8ToCairoArgb32Mut, juicyRGB8ToCairoRgb24Mut,
	juicyY8ToCairoA8Mut, juicyY8ToCairoA1Mut,
	juicyRGB8ToCairoRgb16_565Mut, juicyRGB16ToCairoRgb30Mut ) where

import Control.Arrow ((***))
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Foldable (for_)
import Data.Bool (bool)
import Data.Maybe (fromJust)
import Data.Word (Word8)

import qualified Data.CairoImage.Internal as C
import qualified Codec.Picture as J

---------------------------------------------------------------------------

-- * CAIRO IMAGE => JUICY IMAGE
-- * JUICY IMAGE => CAIRO IMAGE
-- * CAIRO MUTABLE IMAGE => JUICY IMAGE
-- * JUICY IMAGE => CAIRO MUTABLE IMAGE
-- * CAIRO PIXEL => JUICY PIXEL
-- * JUICY PIXEL => CAIRO PIXEL

---------------------------------------------------------------------------
-- CAIRO IMAGE => JUICY IMAGE
---------------------------------------------------------------------------

cairoToJuicy :: (C.Image i, J.Pixel p) => (C.Pixel i -> p) -> i -> J.Image p
cairoToJuicy f i = (uncurry . J.generateImage)
	(\x y -> f . fromJust $ C.pixelAt i (fromIntegral x) (fromIntegral y))
	(fromIntegral *** fromIntegral $ C.imageSize i)

cairoArgb32ToJuicyRGBA8 :: C.Argb32 -> J.Image J.PixelRGBA8
cairoArgb32ToJuicyRGBA8 = cairoToJuicy pixelArgb32ToPixelRGBA8

cairoRgb24ToJuicyRGB8 :: C.Rgb24 -> J.Image J.PixelRGB8
cairoRgb24ToJuicyRGB8 = cairoToJuicy pixelRgb24ToPixelRGB8

cairoA8ToJuicyY8 :: C.A8 -> J.Image J.Pixel8
cairoA8ToJuicyY8 = cairoToJuicy pixelA8ToPixel8

cairoA1ToJuicyY8 :: C.A1 -> J.Image J.Pixel8
cairoA1ToJuicyY8 = cairoToJuicy pixelA1ToPixel8

cairoRgb16_565ToJuicyRGB8 :: C.Rgb16_565 -> J.Image J.PixelRGB8
cairoRgb16_565ToJuicyRGB8 = cairoToJuicy pixelRgb16_565ToPixelRGB8

cairoRgb30ToJuicyRGB16 :: C.Rgb30 -> J.Image J.PixelRGB16
cairoRgb30ToJuicyRGB16 = cairoToJuicy pixelRgb30ToPixelRGB16

---------------------------------------------------------------------------
-- JUICY IMAGE => CAIRO IMAGE
---------------------------------------------------------------------------

juicyToCairo :: (J.Pixel p, C.Image i) => (p -> C.Pixel i) -> J.Image p -> i
juicyToCairo f i = C.generateImage
	(fromIntegral $ J.imageWidth i)
	(fromIntegral $ J.imageHeight i)
	(\x y -> f $ J.pixelAt i (fromIntegral x) (fromIntegral y))

juicyRGBA8ToCairoArgb32 :: J.Image J.PixelRGBA8 -> C.Argb32
juicyRGBA8ToCairoArgb32 = juicyToCairo pixelRGBA8ToPixelArgb32

juicyRGB8ToCairoRgb24 :: J.Image J.PixelRGB8 -> C.Rgb24
juicyRGB8ToCairoRgb24 = juicyToCairo pixelRGB8ToPixelRgb24

juicyY8ToCairoA8 :: J.Image J.Pixel8 -> C.A8
juicyY8ToCairoA8 = juicyToCairo pixel8ToPixelA8

juicyY8ToCairoA1 :: Word8 -> J.Image J.Pixel8 -> C.A1
juicyY8ToCairoA1 = juicyToCairo . pixel8ToPixelA1

juicyRGB8ToCairoRgb16_565 :: J.Image J.PixelRGB8 -> C.Rgb16_565
juicyRGB8ToCairoRgb16_565 = juicyToCairo pixelRGB8ToPixelRgb16_565

juicyRGB16ToCairoRgb30 :: J.Image J.PixelRGB16 -> C.Rgb30
juicyRGB16ToCairoRgb30 = juicyToCairo pixelRGB16ToPixelRgb30

---------------------------------------------------------------------------
-- CAIRO MUTABLE IMAGE => JUICY IMAGE
---------------------------------------------------------------------------

cairoMutToJuicy :: (C.ImageMut im, J.Pixel p, PrimMonad m) => (C.PixelMut im -> p) -> im (PrimState m) -> m (J.Image p)
cairoMutToJuicy f i = uncurry J.withImage
	(fromIntegral *** fromIntegral $ C.imageMutSize i)
	\x y -> f . fromJust <$> C.getPixel i (fromIntegral x) (fromIntegral y)

cairoArgb32MutToJuicyRGBA8 :: PrimMonad m => C.Argb32Mut (PrimState m) -> m (J.Image J.PixelRGBA8)
cairoArgb32MutToJuicyRGBA8 = cairoMutToJuicy pixelArgb32ToPixelRGBA8

cairoRgb24MutToJuicyRGB8 :: PrimMonad m => C.Rgb24Mut (PrimState m) -> m (J.Image J.PixelRGB8)
cairoRgb24MutToJuicyRGB8 = cairoMutToJuicy pixelRgb24ToPixelRGB8

cairoA8MutToJuicyY8 :: PrimMonad m => C.A8Mut (PrimState m) -> m (J.Image J.Pixel8)
cairoA8MutToJuicyY8 = cairoMutToJuicy pixelA8ToPixel8

cairoA1MutToJuicyY8 :: PrimMonad m => C.A1Mut (PrimState m) -> m (J.Image J.Pixel8)
cairoA1MutToJuicyY8 = cairoMutToJuicy pixelA1ToPixel8

cairoRgb16_565MutToJuicyRGB8 :: PrimMonad m => C.Rgb16_565Mut (PrimState m) -> m (J.Image J.PixelRGB8)
cairoRgb16_565MutToJuicyRGB8 = cairoMutToJuicy pixelRgb16_565ToPixelRGB8

cairoRgb30MutToJuicyRGB16 :: PrimMonad m => C.Rgb30Mut (PrimState m) -> m (J.Image J.PixelRGB16)
cairoRgb30MutToJuicyRGB16 = cairoMutToJuicy pixelRgb30ToPixelRGB16

---------------------------------------------------------------------------
-- JUICY IMAGE => CAIRO MUTABLE IMAGE
---------------------------------------------------------------------------

juicyToCairoMut :: (J.Pixel p, C.ImageMut im, PrimMonad m) => (p -> C.PixelMut im) -> J.Image p -> m (im (PrimState m))
juicyToCairoMut f i = do
	im <- C.newImageMut (fromIntegral w) (fromIntegral h)
	for_ [0 .. h] \y -> for_ [0 .. w] \x ->
		C.putPixel im (fromIntegral x) (fromIntegral y) . f $ J.pixelAt i x y
	pure im
	where
	w = J.imageWidth i
	h = J.imageHeight i

juicyRGBA8ToCairoArgb32Mut :: PrimMonad m => J.Image J.PixelRGBA8 -> m (C.Argb32Mut (PrimState m))
juicyRGBA8ToCairoArgb32Mut = juicyToCairoMut pixelRGBA8ToPixelArgb32

juicyRGB8ToCairoRgb24Mut :: PrimMonad m => J.Image J.PixelRGB8 -> m (C.Rgb24Mut (PrimState m))
juicyRGB8ToCairoRgb24Mut = juicyToCairoMut pixelRGB8ToPixelRgb24

juicyY8ToCairoA8Mut :: PrimMonad m => J.Image J.Pixel8 -> m (C.A8Mut (PrimState m))
juicyY8ToCairoA8Mut = juicyToCairoMut pixel8ToPixelA8

juicyY8ToCairoA1Mut :: PrimMonad m => Word8 -> J.Image J.Pixel8 -> m (C.A1Mut (PrimState m))
juicyY8ToCairoA1Mut = juicyToCairoMut . pixel8ToPixelA1

juicyRGB8ToCairoRgb16_565Mut :: PrimMonad m => J.Image J.PixelRGB8 -> m (C.Rgb16_565Mut (PrimState m))
juicyRGB8ToCairoRgb16_565Mut = juicyToCairoMut pixelRGB8ToPixelRgb16_565

juicyRGB16ToCairoRgb30Mut :: PrimMonad m => J.Image J.PixelRGB16 -> m (C.Rgb30Mut (PrimState m))
juicyRGB16ToCairoRgb30Mut = juicyToCairoMut pixelRGB16ToPixelRgb30

---------------------------------------------------------------------------
-- CAIRO PIXEL => JUICY PIXEL
---------------------------------------------------------------------------

pixelArgb32ToPixelRGBA8 :: C.PixelArgb32 -> J.PixelRGBA8
pixelArgb32ToPixelRGBA8 (C.PixelArgb32Straight a r g b) = J.PixelRGBA8 r g b a

pixelRgb24ToPixelRGB8 :: C.PixelRgb24 -> J.PixelRGB8
pixelRgb24ToPixelRGB8 (C.PixelRgb24 r g b) = J.PixelRGB8 r g b

pixelA8ToPixel8 :: C.PixelA8 -> J.Pixel8
pixelA8ToPixel8 (C.PixelA8 b) = b

pixelA1ToPixel8 :: C.PixelA1 -> J.Pixel8
pixelA1ToPixel8 = \case C.PixelA1 C.O -> 0x00; C.PixelA1 C.I -> 0xff

pixelRgb16_565ToPixelRGB8 :: C.PixelRgb16_565 -> J.PixelRGB8
pixelRgb16_565ToPixelRGB8 (C.PixelRgb16_565 r g b) = J.PixelRGB8 r g b

pixelRgb30ToPixelRGB16 :: C.PixelRgb30 -> J.PixelRGB16
pixelRgb30ToPixelRGB16 (C.PixelRgb30 r g b) = J.PixelRGB16 r g b

---------------------------------------------------------------------------
-- JUICY PIXEL => CAIRO PIXEL
---------------------------------------------------------------------------

pixelRGBA8ToPixelArgb32 :: J.PixelRGBA8 -> C.PixelArgb32
pixelRGBA8ToPixelArgb32 (J.PixelRGBA8 r g b a) = C.PixelArgb32Straight a r g b

pixelRGB8ToPixelRgb24 :: J.PixelRGB8 -> C.PixelRgb24
pixelRGB8ToPixelRgb24 (J.PixelRGB8 r g b) = C.PixelRgb24 r g b

pixel8ToPixelA8 :: J.Pixel8 -> C.PixelA8
pixel8ToPixelA8 = C.PixelA8

pixel8ToPixelA1 :: Word8 -> J.Pixel8 -> C.PixelA1
pixel8ToPixelA1 t = C.PixelA1 . bool C.O C.I . (t <=)

pixelRGB8ToPixelRgb16_565 :: J.PixelRGB8 -> C.PixelRgb16_565
pixelRGB8ToPixelRgb16_565 (J.PixelRGB8 r g b) = C.PixelRgb16_565 r g b

pixelRGB16ToPixelRgb30 :: J.PixelRGB16 -> C.PixelRgb30
pixelRGB16ToPixelRgb30 (J.PixelRGB16 r g b) = C.PixelRgb30 r g b
