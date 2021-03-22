memo
====

enhancement
-----------

* [x] make converter JuicyPixel's ImageY8 <=> CairoImage's A1
	+ [x] converter from Pixel8 to PixelA1
	+ [x] converter from JuicyPixel's ImageY8 to CairoImage's A1
	+ [x] converter from PixelA1 to Pixel8
	+ [x] converter from CairoImage's A1 to JuicyPixel's ImageY8
* [x] make converter JuicyPixel's ImageFoo <=> Image Foo of Cairo Mutable Image
	+ [x] define `cairoMutToJuicy`
	+ [x] define `juicyToCairoMut`
	+ [x] Argb32 <=> RGBA8
	+ [x] Rgb24 <=> RGB8
	+ [x] A8 <=> Y8
	+ [x] A1 <=> Y8
	+ [x] Rgb16\_565 <=> RGB8
	+ [x] Rgb30 <=> RGB16
* [x] repair A8 and A1
	+ [x] add A8 <=> YA8
		- [x] `juicyYA8ToCairoA8`
		- [x] `cairoA8ToJuicyYA8`
		- [x] `juicyYA8ToCairoA8Mut`
		- [x] `cairoA8MutToJuicyYA8`
	+ [x] add A8 <=> RGBA8
		- [x] `juicyRGBA8ToCairoA8`
		- [x] `cairoA8ToJuicyRGBA8`
		- [x] `juicyRGBA8ToCairoA8Mut`
		- [x] `cairoA8MutToJuicyRGBA8`
	+ [x] add A1 <=> YA8
		- [x] `juicyYA8ToCairoA1`
		- [x] `cairoA1ToJuicyYA8`
		- [x] `juicyYA8ToCairoA1Mut`
		- [x] `cairoA1MutToJuicyYA8`
	+ [x] add A1 <=> RGBA8
		- [x] `juicyRGBA8ToCairoA1`
		- [x] `cairoA1ToJuicyRGBA8`
		- [x] `juicyRGBA8ToCairoA1Mut`
		- [x] `cairoA1MutToJuicyRGBA8`
	+ [x] remove A8 <=> Y8
	+ [x] remove A1 <=> Y8
* [ ] refactoring

refactoring
-----------

* [x] refactor document
	+ [x] Data.JuicyCairo
		- [x] Immutable
			* [x] Cairo Image => JuicyPixels Image
				+ [x] cairoToJuicy
				+ [x] Arg 32 => RGBA 8
				+ [x] Rgb 24 => RGB 8
				+ [x] A 8 => YA 8
				+ [x] A 8 => RGBA 8
				+ [x] A 1 => YA 8
				+ [x] A 1 => RGBA 8
				+ [x] Rgb 16 565 => RGB 8
				+ [x] Rgb 30 => RGB 16
			* [x] JuicyPixels Image => Cairo Image
		- [x] Mutable
			* [x] Cairo Mutable Image => JuicyPixels Image
			* [x] JuicyPixels Image => Cairo Mutable Image
* [ ] refactor Data.JuicyCairo
	+ [x] export list
	+ [x] import list
	+ [x] structure
		- [x] CAIRO IMAGE => JUICY IMAGE
		- [x] JUICY IMAGE => CAIRO IMAGE
		- [x] CAIRO MUTABLE IMAGE => JUICY IMAGE
		- [x] JUICY IMAGE => CAIRO MUTABLE IMAGE
		- [x] CAIRO PIXEL => JUICY PIXEL
		- [x] JUICY PIXEL => CAIRO PIXEL
	+ [ ] body
		- [x] CAIRO IMAGE => JUICY IMAGE
			* [x] function `cairoToJuicy`
			* [x] Argb32
			* [x] Rgb24
			* [x] A8
				+ [x] RGBA 8
				+ [x] YA 8
			* [x] A1
				+ [x] RGBA 8
				+ [x] YA 8
			* [x] Rgb16_565
			* [x] Rgb30
		- [x] JUICY IMAGE => CAIRO IMAGE
			* [x] function `juicyToCairo`
			* [x] Argb32
			* [x] Rgb24
			* [x] A8
				+ [x] RGBA 8
				+ [x] YA 8
			* [x] A1
				+ [x] RGBA 8
				+ [x] YA 8
			* [x] Rgb16_565
			* [x] Rgb30
		- [x] CAIRO MUTABLE IMAGE => JUICY IMAGE
			* [x] function `cairoMutToJuicy`
			* [x] Argb32
			* [x] Rgb24
			* [x] A8
				+ [x] RGBA 8
				+ [x] YA 8
			* [x] A1
				+ [x] RGBA 8
				+ [x] YA 8
			* [x] Rgb16_565
			* [x] Rgb30
		- [x] JUICY IMAGE => CAIRO MUTABLE IMAGE
			* [x] function `juicyToCairoMut`
			* [x] Argb32
			* [x] Rgb24
			* [x] A8
				+ [x] RGBA 8
				+ [x] YA 8
			* [x] A1
				+ [x] RGBA 8
				+ [x] YA 8
			* [x] Rgb16_565
			* [x] Rgb30
		- [ ] CAIRO PIXEL => JUICY PIXEL
			* [ ] Argb32
			* [ ] Rgb24
			* [ ] A8
				+ [ ] RGBA 8
				+ [ ] YA 8
			* [ ] A1
				+ [ ] RGBA 8
				+ [ ] YA 8
			* [ ] Rgb16_565
			* [ ] Rgb30
		- [ ] JUICY PIXEL => CAIRO PIXEL
			* [ ] Argb32
			* [ ] Rgb24
			* [ ] A8
				+ [ ] RGBA 8
				+ [ ] YA 8
			* [ ] A1
				+ [ ] RGBA 8
				+ [ ] YA 8
			* [ ] Rgb16_565
			* [ ] Rgb30
