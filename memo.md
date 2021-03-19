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
* [ ] repair A8 and A1
	+ [ ] add A8 <=> YA8
	+ [ ] add A8 <=> RGBA8
	+ [ ] add A1 <=> YA8
	+ [ ] add A1 <=> RGBA8
	+ [ ] remove A8 <=> Y8
	+ [ ] remove A1 <=> Y8

refactoring
-----------

* [x] refactor document
	+ [x] Data.JuicyCairo
* [x] refactor Data.JuicyCairo
	+ [x] export list
	+ [x] import list
	+ [x] structure
	+ [x] body
		- [x] CAIRO IMAGE => JUICY IMAGE
			* [x] function `cairoToJuicy`
			* [x] Argb32
			* [x] Rgb24
			* [x] A8
			* [x] A1
			* [x] Rgb16_565
			* [x] Rgb30
		- [x] JUICY IMAGE => CAIRO IMAGE
			* [x] function `juicyToCairo`
			* [x] Argb32
			* [x] Rgb24
			* [x] A8
			* [x] A1
			* [x] Rgb16_565
			* [x] Rgb30
		- [x] CAIRO MUTABLE IMAGE => JUICY IMAGE
			* [x] function `cairoMutToJuicy`
			* [x] Argb32
			* [x] Rgb24
			* [x] A8
			* [x] A1
			* [x] Rgb16_565
			* [x] Rgb30
		- [x] JUICY IMAGE => CAIRO MUTABLE IMAGE
			* [x] function `juicyToCairoMut`
			* [x] Argb32
			* [x] Rgb24
			* [x] A8
			* [x] A1
			* [x] Rgb16_565
			* [x] Rgb30
		- [x] CAIRO PIXEL => JUICY PIXEL
			* [x] Argb32
			* [x] Rgb24
			* [x] A8
			* [x] A1
			* [x] Rgb16_565
			* [x] Rgb30
		- [x] JUICY PIXEL => CAIRO PIXEL
			* [x] Argb32
			* [x] Rgb24
			* [x] A8
			* [x] A1
			* [x] Rgb16_565
			* [x] Rgb30
