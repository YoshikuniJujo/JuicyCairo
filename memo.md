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

refactoring
-----------

* [x] refactor document
	+ [x] Data.JuicyCairo
* [ ] refactor Data.JuicyCairo
	+ [x] export list
	+ [x] import list
	+ [x] structure
	+ [ ] body
		- [ ] CAIRO IMAGE => JUICY IMAGE
			* [ ] function `cairoToJuicy`
			* [ ] Argb32
			* [ ] Rgb24
			* [ ] A8
			* [ ] A1
			* [ ] Rgb16_565
			* [ ] Rgb30
		- [ ] JUICY IMAGE => CAIRO IMAGE
			* [ ] function `juicyToCairo`
			* [ ] Argb32
			* [ ] Rgb24
			* [ ] A8
			* [ ] A1
			* [ ] Rgb16_565
			* [ ] Rgb30
		- [ ] CAIRO MUTABLE IMAGE => JUICY IMAGE
			* [ ] function `cairoMutToJuicy`
			* [ ] Argb32
			* [ ] Rgb24
			* [ ] A8
			* [ ] A1
			* [ ] Rgb16_565
			* [ ] Rgb30
		- [ ] JUICY IMAGE => CAIRO MUTABLE IMAGE
			* [ ] function `juicyToCairoMut`
			* [ ] Argb32
			* [ ] Rgb24
			* [ ] A8
			* [ ] A1
			* [ ] Rgb16_565
			* [ ] Rgb30
		- [ ] CAIRO PIXEL => JUICY PIXEL
			* [ ] Argb32
			* [ ] Rgb24
			* [ ] A8
			* [ ] A1
			* [ ] Rgb16_565
			* [ ] Rgb30
		- [ ] JUICY PIXEL => CAIRO PIXEL
			* [ ] Argb32
			* [ ] Rgb24
			* [ ] A8
			* [ ] A1
			* [ ] Rgb16_565
			* [ ] Rgb30
