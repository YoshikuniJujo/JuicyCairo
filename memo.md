memo
====

enhancement
-----------

* [x] make converter JuicyPixel's ImageY8 <=> CairoImage's A1
	+ [x] converter from Pixel8 to PixelA1
	+ [x] converter from JuicyPixel's ImageY8 to CairoImage's A1
	+ [x] converter from PixelA1 to Pixel8
	+ [x] converter from CairoImage's A1 to JuicyPixel's ImageY8
* [ ] make converter JuicyPixel's ImageFoo <=> Image Foo of Cairo Mutable Image
	+ [x] define `cairoMutToJuicy`
	+ [x] define `juicyToCairoMut`
	+ [x] Argb32 <=> RGBA8
	+ [x] Rgb24 <=> RGB8
	+ [ ] A8 <=> Y8
	+ [ ] A1 <=> Y8
	+ [ ] Rgb16\_565 <=> RGB8
	+ [ ] Rgb30 <=> RGB16

refactoring
-----------

* [ ] refactor document
	+ [ ] Data.JuicyCairo
* [ ] refactor Data.JuicyCairo
	+ [ ] export list
	+ [ ] import list
	+ [ ] structure
	+ [ ] body
