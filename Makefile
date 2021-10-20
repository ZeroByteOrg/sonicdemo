IMG2TILESET	= tools/img2tileset.py
VGM2ZSM		= tools/vgm2zsm
BGCONVERT	= tools/bgconvert

# My environment has a front-end shell script "x16" to run the
# official emulator. It adds a -v option to easily switch revisions.
# Comment them out and uncomment the standard commands for your
# environment - you will likely need full paths...

EMU38		= x16 -v r38
#EMU38		= x16emu
EMU39		= x16 -v r39
#EMU39		= x16emu
BOX16		= box16

BINHEADER	= assets/prgheader.bin
PRG		= DEMO38.PRG DEMOWEB.PRG DEMO39.PRG
GFX		= SPRITES.BIN TILESET.BIN FLOWER.BIN
MAP		= FG.BIN BG.BIN
BGM		= BGM.ZSM BGM38.ZSM BGM.BIN
ASSETS		= $(GFX) $(MAP) $(BGM)
TARGETS		= $(PRG) $(ASSETS)
PKG		= sonicdemo.zip


FLAGS		= -t cx16 -g

.PHONY:	all
all:
	@echo "no building done by default"

DEMO38.PRG: musictile.asm x16.inc
	cl65 $(FLAGS) -Ln demo38.sym --asm-define REV=38 -o $@ $<

DEMO39.PRG: musictile.asm x16.inc
	cl65 $(FLAGS) -Ln demo39.sym --asm-define REV=39 -o $@ $<

DEMOWEB.PRG: musictile.asm x16.inc
	cl65 $(FLAGS) --asm-define REV=38 --asm-define WEBEMU -o $@ $<

S1.BIN:	assets/sonicears.png
	$(IMG2TILESET) -n -b 4 -s 32x8 $< $@

S2.BIN: assets/sonicmain.png
	$(IMG2TILESET) -n -b 4 -s 32x32 $< $@

SPRITES.BIN: S1.BIN S2.BIN
	cat $(BINHEADER) S1.BIN S2.BIN > $@

FG.BIN: assets/fgmap.bin
	$(BGCONVERT) $< $@
	
BG.BIN: assets/bgmap.bin
	$(BGCONVERT) $< $@

TILESET.BIN: assets/tiles.png
	$(IMG2TILESET) -b 4 -s 8x8 $< $@

FLOWER.BIN: assets/flowerframes.png
	$(IMG2TILESET) -b 4 -s 8x8 $< $@

BGM.ZSM: assets/greenhillzone.vgm $(VGM2ZSM)
	$(VGM2ZSM) $< $@

BGM38.ZSM: assets/greenhillzone.vgm $(VGM2ZSM)
	$(VGM2ZSM) -4 $< $@

BGM.BIN: BGM38.ZSM
	cp $< $@

$(PKG): $(TARGETS)
	zip $@ $(TARGETS)

$(BINHEADER):
	@echo ERROR: $(BINHEADER) not found!

$(VGM2ZSM):
	@echo ERROR: Tool $(VGM2ZSM) not found!

run38: $(TARGETS)
	$(EMU38) -prg DEMO38.PRG -run -abufs 32

run39: $(TARGETS)
	$(EMU39) -prg DEMO39.PRG -run -abufs 32
	
runbox: $(TARGETS)
	$(BOX16) -prg DEMO39.PRG -run -sym demo39.sym

.PHONY: zip
zip: $(PKG)

.PHONY: assets
assets: $(ASSETS)

.PHONY: bgm
bgm: $(BGM)

.PHONY: prg
prg: $(PRG)

.PHONY: run
run: run39

clean:
	rm -f $(TARGETS)
	rm -f S1.BIN S2.BIN
	rm -f $(PKG)
