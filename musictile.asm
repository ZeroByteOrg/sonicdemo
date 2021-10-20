.include "x16.inc" ; x16.inc by SlithyMatt

.export data

.struct SONGPTR
	addr		.word	1
	bank		.byte	1
.endstruct

.struct SPRITEREG
	addr		.word	1
	xpos		.word	1
	ypos		.word	1
	orient		.byte	1
	attr		.byte	1
.endstruct


databank	= 2
playerbank	= 1
waterpalette	= $1FA50
watercolors 	= palette + ($28 << 1)
tilebase		= $8000
flowervram		= $eb80
sonic_speed		= $64
;sonic_speed		= $10
sonic_x			= 100
sonic_y			= 152
num_raster_irqs = 16

.segment "ZEROPAGE"

data:		.tag	SONGPTR
delay:		.res	1
cmd:		.res	1
frame:		.res	1
water:		.res	1
flower: 	.res	1
sonicframe:	.res	1
dirty:		.res	1	; "dirty" elements for VBLANK to handle
FOO:		.res	1

; flag bits of the dirty register
DIRTY_SCROLL	=	1
DIRTY_SPRITE	=	1 << 1
DIRTY_TILE		=	1 << 2
DIRTY_PALETTE	=	1 << 4
DIRTY_FRAME		=   1 << 7	; if this flag is 0, then VBLANK skips all items


.org $080D
.segment "STARTUP"
.segment "INIT"
.segment "ONCE"

.segment "CODE"

   jmp start

fgscroll_frac:	.res	1
fgscroll:		.res	2
bgscroll_frac:	.res	1
bgscroll:		.res	2
speed:			.res	1	; high nybble = pixel, low nybble = subpixel
line:			.res	1	; index of the line IRQ cycle

loop_pointer:	.tag	SONGPTR

parallax_lo:
		.res	(num_raster_irqs) + 1
parallax_hi:
		.res	(num_raster_irqs) + 1



do_kernal:
			jmp	(kernal_irq)

irq:
			lda VERA_isr
			sta VERA_isr  ;	acknowledge all VERA IRQ flags
			lsr
			bcs	vblank
lineirq:
			ldx	line
			; update h_scroll
			lda parallax_lo,x
			sta VERA_L0_hscroll_l
			lda parallax_hi,x
			sta VERA_L0_hscroll_h

			; update raster line pointer
			inx
			cpx #num_raster_irqs
			bne	:+
			ldx #0
:			stx line

			; set the next raster IRQ line in VERA
			lda rasterlines_lo,x
			sta	VERA_irqline_l
			lda rasterlines_hi,x
			ror					; check LSB = 1 or not...
			bcs	:+
			lda #$03
			bra :++
:			lda #$83
:			sta VERA_ien

			ply
			plx
			pla
			rti
			
vblank:
			lda	dirty
			bpl	do_kernal
VBLANK_doscroll:
			lsr
			bcc VBLANK_dopalette
			ldx fgscroll+1
			stx VERA_L1_hscroll_h
			ldx fgscroll
			stx VERA_L1_hscroll_l
			ldx bgscroll+1
			stx VERA_L0_hscroll_h
			ldx bgscroll
			stx VERA_L0_hscroll_l
			; as this is just a demo, no need to truly stick to
			; the normal "move data during vblank" rules, as we KNOW
			; the palette and sprites can be updated in time by the main
			; loop - leaving this here as "reference" for in case I
			; decide to move this functionality into VBLANK as it 'should' be...
VBLANK_dopalette:
			lsr
			bcc VBLANK_dotiles
VBLANK_dotiles:
			lsr
			bcc VBLANK_dosprites
VBLANK_dosprites:		
			lsr
			bcc VBLANK_done
			;jsr draw_sonic
			
VBLANK_done:
			stz dirty
			jmp	(kernal_irq)

kernal_irq:	.word	$ffff

init_player:
			; hard-wired to play from $a000 and loop full tune for now.
			lda #1 ; start delay = 1
			sta delay
init_dataptr:
			ldx #databank
			lda #3
			sta data + SONGPTR::addr
			lda #$a0
			sta data + SONGPTR::addr + 1
			stx data + SONGPTR::bank
			rts

startmusic:
			rts
stopmusic:
			stz	delay
			rts

nextdata:
			; advance the data pointer, with bank-wrap if necessary
			inc	data
			beq	:+
			rts
			; next page
:			lda data+1
			inc
			cmp	#$c0		; Check for bank wrap.
			bcc @nobankwrap
			; bank wrapped.
			lda #$a0		; return to page $a000
			inc RAM_BANK	; bank in the next RAM bank
			inc data + SONGPTR::bank
			
			; TODO: Make this a cpx w/ actual # of avail banks.
			;       (don't assume 2MB of HIRAM installed)
			beq	@die		; out-of-memory error
@nobankwrap:
			sta	data+1	
			rts
@die:
			; stop the music and return error (carry bit = 1)
			jsr stopmusic
			lda #1
			ror
			rts

delayframe:
			jsr nextdata
			lda (data)
			bne :+
			inc				; if delay=0, set delay=1
:			sta delay
			jsr nextdata
noop:
			rts
playmusic:
			; first check the delay. 0 = not playing.
			lda delay
			beq noop
			; delay >0. Decrement, and if now 0, then play, else exit.
			dec	
			sta delay
			bne noop
			; bank in the song data
			lda data + SONGPTR::bank
			sta RAM_BANK
			; point VERA to PSG page
			lda #$01		; bank 1, 0 stride
			sta VERA_addr_bank
			lda #$f9		; PSG are on page $F9 of VRAM
			sta VERA_addr_high
nextnote:	; data->next command in song data.
			; Load next command and advance the pointer.
			lda (data)
			beq delayframe	; cmd 0 = delay frame.
			bmi loopsong	; cmd $80-$FF = end of data
			; cmd is not control-related, so load cmd + 2 bytes
			sta cmd			; tmp store cmd in ZP
			jsr nextdata
			; get reg / val for chip write - hold in X and Y
			lda (data)
			tax
			jsr nextdata
			lda (data)
			tay
			jsr nextdata
			; check cmd to see which sound chip should be updated
			lda cmd
			cmp #2		; 2 = PSG note
			beq	playPSG
			cmp #1		; 1 = FM note
			bne nextnote	; skip non-supported commands
playFM:
			bit YM_data
			bmi	playFM		; wait for YM busy flag to be clear
			stx	YM_reg
			nop
			sty	YM_data
			bra	nextnote
playPSG:
			txa				; for PSG, move "reg" value into A...
			clc
			adc #$c0		; ...to offset it properly into VRAM location
			sta VERA_addr_low
			sty VERA_data0
			bra nextnote
			
loopsong:
;			; check if loop_ptr = 000
;			lda loop_pointer
;			ora	loop_pointer+1
;			ora loop_pointer+2
			; check if loop_banks = $FF
			lda loop_pointer + SONGPTR::bank
			cmp #$FF
			bne :+
			jmp stopmusic
:			lda	loop_pointer + SONGPTR::addr
			sta	data + SONGPTR::addr
			lda	loop_pointer + SONGPTR::addr+1
			clc
			adc	#$a0
			sta	data + SONGPTR::addr+1
			lda loop_pointer + SONGPTR::bank
;			clc
;			adc	#databank
			sta	data + SONGPTR::bank
			sta RAM_BANK
			jmp	nextnote

rotate_palette:
			VERA_SET_ADDR	waterpalette, 1
			ldy #3
			lda water
			dec
			and	#3			; clamp water index to 0-3
			sta water
			asl				; index *2 because colors use 2 bytes each
			tax
nextcolor:					; transfer 8 bytes into the water animation palette
			inx
			inx
			cpx #8
			bcc :+
			ldx #0
:			lda watercolors,x
			sta VERA_data0
			lda watercolors+1,x
			sta VERA_data0
			dey
			bpl nextcolor
			rts
			
rotate_flower:
			VERA_SET_ADDR	flowervram, 1
			lda flower
			eor #1
			sta flower
			tax
			lda flowertable,x
			sta ZP_PTR_1
			lda flowertable+2,x
			sta ZP_PTR_1+1
			ldx #2
			ldy #0
:			lda (ZP_PTR_1),y
			sta VERA_data0
			iny
			bne :-
			dex
			beq :+
			inc ZP_PTR_1+1	; next page
			bra :-
:			rts

animate_sonic:
			lda sonicframe
			inc
			and #3
			sta sonicframe
			asl
			tax
			lda	sonic_frames,x
			sta sonic_spriteregs + SPRITEREG::addr
			lda sonic_frames + 8,x
			sta sonic_spriteregs + 8 + SPRITEREG::addr
			lda sonic_frames+1,x
			sta sonic_spriteregs + 1 + SPRITEREG::addr
			lda sonic_frames+9,x
			sta sonic_spriteregs + 9 + SPRITEREG::addr
			lda	dirty
			ora	#DIRTY_SPRITE
			sta dirty	
			rts

height_table:
			.word	77,		81		; tile ids of highest ground
			.word	192,	125		; tile ids of next-highest
			.word	206,	496		; ... etc.
			.word	89,		488
			.word 	104

y_offsets:
			.byte	2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0
			.byte	0,0,0,0,2,2,4,4,6,8,8,6,4,4,2,2

move_sonic:
			; cheesey method: use above table of Y offsets. As the ground
			; is just a 32-tile repeating pattern, the table stores the dips
			; divide h_scroll by 8 to get the column. Presto!
			lda	fgscroll
			lsr
			lsr
			lsr
			tax
			lda	y_offsets,x
			clc
			adc	#<sonic_y
			sta	sonic_spriteregs + 12 ; store to Y of sonic's ears
			adc #8
			sta sonic_spriteregs + 4  ; store to Y of sonic's body
			rts

old_move_sonic:
			; get the tile ID at Sonic's position.
			; adjust Sonic's Y coordinate accordingly.
			
			; check tile at column = 14 ; row = 50
			tile_row	:= 25 * 64 * 2 ; row 25 VRAM location = row * 2bytes/column, 64 columns...
			
			; formula to get the column = (sonic_x + h_scroll) / 8
			; we only need the low byte, so only storing that portion of the result in vram_loc
			tile_column	= r0	; use kernal register 0 for vram_loc tmp variable
			lda #<sonic_x
			clc
			adc fgscroll
			adc #16
			sta	tile_column
			lda #>sonic_x
			adc fgscroll+1
			lsr
			ror tile_column
			lsr
			ror tile_column
			lsr
			ror tile_column
			
			VERA_SET_ADDR	tile_row,1
			lda tile_column
			and	#$3f	; limit column number to 0..63
; testing:
			sta FOO
; -----
			asl
			sta VERA_addr_low
; testing:
;			stz VERA_data0
;			stz VERA_data0
;			sta VERA_addr_low
; --------
			lda VERA_data0			; get tile ID at location
			tax						; stash it in X
			lda VERA_data0			; get tile ATTR+ID_Hi at location
			and	#$03				; mask off the ATTR bits
			tay						; stash in Y
			txa
			ldx #20
			; at this point: A = tile_id_lo, Y = tile_id_hi, X = table index
@next_id:
			dex
			dex
			bmi	@done ; potential bug- what if the tile ID isn't found? Shouldn't happen tho...
			cmp	height_table,x
			bne	@next_id
			pha
			tya
			cmp	height_table+1,x
			beq	@done
			tay
			pla
			bra @next_id
@done:
			txa	; it's a happy coincidence that X was actually 2x, and we need 2x anyway...
			clc
			adc	sonic_y
			sta	sonic_spriteregs + 12 ; store to Sonic's ears sprite
			adc	#8
			sta sonic_spriteregs + 4  ; store to Sonic's body sprite
			rts

draw_sonic:
			VERA_SET_ADDR	(VRAM_sprattr + 15), -1
			ldy #15
:			lda sonic_spriteregs,y
			sta VERA_data0
			dey
			bpl :-
			rts
			
do_scroll:
			stz ZP_PTR_1
			lda	speed
			; store the high nybble of speed into low nybble of ZP temp variable
			asl
			rol	ZP_PTR_1
			asl
			rol	ZP_PTR_1
			asl
			rol	ZP_PTR_1
			asl
			rol	ZP_PTR_1
			; low nybble of speed is now 4 MSB of A. Add to scroll_frac
			clc
			adc	fgscroll_frac
			sta fgscroll_frac
			sta bgscroll_frac	; go ahead and put this in bgscroll frac bits before /8 operation
			lda fgscroll
			adc ZP_PTR_1	; add the per-pixel amount of speed to the scroll
			sta fgscroll
			sta bgscroll
			lda fgscroll+1
			adc #0
			sta fgscroll+1
			; now divide scroll by 8 for BG scroll
			lsr
			ror bgscroll
			ror bgscroll_frac
			lsr
			ror bgscroll
			ror bgscroll_frac
			lsr
			ror bgscroll
			ror bgscroll_frac
			sta bgscroll+1
			
			; now create the raster line scroll table - inc bgscroll by RASTER_AMT each line
			ldx #0
			delta = r0 ; rename it to something sensible for this algo. for readability
			tmpscroll = r2 ; same here
			;store current BG scroll in tmpscroll, and BG scroll / 2 in delta
			;frac = low byte, scroll_lo = mid byte, scroll_hi = hi byte
			lda bgscroll+1
			sta tmpscroll+2
			lsr
			sta delta+2
			lda bgscroll
			sta tmpscroll+1
			ror
			sta delta+1
			lda bgscroll_frac
			sta tmpscroll
			ror
			sta delta
:			lda tmpscroll
			clc
			adc delta
			sta tmpscroll
			lda tmpscroll+1
			adc delta+1
			sta parallax_lo,x
			sta tmpscroll+1
			lda tmpscroll+2
			adc delta+2
			sta parallax_hi,x
			sta tmpscroll+2
			inx
			cpx #num_raster_irqs
			bcc :-
			
			lda dirty
			ora #DIRTY_SCROLL
			sta dirty
			rts

start:
			sei
			
			; disable video while the data loads
			stz VERA_dc_video
			
			;  ==== load zsm music file into memory ====

			; set BANKRAM to the first bank where song should load
			lda	#databank
			sta	RAM_BANK
			lda #songname_end - songname
			ldx #<songname
			ldy #>songname
			jsr SETNAM
			lda #0	; logical file id 0
			ldx	#8	; device 8
			ldy #0	; no command
			jsr	SETLFS
			; load song to $A000
			lda	#0		; 0=load, 1=verify, 2|3 = VLOAD to VRAM bank0/bank1
			ldx	#0
			ldy #$a0
			jsr LOAD

			;load the background tilemap data
			lda #bgname_end-bgname
			ldx #<bgname
			ldy #>bgname
			jsr SETNAM
			lda #0	; logical file id 0
			ldx	#8	; device 8
			ldy #0	; no command
			jsr	SETLFS
			; load BG to $4800 in VRAM
			lda	#2		; 0=load, 1=verify, 2|3 = VLOAD to VRAM bank0/bank1
			ldx	#0
			ldy #$48
			jsr LOAD

			;load the foreground tilemap data
			lda #fgname_end-fgname
			ldx #<fgname
			ldy #>fgname
			jsr SETNAM
			lda #0	; logical file id 0
			ldx	#8	; device 8
			ldy #0	; no command
			jsr	SETLFS
			; load FG to $0000 in VRAM
			lda	#2		; 0=load, 1=verify, 2|3 = VLOAD to VRAM bank0/bank1
			ldx	#0
			ldy #0
			jsr LOAD
			
			;load the tileset
			lda #tilesetname_end-tilesetname
			ldx #<tilesetname
			ldy #>tilesetname
			jsr SETNAM
			lda #0	; logical file id 0
			ldx	#8	; device 8
			ldy #0	; no command
			jsr	SETLFS
			; load tileset to $08000 in VRAM
			lda	#2		; 0=load, 1=verify, 2|3 = VLOAD to VRAM bank0/bank1
			ldx	#0
			ldy #$80
			jsr LOAD

			;load the sprites
			lda #spritesname_end-spritesname
			ldx #<spritesname
			ldy #>spritesname
			jsr SETNAM
			lda #0	; logical file id 0
			ldx	#8	; device 8
			ldy #0	; no command
			jsr	SETLFS
			; load sprites to $10000 in VRAM
			lda	#3		; 0=load, 1=verify, 2|3 = VLOAD to VRAM bank0/bank1
			ldx	#0
			ldy #0
			jsr LOAD

			;load the animated flower tiles
			lda #flowername_end-flowername
			ldx #<flowername
			ldy #>flowername
			jsr SETNAM
			lda #0	; logical file id 0
			ldx	#8	; device 8
			ldy #0	; no command
			jsr	SETLFS
			; load tileset into RAM at flowerframes:
			lda	#0		; 0=load, 1=verify, 2|3 = VLOAD to VRAM bank0/bank1
			ldx	#<flowerframes
			ldy #>flowerframes
			jsr LOAD

			; load the palette into VRAM
			palette_size = palette_end - palette
			ldx #palette_size
			VERA_SET_ADDR	($1FA00 + palette_size - 1), -1
:			lda	palette-1,x
			sta VERA_data0
			dex
			bne :-

			; configure VERA layers
			ldy #13 ; there are 7 registers per layer
:			lda	layer0_cfg,y
			sta	VERA_L0_config,y
			dey
			bpl :-
			
			; configure the Sonic sprites
			jsr draw_sonic
			
			; copy the music loop pointer from the loaded file
			lda #databank
			sta RAM_BANK
			lda $a000
			sta loop_pointer + SONGPTR::addr
			lda $a001
			sta	loop_pointer + SONGPTR::addr + 1
			lda	$a002
			clc
			adc	#databank
			sta loop_pointer + SONGPTR::bank
			
			; save the current IRQ vector so player can call it when done
			lda IRQVec
			sta kernal_irq
			lda IRQVec+1
			sta kernal_irq+1
			; install player as the IRQ handler
			lda #<irq
			sta IRQVec
			lda #>irq
			sta IRQVec+1

			; configure VERA line IRQ with first entry from rasterlines table
			lda rasterlines_lo
			sta VERA_irqline_l
			lda rasterlines_hi
			ror
			ror
			and #$80
			ora #3
			sta VERA_ien

			; initialize variables
			lda #sonic_speed
			sta speed
			stz	dirty
			stz water
			stz flower
			stz sonicframe
			lda #1
			sta line
			stz line

			; configure VERA display mode
			lda #$40				; set resolution = 320x240
			sta VERA_dc_hscale
			sta VERA_dc_vscale
			lda #$71				; Sprite,L1,L0 enabled, outmode = VGA
;			lda #$51				; Sprite,  ,L0 enabled, outmode = VGA
			sta VERA_dc_video

			cli
			
			jsr init_player
			jsr startmusic
			
mainloop:
			bit	dirty
			bmi	mainloop
			inc frame
			jsr do_scroll
;			inc	bgscroll
;			bne	:+
;			inc bgscroll+1

			lda frame
			and #$0f
			bne :+
			jsr rotate_flower
:			and #$03
			bne	:+
			jsr rotate_palette
			jsr animate_sonic
			jsr move_sonic
			jsr draw_sonic
:
.ifndef NOSOUND
			jsr playmusic
.endif
			lda dirty
			ora #$80
			sta dirty
			bra mainloop
			
flowertable:
			.byte <flower1, <flower2, >flower1, >flower2

sonic_spriteregs:
;		.byte $10, $08, $64, $00, $a0, $00, $0c, $a0 ; sonic's body
;		.byte $00, $08, $64, $00, $98, $00, $0c, $20 ; sonic's ears
		.byte $10, $08, <sonic_x, >sonic_x, <(sonic_y+8), >(sonic_y+8), $0c, $a0 ; sonic's body
		.byte $00, $08, <sonic_x, >sonic_x, <sonic_y, >sonic_y, $0c, $20 ; sonic's ears
		

flowerframes:
flower1:	.res 512
flower2:	.res 512


			
.segment	"RODATA"
.ifdef WEBEMU
		songname:		.byte "bgm.bin"
.else
	.if VERSION = 38
		songname:		.byte "bgm38.zsm"
	.else
		songname:		.byte "bgm.zsm"
	.endif
.endif
songname_end:

bgname:			.byte "bg.bin"
bgname_end:

fgname:			.byte "fg.bin"
fgname_end:

tilesetname:	.byte "tileset.bin"
tilesetname_end:

flowername:		.byte "flower.bin"
flowername_end:

spritesname:	.byte "sprites.bin"
spritesname_end:

layer0_cfg:
		.byte $12, $24, $40, $00, $00, $00, $00
		
layer1_cfg:
		.byte $12, $00, $40, $00, $00, $00, $00

palette:
		.word $09f, $000, $229, $44b, $66d, $99f, $fff, $bbb
		.word $999, $444, $fb9, $b64, $f00, $900, $400, $ff0
		
		.word $009, $000, $242, $464, $696, $9d9, $fff, $bbb
		.word $999, $444, $bf9, $b64, $ff0, $990, $440, $f00
		
		.word $09f, $200, $fff, $620, $940, $d60, $f90, $fd0
		.word $69b, $69f, $9bf, $bdf, $040, $060, $4b0, $9f0
		
		.word $29d, $20b, $24d, $69f, $bdf, $dff, $fff, $dbf
		.word $b9f, $96f, $9f0, $4b0, $200, $620, $d60, $fd0
		
		.word $000, $000, $fff, $eac, $97d, $18c, $6ad, $9ef
		.word $5cb, $3c8, $ac5, $fc5, $fb5, $f85, $f64, $f54
palette_end:


sonic_frames:
		.byte $10, $08, $20, $08, $30, $08, $40, $08
		.byte $00, $08, $04, $08, $00, $08, $04, $08

RASTER0 = 224
RASTER1 = 306
RASTER2 = 310
RASTER3 = 318
RASTER4 = 330
RASTER5 = 338
RASTER6 = 350
RASTER7 = 358
RASTER8 = 370
RASTER9 = 378
RASTER10 = 390
RASTER11 = 398
RASTER12 = 410
RASTER13 = 418
RASTER14 = 430
RASTER15 = 438

rasterlines_lo:
		.byte	<RASTER0, <RASTER1, <RASTER2, <RASTER3, <RASTER4
		.byte	<RASTER5, <RASTER6, <RASTER7, <RASTER8, <RASTER9
		.byte	<RASTER10, <RASTER11, <RASTER12, <RASTER13
		.byte	<RASTER14, <RASTER15
rasterlines_hi:
		.byte	>RASTER0, >RASTER1, >RASTER2, >RASTER3, >RASTER4
		.byte	>RASTER5, >RASTER6, >RASTER7, >RASTER8, >RASTER9
		.byte	>RASTER10, >RASTER11, >RASTER12, >RASTER13
		.byte	>RASTER14, >RASTER15
