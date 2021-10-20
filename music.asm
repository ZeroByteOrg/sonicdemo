.include "x16.inc" ; include this one for R38
;.include "x16r39.inc" ; x16.inc by SlithyMatt

.export data

.struct SONGPTR
	addr		.word	1
	bank		.byte	1
.endstruct

databank	= 2
playerbank	= 1

.segment "ZEROPAGE"

data:	.tag	SONGPTR
delay:	.res	1
cmd:	.res	3

.org $080D
.segment "STARTUP"
.segment "INIT"
.segment "ONCE"
.segment "CODE"

   jmp start

loop_pointer:	.tag	SONGPTR

irq:
			jsr	playmusic
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
; Original hardwired loop of entire song (kept here for reference)
;			ldx #databank
;			lda #$a0
;			stz data + SONGPTR::addr
;			sta data + SONGPTR::addr + 1
;			stx data + SONGPTR::bank
;			stx RAM_BANK
;			jmp	nextnote

			; check if loop_ptr = 000
;			lda loop_pointer
;			ora	loop_pointer+1
;			ora loop_pointer+2
			lda loop_pointer + SONGPTR::bank
			cmp	#$FF	; no, check if loop bank = $FF instead
			bne :+
			jmp stopmusic
:			lda	loop_pointer + SONGPTR::addr
			sta	data + SONGPTR::addr
			lda	loop_pointer + SONGPTR::addr+1
			clc
			adc	#$a0
			sta	data + SONGPTR::addr+1
			lda loop_pointer + SONGPTR::bank
			clc
			adc	databank
			sta	data + SONGPTR::bank
			sta RAM_BANK
			jmp	nextnote
			
start:
			sei
			
			;  ==== load zsm file into memory ====

			; set BANKRAM to the first bank where song should load
			lda	#databank
			sta	RAM_BANK
			lda #bgname-filename
			ldx #<filename
			ldy #>filename
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

			lda #palname-bgname
			ldx #<bgname
			ldy #>bgname
			jsr SETNAM
			lda #0	; logical file id 0
			ldx	#8	; device 8
			ldy #0	; no command
			jsr	SETLFS
			; load song to $A000
			lda	#2		; 0=load, 1=verify, 2|3 = VLOAD to VRAM bank0/bank1
			ldx	#0
			ldy #$48
			jsr LOAD

			lda #filename_end-palname
			ldx #<palname
			ldy #>palname
			jsr SETNAM
			lda #0	; logical file id 0
			ldx	#8	; device 8
			ldy #0	; no command
			jsr	SETLFS
			; load song to $A000
			lda	#3		; 0=load, 1=verify, 2|3 = VLOAD to VRAM bank0/bank1
			ldx	#0
			ldy #$fa
			jsr LOAD

			lda #7
			sta $9f2d
			lda #$24
			sta $9f2f
			lda #$11
			sta $9f29
			lda #64
			sta $9f2a
			sta $9f2b
			
			; copy the loop pointer from the loaded file
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
			cli
			
			jsr init_player
			jsr startmusic
forever:	bra forever

.segment	"RODATA"
filename:	.byte "bgm.zsm"
bgname:		.byte "01.bin"
palname:	.byte "01.pal"
filename_end:

			

