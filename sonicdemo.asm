;.include "x16.inc" ; include this one for R38
.include "x16r39.inc" ; x16.inc by SlithyMatt

.segment "ZEROPAGE"

.org $080D
.segment "STARTUP"
.segment "INIT"
.segment "ONCE"
.segment "CODE"

   jmp start

irq_return:
			ply
			plx
			pla
			cli
			rti
irq:
			lda VERA_isr
			and #$03		; check 2 LSB (1=line IRQ, 0=vsync IRQ)
			lsr
			bcs	irq_vsync
			beq irq_return	; this shouldn't happen as we're not enabling other IRQ sources
irq_line:
			lda #$02		; acknowledge the line IRQ
			sta VERA_isr
			
			; do some stuff to do the super parallax effect
			jmp irq_return
			
irq_vsync:
			; reset the BG scroll value here
			; ...
			
			jsr	playmusic	; probably move this to somewhere during the display area....
			jmp	(kernal_irq)

kernal_irq:	.word	$ffff
			
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

			; configure VERA
			lda #7
			sta $9f2d
			lda #$24
			sta $9f2f
			lda #$11
			sta $9f29
			lda #64
			sta $9f2a
			sta $9f2b
			
			; save the current IRQ vector so player can call it when done
			lda IRQVec
			sta kernal_irq
			lda IRQVec+1
			sta kernal_irq+1
			; install the IRQ handler
			lda #<irq
			sta IRQVec
			lda #>irq
			sta IRQVec+1
			cli
			
			jsr init_player
			jsr startmusic
forever:	bra forever

load:		

.segment	"RODATA"
filename:	.byte "bgm.zsm"
bgname:		.byte "01.bin"
palname:	.byte "01.pal"
filename_end:

			

