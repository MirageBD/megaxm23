.define imgscreen		$a000	; size = 80*50*2 = $1f40
.define imgchars		$c000	; 40 * 64 = $0a00
;.define screen			$c000
.define imgdata			$10000  ; 320*200*3 = $2ee00
.define moddata			$40000

; ----------------------------------------------------------------------------------------------------

.segment "MAIN"

entry_main

		sei

		lda #$35
		sta $01

		lda #%10000000									; Clear bit 7 - HOTREG
		trb $d05d

		lda #$00										; unmap
		tax
		tay
		taz
		map
		eom

		lda #$41										; enable 40MHz
		sta $00

		lda #$47										; enable C65GS/VIC-IV IO registers
		sta $d02f
		lda #$53
		sta $d02f
		eom

		;lda #%10000000									; force PAL mode, because I can't be bothered with fixing it for NTSC
		;trb $d06f										; clear bit 7 for PAL ; trb $d06f 
		;tsb $d06f										; set bit 7 for NTSC  ; tsb $d06f

		lda #%11111000									; unmap c65 roms $d030 by clearing bits 3-7
		trb $d030
		lda #%00000100									; PAL - Use PALETTE ROM (0) or RAM (1) entries for colours 0 - 15
		tsb $d030

		;lda #$05										; enable Super-Extended Attribute Mode by asserting the FCLRHI and CHR16 signals - set bits 2 and 0 of $D054.
		;sta $d054

		;lda #%10100000									; CLEAR bit7=40 column, bit5=Enable extended attributes and 8 bit colour entries
		;trb $d031

		lda #80											; set to 80 for etherload
		sta $d05e

		lda #$7f										; disable CIA interrupts
		sta $dc0d
		sta $dd0d
		lda $dc0d
		lda $dd0d

		lda #$00										; disable IRQ raster interrupts because C65 uses raster interrupts in the ROM
		sta $d01a

		lda #$ff
		sta $d012
		lda #<fastload_irq_handler
		sta $fffe
		lda #>fastload_irq_handler
		sta $ffff

		lda #$01										; ACK
		sta $d01a

		cli

		jsr fl_init
		jsr fl_waiting
		FLOPPY_IFFL_FAST_LOAD_INIT "MXM23.IFFLCRCH"
		FLOPPY_IFFL_FAST_LOAD							; load MIM file
		FLOPPY_IFFL_FAST_LOAD							; load MOD file
		jsr fl_exit

		sei

		lda #$35
		sta $01

		lda #<.loword(moddata)
		sta adrPepMODL+0
		lda #>.loword(moddata)
		sta adrPepMODL+1
		lda #<.hiword(moddata)
		sta adrPepMODH+0
		lda #>.hiword(moddata)
		sta adrPepMODH+1

		jsr peppitoInit

		lda #$80
		sta $d020
		lda #$00
		sta $d021

		lda #$05										; enable Super-Extended Attribute Mode by asserting the FCLRHI and CHR16 signals - set bits 2 and 0 of $D054.
		sta $d054

		lda #%10100000									; CLEAR bit7=40 column, bit5=Enable extended attributes and 8 bit colour entries
		trb $d031

		lda #80											; set to 80 for etherload
		sta $d05e

		lda #$32										; pal screen start
		sta palntscscreenstart
		bit $d06f
		bpl :+
		lda #$1a										; ntsc screen start
		sta palntscscreenstart
:

		; WHY THE HELL DO I NEED TO FILL 2 PALETTES HERE???


		lda $d070										; BANK IN BITMAP PALETTE - select mapped bank with the upper 2 bits of $d070
		and #%00111111
		sta $d070

		ldx #$00										; set bitmap palette
:		txa
		sta $d100,x
		sta $d200,x
		sta $d300,x
		inx
		bne :-

		lda $d070
		and #%11001111									; clear bits 4 and 5 (BTPALSEL) so bitmap uses palette 0
		sta $d070





		lda $d070										; select mapped bank with the upper 2 bits of $d070
		and #%00111111
		ora #%10000000									; select palette 02
		sta $d070

		lda #$00
		ldx #$00										; set bitmap palette
:		sta $d100,x
		sta $d200,x
		sta $d300,x
		inx
		bne :-

		lda $d070										; select mapped bank with the upper 2 bits of $d070
		and #%00111111
		ora #%11000000									; select palette 03
		sta $d070

		lda #$00
		ldx #$00										; set bitmap palette
:		sta $d100,x
		sta $d200,x
		sta $d300,x
		inx
		bne :-

		lda $d070
		and #%11111100									; set alt palette to 2
		ora #%00000010
		sta $d070







		lda #<$0800										; set (offset!) pointer to colour ram
		sta $d064
		lda #>$0800
		sta $d065

		DMA_RUN_JOB imgrender_clearcolorramjob
		DMA_RUN_JOB imgrender_clearbitmapjob1
		DMA_RUN_JOB imgrender_clearbitmapjob2

		lda #<.loword(SAFE_COLOR_RAM_PLUS_ONE)
		sta uidraw_colptr+0
		lda #>.loword(SAFE_COLOR_RAM_PLUS_ONE)
		sta uidraw_colptr+1
		lda #<.hiword(SAFE_COLOR_RAM_PLUS_ONE)
		sta uidraw_colptr+2
		lda #>.hiword(SAFE_COLOR_RAM_PLUS_ONE)
		sta uidraw_colptr+3

		lda #0
		sta img_misccounter

img_fillsetaltpalbits

		ldz #30*2										; set columns 30-40 to use alt palette
		lda #%01101111
:		sta [uidraw_colptr],z
		inz
		inz
		cpz #40*2
		bne :-

		clc
		lda uidraw_colptr+0
		adc #80
		sta uidraw_colptr+0
		lda uidraw_colptr+1
		adc #0
		sta uidraw_colptr+1
		lda uidraw_colptr+2
		adc #0
		sta uidraw_colptr+2
		lda uidraw_colptr+3
		adc #0
		sta uidraw_colptr+3

		inc img_misccounter
		lda img_misccounter
		cmp #25
		bne img_fillsetaltpalbits


		; fill full colour char pattern ($f000)

		lda #<imgchars
		sta imgri1+1
		lda #>imgchars
		sta imgri1+2

		lda #$00
		sta img_misccounter

		ldx #$00
		ldy #$00
:		tya
		adc img_fcblock,x
imgri1	sta imgchars,x
		inx
		cpx #64
		bne :-

		clc
		lda imgri1+1
		adc #64
		sta imgri1+1
		lda imgri1+2
		adc #0
		sta imgri1+2
		clc
		tya
		adc #$08
		tay
		inc img_misccounter
		lda img_misccounter
		cmp #42
		bne :-

		; fill screen ($e000)

		lda #$00
		sta screencolumn
		sta screenrow

		lda #<(imgscreen+0)
		sta put0+1
		lda #>(imgscreen+0)
		sta put0+2
		lda #<(imgscreen+1)
		sta put1+1
		lda #>(imgscreen+1)
		sta put1+2

		; imgchars = $f000
		; imgchars/64 = $0800

putstart
		ldx screencolumn
		clc
		lda img_rowchars,x								; char to plot
		adc #<(imgchars/64)
put0	sta imgscreen+0									; plot left of 2 chars

		lda #>(imgchars/64)
put1	sta imgscreen+1									; plot right of 2 chars

		clc												; add 2 to screenpos low
		lda put0+1
		adc #02
		sta put0+1
		lda put0+2
		adc #0
		sta put0+2

		clc												; add 2 to screenpos high
		lda put1+1
		adc #02
		sta put1+1
		lda put1+2
		adc #0
		sta put1+2

		inc screencolumn								; increase screen column until 40
		lda screencolumn
		cmp #40
		bne putstart

		lda #0											; reset screencolumn to 0, increase row until 25
		sta screencolumn
		inc screenrow
		lda screenrow
		cmp #25
		beq endscreenplot

		jmp putstart

endscreenplot

		lda #40*2										; logical chars per row
		sta $d058
		lda #$00
		sta $d059

		lda #%00100000									; set H320, V200, ATTR
		sta $d031

		lda #$00
		sta $d016

		lda #$50										; set TEXTXPOS to same as SDBDRWDLSB
		lda $d04c
		lda #$42
		sta $d05c

		lda #$01
		sta $d05b										; Set display to V200
		lda #25
		sta $d07b										; Display 25 rows of text

		lda #<imgscreen									; set pointer to screen ram
		sta $d060
		lda #>imgscreen
		sta $d061
		lda #(imgscreen & $ff0000) >> 16
		sta $d062
		lda #$00
		sta $d063

		lda #$7f										; disable CIA interrupts
		sta $dc0d
		sta $dd0d
		lda $dc0d
		lda $dd0d

		lda #$00										; disable IRQ raster interrupts because C65 uses raster interrupts in the ROM
		sta $d01a

		lda #$ff										; setup IRQ interrupt
		sta $d012
		lda #<img_render_irq
		sta $fffe
		lda #>img_render_irq
		sta $ffff

		lda #$01										; ACK
		sta $d01a

		cli
		
loop
		;inc $d020
		lda $d020
		jmp loop


; ----------------------------------------------------------------------------------------------------------------------------------------

img_render_irq

		php
		pha
		phx
		phy
		phz

		ldx #$00

		lda redleftlo,x
		sta imgrucr+0
		lda redleftmid,x
		sta imgrucr+1
		lda redlefthi,x
		sta imgrucr+2

		lda greenleftlo,x
		sta imgrucg+0
		lda greenleftmid,x
		sta imgrucg+1
		lda greenlefthi,x
		sta imgrucg+2

		lda blueleftlo,x
		sta imgrucb+0
		lda blueleftmid,x
		sta imgrucb+1
		lda bluelefthi,x
		sta imgrucb+2

		clc
		lda palntscscreenstart
		adc #$01
:		cmp $d012
		bne :-

		lda #$00
		sta $d020

img_render_irq_loop

		;lda #$8c
		;sta $d020
		;inc $d020

		lda $d070										; BANK IN BITMAP PALETTE - select mapped bank with the upper 2 bits of $d070
		and #%00111111
		sta $d070

			sta $d707										; inline DMA copy
			.byte $00										; end of job options
			.byte $00										; copy
			.word 120										; count
imgrucr		.word $0000										; src
			.byte $00										; src bank and flags
			.word $d108										; dst
			.byte (($d108 >> 16) & $0f) | %10000000			; dst bank and flags
			.byte $00										; cmd hi
			.word $0000										; modulo, ignored

			sta $d707										; inline DMA copy
			.byte $00										; end of job options
			.byte $00										; copy
			.word 120										; count
imgrucg		.word $0000										; src
			.byte $00										; src bank and flags
			.word $d208										; dst
			.byte (($d208 >> 16) & $0f) | %10000000			; dst bank and flags
			.byte $00										; cmd hi
			.word $0000										; modulo, ignored

			sta $d707										; inline DMA copy
			.byte $00										; end of job options
			.byte $00										; copy
			.word 120										; count
imgrucb		.word $0000										; src
			.byte $00										; src bank and flags
			.word $d308										; dst
			.byte (($d308 >> 16) & $0f) | %10000000			; dst bank and flags
			.byte $00										; cmd hi
			.word $0000										; modulo, ignored

		;lda #$00
		;sta $d020

		ldy $d012
		iny

		inx

		lda redleftlo,x
		sta imgrucr+0
		lda redleftmid,x
		sta imgrucr+1
		lda redlefthi,x
		sta imgrucr+2

		lda greenleftlo,x
		sta imgrucg+0
		lda greenleftmid,x
		sta imgrucg+1
		lda greenlefthi,x
		sta imgrucg+2

		lda blueleftlo,x
		sta imgrucb+0
		lda blueleftmid,x
		sta imgrucb+1
		lda bluelefthi,x
		sta imgrucb+2

:		cpy $d012
		bne :-

		cpx #200
		beq :+
		jmp img_render_irq_loop

:		lda #$00
		sta $d020

		;jsr peppitoPlay
		
		jsr testdrawline

		lda palntscscreenstart
		sta $d012

		plz
		ply
		plx
		pla
		plp
		asl $d019
		rti

; ----------------------------------------------------------------------------------------------------------------------------------------

testdrawline

		lda #$f0
		sta $d020

		lda #$10
		sta $d020

		MATH_SET x0, $00100000
		MATH_SET x1, $00600000
		MATH_SET y0, $00200000
		MATH_SET y1, $00300000

		inc frame
		ldx frame

		lda sine,x
		lsr
		lsr
		clc
		adc #$01
		sta x0+2

		lda sine+$0040,x
		lsr
		lsr
		clc
		adc #$01
		sta y0+2

		lda sine,x
		lsr
		clc
		adc #$01
		sta x1+2

		lda sine+$0040,x
		lsr
		clc
		adc #$01
		sta y1+2

		jsr aaline_draw

		lda #$00
		sta $d020

		rts

frame
		.byte $00

; ----------------------------------------------------------------------------------------------------------------------------------------

imgrender_clearcolorramjob
				.byte $0a										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $80, $00									; source megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $81, (SAFE_COLOR_RAM) >> 20				; dest megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $84, $00									; Destination skip rate (256ths of bytes)
				.byte $85, $02									; Destination skip rate (whole bytes)

				.byte $00										; No more options

																; 12 byte DMA List structure starts here
				.byte %00000111									; Command LSB
																;     0–1 DMA Operation Type (Only Copy and Fill implemented at the time of writing)
																;             %00 = Copy
																;             %01 = Mix (via MINTERMs)
																;             %10 = Swap
																;             %11 = Fill
																;       2 Chain (i.e., another DMA list follows)
																;       3 Yield to interrupts
																;       4 MINTERM -SA,-DA bit
																;       5 MINTERM -SA, DA bit
																;       6 MINTERM  SA,-DA bit
																;       7 MINTERM  SA, DA bit

				.word 80*50										; Count LSB + Count MSB

				.byte %00000000										; this is normally the source addres, but contains the fill value now
				.byte 0
				.byte $00										; source bank (ignored)

				.word (SAFE_COLOR_RAM) & $ffff					; Destination Address LSB + Destination Address MSB
				.byte (((SAFE_COLOR_RAM) >> 16) & $0f)			; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.
				;.byte %00000000									; Command MSB

				.word $0000

				.byte $00										; No more options
				.byte %00000011									; Command LSB
																;     0–1 DMA Operation Type (Only Copy and Fill implemented at the time of writing)
																;             %00 = Copy
																;             %01 = Mix (via MINTERMs)
																;             %10 = Swap
																;             %11 = Fill
																;       2 Chain (i.e., another DMA list follows)
																;       3 Yield to interrupts
																;       4 MINTERM -SA,-DA bit
																;       5 MINTERM -SA, DA bit
																;       6 MINTERM  SA,-DA bit
																;       7 MINTERM  SA, DA bit

				.word 80*50										; Count LSB + Count MSB

				.word $000f										; ff = red = transparency. this is normally the source addres, but contains the fill value now
				.byte $00										; source bank (ignored)

				.word (SAFE_COLOR_RAM+1) & $ffff				; Destination Address LSB + Destination Address MSB
				.byte (((SAFE_COLOR_RAM+1) >> 16) & $0f)		; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.
				;.byte %00000000								; Command MSB

				.word $0000

; ----------------------------------------------------------------------------------------------------------------------------------------

imgrender_clearbitmapjob1
				.byte $0a										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $80, $00									; source megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $81, (imgdata) >> 20						; dest megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $84, $00									; Destination skip rate (256ths of bytes)
				.byte $85, $01									; Destination skip rate (whole bytes)

				.byte $00										; No more options

																; 12 byte DMA List structure starts here
				.byte %00000111									; Command LSB
																;     0–1 DMA Operation Type (Only Copy and Fill implemented at the time of writing)
																;             %00 = Copy
																;             %01 = Mix (via MINTERMs)
																;             %10 = Swap
																;             %11 = Fill
																;       2 Chain (i.e., another DMA list follows)
																;       3 Yield to interrupts
																;       4 MINTERM -SA,-DA bit
																;       5 MINTERM -SA, DA bit
																;       6 MINTERM  SA,-DA bit
																;       7 MINTERM  SA, DA bit

				.word 0 ; 320*200									; Count LSB + Count MSB

				.word $0000										; this is normally the source addres, but contains the fill value now
				.byte $00										; source bank (ignored)

				.word (imgdata) & $ffff							; Destination Address LSB + Destination Address MSB
				.byte (((imgdata) >> 16) & $0f)					; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.
				;.byte %00000000									; Command MSB

				.word $0000

; ----------------------------------------------------------------------------------------------------------------------------------------

imgrender_clearbitmapjob2
				.byte $0a										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $80, $00									; source megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $81, (imgdata+1*65536) >> 20				; dest megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $84, $00									; Destination skip rate (256ths of bytes)
				.byte $85, $01									; Destination skip rate (whole bytes)

				.byte $00										; No more options

																; 12 byte DMA List structure starts here
				.byte %00000111									; Command LSB
																;     0–1 DMA Operation Type (Only Copy and Fill implemented at the time of writing)
																;             %00 = Copy
																;             %01 = Mix (via MINTERMs)
																;             %10 = Swap
																;             %11 = Fill
																;       2 Chain (i.e., another DMA list follows)
																;       3 Yield to interrupts
																;       4 MINTERM -SA,-DA bit
																;       5 MINTERM -SA, DA bit
																;       6 MINTERM  SA,-DA bit
																;       7 MINTERM  SA, DA bit

				.word 0 ; 320*200									; Count LSB + Count MSB

				.word $0000										; this is normally the source addres, but contains the fill value now
				.byte $00										; source bank (ignored)

				.word (imgdata+1*65536) & $ffff					; Destination Address LSB + Destination Address MSB
				.byte (((imgdata+1*65536) >> 16) & $0f)			; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.
				;.byte %00000000									; Command MSB

				.word $0000


; ----------------------------------------------------------------------------------------------------------------------------------------

.align 256
redleftlo
		.repeat 200, I
			.byte <.loword((imgdata) + I*3*256 + 0*256)
		.endrepeat

.align 256
redleftmid
		.repeat 200, I
			.byte >.loword((imgdata) + I*3*256 + 0*256)
		.endrepeat

.align 256
redlefthi
		.repeat 200, I
			.byte <.hiword((imgdata) + I*3*256 + 0*256)
		.endrepeat

.align 256
greenleftlo
		.repeat 200, I
			.byte <.loword((imgdata) + I*3*256 + 1*256)
		.endrepeat

.align 256
greenleftmid
		.repeat 200, I
			.byte >.loword((imgdata) + I*3*256 + 1*256)
		.endrepeat

.align 256
greenlefthi
		.repeat 200, I
			.byte <.hiword((imgdata) + I*3*256 + 1*256)
		.endrepeat

.align 256
blueleftlo
		.repeat 200, I
			.byte <.loword((imgdata) + I*3*256 + 2*256)
		.endrepeat

.align 256
blueleftmid
		.repeat 200, I
			.byte >.loword((imgdata) + I*3*256 + 2*256)
		.endrepeat

.align 256
bluelefthi
		.repeat 200, I
			.byte <.hiword((imgdata) + I*3*256 + 2*256)
		.endrepeat

palntscscreenstart
		.byte $32

screencolumn	.byte 0
screenrow		.byte 0

img_fcblock
		.repeat 8
			.byte 0, 1, 2, 3, 4, 5, 6, 7
		.endrepeat

img_misccounter
		.byte 0

img_rowchars
		.byte 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,    1,2,3,4,5,6,7,8,9,10

; ----------------------------------------------------------------------------------------------------------------------------------------
