.macro SWAP this, that
		ldq this
		stq FP_A
		ldq that
		stq this
		ldq FP_A
		stq that
.endmacro

.macro SWAPNYBBLE
	asl
	adc #$80
	rol
	asl
	adc #$80
	rol  
.endmacro

.macro LINE_SET
.scope
		sta [uidraw_scrptr],z				; red values
		inc uidraw_scrptr+1
		bne :+
		inc uidraw_scrptr+2
:		sta [uidraw_scrptr],z				; green values
		inc uidraw_scrptr+1
		bne :+
		inc uidraw_scrptr+2
:		sta [uidraw_scrptr],z				; blue values
.endscope
.endmacro

.macro LINE_CALCGRADIENTS
		lda intersectY+1
		SWAPNYBBLE
		sta plotg1
		sec
		lda #$ff
		sbc plotg1
		sta plotg0
.endmacro

.macro LINE_STEEP1
		clc
		ldy intersectY+2
		tya
		adc times768_1,x
		sta uidraw_scrptr+0
		lda times768_2,x
		adc #$00
		sta uidraw_scrptr+1
		lda times768_3,x
		adc #$00
		sta uidraw_scrptr+2
		lda times768_4,x
		adc #$00
		sta uidraw_scrptr+3
.endmacro

.macro LINE_NONSTEEP1
		clc
		ldy intersectY+2
		txa
		adc times768_1,y
		sta uidraw_scrptr+0
		lda times768_2,y
		adc #$00
		sta uidraw_scrptr+1
		lda times768_3,y
		adc #$00
		sta uidraw_scrptr+2
		lda times768_4,y
		adc #$00
		sta uidraw_scrptr+3
.endmacro

.macro LINE_STEEP2
		clc
		dey
		tya
		adc times768_1,x
		sta uidraw_scrptr+0
		lda times768_2,x
		adc #$00
		sta uidraw_scrptr+1
		lda times768_3,x
		adc #$00
		sta uidraw_scrptr+2
		lda times768_4,x
		adc #$00
		sta uidraw_scrptr+3
.endmacro

.macro LINE_NONSTEEP2
		clc
		dey
		txa
		adc times768_1,y
		sta uidraw_scrptr+0
		lda times768_2,y
		adc #$00
		sta uidraw_scrptr+1
		lda times768_3,y
		adc #$00
		sta uidraw_scrptr+2
		lda times768_4,y
		adc #$00
		sta uidraw_scrptr+3
.endmacro

steep			.byte $00
plotg0			.byte $00				; plot gradient
plotg1			.byte $00				; plot gradient inverse

aaline_setup

		; swap points if needed
		MATH_SUB y1, y0, ytemp
		MATH_SUB x1, x0, xtemp
		MATH_ABS ytemp, ytemp
		MATH_ABS xtemp, xtemp

		lda #$00
		sta steep

		ldq ytemp
		cmpq xtemp
		bmi :+
		lda #$01
		sta steep
		SWAP x0, y0
		SWAP x1, y1

:		ldq x0
		cmpq x1
		bmi :+
		SWAP x0, x1
		SWAP y0, y1

:		MATH_SUB x1, x0, xtemp
		MATH_SUB y1, y0, ytemp
		MATH_DIV ytemp, xtemp, gradient

		MATH_MOV x0, xstart
		MATH_MOV x1, xend
		MATH_MOV y0, intersectY

		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

aaline_draw

		lda steep
		beq :+
		jmp aaline_drawsteep
:		jmp aaline_drawnonsteep

; ----------------------------------------------------------------------------------------------------------------------------------------

aaline_clear

		lda steep
		beq :+
		jmp aaline_clearsteep
:		jmp aaline_clearnonsteep

; ----------------------------------------------------------------------------------------------------------------------------------------

aaline_drawsteep

		ldx x0+2

aaline_drawsteep_loop
		phx

		LINE_CALCGRADIENTS
		LINE_STEEP1

		ldz #$00
		lda plotg1
		LINE_SET

		LINE_STEEP2

		;ldz #$00
		lda plotg0
		LINE_SET

		MATH_ADD intersectY, gradient, intersectY

		plx
		inx
		cpx x1+2
		beq :+
		jmp aaline_drawsteep_loop

:		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

aaline_drawnonsteep
		ldx x0+2

aaline_drawnonsteep_loop
		phx

		LINE_CALCGRADIENTS
		LINE_NONSTEEP1

		ldz #$00
		lda plotg1
		LINE_SET

		LINE_NONSTEEP2

		;ldz #$00
		lda plotg0
		LINE_SET

		MATH_ADD intersectY, gradient, intersectY

		plx
		inx
		cpx x1+2
		beq :+
		jmp aaline_drawnonsteep_loop

:		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

aaline_clearsteep

		ldx x0+2

aaline_clearsteep_loop
		phx

		LINE_CALCGRADIENTS
		LINE_STEEP1

		ldz #$00
		lda #$00	; clear
		LINE_SET

		LINE_STEEP2

		;ldz #$00
		lda #$00	; clear
		LINE_SET

		MATH_ADD intersectY, gradient, intersectY

		plx
		inx
		cpx x1+2
		beq :+
		jmp aaline_clearsteep_loop

:		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

aaline_clearnonsteep
		ldx x0+2

aaline_clearnonsteep_loop
		phx

		LINE_CALCGRADIENTS
		LINE_NONSTEEP1

		ldz #$00
		lda #$00	; clear
		LINE_SET

		LINE_NONSTEEP2

		;ldz #$00
		lda #$00	; clear
		LINE_SET

		MATH_ADD intersectY, gradient, intersectY

		plx
		inx
		cpx x1+2
		beq :+
		jmp aaline_clearnonsteep_loop

:		rts

; ----------------------------------------------------------------------------------------------------------------------------------------

.align 256

times768_1
	.repeat 256,I
		.byte <.loword(imgdata+I*3*256)
	.endrepeat

times768_2
	.repeat 256,I
		.byte >.loword(imgdata+I*3*256)
	.endrepeat

times768_3
	.repeat 256,I
		.byte <.hiword(imgdata+I*3*256)
	.endrepeat

times768_4
	.repeat 256,I
		.byte >.hiword(imgdata+I*3*256)
	.endrepeat

sine:

.byte 255, 254, 254, 254, 254, 254, 253, 253, 252, 251, 251, 250, 249, 248, 247, 246, 245, 244, 242, 241, 240, 238, 236, 235, 233, 231, 230, 228, 226, 224, 222, 219
.byte 217, 215, 213, 210, 208, 206, 203, 201, 198, 195, 193, 190, 187, 185, 182, 179, 176, 173, 170, 167, 164, 161, 158, 155, 152, 149, 146, 143, 140, 137, 134, 131
.byte 128, 124, 121, 118, 115, 112, 109, 106, 103, 100, 097, 094, 091, 088, 085, 082, 079, 076, 073, 070, 068, 065, 062, 060, 057, 054, 052, 049, 047, 045, 042, 040
.byte 038, 036, 033, 031, 029, 027, 025, 024, 022, 020, 019, 017, 015, 014, 013, 011, 010, 009, 008, 007, 006, 005, 004, 004, 003, 002, 002, 001, 001, 001, 001, 001
.byte 001, 001, 001, 001, 001, 001, 002, 002, 003, 004, 004, 005, 006, 007, 008, 009, 010, 011, 013, 014, 015, 017, 019, 020, 022, 024, 025, 027, 029, 031, 033, 036
.byte 038, 040, 042, 045, 047, 049, 052, 054, 057, 060, 062, 065, 068, 070, 073, 076, 079, 082, 085, 088, 091, 094, 097, 100, 103, 106, 109, 112, 115, 118, 121, 124
.byte 127, 131, 134, 137, 140, 143, 146, 149, 152, 155, 158, 161, 164, 167, 170, 173, 176, 179, 182, 185, 187, 190, 193, 195, 198, 201, 203, 206, 208, 210, 213, 215
.byte 217, 219, 222, 224, 226, 228, 230, 231, 233, 235, 236, 238, 240, 241, 242, 244, 245, 246, 247, 248, 249, 250, 251, 251, 252, 253, 253, 254, 254, 254, 254, 254

.byte 254, 254, 254, 254, 254, 254, 253, 253, 252, 251, 251, 250, 249, 248, 247, 246, 245, 244, 242, 241, 240, 238, 236, 235, 233, 231, 230, 228, 226, 224, 222, 219
.byte 217, 215, 213, 210, 208, 206, 203, 201, 198, 195, 193, 190, 187, 185, 182, 179, 176, 173, 170, 167, 164, 161, 158, 155, 152, 149, 146, 143, 140, 137, 134, 131
.byte 128, 124, 121, 118, 115, 112, 109, 106, 103, 100, 097, 094, 091, 088, 085, 082, 079, 076, 073, 070, 068, 065, 062, 060, 057, 054, 052, 049, 047, 045, 042, 040
.byte 038, 036, 033, 031, 029, 027, 025, 024, 022, 020, 019, 017, 015, 014, 013, 011, 010, 009, 008, 007, 006, 005, 004, 004, 003, 002, 002, 001, 001, 001, 001, 001
.byte 001, 001, 001, 001, 001, 001, 002, 002, 003, 004, 004, 005, 006, 007, 008, 009, 010, 011, 013, 014, 015, 017, 019, 020, 022, 024, 025, 027, 029, 031, 033, 036
.byte 038, 040, 042, 045, 047, 049, 052, 054, 057, 060, 062, 065, 068, 070, 073, 076, 079, 082, 085, 088, 091, 094, 097, 100, 103, 106, 109, 112, 115, 118, 121, 124
.byte 127, 131, 134, 137, 140, 143, 146, 149, 152, 155, 158, 161, 164, 167, 170, 173, 176, 179, 182, 185, 187, 190, 193, 195, 198, 201, 203, 206, 208, 210, 213, 215
.byte 217, 219, 222, 224, 226, 228, 230, 231, 233, 235, 236, 238, 240, 241, 242, 244, 245, 246, 247, 248, 249, 250, 251, 251, 252, 253, 253, 254, 254, 254, 254, 254
