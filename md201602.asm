;
; MD201602
;

; Code and graphics by T.M.R/Cosine
; Music by Sack/Cosine


; This source code is formatted for the ACME cross assembler from
; http://sourceforge.net/projects/acme-crossass/
; Compression is handled with PuCrunch which can be downloaded at
; http://csdb.dk/release/?id=6089

; build.bat will call both to create an assembled file and then the
; crunched release version.


; Select an output filename
		!to "md201602.prg",cbm


; Yank in binary data
		* = $0900
music		!binary "data/here_comes_your_man.raw"

		* = $1c00
		!binary "data/logo_colour.raw"

		* = $2000
		!binary "data/logo_bitmap.raw"

		* = $4000
dycp_font	!binary "data/5px.chr"


; Constants: raster split positions
rstr1p		= $00
rstr2p		= $79
rstr3p		= $8d
rstr4p		= $b1

; Label assignments
dycp_buffer_1	= $40
dycp_buffer_2	= $68
dycp_buffer_3	= $90
line_cache	= $b8

copy_cnt_low	= $e0
copy_cnt_high	= $e1

rn		= $e0
sync		= $e1
scroll_x	= $e2
irq_store_1	= $e3


cos_at_1	= $e4
cos_offset_1	= $0c	; constant
cos_speed_1	= $03	; constant

cos_at_2	= $e5
cos_offset_2	= $0c	; constant
cos_speed_2	= $03	; constant

cos_at_3	= $e6
cos_offset_3	= $0c	; constant
cos_speed_3	= $03	; constant

cos_at_4	= $e7

l2_d016		= $e8
l2_d018		= $e9
l2_dd00		= $ea


dycp_workspace	= $5000
dycp_xfont	= $4800


; Entry point at $0812
		* = $8000
entry		sei

; Turn off the ROM and set up the interrupts
		lda #$35
		sta $01

		lda #$01
		sta rn

		lda #<nmi
		sta $fffa
		lda #>nmi
		sta $fffb

		lda #<int
		sta $fffe
		lda #>int
		sta $ffff

		lda #$7f
		sta $dc0d
		sta $dd0d

		lda $dc0d
		lda $dd0d

		lda #rstr1p
		sta $d012

		lda #$1b
		sta $d011
		lda #$01
		sta $d019
		sta $d01a

; Wipte the zero page from $40 to the end
		ldx #$40
		lda #$00
nuke_zp		sta $00,x
		inx
		cpx #$fc
		bne nuke_zp

; Turn off the I/O
		lda #$34
		sta $01

; Invert the bitmap (because the version of P1 I use gets it wrong!)
		ldx #$00
picture_invert	lda $2000,x
		eor #$ff
		sta $2000,x
		inx
		bne picture_invert

		inc picture_invert+$02
		inc picture_invert+$07
		lda picture_invert+$02
		cmp #$40
		bne picture_invert-$02

; Duplicate and pre-shift the bitmap for the logo swing
		jsr bmp_copy
		lda #$60
		sta bmp_copy_read+$02
		lda #$c0
		sta bmp_copy_write+$02
		jsr bmp_copy

		lda #$c0
		sta bmp_copy_read+$02
		lda #$e0
		sta bmp_copy_write+$02
		jsr bmp_copy

; And again for the colour data
		jsr bmp_col_copy
		lda #$5c
		sta bmp_cc_read+$02
		lda #$d0
		sta bmp_cc_write+$02
		jsr bmp_col_copy
		lda #$d0
		sta bmp_cc_read+$02
		lda #$f0
		sta bmp_cc_write+$02
		jsr bmp_col_copy

; A little "duct tape" to cover the gaps (cheap and cheerless approach)
		lda #$44
		sta $5c00
		sta $d000
		sta $d001
		sta $f000
		sta $f001
		sta $f002

		lda #$88
		sta $5e80
		sta $d280
		sta $d281
		sta $f280
		sta $f281
		sta $f282

; Turn on the I/O again
		lda #$35
		sta $01

; Copy the picture's colour RAM to bank 0's screen
		ldx #$00
colour_copy_1	lda $1c00,x
		sta $0400,x
		lda $1d00,x
		sta $0500,x
		lda $1e00,x
		sta $0600,x
		lda $1ee8,x
		sta $06e8,x
		inx
		bne colour_copy_1

; Reset colour RAM (for the DYCP's benefit)
		ldx #$00
colour_init	lda #$01
		sta $d800,x
		sta $d900,x
		sta $da00,x
		sta $dae8,x
		inx
		bne colour_init


; Generate the DYCP work area on screen
		ldx #$00
		lda #$01
		clc
dycp_scrn_gen	sta $5d90,x
		adc #$01
		sta $5db8,x
		adc #$01
		sta $5de0,x
		adc #$01
		sta $5e08,x
		adc #$01
		sta $5e30,x
		adc #$02
		inx
		cpx #$27
		bne dycp_scrn_gen

		ldx #$00
		txa
dycp_edge_gen	sta $5d68,x
		sta $5e58,x
		sta $d968,x
		sta $da58,x
		inx
		cpx #$27
		bne dycp_edge_gen

; Store the middle screen line of the DYCP for later
		ldx #$00
screen_cache	lda $5de0,x
		sta line_cache,x
		inx
		cpx #$28
		bne screen_cache

; Font converter
		ldx #$00
font_xvert	jsr dycp_font_rd
		sta dycp_xfont+$000,x
		jsr dycp_font_rd
		sta dycp_xfont+$100,x
		jsr dycp_font_rd
		sta dycp_xfont+$200,x
		jsr dycp_font_rd
		sta dycp_xfont+$300,x
		jsr dycp_font_rd
		sta dycp_xfont+$400,x
		jsr dycp_font_rd
		sta dycp_xfont+$500,x
		jsr dycp_font_rd
		sta dycp_xfont+$600,x
		jsr dycp_font_rd
		sta dycp_xfont+$700,x
		inx
		bne font_xvert

; Reset some of the labels
		lda #$00
		sta cos_at_1
		lda #$28
		sta cos_at_2
		lda #$50
		sta cos_at_3

		jsr reset_1
		jsr reset_2
		jsr reset_3

; Initialise the music
		lda #$00
		jsr music+$00

		cli

; The main runtime loop that updates the DYCPs
main_loop	lda #$00
		sta sync
sw_loop		cmp sync
		beq sw_loop

		jsr dycp_update

		jmp main_loop


; Bitmap copy and shift subroutine
bmp_copy	lda #$1f
		sta copy_cnt_high
		lda #$40
		sta copy_cnt_low

bmp_copy_main	ldx #$00
bmp_copy_read	lda $2000,x
bmp_copy_write	sta $6008,x

		dec copy_cnt_low
		bne bmp_copy_skip
		dec copy_cnt_high
		bmi bmp_copy_exit

bmp_copy_skip	inx
		bne bmp_copy_read
		inc bmp_copy_read+$02
		inc bmp_copy_write+$02
		jmp bmp_copy_main

bmp_copy_exit	rts

; Bitmap colour copy and shift subroutine
bmp_col_copy	lda #$04
		sta copy_cnt_low

bmp_col_copy_m	ldx #$00
bmp_cc_read	lda $1c00,x
bmp_cc_write	sta $5c01,x
		inx
		bne bmp_cc_read
		inc bmp_cc_read+$02
		inc bmp_cc_write+$02
		dec copy_cnt_low
		bne bmp_col_copy_m
		rts

; IRQ interrupt
int		pha
		txa
		pha
		tya
		pha

		lda $d019
		and #$01
		sta $d019
		bne ya
		jmp ea31

ya		lda rn
		cmp #$02
		beq rout2

		cmp #$03
		bne *+$05
		jmp rout3

		cmp #$04
		bne *+$05
		jmp rout4


; Raster split 1
rout1		lda #$02
		sta rn
		lda #rstr2p
		sta $d012

		lda #$04
		sta $d020
		lda #$06
		sta $d021

		lda #$3b
		sta $d011

		lda cos_at_4
		clc
		adc #$03
		sta cos_at_4
		tax
		ldy logo_cosinus,x
		tya
		and #$07
		eor #$07
		sta $d016
		tya
		lsr
		lsr
		lsr
		and #$03
		tax
		lda logo_d018,x
		sta $d018
		lda logo_dd00,x
		sta $dd00

		lda cos_at_4
		clc
		adc #$80
		tax
		ldy logo_cosinus,x
		tya
		and #$07
		eor #$07
		sta l2_d016
		tya
		lsr
		lsr
		lsr
		and #$03
		tax
		lda logo_d018,x
		sta l2_d018
		lda logo_dd00,x
		sta l2_dd00

; Plau the music
		jsr music+$03

		jmp ea31


; Raster split 2
rout2		lda #$03
		sta rn
		lda #rstr3p
		sta $d012

; Switch to character mode for the DYCP
		ldx #$0a
		dex
		bne *-$01
		nop
		nop

		lda #$74
		ldx #$c6
		ldy #$1b
		sta $d018
		stx $dd00
		sty $d011

		nop
		nop
		nop
		nop

		lda scroll_x
		and #$03
		asl
		eor #$07
		sta $d016

		jmp ea31


		* = ((*/$100)+1)*$100

; Raster split 3
rout3

; Stabilising the rasters
		ldx #$0e
		dex
		bne *-$01
		nop
		lda $d012
		cmp #rstr3p+$01
		beq *+$02
;		sta $d020

		ldx #$0a
		dex
		bne *-$01
		nop
		lda $d012
		cmp #rstr3p+$02
		beq *+$02
;		sta $d020

		ldx #$0a
		dex
		bne *-$01
		nop
		nop
		lda $d012
		cmp #rstr3p+$03
		beq *+$02
;		sta $d020

		ldx #$16
		dex
		bne *-$01

; Colour split bars that the scrollers are wrapped around
		lda #$00
		sta $d021
		ldx #$06
		dex
		bne *-$01
		bit $ea

		lda #$06
		sta $d021
		lda #$0b
		sta $d021
		lda #$04
		sta $d021
		lda #$0e
		sta $d021
		lda #$04
		sta $d021
		lda #$0b
		sta $d021
		lda #$06
		sta $d021

		bit $ea
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop

		lda #$0b
		sta $d021
		lda #$04
		sta $d021
		lda #$0e
		sta $d021
		lda #$03
		sta $d021
		lda #$0e
		sta $d021
		lda #$04
		sta $d021
		lda #$0b
		sta $d021

		bit $ea
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop

		lda #$04
		sta $d021
		lda #$0e
		sta $d021
		lda #$03
		sta $d021
		lda #$0d
		sta $d021
		lda #$03
		sta $d021
		lda #$0e
		sta $d021
		lda #$04
		sta $d021

		bit $ea
		nop
		nop
		nop
		lda #$08
		sta $d020
		nop
		nop
		nop

		lda #$02
		sta $d021
		lda #$08
		sta $d021
		lda #$0a
		sta $d021
		lda #$0f
		sta $d021
		lda #$0a
		sta $d021
		lda #$08
		sta $d021
		lda #$02
		sta $d021

		bit $ea
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop

		lda #$09
		sta $d021
		lda #$02
		sta $d021
		lda #$08
		sta $d021
		lda #$0a
		sta $d021
		lda #$08
		sta $d021
		lda #$02
		sta $d021
		lda #$09
		sta $d021

		bit $ea
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop

		lda #$00
		sta $d021
		lda #$09
		sta $d021
		lda #$02
		sta $d021
		lda #$08
		sta $d021
		lda #$02
		sta $d021
		lda #$09
		sta $d021
		lda #$00
		sta $d021

		lda #$00
		sta $d021
		ldx #$0c
		dex
		bne *-$01
		lda #$09
		sta $d021


		lda #$04
		sta rn
		lda #rstr4p
		sta $d012

		jmp ea31


; Raster split 4
rout4		lda #$01
		sta rn
		lda #rstr1p
		sta $d012

		ldx #$08
		dex
		bne *-$01
		nop

		lda l2_d018
		sta $d018
		lda l2_d016
		sta $d016
		lda l2_dd00
		sta $dd00
		lda #$3b
		sta $d011

		lda #$01
		sta sync

; Exit the interrupt
ea31		pla
		tay
		pla
		tax
		pla
nmi		rti


; Clear DYCP 1
dycp_update	ldx cos_at_1
		ldy dycp_cosinus,x
		lda #$00
		sta dycp_workspace+$008,y
		sta dycp_workspace+$009,y
		sta dycp_workspace+$00a,y
		sta dycp_workspace+$00b,y
		sta dycp_workspace+$00c,y

!set dycp_cnt=$01
!do {
		txa
		clc
		adc #cos_offset_1
		tax
		ldy dycp_cosinus,x
		lda #$00
		sta dycp_workspace+$008+(dycp_cnt*$30),y
		sta dycp_workspace+$009+(dycp_cnt*$30),y
		sta dycp_workspace+$00a+(dycp_cnt*$30),y
		sta dycp_workspace+$00b+(dycp_cnt*$30),y
		sta dycp_workspace+$00c+(dycp_cnt*$30),y

		!set dycp_cnt=dycp_cnt+$01
} until dycp_cnt=$27

; Clear DYCP 2
		ldx cos_at_2
		ldy dycp_cosinus,x
		lda #$00
		sta dycp_workspace+$008,y
		sta dycp_workspace+$009,y
		sta dycp_workspace+$00a,y
		sta dycp_workspace+$00b,y
		sta dycp_workspace+$00c,y

!set dycp_cnt=$01
!do {
		txa
		clc
		adc #cos_offset_1
		tax
		ldy dycp_cosinus,x
		lda #$00
		sta dycp_workspace+$008+(dycp_cnt*$30),y
		sta dycp_workspace+$009+(dycp_cnt*$30),y
		sta dycp_workspace+$00a+(dycp_cnt*$30),y
		sta dycp_workspace+$00b+(dycp_cnt*$30),y
		sta dycp_workspace+$00c+(dycp_cnt*$30),y

		!set dycp_cnt=dycp_cnt+$01
} until dycp_cnt=$27

; Clear DYCP 3
		ldx cos_at_3
		ldy dycp_cosinus,x
		lda #$00
		sta dycp_workspace+$008,y
		sta dycp_workspace+$009,y
		sta dycp_workspace+$00a,y
		sta dycp_workspace+$00b,y
		sta dycp_workspace+$00c,y

!set dycp_cnt=$01
!do {
		txa
		clc
		adc #cos_offset_1
		tax
		ldy dycp_cosinus,x
		lda #$00
		sta dycp_workspace+$008+(dycp_cnt*$30),y
		sta dycp_workspace+$009+(dycp_cnt*$30),y
		sta dycp_workspace+$00a+(dycp_cnt*$30),y
		sta dycp_workspace+$00b+(dycp_cnt*$30),y
		sta dycp_workspace+$00c+(dycp_cnt*$30),y

		!set dycp_cnt=dycp_cnt+$01
} until dycp_cnt=$27


; Update the hardware smooth scroll and shift the line if needed
		ldx scroll_x
		inx
		cpx #$04
		beq *+$05
		jmp scr_xb

; Update DYCP 1
!set dycp_cnt=$00
!do {
		lda dycp_buffer_1+$01+dycp_cnt
		sta dycp_buffer_1+$00+dycp_cnt

		!set dycp_cnt=dycp_cnt+$01
} until dycp_cnt=$26

mread_1		lda scroll_text_1
		bne okay_1
		jsr reset_1
		jmp mread_1

okay_1		cmp #$20
		bne *+$04
		lda #$00
		sta dycp_buffer_1+$26

		inc mread_1+$01
		bne *+$05
		inc mread_1+$02


		lda cos_at_1
		clc
		adc #cos_offset_1
		sta cos_at_1

; Update DYCP 2
!set dycp_cnt=$00
!do {
		lda dycp_buffer_2+$01+dycp_cnt
		sta dycp_buffer_2+$00+dycp_cnt

		!set dycp_cnt=dycp_cnt+$01
} until dycp_cnt=$26

mread_2		lda scroll_text_2
		bne okay_2
		jsr reset_2
		jmp mread_2

okay_2		cmp #$20
		bne *+$04
		lda #$00
		sta dycp_buffer_2+$26

		inc mread_2+$01
		bne *+$05
		inc mread_2+$02


		lda cos_at_2
		clc
		adc #cos_offset_2
		sta cos_at_2

; Update DYCP 3
!set dycp_cnt=$00
!do {
		lda dycp_buffer_3+$01+dycp_cnt
		sta dycp_buffer_3+$00+dycp_cnt

		!set dycp_cnt=dycp_cnt+$01
} until dycp_cnt=$26

mread_3		lda scroll_text_3
		bne okay_3
		jsr reset_3
		jmp mread_3

okay_3		cmp #$20
		bne *+$04
		lda #$00
		sta dycp_buffer_3+$26

		inc mread_3+$01
		bne *+$05
		inc mread_3+$02


		lda cos_at_3
		clc
		adc #cos_offset_3
		sta cos_at_3


		ldx #$00
scr_xb		stx scroll_x


; Update curve positions
		lda cos_at_1
		clc
		adc #cos_speed_1
		sta cos_at_1

		lda cos_at_2
		clc
		adc #cos_speed_2
		sta cos_at_2

		lda cos_at_3
		clc
		adc #cos_speed_3
		sta cos_at_3

; Draw DYCP 1
dycp_draw	ldx cos_at_1
		stx irq_store_1
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$00
		beq dd1_char_01
		lda dycp_xfont+$000,x
		sta dycp_workspace+$008,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$009,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$00a,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$00b,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$00c,y

dd1_char_01	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$01
		beq dd1_char_02
		lda dycp_xfont+$000,x
		sta dycp_workspace+$038,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$039,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$03a,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$03b,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$03c,y

dd1_char_02	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$02
		beq dd1_char_03
		lda dycp_xfont+$000,x
		sta dycp_workspace+$068,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$069,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$06a,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$06b,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$06c,y

dd1_char_03	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$03
		beq dd1_char_04
		lda dycp_xfont+$000,x
		sta dycp_workspace+$098,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$099,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$09a,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$09b,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$09c,y

dd1_char_04	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$04
		beq dd1_char_05
		lda dycp_xfont+$000,x
		sta dycp_workspace+$0c8,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$0c9,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$0ca,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$0cb,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$0cc,y

dd1_char_05	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$05
		beq dd1_char_06
		lda dycp_xfont+$000,x
		sta dycp_workspace+$0f8,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$0f9,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$0fa,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$0fb,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$0fc,y

dd1_char_06	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$06
		beq dd1_char_07
		lda dycp_xfont+$000,x
		sta dycp_workspace+$128,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$129,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$12a,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$12b,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$12c,y

dd1_char_07	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$07
		beq dd1_char_08
		lda dycp_xfont+$000,x
		sta dycp_workspace+$158,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$159,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$15a,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$15b,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$15c,y

dd1_char_08	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$08
		beq dd1_char_09
		lda dycp_xfont+$000,x
		sta dycp_workspace+$188,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$189,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$18a,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$18b,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$18c,y

dd1_char_09	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$09
		beq dd1_char_0a
		lda dycp_xfont+$000,x
		sta dycp_workspace+$1b8,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$1b9,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$1ba,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$1bb,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$1bc,y

dd1_char_0a	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$0a
		beq dd1_char_0b
		lda dycp_xfont+$000,x
		sta dycp_workspace+$1e8,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$1e9,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$1ea,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$1eb,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$1ec,y

dd1_char_0b	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$0b
		beq dd1_char_0c
		lda dycp_xfont+$000,x
		sta dycp_workspace+$218,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$219,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$21a,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$21b,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$21c,y

dd1_char_0c	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$0c
		beq dd1_char_0d
		lda dycp_xfont+$000,x
		sta dycp_workspace+$248,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$249,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$24a,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$24b,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$24c,y

dd1_char_0d	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$0d
		beq dd1_char_0e
		lda dycp_xfont+$000,x
		sta dycp_workspace+$278,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$279,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$27a,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$27b,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$27c,y

dd1_char_0e	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$0e
		beq dd1_char_0f
		lda dycp_xfont+$000,x
		sta dycp_workspace+$2a8,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$2a9,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$2aa,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$2ab,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$2ac,y

dd1_char_0f	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$0f
		beq dd1_char_10
		lda dycp_xfont+$000,x
		sta dycp_workspace+$2d8,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$2d9,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$2da,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$2db,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$2dc,y

dd1_char_10	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$10
		beq dd1_char_11
		lda dycp_xfont+$000,x
		sta dycp_workspace+$308,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$309,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$30a,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$30b,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$30c,y

dd1_char_11	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$11
		beq dd1_char_12
		lda dycp_xfont+$000,x
		sta dycp_workspace+$338,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$339,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$33a,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$33b,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$33c,y

dd1_char_12	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$12
		beq dd1_char_13
		lda dycp_xfont+$000,x
		sta dycp_workspace+$368,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$369,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$36a,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$36b,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$36c,y

dd1_char_13	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$13
		beq dd1_char_14
		lda dycp_xfont+$000,x
		sta dycp_workspace+$398,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$399,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$39a,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$39b,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$39c,y

dd1_char_14	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$14
		beq dd1_char_15
		lda dycp_xfont+$000,x
		sta dycp_workspace+$3c8,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$3c9,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$3ca,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$3cb,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$3cc,y

dd1_char_15	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$15
		beq dd1_char_16
		lda dycp_xfont+$000,x
		sta dycp_workspace+$3f8,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$3f9,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$3fa,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$3fb,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$3fc,y

dd1_char_16	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$16
		beq dd1_char_17
		lda dycp_xfont+$000,x
		sta dycp_workspace+$428,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$429,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$42a,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$42b,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$42c,y

dd1_char_17	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$17
		beq dd1_char_18
		lda dycp_xfont+$000,x
		sta dycp_workspace+$458,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$459,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$45a,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$45b,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$45c,y

dd1_char_18	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$18
		beq dd1_char_19
		lda dycp_xfont+$000,x
		sta dycp_workspace+$488,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$489,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$48a,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$48b,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$48c,y

dd1_char_19	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$19
		beq dd1_char_1a
		lda dycp_xfont+$000,x
		sta dycp_workspace+$4b8,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$4b9,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$4ba,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$4bb,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$4bc,y

dd1_char_1a	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$1a
		beq dd1_char_1b
		lda dycp_xfont+$000,x
		sta dycp_workspace+$4e8,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$4e9,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$4ea,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$4eb,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$4ec,y

dd1_char_1b	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$1b
		beq dd1_char_1c
		lda dycp_xfont+$000,x
		sta dycp_workspace+$518,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$519,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$51a,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$51b,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$51c,y

dd1_char_1c	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$1c
		beq dd1_char_1d
		lda dycp_xfont+$000,x
		sta dycp_workspace+$548,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$549,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$54a,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$54b,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$54c,y

dd1_char_1d	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$1d
		beq dd1_char_1e
		lda dycp_xfont+$000,x
		sta dycp_workspace+$578,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$579,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$57a,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$57b,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$57c,y

dd1_char_1e	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$1e
		beq dd1_char_1f
		lda dycp_xfont+$000,x
		sta dycp_workspace+$5a8,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$5a9,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$5aa,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$5ab,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$5ac,y

dd1_char_1f	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$1f
		beq dd1_char_20
		lda dycp_xfont+$000,x
		sta dycp_workspace+$5d8,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$5d9,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$5da,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$5db,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$5dc,y

dd1_char_20	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$20
		beq dd1_char_21
		lda dycp_xfont+$000,x
		sta dycp_workspace+$608,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$609,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$60a,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$60b,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$60c,y

dd1_char_21	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$21
		beq dd1_char_22
		lda dycp_xfont+$000,x
		sta dycp_workspace+$638,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$639,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$63a,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$63b,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$63c,y

dd1_char_22	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$22
		beq dd1_char_23
		lda dycp_xfont+$000,x
		sta dycp_workspace+$668,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$669,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$66a,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$66b,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$66c,y

dd1_char_23	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$23
		beq dd1_char_24
		lda dycp_xfont+$000,x
		sta dycp_workspace+$698,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$699,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$69a,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$69b,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$69c,y

dd1_char_24	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$24
		beq dd1_char_25
		lda dycp_xfont+$000,x
		sta dycp_workspace+$6c8,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$6c9,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$6ca,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$6cb,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$6cc,y

dd1_char_25	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$25
		beq dd1_char_26
		lda dycp_xfont+$000,x
		sta dycp_workspace+$6f8,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$6f9,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$6fa,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$6fb,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$6fc,y

dd1_char_26	lda irq_store_1
		clc
		adc #cos_offset_1
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_1+$26
		beq dd1_char_out
		lda dycp_xfont+$000,x
		sta dycp_workspace+$728,y
		lda dycp_xfont+$100,x
		sta dycp_workspace+$729,y
		lda dycp_xfont+$200,x
		sta dycp_workspace+$72a,y
		lda dycp_xfont+$300,x
		sta dycp_workspace+$72b,y
		lda dycp_xfont+$400,x
		sta dycp_workspace+$72c,y

dd1_char_out

; Draw DYCP 2
dd2_char_00	ldx cos_at_2
		stx irq_store_1
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$00
		beq dd2_char_01
		lda dycp_xfont+$000,x
		ora dycp_workspace+$008,y
		sta dycp_workspace+$008,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$009,y
		sta dycp_workspace+$009,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$00a,y
		sta dycp_workspace+$00a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$00b,y
		sta dycp_workspace+$00b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$00c,y
		sta dycp_workspace+$00c,y

dd2_char_01	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$01
		beq dd2_char_02
		lda dycp_xfont+$000,x
		ora dycp_workspace+$038,y
		sta dycp_workspace+$038,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$039,y
		sta dycp_workspace+$039,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$03a,y
		sta dycp_workspace+$03a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$03b,y
		sta dycp_workspace+$03b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$03c,y
		sta dycp_workspace+$03c,y

dd2_char_02	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$02
		beq dd2_char_03
		lda dycp_xfont+$000,x
		ora dycp_workspace+$068,y
		sta dycp_workspace+$068,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$069,y
		sta dycp_workspace+$069,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$06a,y
		sta dycp_workspace+$06a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$06b,y
		sta dycp_workspace+$06b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$06c,y
		sta dycp_workspace+$06c,y

dd2_char_03	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$03
		beq dd2_char_04
		lda dycp_xfont+$000,x
		ora dycp_workspace+$098,y
		sta dycp_workspace+$098,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$099,y
		sta dycp_workspace+$099,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$09a,y
		sta dycp_workspace+$09a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$09b,y
		sta dycp_workspace+$09b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$09c,y
		sta dycp_workspace+$09c,y

dd2_char_04	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$04
		beq dd2_char_05
		lda dycp_xfont+$000,x
		ora dycp_workspace+$0c8,y
		sta dycp_workspace+$0c8,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$0c9,y
		sta dycp_workspace+$0c9,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$0ca,y
		sta dycp_workspace+$0ca,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$0cb,y
		sta dycp_workspace+$0cb,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$0cc,y
		sta dycp_workspace+$0cc,y

dd2_char_05	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$05
		beq dd2_char_06
		lda dycp_xfont+$000,x
		ora dycp_workspace+$0f8,y
		sta dycp_workspace+$0f8,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$0f9,y
		sta dycp_workspace+$0f9,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$0fa,y
		sta dycp_workspace+$0fa,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$0fb,y
		sta dycp_workspace+$0fb,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$0fc,y
		sta dycp_workspace+$0fc,y

dd2_char_06	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$06
		beq dd2_char_07
		lda dycp_xfont+$000,x
		ora dycp_workspace+$128,y
		sta dycp_workspace+$128,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$129,y
		sta dycp_workspace+$129,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$12a,y
		sta dycp_workspace+$12a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$12b,y
		sta dycp_workspace+$12b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$12c,y
		sta dycp_workspace+$12c,y

dd2_char_07	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$07
		beq dd2_char_08
		lda dycp_xfont+$000,x
		ora dycp_workspace+$158,y
		sta dycp_workspace+$158,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$159,y
		sta dycp_workspace+$159,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$15a,y
		sta dycp_workspace+$15a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$15b,y
		sta dycp_workspace+$15b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$15c,y
		sta dycp_workspace+$15c,y

dd2_char_08	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$08
		beq dd2_char_09
		lda dycp_xfont+$000,x
		ora dycp_workspace+$188,y
		sta dycp_workspace+$188,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$178,y
		sta dycp_workspace+$189,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$18a,y
		sta dycp_workspace+$18a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$18b,y
		sta dycp_workspace+$18b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$18c,y
		sta dycp_workspace+$18c,y

dd2_char_09	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$09
		beq dd2_char_0a
		lda dycp_xfont+$000,x
		ora dycp_workspace+$1b8,y
		sta dycp_workspace+$1b8,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$1b9,y
		sta dycp_workspace+$1b9,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$1ba,y
		sta dycp_workspace+$1ba,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$1bb,y
		sta dycp_workspace+$1bb,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$1bc,y
		sta dycp_workspace+$1bc,y

dd2_char_0a	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$0a
		beq dd2_char_0b
		lda dycp_xfont+$000,x
		ora dycp_workspace+$1e8,y
		sta dycp_workspace+$1e8,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$1e9,y
		sta dycp_workspace+$1e9,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$1ea,y
		sta dycp_workspace+$1ea,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$1eb,y
		sta dycp_workspace+$1eb,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$1ec,y
		sta dycp_workspace+$1ec,y

dd2_char_0b	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$0b
		beq dd2_char_0c
		lda dycp_xfont+$000,x
		ora dycp_workspace+$218,y
		sta dycp_workspace+$218,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$219,y
		sta dycp_workspace+$219,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$21a,y
		sta dycp_workspace+$21a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$21b,y
		sta dycp_workspace+$21b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$21c,y
		sta dycp_workspace+$21c,y

dd2_char_0c	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$0c
		beq dd2_char_0d
		lda dycp_xfont+$000,x
		ora dycp_workspace+$248,y
		sta dycp_workspace+$248,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$249,y
		sta dycp_workspace+$249,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$24a,y
		sta dycp_workspace+$24a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$24b,y
		sta dycp_workspace+$24b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$24c,y
		sta dycp_workspace+$24c,y

dd2_char_0d	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$0d
		beq dd2_char_0e
		lda dycp_xfont+$000,x
		ora dycp_workspace+$278,y
		sta dycp_workspace+$278,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$279,y
		sta dycp_workspace+$279,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$27a,y
		sta dycp_workspace+$27a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$27b,y
		sta dycp_workspace+$27b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$27c,y
		sta dycp_workspace+$27c,y

dd2_char_0e	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$0e
		beq dd2_char_0f
		lda dycp_xfont+$000,x
		ora dycp_workspace+$2a8,y
		sta dycp_workspace+$2a8,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$2a9,y
		sta dycp_workspace+$2a9,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$2aa,y
		sta dycp_workspace+$2aa,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$2ab,y
		sta dycp_workspace+$2ab,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$2ac,y
		sta dycp_workspace+$2ac,y

dd2_char_0f	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$0f
		beq dd2_char_10
		lda dycp_xfont+$000,x
		ora dycp_workspace+$2d8,y
		sta dycp_workspace+$2d8,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$2d9,y
		sta dycp_workspace+$2d9,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$2da,y
		sta dycp_workspace+$2da,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$2db,y
		sta dycp_workspace+$2db,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$2dc,y
		sta dycp_workspace+$2dc,y

dd2_char_10	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$10
		beq dd2_char_11
		lda dycp_xfont+$000,x
		ora dycp_workspace+$308,y
		sta dycp_workspace+$308,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$309,y
		sta dycp_workspace+$309,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$30a,y
		sta dycp_workspace+$30a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$30b,y
		sta dycp_workspace+$30b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$30c,y
		sta dycp_workspace+$30c,y

dd2_char_11	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$11
		beq dd2_char_12
		lda dycp_xfont+$000,x
		ora dycp_workspace+$338,y
		sta dycp_workspace+$338,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$339,y
		sta dycp_workspace+$339,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$33a,y
		sta dycp_workspace+$33a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$33b,y
		sta dycp_workspace+$33b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$33c,y
		sta dycp_workspace+$33c,y

dd2_char_12	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$12
		beq dd2_char_13
		lda dycp_xfont+$000,x
		ora dycp_workspace+$368,y
		sta dycp_workspace+$368,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$369,y
		sta dycp_workspace+$369,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$36a,y
		sta dycp_workspace+$36a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$36b,y
		sta dycp_workspace+$36b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$36c,y
		sta dycp_workspace+$36c,y

dd2_char_13	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$13
		beq dd2_char_14
		lda dycp_xfont+$000,x
		ora dycp_workspace+$398,y
		sta dycp_workspace+$398,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$399,y
		sta dycp_workspace+$399,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$39a,y
		sta dycp_workspace+$39a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$39b,y
		sta dycp_workspace+$39b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$39c,y
		sta dycp_workspace+$39c,y

dd2_char_14	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$14
		beq dd2_char_15
		lda dycp_xfont+$000,x
		ora dycp_workspace+$3c8,y
		sta dycp_workspace+$3c8,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$3c9,y
		sta dycp_workspace+$3c9,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$3ca,y
		sta dycp_workspace+$3ca,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$3cb,y
		sta dycp_workspace+$3cb,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$3cc,y
		sta dycp_workspace+$3cc,y

dd2_char_15	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$15
		beq dd2_char_16
		lda dycp_xfont+$000,x
		ora dycp_workspace+$3f8,y
		sta dycp_workspace+$3f8,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$3f9,y
		sta dycp_workspace+$3f9,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$3fa,y
		sta dycp_workspace+$3fa,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$3fb,y
		sta dycp_workspace+$3fb,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$3fc,y
		sta dycp_workspace+$3fc,y

dd2_char_16	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$16
		beq dd2_char_17
		lda dycp_xfont+$000,x
		ora dycp_workspace+$428,y
		sta dycp_workspace+$428,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$429,y
		sta dycp_workspace+$429,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$42a,y
		sta dycp_workspace+$42a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$42b,y
		sta dycp_workspace+$42b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$42c,y
		sta dycp_workspace+$42c,y

dd2_char_17	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$17
		beq dd2_char_18
		lda dycp_xfont+$000,x
		ora dycp_workspace+$458,y
		sta dycp_workspace+$458,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$459,y
		sta dycp_workspace+$459,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$45a,y
		sta dycp_workspace+$45a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$45b,y
		sta dycp_workspace+$45b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$45c,y
		sta dycp_workspace+$45c,y

dd2_char_18	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$18
		beq dd2_char_19
		lda dycp_xfont+$000,x
		ora dycp_workspace+$488,y
		sta dycp_workspace+$488,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$489,y
		sta dycp_workspace+$489,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$48a,y
		sta dycp_workspace+$48a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$48b,y
		sta dycp_workspace+$48b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$48c,y
		sta dycp_workspace+$48c,y

dd2_char_19	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$19
		beq dd2_char_1a
		lda dycp_xfont+$000,x
		ora dycp_workspace+$4b8,y
		sta dycp_workspace+$4b8,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$4b9,y
		sta dycp_workspace+$4b9,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$4ba,y
		sta dycp_workspace+$4ba,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$4bb,y
		sta dycp_workspace+$4bb,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$4bc,y
		sta dycp_workspace+$4bc,y

dd2_char_1a	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$1a
		beq dd2_char_1b
		lda dycp_xfont+$000,x
		ora dycp_workspace+$4e8,y
		sta dycp_workspace+$4e8,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$4e9,y
		sta dycp_workspace+$4e9,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$4ea,y
		sta dycp_workspace+$4ea,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$4eb,y
		sta dycp_workspace+$4eb,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$4ec,y
		sta dycp_workspace+$4ec,y

dd2_char_1b	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$1b
		beq dd2_char_1c
		lda dycp_xfont+$000,x
		ora dycp_workspace+$518,y
		sta dycp_workspace+$518,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$519,y
		sta dycp_workspace+$519,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$51a,y
		sta dycp_workspace+$51a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$51b,y
		sta dycp_workspace+$51b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$51c,y
		sta dycp_workspace+$51c,y

dd2_char_1c	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$1c
		beq dd2_char_1d
		lda dycp_xfont+$000,x
		ora dycp_workspace+$548,y
		sta dycp_workspace+$548,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$549,y
		sta dycp_workspace+$549,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$54a,y
		sta dycp_workspace+$54a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$54b,y
		sta dycp_workspace+$54b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$54c,y
		sta dycp_workspace+$54c,y

dd2_char_1d	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$1d
		beq dd2_char_1e
		lda dycp_xfont+$000,x
		ora dycp_workspace+$578,y
		sta dycp_workspace+$578,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$579,y
		sta dycp_workspace+$579,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$57a,y
		sta dycp_workspace+$57a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$57b,y
		sta dycp_workspace+$57b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$57c,y
		sta dycp_workspace+$57c,y

dd2_char_1e	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$1e
		beq dd2_char_1f
		lda dycp_xfont+$000,x
		ora dycp_workspace+$5a8,y
		sta dycp_workspace+$5a8,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$5a9,y
		sta dycp_workspace+$5a9,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$5aa,y
		sta dycp_workspace+$5aa,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$5ab,y
		sta dycp_workspace+$5ab,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$5ac,y
		sta dycp_workspace+$5ac,y

dd2_char_1f	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$1f
		beq dd2_char_20
		lda dycp_xfont+$000,x
		ora dycp_workspace+$5d8,y
		sta dycp_workspace+$5d8,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$5d9,y
		sta dycp_workspace+$5d9,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$5da,y
		sta dycp_workspace+$5da,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$5db,y
		sta dycp_workspace+$5db,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$5dc,y
		sta dycp_workspace+$5dc,y

dd2_char_20	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$20
		beq dd2_char_21
		lda dycp_xfont+$000,x
		ora dycp_workspace+$608,y
		sta dycp_workspace+$608,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$609,y
		sta dycp_workspace+$609,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$60a,y
		sta dycp_workspace+$60a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$60b,y
		sta dycp_workspace+$60b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$60c,y
		sta dycp_workspace+$60c,y

dd2_char_21	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$21
		beq dd2_char_22
		lda dycp_xfont+$000,x
		sta dycp_workspace+$638,y
		ora dycp_workspace+$638,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$639,y
		sta dycp_workspace+$639,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$63a,y
		sta dycp_workspace+$63a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$63b,y
		sta dycp_workspace+$63b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$63c,y
		sta dycp_workspace+$63c,y

dd2_char_22	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$22
		beq dd2_char_23
		lda dycp_xfont+$000,x
		ora dycp_workspace+$668,y
		sta dycp_workspace+$668,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$669,y
		sta dycp_workspace+$669,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$66a,y
		sta dycp_workspace+$66a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$66b,y
		sta dycp_workspace+$66b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$66c,y
		sta dycp_workspace+$66c,y

dd2_char_23	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$23
		beq dd2_char_24
		lda dycp_xfont+$000,x
		ora dycp_workspace+$698,y
		sta dycp_workspace+$698,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$699,y
		sta dycp_workspace+$699,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$69a,y
		sta dycp_workspace+$69a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$69b,y
		sta dycp_workspace+$69b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$69c,y
		sta dycp_workspace+$69c,y

dd2_char_24	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$24
		beq dd2_char_25
		lda dycp_xfont+$000,x
		ora dycp_workspace+$6c8,y
		sta dycp_workspace+$6c8,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$6c9,y
		sta dycp_workspace+$6c9,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$6ca,y
		sta dycp_workspace+$6ca,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$6cb,y
		sta dycp_workspace+$6cb,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$6cc,y
		sta dycp_workspace+$6cc,y

dd2_char_25	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$25
		beq dd2_char_26
		lda dycp_xfont+$000,x
		ora dycp_workspace+$6f8,y
		sta dycp_workspace+$6f8,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$6f9,y
		sta dycp_workspace+$6f9,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$6fa,y
		sta dycp_workspace+$6fa,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$6fb,y
		sta dycp_workspace+$6fb,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$6fc,y
		sta dycp_workspace+$6fc,y

dd2_char_26	lda irq_store_1
		clc
		adc #cos_offset_2
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_2+$26
		beq dd2_char_out
		lda dycp_xfont+$000,x
		ora dycp_workspace+$728,y
		sta dycp_workspace+$728,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$729,y
		sta dycp_workspace+$729,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$72a,y
		sta dycp_workspace+$72a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$72b,y
		sta dycp_workspace+$72b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$72c,y
		sta dycp_workspace+$72c,y

dd2_char_out

; Draw DYCP 3
dd3_char_00	ldx cos_at_3
		stx irq_store_1
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$00
		beq dd3_char_01
		lda dycp_xfont+$000,x
		ora dycp_workspace+$008,y
		sta dycp_workspace+$008,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$009,y
		sta dycp_workspace+$009,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$00a,y
		sta dycp_workspace+$00a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$00b,y
		sta dycp_workspace+$00b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$00c,y
		sta dycp_workspace+$00c,y

dd3_char_01	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$01
		beq dd3_char_02
		lda dycp_xfont+$000,x
		ora dycp_workspace+$038,y
		sta dycp_workspace+$038,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$039,y
		sta dycp_workspace+$039,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$03a,y
		sta dycp_workspace+$03a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$03b,y
		sta dycp_workspace+$03b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$03c,y
		sta dycp_workspace+$03c,y

dd3_char_02	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$02
		beq dd3_char_03
		lda dycp_xfont+$000,x
		ora dycp_workspace+$068,y
		sta dycp_workspace+$068,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$069,y
		sta dycp_workspace+$069,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$06a,y
		sta dycp_workspace+$06a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$06b,y
		sta dycp_workspace+$06b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$06c,y
		sta dycp_workspace+$06c,y

dd3_char_03	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$03
		beq dd3_char_04
		lda dycp_xfont+$000,x
		ora dycp_workspace+$098,y
		sta dycp_workspace+$098,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$099,y
		sta dycp_workspace+$099,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$09a,y
		sta dycp_workspace+$09a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$09b,y
		sta dycp_workspace+$09b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$09c,y
		sta dycp_workspace+$09c,y

dd3_char_04	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$04
		beq dd3_char_05
		lda dycp_xfont+$000,x
		ora dycp_workspace+$0c8,y
		sta dycp_workspace+$0c8,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$0c9,y
		sta dycp_workspace+$0c9,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$0ca,y
		sta dycp_workspace+$0ca,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$0cb,y
		sta dycp_workspace+$0cb,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$0cc,y
		sta dycp_workspace+$0cc,y

dd3_char_05	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$05
		beq dd3_char_06
		lda dycp_xfont+$000,x
		ora dycp_workspace+$0f8,y
		sta dycp_workspace+$0f8,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$0f9,y
		sta dycp_workspace+$0f9,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$0fa,y
		sta dycp_workspace+$0fa,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$0fb,y
		sta dycp_workspace+$0fb,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$0fc,y
		sta dycp_workspace+$0fc,y

dd3_char_06	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$06
		beq dd3_char_07
		lda dycp_xfont+$000,x
		ora dycp_workspace+$128,y
		sta dycp_workspace+$128,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$129,y
		sta dycp_workspace+$129,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$12a,y
		sta dycp_workspace+$12a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$12b,y
		sta dycp_workspace+$12b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$12c,y
		sta dycp_workspace+$12c,y

dd3_char_07	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$07
		beq dd3_char_08
		lda dycp_xfont+$000,x
		ora dycp_workspace+$158,y
		sta dycp_workspace+$158,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$159,y
		sta dycp_workspace+$159,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$15a,y
		sta dycp_workspace+$15a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$15b,y
		sta dycp_workspace+$15b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$15c,y
		sta dycp_workspace+$15c,y

dd3_char_08	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$08
		beq dd3_char_09
		lda dycp_xfont+$000,x
		ora dycp_workspace+$188,y
		sta dycp_workspace+$188,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$178,y
		sta dycp_workspace+$189,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$18a,y
		sta dycp_workspace+$18a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$18b,y
		sta dycp_workspace+$18b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$18c,y
		sta dycp_workspace+$18c,y

dd3_char_09	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$09
		beq dd3_char_0a
		lda dycp_xfont+$000,x
		ora dycp_workspace+$1b8,y
		sta dycp_workspace+$1b8,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$1b9,y
		sta dycp_workspace+$1b9,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$1ba,y
		sta dycp_workspace+$1ba,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$1bb,y
		sta dycp_workspace+$1bb,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$1bc,y
		sta dycp_workspace+$1bc,y

dd3_char_0a	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$0a
		beq dd3_char_0b
		lda dycp_xfont+$000,x
		ora dycp_workspace+$1e8,y
		sta dycp_workspace+$1e8,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$1e9,y
		sta dycp_workspace+$1e9,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$1ea,y
		sta dycp_workspace+$1ea,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$1eb,y
		sta dycp_workspace+$1eb,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$1ec,y
		sta dycp_workspace+$1ec,y

dd3_char_0b	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$0b
		beq dd3_char_0c
		lda dycp_xfont+$000,x
		ora dycp_workspace+$218,y
		sta dycp_workspace+$218,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$219,y
		sta dycp_workspace+$219,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$21a,y
		sta dycp_workspace+$21a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$21b,y
		sta dycp_workspace+$21b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$21c,y
		sta dycp_workspace+$21c,y

dd3_char_0c	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$0c
		beq dd3_char_0d
		lda dycp_xfont+$000,x
		ora dycp_workspace+$248,y
		sta dycp_workspace+$248,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$249,y
		sta dycp_workspace+$249,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$24a,y
		sta dycp_workspace+$24a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$24b,y
		sta dycp_workspace+$24b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$24c,y
		sta dycp_workspace+$24c,y

dd3_char_0d	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$0d
		beq dd3_char_0e
		lda dycp_xfont+$000,x
		ora dycp_workspace+$278,y
		sta dycp_workspace+$278,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$279,y
		sta dycp_workspace+$279,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$27a,y
		sta dycp_workspace+$27a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$27b,y
		sta dycp_workspace+$27b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$27c,y
		sta dycp_workspace+$27c,y

dd3_char_0e	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$0e
		beq dd3_char_0f
		lda dycp_xfont+$000,x
		ora dycp_workspace+$2a8,y
		sta dycp_workspace+$2a8,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$2a9,y
		sta dycp_workspace+$2a9,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$2aa,y
		sta dycp_workspace+$2aa,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$2ab,y
		sta dycp_workspace+$2ab,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$2ac,y
		sta dycp_workspace+$2ac,y

dd3_char_0f	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$0f
		beq dd3_char_10
		lda dycp_xfont+$000,x
		ora dycp_workspace+$2d8,y
		sta dycp_workspace+$2d8,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$2d9,y
		sta dycp_workspace+$2d9,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$2da,y
		sta dycp_workspace+$2da,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$2db,y
		sta dycp_workspace+$2db,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$2dc,y
		sta dycp_workspace+$2dc,y

dd3_char_10	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$10
		beq dd3_char_11
		lda dycp_xfont+$000,x
		ora dycp_workspace+$308,y
		sta dycp_workspace+$308,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$309,y
		sta dycp_workspace+$309,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$30a,y
		sta dycp_workspace+$30a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$30b,y
		sta dycp_workspace+$30b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$30c,y
		sta dycp_workspace+$30c,y

dd3_char_11	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$11
		beq dd3_char_12
		lda dycp_xfont+$000,x
		ora dycp_workspace+$338,y
		sta dycp_workspace+$338,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$339,y
		sta dycp_workspace+$339,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$33a,y
		sta dycp_workspace+$33a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$33b,y
		sta dycp_workspace+$33b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$33c,y
		sta dycp_workspace+$33c,y

dd3_char_12	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$12
		beq dd3_char_13
		lda dycp_xfont+$000,x
		ora dycp_workspace+$368,y
		sta dycp_workspace+$368,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$369,y
		sta dycp_workspace+$369,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$36a,y
		sta dycp_workspace+$36a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$36b,y
		sta dycp_workspace+$36b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$36c,y
		sta dycp_workspace+$36c,y

dd3_char_13	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$13
		beq dd3_char_14
		lda dycp_xfont+$000,x
		ora dycp_workspace+$398,y
		sta dycp_workspace+$398,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$399,y
		sta dycp_workspace+$399,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$39a,y
		sta dycp_workspace+$39a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$39b,y
		sta dycp_workspace+$39b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$39c,y
		sta dycp_workspace+$39c,y

dd3_char_14	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$14
		beq dd3_char_15
		lda dycp_xfont+$000,x
		ora dycp_workspace+$3c8,y
		sta dycp_workspace+$3c8,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$3c9,y
		sta dycp_workspace+$3c9,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$3ca,y
		sta dycp_workspace+$3ca,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$3cb,y
		sta dycp_workspace+$3cb,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$3cc,y
		sta dycp_workspace+$3cc,y

dd3_char_15	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$15
		beq dd3_char_16
		lda dycp_xfont+$000,x
		ora dycp_workspace+$3f8,y
		sta dycp_workspace+$3f8,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$3f9,y
		sta dycp_workspace+$3f9,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$3fa,y
		sta dycp_workspace+$3fa,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$3fb,y
		sta dycp_workspace+$3fb,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$3fc,y
		sta dycp_workspace+$3fc,y

dd3_char_16	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$16
		beq dd3_char_17
		lda dycp_xfont+$000,x
		ora dycp_workspace+$428,y
		sta dycp_workspace+$428,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$429,y
		sta dycp_workspace+$429,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$42a,y
		sta dycp_workspace+$42a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$42b,y
		sta dycp_workspace+$42b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$42c,y
		sta dycp_workspace+$42c,y

dd3_char_17	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$17
		beq dd3_char_18
		lda dycp_xfont+$000,x
		ora dycp_workspace+$458,y
		sta dycp_workspace+$458,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$459,y
		sta dycp_workspace+$459,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$45a,y
		sta dycp_workspace+$45a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$45b,y
		sta dycp_workspace+$45b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$45c,y
		sta dycp_workspace+$45c,y

dd3_char_18	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$18
		beq dd3_char_19
		lda dycp_xfont+$000,x
		ora dycp_workspace+$488,y
		sta dycp_workspace+$488,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$489,y
		sta dycp_workspace+$489,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$48a,y
		sta dycp_workspace+$48a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$48b,y
		sta dycp_workspace+$48b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$48c,y
		sta dycp_workspace+$48c,y

dd3_char_19	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$19
		beq dd3_char_1a
		lda dycp_xfont+$000,x
		ora dycp_workspace+$4b8,y
		sta dycp_workspace+$4b8,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$4b9,y
		sta dycp_workspace+$4b9,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$4ba,y
		sta dycp_workspace+$4ba,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$4bb,y
		sta dycp_workspace+$4bb,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$4bc,y
		sta dycp_workspace+$4bc,y

dd3_char_1a	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$1a
		beq dd3_char_1b
		lda dycp_xfont+$000,x
		ora dycp_workspace+$4e8,y
		sta dycp_workspace+$4e8,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$4e9,y
		sta dycp_workspace+$4e9,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$4ea,y
		sta dycp_workspace+$4ea,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$4eb,y
		sta dycp_workspace+$4eb,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$4ec,y
		sta dycp_workspace+$4ec,y

dd3_char_1b	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$1b
		beq dd3_char_1c
		lda dycp_xfont+$000,x
		ora dycp_workspace+$518,y
		sta dycp_workspace+$518,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$519,y
		sta dycp_workspace+$519,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$51a,y
		sta dycp_workspace+$51a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$51b,y
		sta dycp_workspace+$51b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$51c,y
		sta dycp_workspace+$51c,y

dd3_char_1c	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$1c
		beq dd3_char_1d
		lda dycp_xfont+$000,x
		ora dycp_workspace+$548,y
		sta dycp_workspace+$548,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$549,y
		sta dycp_workspace+$549,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$54a,y
		sta dycp_workspace+$54a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$54b,y
		sta dycp_workspace+$54b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$54c,y
		sta dycp_workspace+$54c,y

dd3_char_1d	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$1d
		beq dd3_char_1e
		lda dycp_xfont+$000,x
		ora dycp_workspace+$578,y
		sta dycp_workspace+$578,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$579,y
		sta dycp_workspace+$579,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$57a,y
		sta dycp_workspace+$57a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$57b,y
		sta dycp_workspace+$57b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$57c,y
		sta dycp_workspace+$57c,y

dd3_char_1e	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$1e
		beq dd3_char_1f
		lda dycp_xfont+$000,x
		ora dycp_workspace+$5a8,y
		sta dycp_workspace+$5a8,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$5a9,y
		sta dycp_workspace+$5a9,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$5aa,y
		sta dycp_workspace+$5aa,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$5ab,y
		sta dycp_workspace+$5ab,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$5ac,y
		sta dycp_workspace+$5ac,y

dd3_char_1f	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$1f
		beq dd3_char_20
		lda dycp_xfont+$000,x
		ora dycp_workspace+$5d8,y
		sta dycp_workspace+$5d8,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$5d9,y
		sta dycp_workspace+$5d9,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$5da,y
		sta dycp_workspace+$5da,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$5db,y
		sta dycp_workspace+$5db,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$5dc,y
		sta dycp_workspace+$5dc,y

dd3_char_20	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$20
		beq dd3_char_21
		lda dycp_xfont+$000,x
		ora dycp_workspace+$608,y
		sta dycp_workspace+$608,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$609,y
		sta dycp_workspace+$609,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$60a,y
		sta dycp_workspace+$60a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$60b,y
		sta dycp_workspace+$60b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$60c,y
		sta dycp_workspace+$60c,y

dd3_char_21	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$21
		beq dd3_char_22
		lda dycp_xfont+$000,x
		sta dycp_workspace+$638,y
		ora dycp_workspace+$638,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$639,y
		sta dycp_workspace+$639,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$63a,y
		sta dycp_workspace+$63a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$63b,y
		sta dycp_workspace+$63b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$63c,y
		sta dycp_workspace+$63c,y

dd3_char_22	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$22
		beq dd3_char_23
		lda dycp_xfont+$000,x
		ora dycp_workspace+$668,y
		sta dycp_workspace+$668,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$669,y
		sta dycp_workspace+$669,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$66a,y
		sta dycp_workspace+$66a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$66b,y
		sta dycp_workspace+$66b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$66c,y
		sta dycp_workspace+$66c,y

dd3_char_23	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$23
		beq dd3_char_24
		lda dycp_xfont+$000,x
		ora dycp_workspace+$698,y
		sta dycp_workspace+$698,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$699,y
		sta dycp_workspace+$699,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$69a,y
		sta dycp_workspace+$69a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$69b,y
		sta dycp_workspace+$69b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$69c,y
		sta dycp_workspace+$69c,y

dd3_char_24	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$24
		beq dd3_char_25
		lda dycp_xfont+$000,x
		ora dycp_workspace+$6c8,y
		sta dycp_workspace+$6c8,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$6c9,y
		sta dycp_workspace+$6c9,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$6ca,y
		sta dycp_workspace+$6ca,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$6cb,y
		sta dycp_workspace+$6cb,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$6cc,y
		sta dycp_workspace+$6cc,y

dd3_char_25	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$25
		beq dd3_char_26
		lda dycp_xfont+$000,x
		ora dycp_workspace+$6f8,y
		sta dycp_workspace+$6f8,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$6f9,y
		sta dycp_workspace+$6f9,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$6fa,y
		sta dycp_workspace+$6fa,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$6fb,y
		sta dycp_workspace+$6fb,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$6fc,y
		sta dycp_workspace+$6fc,y

dd3_char_26	lda irq_store_1
		clc
		adc #cos_offset_3
		sta irq_store_1
		tax
		ldy dycp_cosinus,x
		ldx dycp_buffer_3+$26
		beq dd3_char_out
		lda dycp_xfont+$000,x
		ora dycp_workspace+$728,y
		sta dycp_workspace+$728,y
		lda dycp_xfont+$100,x
		ora dycp_workspace+$729,y
		sta dycp_workspace+$729,y
		lda dycp_xfont+$200,x
		ora dycp_workspace+$72a,y
		sta dycp_workspace+$72a,y
		lda dycp_xfont+$300,x
		ora dycp_workspace+$72b,y
		sta dycp_workspace+$72b,y
		lda dycp_xfont+$400,x
		ora dycp_workspace+$72c,y
		sta dycp_workspace+$72c,y

dd3_char_out

; Update the character screen as needed
		ldx cos_at_2
!set char_cnt=$00
!do {
		lda line_cache+char_cnt
		cpx #$80
		bcs *+$04
		lda #$00
		sta $5de0+char_cnt
		txa
		clc
		adc #cos_offset_2
		tax

		!set char_cnt=char_cnt+$01
} until char_cnt=$27

		rts


; Self mod code for the second font builder
dycp_font_rd	lda dycp_font
		inc dycp_font_rd+$01
		bne *+$05
		inc dycp_font_rd+$02
		rts

; Scroller self mod reset - scroller 1
reset_1		lda #<scroll_text_1
		sta mread_1+$01
		lda #>scroll_text_1
		sta mread_1+$02
		rts

; Scroller self mod reset - scroller 2
reset_2		lda #<scroll_text_2
		sta mread_2+$01
		lda #>scroll_text_2
		sta mread_2+$02
		rts

; Scroller self mod reset - scroller 3
reset_3		lda #<scroll_text_3
		sta mread_3+$01
		lda #>scroll_text_3
		sta mread_3+$02
		rts


; Cosine curve for the DYCP scrollers
		* = ((*/$100)+1)*$100
dycp_cosinus	!byte $23,$23,$23,$23,$23,$23,$23,$23
		!byte $23,$23,$23,$23,$23,$23,$22,$22
		!byte $22,$22,$22,$22,$21,$21,$21,$21
		!byte $20,$20,$20,$20,$1f,$1f,$1f,$1f
		!byte $1e,$1e,$1e,$1d,$1d,$1d,$1c,$1c
		!byte $1b,$1b,$1b,$1a,$1a,$1a,$19,$19
		!byte $18,$18,$18,$17,$17,$16,$16,$15
		!byte $15,$15,$14,$14,$13,$13,$12,$12

		!byte $11,$11,$11,$10,$10,$0f,$0f,$0e
		!byte $0e,$0e,$0d,$0d,$0c,$0c,$0b,$0b
		!byte $0b,$0a,$0a,$09,$09,$09,$08,$08
		!byte $07,$07,$07,$06,$06,$06,$05,$05
		!byte $05,$04,$04,$04,$04,$03,$03,$03
		!byte $03,$02,$02,$02,$02,$01,$01,$01
		!byte $01,$01,$01,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00

		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$01,$01
		!byte $01,$01,$01,$01,$02,$02,$02,$02
		!byte $03,$03,$03,$03,$04,$04,$04,$05
		!byte $05,$05,$05,$06,$06,$06,$07,$07
		!byte $08,$08,$08,$09,$09,$09,$0a,$0a
		!byte $0b,$0b,$0c,$0c,$0c,$0d,$0d,$0e
		!byte $0e,$0f,$0f,$0f,$10,$10,$11,$11

		!byte $12,$12,$12,$13,$13,$14,$14,$15
		!byte $15,$16,$16,$16,$17,$17,$18,$18
		!byte $18,$19,$19,$1a,$1a,$1a,$1b,$1b
		!byte $1c,$1c,$1c,$1d,$1d,$1d,$1e,$1e
		!byte $1e,$1f,$1f,$1f,$1f,$20,$20,$20
		!byte $21,$21,$21,$21,$21,$22,$22,$22
		!byte $22,$22,$22,$23,$23,$23,$23,$23
		!byte $23,$23,$23,$23,$23,$23,$23,$23

; Cosine curve for the logos
logo_cosinus	!byte $1f,$1f,$1f,$1f,$1f,$1f,$1f,$1f
		!byte $1f,$1f,$1f,$1f,$1f,$1f,$1f,$1e
		!byte $1e,$1e,$1e,$1e,$1e,$1d,$1d,$1d
		!byte $1d,$1d,$1c,$1c,$1c,$1c,$1b,$1b
		!byte $1b,$1b,$1a,$1a,$1a,$19,$19,$19
		!byte $18,$18,$18,$17,$17,$17,$16,$16
		!byte $16,$15,$15,$14,$14,$14,$13,$13
		!byte $13,$12,$12,$11,$11,$11,$10,$10

		!byte $0f,$0f,$0f,$0e,$0e,$0e,$0d,$0d
		!byte $0c,$0c,$0c,$0b,$0b,$0a,$0a,$0a
		!byte $09,$09,$09,$08,$08,$08,$07,$07
		!byte $07,$06,$06,$06,$05,$05,$05,$04
		!byte $04,$04,$04,$03,$03,$03,$03,$02
		!byte $02,$02,$02,$02,$01,$01,$01,$01
		!byte $01,$01,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00

		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$01
		!byte $01,$01,$01,$01,$01,$02,$02,$02
		!byte $02,$02,$03,$03,$03,$03,$04,$04
		!byte $04,$05,$05,$05,$05,$06,$06,$06
		!byte $07,$07,$07,$08,$08,$08,$09,$09
		!byte $09,$0a,$0a,$0b,$0b,$0b,$0c,$0c
		!byte $0c,$0d,$0d,$0e,$0e,$0e,$0f,$0f

		!byte $10,$10,$10,$11,$11,$12,$12,$12
		!byte $13,$13,$13,$14,$14,$15,$15,$15
		!byte $16,$16,$16,$17,$17,$17,$18,$18
		!byte $18,$19,$19,$19,$1a,$1a,$1a,$1b
		!byte $1b,$1b,$1b,$1c,$1c,$1c,$1c,$1d
		!byte $1d,$1d,$1d,$1d,$1e,$1e,$1e,$1e
		!byte $1e,$1e,$1f,$1f,$1f,$1f,$1f,$1f
		!byte $1f,$1f,$1f,$1f,$1f,$1f,$1f,$1f

; Data for the logo swinger to use
logo_d018	!byte $c8,$40,$78,$18
logo_dd00	!byte $c4,$c4,$c6,$c7


; Here comes the unreadable bit...!
scroll_text_1	!scr "another month and another little emission from cosine, "
		!scr "this time replicating the dycp scroll wrapping around "
		!scr "some vertical splits from the end part of spirit's demo "
		!scr $22,"spirited",$22,"... now with extra dycp.   cosine - "
		!scr "giving you more dycp for your money and delivering on "
		!scr "time - just!"
		!scr "        "

		!scr "yeah, this one is being released quite close to the wire "
		!scr "again but in my defence february is a short month!"
		!scr "        "

		!scr "i do love a good dycp...   this one doesn't go anywhere "
		!scr "near the existing records of course, but i don't remember "
		!scr "doing a traditional six characters (or to be strictly "
		!scr "accurate, five characters for ",$22,"design",$22," "
		!scr "reasons) high routine on the c64;  there is one in "
		!scr "radiant on the plus/4, but this is a more optimal "
		!scr "routine as well."
		!scr "        "

		!scr "there isn't much point in writing a huge text for this "
		!scr "scroller (and let's face it, i'd struggle with that anyway) "
		!scr "because having three going at once makes them hard to "
		!scr "read, but the source code at github contains the text if "
		!scr "anyone wants to read it at their leisure..."
		!scr "        "

		!scr "and with that in mind, i might was well wrap up here  -  "
		!scr "this was t.m.r of cosine on the 29th of february 2016, "
		!scr "bye until next time ladles and jellyspoons!"

		!scr "        "

		!byte $00


scroll_text_2	!scr "                    "
		!scr "cosine-powered waves ripple out towards:          "
		!scr "abyss connection         "
		!scr "arkanix labs         "
		!scr "artstate         "
		!scr "ate bit         "
		!scr "atlantis and f4cg         "
		!scr "booze design         "
		!scr "camelot         "
		!scr "chorus         "
		!scr "chrome         "
		!scr "cncd         "
		!scr "cpu         "
		!scr "crescent         "
		!scr "crest         "
		!scr "covert bitops         "
		!scr "defence force         "
		!scr "dekadence         "
		!scr "desire         "
		!scr "dac         "
		!scr "dmagic         "
		!scr "dualcrew         "
		!scr "exclusive on         "
		!scr "fairlight         "
		!scr "fire         "
		!scr "focus         "
		!scr "french touch         "
		!scr "funkscientist productions         "
		!scr "genesis project         "
		!scr "gheymaid inc.         "
		!scr "hitmen         "
		!scr "hokuto force         "
		!scr "level64         "
		!scr "maniacs of noise         "
		!scr "mayday         "
		!scr "meanteam         "
		!scr "metalvotze         "
		!scr "noname         "
		!scr "nostalgia         "
		!scr "nuance         "
		!scr "offence         "
		!scr "onslaught         "
		!scr "orb         "
		!scr "oxyron         "
		!scr "padua         "
		!scr "plush         "
		!scr "psytronik         "
		!scr "reptilia         "
		!scr "resource         "
		!scr "rgcd         "
		!scr "secure         "
		!scr "shape         "
		!scr "side b         "
		!scr "slash         "
		!scr "slipstream         "
		!scr "success and trc         "
		!scr "style         "
		!scr "suicyco industries         "
		!scr "taquart         "
		!scr "tempest         "
		!scr "tek         "
		!scr "triad         "
		!scr "trsi         "
		!scr "viruz         "
		!scr "vision         "
		!scr "wow         "
		!scr "wrath         "
		!scr "xenon         "
		!scr "and everybody we may have forgot - let us know if that's "
		!scr "the case!"
		!scr "          "
		!byte $00

scroll_text_3	!scr "                                      "
		!scr "cosine's monthly demo - february 2016"
		!scr "      "
		!scr "programming and graphics by t.m.r"
		!scr "          "
		!scr "music by sack, called ",$22,"here comes your man",$22

		!byte $00
