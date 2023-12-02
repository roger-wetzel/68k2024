; SPREADPOINT
; 68K INSIDE 2024 ANNOUNCEMENT (AMIGA, PAL, >= OCS, >= 68000, >= 512 KB)
; (C) 2023 DEPECHE

; Build with vasm
; vasmm68k_mot -kick1hunks -Fhunkexe -o 68k2024 -nosym 68k2024.s

AbsExecBase	equ	4
OldOpenLibrary	equ	-408
CloseLibrary	equ	-414
Write		equ	-48
Output		equ	-60
AvailMem	equ	-216
AllocMem	equ	-198
FreeMem		equ	-210
TypeOfMem	equ	-534
WaitTOF		equ	-270
Forbid		equ	-132
Permit		equ	-138
LoadView	equ	-222

custom		equ	$dff000

inviswidth	equ	24
viswidth	equ	40
pwidth		equ	viswidth+inviswidth
pheight		equ	256 ; px
safteyheight	equ	80 ; px (below bitplane)
psize		equ	pwidth*(pheight+safteyheight)
numplanes	equ	1

; profiling
availablemem	equ	0
numbers		equ	0
testing		equ	0
timing		equ	0
lsptiming	equ	0

; DMACON
; see http://coppershade.org/articles/Code/Reference/DMACON/
SET		equ	1<<15		; 0=clear, 1=set bits that are set to 1 below
BLTPRI		equ	1<<10		; Blitter DMA priority (over CPU micro) "blitter nasty"
DMAEN		equ	1<<9		; Enable all DMA below
BPLEN		equ	1<<8		; Bit plane DMA
COPEN		equ	1<<7		; Copper DMA
BLTEN		equ	1<<6		; Blitter DMA

*------	ALLOCATE MEMORY AND SAVE STATE -----------------------------------*

base	movem.l	a0-a6/d0-d7,-(a7)	;
	bsr	alloc			;
	bne	.exit			; out of memory error?

	if availablemem
	move.l	AbsExecBase.w,a6	;
	move.l	#MEMF_CHIP,d1		;
	jsr	AvailMem(a6)		;
	move.l	d0,$210.w		; Free (available) memory
	endif

	move.l	AbsExecBase.w,a6	;
	lea	.gfx(pc),a1		;
	jsr	OldOpenLibrary(a6)	; open gfx library
	move.l	d0,a6			;
	beq	.exit			; could not open gfx library
	move.l 	34(a6),-(a7)		; view
	move.l	d0,-(a7)		; gfx base
	move.l 	38(a6),-(a7)		; copper list 1
;	move.l 	50(a6),-(a7)		; copper list 2
	sub.l	a1,a1			;
	jsr	LoadView(a6)		;
	jsr	WaitTOF(a6)		;
	jsr	WaitTOF(a6)		;
	move.l	AbsExecBase.w,a6	;	
	jsr	Forbid(a6)		;
	
	lea	custom,a6		;
	bsr	waitblitter		;

	move.w	$02(a6),-(a7)		; store DMA control
	move.w	$1c(a6),-(a7)		; store interrupt enable bits
	move.l	$6c.w,-(a7)		; store irq3
	move.w	#$7fff,d0		;
	move.w	d0,$9a(a6)		;
	move.w	d0,$9c(a6)		; delete all interrupt requests
	move.w	d0,$96(a6)		; disable all DMAs

	clr.w	-(a7)			; store LED state
	btst	#1,$bfe001		;
	beq	.ledstate		;
	not.w	(a7)			;
.ledstate
	bset	#1,$bfe001		; LED dark

*------	INIT -------------------------------------------------------------*

	lea	vars(pc),a5		;
		
	lea	playcmds(pc),a0		; init player
	move.l	a0,v_cmdspointer(a5)	;
		
	bsr	lspinit			;
	bsr	generatedrawcharcode	;

	lea	text1(pc),a0		;
	move.l	b_text1(pc),a1		;
	move.w	#text1end-text1-1,d7	;
	bsr	precalctext		;

	lea	text2(pc),a0		;
	move.l	b_text2(pc),a1		;
	move.w	#text2end-text2-1,d7	;
	bsr	precalctext		;

	lea	text3(pc),a0		;
	move.l	b_text3(pc),a1		;
	move.w	#text3end-text3-1,d7	;
	bsr	precalctext		;

	lea	text4(pc),a0		;
	move.l	b_text4(pc),a1		;
	move.w	#text4end-text4-1,d7	;
	bsr	precalctext		;

	lea	text5(pc),a0		;
	move.l	b_text5(pc),a1		;
	move.w	#text5end-text5-1,d7	;
	bsr	precalctext		;

	lea	text6(pc),a0		;
	move.l	b_text6(pc),a1		;
	move.w	#text6end-text6-1,d7	;
	bsr	precalctext		;

	lea	texts(pc),a0		;
	move.l	b_textspiral(pc),a1	;
	move.w	#textsend-texts-1,d7	;
	bsr	precalctext		;

	lea	irq3(pc),a0		;
	move.l	a0,$6c.w		;

	bsr	waitraster		; avoid flickering (?)
	move.l	b_clist2(pc),$80(a6)	;

	move.w	#SET+DMAEN+BPLEN+BLTEN+COPEN,$96(a6) ; no BLTPRI
	move.w	#$c030,$9a(a6)		; enable coper and vertb interrupts

*------	IDLE LOOP --------------------------------------------------------*

.idle	btst	#10,$16(a6)		; right mouse button pressed?
	bne	.nopressed		;
	move.b	#3*entrysize,v_hidden(a5) ; show Lord's hidden text instead
.nopressed
	tst.w	v_quit(a5)		;
	beq	.idle			;
	
*------	RESTORE STATE AND EXIT -------------------------------------------*

	bsr	waitblitter		;
	
	tst.w	(a7)+			; restore state
	bne	.leddark		;
	bclr	#1,$bfe001		; LED bright
.leddark
	move.w	#$7fff,d0		;
	move.w	d0,$9a(a6)		;
	move.w	d0,$9c(a6)		;
	move.w	d0,$96(a6)		;
	
	moveq	#0,d0			; volume to zero
	move.w	d0,$a8(a6)		;
	move.w	d0,$b8(a6)		;
	move.w	d0,$c8(a6)		;
	move.w	d0,$d8(a6)		;
	
	move.l	(a7)+,$6c.w		; 
	move.w	(a7)+,d0		;
	or.w	#$c000,d0		;
	move.w	d0,$9a(a6)		;
	move.w	(a7)+,d0		;
	or.w	#$8000,d0		;
	move.w	d0,$96(a6)		;

;	move.l	(a7)+,$84(a6)		; copper list 2
	move.l	(a7)+,$80(a6)		; copper list 1
	move.l	(a7)+,a6		; gfx base
	move.l	(a7)+,a1		; view
	jsr	LoadView(a6)		;
	jsr	WaitTOF(a6)		;
	jsr	WaitTOF(a6)		;
	move.l	a6,a1			; parameter for CloseLibrary
	move.l	AbsExecBase.w,a6	;
	jsr	CloseLibrary(a6)	; close gfx library
	jsr	Permit(a6)		;

	bsr	dealloc			;
.exit	movem.l	(a7)+,a0-a6/d0-d7	;
	moveq	#0,d0			;
	rts				;

.gfx	dc.b	"graphics.library",0
	even


*------	VARS -------------------------------------------------------------*

	rsreset
v_quit		rs.w	1	; signal quit
v_actors	rs.w	1
v_cmdspointer	rs.l	1
v_fadecolindex	rs.w	0
v_x		rs.w	1	; do not change order
v_y		rs.w	1	; do not change order
v_textscreen	rs.l	1	; draw this text screen
v_db		rs.l	0	; (v_db is a marker only)
v_dbplane1a	rs.l	1	; do not change order a: active
v_dbplane1c	rs.l	1	; do not change order c: clearing
v_dbplane1b	rs.l	1	; do not change order b: buffer
v_draw		rs.l	1	; address of draw code (0 = nothing)
v_numletters	rs.w	1	; spiral

	if numbers
v_frame		rs.w	1	; frame counter
	endif

	if numbers&testing
v_number2	rs.l	1
	endif

v_wait		rs.b	1	; player wait time
v_hidden	rs.b	1	; offset to hidden text

sizeofvars	rs.w	0

vars	ds.b	sizeofvars,0
	even


*------ PRINT NUMBER ----------------------------------------------------------*

; d0.l: number, d1.w: pos
	if numbers
printnumber
	move.l	v_dbplane1b(a5),a0	;
	add.l	d1,a0			;

 	moveq	#8-1,d7			;
.loop	move.w	d0,d1			; number
	and.w	#$000f,d1		; mask digit out
	asl.w	#3,d1			; offset to font data
	lea	.digits(pc,d1.w),a1	;
	move.b	(a1)+,(a0)		; print digit
	move.b	(a1)+,pwidth(a0)	;
	move.b	(a1)+,2*pwidth(a0)	;
	move.b	(a1)+,3*pwidth(a0)	;
	move.b	(a1)+,4*pwidth(a0)	;
	asr.l	#4,d0			; next digit
	subq.w	#1,a0			; next x position
	dbf	d7,.loop		;
	rts				;

.digits	dc.b	%11111000	; 0
	dc.b	%10001000
	dc.b	%10001000
	dc.b	%10001000
	dc.b	%11111000
	ds.b	3,0

	dc.b	%00100000	; 1
	dc.b	%00100000
	dc.b	%00100000
	dc.b	%00100000
	dc.b	%00100000
	ds.b	3,0

	dc.b	%11111000	; 2
	dc.b	%00001000
	dc.b	%11111000
	dc.b	%10000000
	dc.b	%11111000
	ds.b	3,0

	dc.b	%11111000	; 3
	dc.b	%00001000
	dc.b	%11111000
	dc.b	%00001000
	dc.b	%11111000
	ds.b	3,0

	dc.b	%10001000	; 4
	dc.b	%10001000
	dc.b	%11111000
	dc.b	%00001000
	dc.b	%00001000
	ds.b	3,0

	dc.b	%11111000	; 5
	dc.b	%10000000
	dc.b	%11111000
	dc.b	%00001000
	dc.b	%11111000
	ds.b	3,0

	dc.b	%11111000	; 6
	dc.b	%10000000
	dc.b	%11111000
	dc.b	%10001000
	dc.b	%11111000
	ds.b	3,0

	dc.b	%11111000	; 7
	dc.b	%00001000
	dc.b	%00010000
	dc.b	%00100000
	dc.b	%00100000
	ds.b	3,0

	dc.b	%11111000	; 8
	dc.b	%10001000
	dc.b	%11111000
	dc.b	%10001000
	dc.b	%11111000
	ds.b	3,0

	dc.b	%11111000	; 9
	dc.b	%10001000
	dc.b	%11111000
	dc.b	%00001000
	dc.b	%11111000
	ds.b	3,0

	dc.b	%11111000	; A
	dc.b	%10001000
	dc.b	%11111000
	dc.b	%10001000
	dc.b	%10001000
	ds.b	3,0

	dc.b	%11110000	; B
	dc.b	%10001000
	dc.b	%11110000
	dc.b	%10001000
	dc.b	%11110000
	ds.b	3,0

	dc.b	%11111000	; C
	dc.b	%10000000
	dc.b	%10000000
	dc.b	%10000000
	dc.b	%11111000
	ds.b	3,0

	dc.b	%11110000	; D
	dc.b	%10001000
	dc.b	%10001000
	dc.b	%10001000
	dc.b	%11110000
	ds.b	3,0

	dc.b	%11111000	; E
	dc.b	%10000000
	dc.b	%11111000
	dc.b	%10000000
	dc.b	%11111000
	ds.b	3,0

	dc.b	%11111000	; F
	dc.b	%10000000
	dc.b	%11111000
	dc.b	%10000000
	dc.b	%10000000
	ds.b	3,0

	endif


*------	WAIT BLITTER -----------------------------------------------------*

; http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0123.html
waitblitter
	btst.b	#14-8,$02(a6)			; DMAB_BLTDONE = 14
.wait	btst.b	#14-8,$02(a6)			;
	bne	.wait				;
	rts					;


*------	WAIT RASTER ------------------------------------------------------*

waitraster
	cmp.b	#1,$06(a6)			;
	bne	waitraster			;
	btst	#0,$05(a6)			;
	bne	waitraster			;
	rts					;


*------	GENERATE DRAW CHAR CODE ------------------------------------------*

;	0090 0000 0001		or.l	#$00000001,(a0)
;	00a8 aaaa aaaa 03e8	or.l	#$aaaaaaaa,$03e8(a0)
;	00a8 0000 001f 5000	or.l	#$0000001f,$5000(a0) -> or.w #$001f,$4ffe(a0)
;	4e75			rts

;	0050 1234		or.w	#$1234,(a0)
;	0068 5678 0200		or.w	#$5678,$0200(a0)

charheight	equ	9
onecharcodesize	equ	64

generatedrawcharcode
	lea	font(pc),a3			; source
	move.l	b_drawcharcode(pc),a1		; destination
	moveq	#(fontend-font)/18-1,d6		; num chars (9 lines * 2 bytes = 18)
.loopchars
	moveq	#0,d1				; shift
.shiftloop
	move.l	a3,a0				; start data of char
	move.l	a1,a2				; destination
	moveq	#0,d2				; bitplane row offset
	
	moveq	#charheight-1,d7		;
.rowloop
	moveq	#0,d0				;
	move.w	(a0)+,d0			; row pixel data of char
	beq	.rowisclear			; stop here because this row is clear
	swap	d0				;
	lsr.l	d1,d0				;
	move.l	d0,d3				; temp (used below)
	tst.w	d2				; no bitplane row offset -> shorter code
	bne	.notfirstrow			;
	
	tst.w	d3				;
	bne	.long				;
	swap	d3				;
	move.w	#$0050,(a2)+			; "or.w #d,(a0)"
	move.w	d3,(a2)+			; d
	bra	.continue			;
	
.long	move.w	#$0090,(a2)+			; "or.l #a,(a0)"
	move.l	d0,(a2)+			; a
	bra	.continue			;

.notfirstrow
	tst.w	d3				;
	bne	.long2				;
	swap	d3				;
	move.w	#$0068,(a2)+			; "or.w #e,f(a0)"
	move.w	d3,(a2)+			; e
	move.w	d2,(a2)+			; f
	bra	.continue			;

.long2	move.l	d0,d3				; e.g. d0=$00001234
	swap	d3				; 
	tst.w	d3				; upper word empty? or.w is enough
	bne	.long3				;
	move.w	#$0068,(a2)+			; "or.w #e,f(a0)"
	move.w	d0,(a2)+			;
	addq.w	#2,d2				; adjust offset
	move.w	d2,(a2)+			;
	subq.w	#2,d2				; adjust offset
	bra	.continue			;	

.long3
	move.w	#$00a8,(a2)+			; "or.l #b,c(a0)"
	move.l	d0,(a2)+			; b
	move.w	d2,(a2)+			; c
.rowisclear
.continue
	add.w	#pwidth,d2			;
	dbf	d7,.rowloop			;
	
	move.w	#$4e75,(a2)+			; "rts"

	add.w	#onecharcodesize,a1		; same char, next shift

	addq.w	#1,d1				; .w instead of .l is ok
	cmp.w	#16,d1				; done?
	bne	.shiftloop			;
	
	add.w	#charheight*2,a3		; source of next char (8 words)
	dbf	d6,.loopchars			;
	rts					;


*------	FONT HIRES -------------------------------------------------------*

	rem
	
	NOT USED because A500 is too slow in hires
	
font	dc.w	%0000001111000000	; index 0
	dc.w	%0000001111000000
	dc.w	%0000001111000000
	dc.w	%0000001111000000
	dc.w	%0000001111000000
	dc.w	%0000000000000000
	dc.w	%0000001111000000
	dc.w	%0000001111000000

	dc.w	%0000001111000000
	dc.w	%0000001111000000
	dc.w	%0000000011000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0000000011100000
	dc.w	%0000001111000000
	dc.w	%0000011100000000
	dc.w	%0000011100000000
	dc.w	%0000011100000000
	dc.w	%0000011100000000
	dc.w	%0000001111000000
	dc.w	%0000000011100000

	dc.w	%0000111000000000
	dc.w	%0000011110000000
	dc.w	%0000000111000000
	dc.w	%0000000111000000
	dc.w	%0000000111000000
	dc.w	%0000000111000000
	dc.w	%0000011110000000
	dc.w	%0000111000000000

	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000011110000000
	dc.w	%0000011110000000
	dc.w	%0000000110000000

	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0011111111110000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000

	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000011110000000
	dc.w	%0000011110000000

	dc.w	%0000000000011110
	dc.w	%0000000001111100
	dc.w	%0000000111110000
	dc.w	%0000011111000000
	dc.w	%0001111100000000
	dc.w	%0111110000000000
	dc.w	%1111000000000000
	dc.w	%1100000000000000

	dc.w	%0111111111111000 ; 0	index 8
	dc.w	%1111111111111110
	dc.w	%0000000000000000
	dc.w	%1111000000011110
	dc.w	%1111000000011110
	dc.w	%1111000000011110
	dc.w	%1111111111111110
	dc.w	%0111111111111100

	dc.w	%0000000000000000
	dc.w	%0001111110000000
	dc.w	%0000000000000000
	dc.w	%0000011110000000
	dc.w	%0000011110000000
	dc.w	%0000011110000000
	dc.w	%0000011110000000
	dc.w	%0000011110000000

	dc.w	%0111111111111110
	dc.w	%1111111111111110
	dc.w	%0000000000000000
	dc.w	%0000000000011110
	dc.w	%1111111111111100
	dc.w	%1111000000000000
	dc.w	%1111111111111110
	dc.w	%1111111111111110

	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%0000000000000000
	dc.w	%0000111111111110
	dc.w	%0000000000011110
	dc.w	%0000000000011110
	dc.w	%1111111111111110
	dc.w	%1111111111111110

	dc.w	%1111000000011110
	dc.w	%1111000000011110
	dc.w	%0000000000000000
	dc.w	%1111111111111110
	dc.w	%0000000000011110
	dc.w	%0000000000011110
	dc.w	%0000000000011110
	dc.w	%0000000000011110

	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%0000000000000000
	dc.w	%1111111111111100
	dc.w	%0000000000011110
	dc.w	%0000000000011110
	dc.w	%1111111111111110
	dc.w	%1111111111111100

	dc.w	%0111111111111100
	dc.w	%1111111111111110
	dc.w	%0000000000000000
	dc.w	%1111011111111110
	dc.w	%1111000000001110
	dc.w	%1111000000001110
	dc.w	%1111111111111110
	dc.w	%0111111111111100

	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%0000000000000000
	dc.w	%0000000001111000
	dc.w	%0000000011110000
	dc.w	%0000000111100000
	dc.w	%0000001111000000
	dc.w	%0000011110000000

	dc.w	%0111111111111100
	dc.w	%1111111111111110
	dc.w	%0000000000000000
	dc.w	%0111111111111100
	dc.w	%1111000000011110
	dc.w	%1111000000011110
	dc.w	%1111111111111110
	dc.w	%0111111111111100

	dc.w	%0111111111111100
	dc.w	%1111111111111110
	dc.w	%1110000000011110
	dc.w	%1111111111111100
	dc.w	%0000000000011110
	dc.w	%0000000000011110
	dc.w	%1111111111111110
	dc.w	%0111111111111100

	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000011110000000
	dc.w	%0000000000000000
	dc.w	%0000011110000000
	dc.w	%0000011110000000
	dc.w	%0000000000000000

	dc.w	%0000011111111110 ; A
	dc.w	%0001111111111110
	dc.w	%0111110000000000
	dc.w	%1111000000011110
	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%1111000000011110
	dc.w	%1111000000011110

	dc.w	%1111111111111100	; index 20
	dc.w	%1111111111111110
	dc.w	%0000000000011110
	dc.w	%1111111111111100
	dc.w	%1111000000011110
	dc.w	%1111000000011110
	dc.w	%1111111111111110
	dc.w	%1111111111111100

	dc.w	%0000011111110000
	dc.w	%0001111111111100
	dc.w	%0000000000011100
	dc.w	%1111000000000000
	dc.w	%1111000000000000
	dc.w	%0111110000011100
	dc.w	%0001111111111100
	dc.w	%0000011111110000

	dc.w	%1111111100000000
	dc.w	%1111111111000000
	dc.w	%0000000011110000
	dc.w	%1111000000111100
	dc.w	%1111000000011100
	dc.w	%1111000001111000
	dc.w	%1111111111100000
	dc.w	%1111111110000000

	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%0000000000000000
	dc.w	%1111111111000000
	dc.w	%1111111111000000
	dc.w	%1111000000000000
	dc.w	%1111111111111110
	dc.w	%1111111111111110

	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%0000000000000000
	dc.w	%1111111111000000
	dc.w	%1111111111000000
	dc.w	%1111000000000000
	dc.w	%1111000000000000
	dc.w	%1111000000000000

	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%1111000000000000
	dc.w	%1111000001111110
	dc.w	%1111000000011110
	dc.w	%1111000000011110
	dc.w	%1111111111111110
	dc.w	%1111111111111110

	dc.w	%1111000000111100
	dc.w	%1111000000111100
	dc.w	%0000000000000000
	dc.w	%1111000000111100
	dc.w	%1111111111111100
	dc.w	%1111000000111100
	dc.w	%1111000000111100
	dc.w	%1111000000111100

	dc.w	%0000011110000000
	dc.w	%0000011110000000
	dc.w	%0000000000000000
	dc.w	%0000011110000000
	dc.w	%0000011110000000
	dc.w	%0000011110000000
	dc.w	%0000011110000000
	dc.w	%0000011110000000

	dc.w	%0000000000011110
	dc.w	%0000000000011110
	dc.w	%0000000000000000
	dc.w	%0000000000011110
	dc.w	%0000000000011110
	dc.w	%1111000000111110
	dc.w	%1111111111111000
	dc.w	%1111111111100000

	dc.w	%1111000000011110
	dc.w	%1111000001111100
	dc.w	%0000000111110000
	dc.w	%1111111111000000
	dc.w	%1111111111000000
	dc.w	%1111000111110000
	dc.w	%1111000001111100
	dc.w	%1111000000011110

	dc.w	%1111000000000000	; index 30
	dc.w	%1111000000000000
	dc.w	%0000000000000000
	dc.w	%1111000000000000
	dc.w	%1111000000000000
	dc.w	%1111000000000000
	dc.w	%1111111111111110
	dc.w	%1111111111111110

	dc.w	%1110000000001110
	dc.w	%1111100000111110
	dc.w	%0000000000000000
	dc.w	%1111111111111110
	dc.w	%1111011111011110
	dc.w	%1111000100011110
	dc.w	%1111000000011110
	dc.w	%1111000000011110

	dc.w	%1111000000011110
	dc.w	%1111100000011110
	dc.w	%1111111000000000
	dc.w	%1111111110011110
	dc.w	%1111001111111110
	dc.w	%1111000011111110
	dc.w	%1111000000111110
	dc.w	%1111000000011110

	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%0000000000000000
	dc.w	%1111000000011110
	dc.w	%1111000000011110
	dc.w	%1111000000011110
	dc.w	%1111111111111110
	dc.w	%1111111111111110

	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%0000000000001110
	dc.w	%1111000000001110
	dc.w	%1111111111111110
	dc.w	%1111000000000000
	dc.w	%1111000000000000
	dc.w	%1111000000000000

	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%0000000000000000
	dc.w	%1111000000111110
	dc.w	%1111000011111000
	dc.w	%1111001111100000
	dc.w	%1111111111111110
	dc.w	%1111111111111110

	dc.w	%0000011111111110
	dc.w	%0001111111111110
	dc.w	%0111110000000000
	dc.w	%1111000000001110
	dc.w	%1111111111111110
	dc.w	%1111000111100000
	dc.w	%1111000001111000
	dc.w	%1111000000011110

	dc.w	%1111111111111110
	dc.w	%1111111111111110
	dc.w	%0000000000000000
	dc.w	%1111111111110000
	dc.w	%0011111111111100
	dc.w	%0000000000011110
	dc.w	%1111111111111110
	dc.w	%1111111111111110

	dc.w	%1111111111111100
	dc.w	%1111111111111100
	dc.w	%0000000000000000
	dc.w	%0000011110000000
	dc.w	%0000011110000000
	dc.w	%0000011110000000
	dc.w	%0000011110000000
	dc.w	%0000011110000000

	dc.w	%1111000000011110
	dc.w	%1111000000011110
	dc.w	%0000000000000000
	dc.w	%1111000000011110
	dc.w	%1111000000011110
	dc.w	%1111000000011110
	dc.w	%1111111111111110
	dc.w	%1111111111111110

	dc.w	%1111000000011110
	dc.w	%1111000000011110
	dc.w	%0000000000000000
	dc.w	%1111000001111000
	dc.w	%1111000111100000
	dc.w	%1111011110000000
	dc.w	%1111111000000000
	dc.w	%1111100000000000

	dc.w	%1111000000011110
	dc.w	%1111000000011110
	dc.w	%0000000000000000
	dc.w	%1111000100011110
	dc.w	%1111011111011110
	dc.w	%1111111111111110
	dc.w	%1111110001111110
	dc.w	%1111000000011110

	dc.w	%1111000000011110
	dc.w	%1111000000011110
	dc.w	%0000000000000000
	dc.w	%0001111111110000
	dc.w	%0001111111110000
	dc.w	%1111100000111110
	dc.w	%1111000000011110
	dc.w	%1111000000011110

	dc.w	%1111000000111100
	dc.w	%1111000000111100
	dc.w	%0000000000000000
	dc.w	%0001110011100000
	dc.w	%0000011110000000
	dc.w	%0000011110000000
	dc.w	%0000011110000000
	dc.w	%0000011110000000

	dc.w	%1111111111111110 ; Z was empty. Made one.
	dc.w	%1111111111111110
	dc.w	%0000000000000000
	dc.w	%0001111111111110
	dc.w	%0011111111111100
	dc.w	%1111000000000000
	dc.w	%1111111111111110
	dc.w	%1111111111111110

	dc.w	%0000000000000000	; a
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0001111111111110
	dc.w	%0000000000001110
	dc.w	%1111111111101110
	dc.w	%1110000000001110
	dc.w	%1111111111111110

	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110111111111110
	dc.w	%1110000000000110
	dc.w	%1110000000000110
	dc.w	%1110000000000110
	dc.w	%1111111111111110

	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1111111111111100
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1111111111111110

	dc.w	%0000000000011100
	dc.w	%0000000000011100
	dc.w	%0000000000011100
	dc.w	%1111111111011100
	dc.w	%1110000000011100
	dc.w	%1110000000011100
	dc.w	%1110000000011100
	dc.w	%1111111111111100

	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1111111111111110
	dc.w	%1110000000000000
	dc.w	%1110111111111110
	dc.w	%1110000000000000
	dc.w	%1111111111111110

	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0001111111111100
	dc.w	%0001110000000000
	dc.w	%1111110111111100
	dc.w	%0001110000000000
	dc.w	%0001110000000000

	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1111111111111110
	dc.w	%1110000000001110
	dc.w	%1111111111101110
	dc.w	%0000000000001110
	dc.w	%1111111111111110

	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110111111111110
	dc.w	%1110000000001110
	dc.w	%1110000000001110
	dc.w	%1110000000001110
	dc.w	%1110000000001110

	dc.w	%0000000000000000
	dc.w	%0000001110000000
	dc.w	%0000000000000000
	dc.w	%0000001110000000
	dc.w	%0000001110000000
	dc.w	%0000001110000000
	dc.w	%0000001110000000
	dc.w	%0000001110000000

	dc.w	%0000000000000000
	dc.w	%0000000000011100
	dc.w	%0000000000000000
	dc.w	%0000000000011100
	dc.w	%0000000000011100
	dc.w	%1110000000011100
	dc.w	%1110000000011100
	dc.w	%1111111111111100

	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000111111110
	dc.w	%1110011100000000
	dc.w	%1110011100000000
	dc.w	%1110000111000000
	dc.w	%1110000001111110

	dc.w	%0000001110000000
	dc.w	%0000001110000000
	dc.w	%0000001110000000
	dc.w	%0000001110000000
	dc.w	%0000001110000000
	dc.w	%0000001110000000
	dc.w	%0000001110000000
	dc.w	%0000001110000000

	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1111111011111110
	dc.w	%1110011011001110
	dc.w	%1110011111001110
	dc.w	%1110000000001110
	dc.w	%1110000000001110

	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1111111111111100
	dc.w	%1110000000001110
	dc.w	%1110000000001110
	dc.w	%1110000000001110
	dc.w	%1110000000001110

	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1111111111111110
	dc.w	%0000000000001110
	dc.w	%1110000000001110
	dc.w	%1110000000001110
	dc.w	%1111111111111110

	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1111111111111110
	dc.w	%1110000000001110
	dc.w	%1110111111111110
	dc.w	%1110000000000000
	dc.w	%1110000000000000

	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0111111111111110
	dc.w	%0111000000001110
	dc.w	%0111111111101110
	dc.w	%0000000000001110
	dc.w	%0000000000001110

	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1111111111111110
	dc.w	%1110000000001110
	dc.w	%1110000000000000
	dc.w	%1110000000000000
	dc.w	%1110000000000000

	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1111111111111110
	dc.w	%1110000000000000
	dc.w	%1111111111111110
	dc.w	%0000000000001110
	dc.w	%1111111111111110

	dc.w	%0000000000000000
	dc.w	%0001110000000000
	dc.w	%0001110000000000
	dc.w	%1111111110000000
	dc.w	%0001110000000000
	dc.w	%0001110000000000
	dc.w	%0001110000000000
	dc.w	%0001111111111110

	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1110000000001110
	dc.w	%1110000000001110
	dc.w	%1110000000001110
	dc.w	%1110000000001110
	dc.w	%0111111111111110

	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1110000000001110
	dc.w	%1110000000001110
	dc.w	%0011100000111000
	dc.w	%0000111011100000
	dc.w	%0000001110000000

	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1110000000001110
	dc.w	%1110000100001110
	dc.w	%1110011111001110
	dc.w	%1111110001111110
	dc.w	%1111000000011110

	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1111100000111110
	dc.w	%0000111011100000
	dc.w	%0000001110000000
	dc.w	%0000111011100000
	dc.w	%1111100000111110

	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1110000000001110
	dc.w	%1110000000001110
	dc.w	%0111111111111110
	dc.w	%0000000000001110
	dc.w	%0011111111111110

	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1111111111111110
	dc.w	%0000000001111000
	dc.w	%0000011110000000
	dc.w	%0111100000000000
	dc.w	%1111111111111110
fontend
	erem


*------	TEXT -------------------------------------------------------------*

text1	dc.b	"- -- --- --------- --- -- -"
	dc.b	"                           "
	dc.b	"    6 8 K   I N S I D E    "
	dc.b	"          2 0 2 4          "
	dc.b	"                           "
	dc.b	"- -- --- --------- --- -- -"
	dc.b	"                           "
	dc.b	"   24th to 26th May 2024   "
	dc.b	"                           "
	dc.b	"   Hameenlinna, Finland    "
	dc.b	"                           "
	dc.b	"- -- --- --------- --- -- -"
	dc.b	"                           "
	dc.b	" https://68k-inside.party  "
	dc.b	"                           "
	dc.b	"- -- --- --------- --- -- -"
	dc.b	"                           "
text1end
	
text2	dc.b	"   Dekadence once again    "
	dc.b	"  brings you the hottest   "
	dc.b	"  party dedicated to all   "
	dc.b	"  machines based on the    "
	dc.b	"  venerable 68000 CPU and  "
	dc.b	"     its descendants!      "
	dc.b	"                           "
	dc.b	"    We've got the usual    "
	dc.b	"   competitions, a very    "
	dc.b	"   pleasant countryside    "
	dc.b	"  location with sauna and  "
	dc.b	"  grilling, and of course  "
	dc.b	" a great party atmosphere! "
	dc.b	"                           "
	dc.b	"Visit 68k-inside.party for "
	dc.b	"info, signups will be open "
	dc.b	"        very soon!         "
text2end

text3	dc.b	"  To give you an idea of   "
	dc.b	"which platforms we support "
	dc.b	"  here's a partial list:   "
	dc.b	"                           "
	dc.b	"                           "
	dc.b	"  Amiga 500, Atari ST(e),  "
	dc.b	"classic Macs, Sharp X68000,"
	dc.b	" Sega Mega Drive, Neo Geo, "
;	dc.b	" Amiga 1200, Atari Falcon, "
	dc.b	"    NeXT, Atari Falcon,    "
	dc.b	"    Sinclair QL, TI-89     "
	dc.b	"                           "
	dc.b	"                           "
	dc.b	" If it has a 68k-based CPU,"
	dc.b	"we wanna see a demo for it!"
	dc.b	"                           "
	dc.b	"                           "
	dc.b	"      See you in May!      "
text3end

texts	dc.b	"Spreadpoint / AFWD / Dekadence send greetings to: "
	dc.b	"Batman Group  AmigaBill  FFP  "
	dc.b	"TEK  mcCoy  "
	dc.b	"Melon  ATW  Desire  PS  "
	dc.b	"SCA  Scoopex  Artstate  "
	dc.b	"Scoopex  Artstate  DHS  "
	dc.b	"Defekt  SFL  "  
	dc.b	"TRSI  Spaceballs  Lemon.  Loonies  "
	dc.b	"Gasman  Slipstream  RAB  TBL  "
	dc.b	"Void  UP!  Altair  NAH  Rebels  "
	dc.b	"Ghostown  h0ffman  "
	dc.b	"Alcatraz  FairLight  "
	dc.b	"Planet Jazz  Insane  Oxygene  Abyss  "
	dc.b	"NGC  Fnuque  SMFX  TTE  Haujobb"
;	dc.b	"Istari  Megastyle  Logicoma  " ; No more CPU time available :-(
;	dc.b	"Truck  Elkmoose  "
;	dc.b	"Cocoon  Cosmic Orbs  Laserbeam  "
;	dc.b	"Zodiac  Lemmy  Hackers  Spacepigs"
textsend


text4	dc.b	" Another intro by Depeche  "
	dc.b	"and Lord. This time we got "
	dc.b	" a little Amiga 500 goodie "
	dc.b	" for the 68k Inside Party. "
	dc.b	"                           "
	dc.b	" I uberreacted quite a bit "
	dc.b	"   on the module with 47   "
	dc.b	" patterns but hey you only "
	dc.b	"  live once. Very happy to "
	dc.b	"  saw some of you in real  "
	dc.b	"  life at the Amiga38 in   "
	dc.b	"     Moenchengladbach.     "
	dc.b	"                           "
	dc.b	"      Greetings go to      "
	dc.b	"    Markus, Dave Haynie,   "
	dc.b	" RJ Mical, Camilla Boemann,"
	dc.b	"  Andreas Magerl, Andreas  "
text4end
	
text5	dc.b	" Loewenstein, Andre Kudra, "
	dc.b	"      Chris Huelsbeck,     "
	dc.b	"        Martin Ahman,      "
	dc.b	"    Factor 5 at Amiga38    "
	dc.b	"and the many others I can't"
	dc.b	"     name all from A38,    "
	dc.b	"   MnemoTroN (thanks for   "
	dc.b	"     the Gotek drive),     "
	dc.b	"   the Swiss video team    "
	dc.b	"    of Amiga38 (can you    "
	dc.b	"    send me the capture    "
	dc.b	"  of my concert please?),  "
	dc.b	"Virgill (thanks for putting"
	dc.b	"  some AmigaKlang things   "
	dc.b	"  together especially for  "
	dc.b	"   me), J.O.E/SCX, Smurf,  "
	dc.b	"     h0ffman, Ziphoid,     "
text5end

text6	dc.b	"Dan/Lemon, Jonas D (thanks "
	dc.b	" for repairing my A4000),  "
	dc.b	" Benny/CCC, Matthias Swat  "
	dc.b	"    and all members of     "
	dc.b	"   Spreadpoint and all I   "
	dc.b	"          forgot.          "
	dc.b	"                           "
	dc.b	"         Write to          "
	dc.b	"    lord@spreadpoint.com   "
	dc.b	"       for inviting        "
	dc.b	"   (audiovisual concert)   "
	dc.b	"   me to your demoparty?   "
	dc.b	"                           "
	dc.b	"      Join the Power!      "
	dc.b	"                           "
	dc.b	"           Lord            "
	dc.b	"                           "
text6end

	even


*------	FONT LOWRES ------------------------------------------------------*

; "8x8" ASCII sorted
	
font	ds.w	9,0	; space

	dc.w	%0001100000000000	; !
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	dc.w	%0000000000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	ds.w	1,0

	ds.w	9,0	; "
	ds.w	9,0	; #
	ds.w	9,0	; $
	ds.w	9,0	; %
	ds.w	9,0	; &

	dc.w	%0001100000000000	; '
	dc.w	%0001100000000000
	dc.w	%0000100000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	ds.w	1,0

	dc.w	%0000110000000000	; (
	dc.w	%0001100000000000
	dc.w	%0001000000000000
	dc.w	%0001000000000000
	dc.w	%0001000000000000
	dc.w	%0001000000000000
	dc.w	%0001100000000000
	dc.w	%0000110000000000
	ds.w	1,0

	dc.w	%0011000000000000	; )
	dc.w	%0001100000000000
	dc.w	%0000100000000000
	dc.w	%0000100000000000
	dc.w	%0000100000000000
	dc.w	%0000100000000000
	dc.w	%0001100000000000
	dc.w	%0011000000000000
	ds.w	1,0

	ds.w	9,0	; *
	ds.w	9,0	; +

	ds.w	1,0			; , (9x8)
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	dc.w	%0000100000000000

	dc.w	%0000000000000000	; -
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0111110000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	ds.w	1,0

	dc.w	%0000000000000000	; .
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	ds.w	1,0

	dc.w	%0000001100000000	; /
	dc.w	%0000011000000000
	dc.w	%0000110000000000
	dc.w	%0001100000000000
	dc.w	%0011000000000000
	dc.w	%0110000000000000
	dc.w	%1100000000000000
	dc.w	%1000000000000000
	ds.w	1,0

	dc.w	%0111111000000000	; 0
	dc.w	%1111111100000000
	dc.w	%0000000000000000
	dc.w	%1100001100000000
	dc.w	%1100001100000000
	dc.w	%1100001100000000
	dc.w	%1111111100000000
	dc.w	%0111111000000000
	ds.w	1,0

	dc.w	%0000000000000000	; 1
	dc.w	%0011100000000000
	dc.w	%0000000000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	ds.w	1,0

	dc.w	%0111111100000000	; 2
	dc.w	%1111111100000000
	dc.w	%0000000000000000
	dc.w	%0000001100000000
	dc.w	%1111111000000000
	dc.w	%1100000000000000
	dc.w	%1111111100000000
	dc.w	%1111111100000000
	ds.w	1,0

	dc.w	%1111111100000000	; 3
	dc.w	%1111111100000000
	dc.w	%0000000000000000
	dc.w	%0011111100000000
	dc.w	%0000001100000000
	dc.w	%0000001100000000
	dc.w	%1111111100000000
	dc.w	%1111111100000000
	ds.w	1,0

	dc.w	%1100001100000000	; 4
	dc.w	%1100001100000000
	dc.w	%0000000000000000
	dc.w	%1111111100000000
	dc.w	%0000001100000000
	dc.w	%0000001100000000
	dc.w	%0000001100000000
	dc.w	%0000001100000000
	ds.w	1,0

	dc.w	%1111111100000000	; 5
	dc.w	%1111111100000000
	dc.w	%0000000000000000
	dc.w	%1111111000000000
	dc.w	%0000001100000000
	dc.w	%0000001100000000
	dc.w	%1111111100000000
	dc.w	%1111111000000000
	ds.w	1,0

	dc.w	%0111111000000000	; 6
	dc.w	%1111111100000000
	dc.w	%0000000000000000
	dc.w	%1101111100000000
	dc.w	%1100001100000000
	dc.w	%1100001100000000
	dc.w	%1111111100000000
	dc.w	%0111111000000000
	ds.w	1,0

	dc.w	%1111111100000000	; 7
	dc.w	%1111111100000000
	dc.w	%0000000000000000
	dc.w	%0000011000000000
	dc.w	%0000110000000000
	dc.w	%0000110000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	ds.w	1,0

	dc.w	%0111111000000000	; 8
	dc.w	%1111111100000000
	dc.w	%0000000000000000
	dc.w	%0111111000000000
	dc.w	%1100001100000000
	dc.w	%1100001100000000
	dc.w	%1111111100000000
	dc.w	%0111111000000000
	ds.w	1,0

	dc.w	%0111111000000000	; 9
	dc.w	%1111111100000000
	dc.w	%1100001100000000
	dc.w	%1111111000000000
	dc.w	%0000001100000000
	dc.w	%0000001100000000
	dc.w	%1111111100000000
	dc.w	%0111111000000000
	ds.w	1,0

	dc.w	%0000000000000000	; :
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0001100000000000
	dc.w	%0000000000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	dc.w	%0000000000000000
	ds.w	1,0

	ds.w	9,0	; ;
	ds.w	9,0	; <
	ds.w	9,0	; =
	ds.w	9,0	; >

	dc.w	%0111111000000000	; ?
	dc.w	%1111111100000000
	dc.w	%0000001100000000
	dc.w	%0001111100000000
	dc.w	%0011100000000000
	dc.w	%0000000000000000
	dc.w	%0011100000000000
	dc.w	%0011100000000000
	ds.w	1,0

	dc.w	%0111111000000000	; @
	dc.w	%1111111100000000
	dc.w	%1100001100000000
	dc.w	%1100111100000000
	dc.w	%1100111100000000
	dc.w	%1100000000000000
	dc.w	%1111111100000000
	dc.w	%0111111000000000
	ds.w	1,0

	dc.w	%0001111100000000	; A
	dc.w	%0011111100000000
	dc.w	%0110000000000000
	dc.w	%1100001100000000
	dc.w	%1111111100000000
	dc.w	%1111111100000000
	dc.w	%1100001100000000
	dc.w	%1100001100000000
	ds.w	1,0

	dc.w	%1111111000000000	; B
	dc.w	%1111111100000000
	dc.w	%0000001100000000
	dc.w	%1111111000000000
	dc.w	%1100001100000000
	dc.w	%1100001100000000
	dc.w	%1111111100000000
	dc.w	%1111111000000000
	ds.w	1,0

	dc.w	%0001110000000000	; C
	dc.w	%0011111000000000
	dc.w	%0000001000000000
	dc.w	%1100000000000000
	dc.w	%1100000000000000
	dc.w	%0110001000000000
	dc.w	%0011111000000000
	dc.w	%0001110000000000
	ds.w	1,0

	dc.w	%1111000000000000	; D
	dc.w	%1111100000000000
	dc.w	%0000110000000000
	dc.w	%1100011000000000
	dc.w	%1100001000000000
	dc.w	%1100011000000000
	dc.w	%1111110000000000
	dc.w	%1111100000000000
	ds.w	1,0

	dc.w	%1111111100000000	; E
	dc.w	%1111111100000000
	dc.w	%0000000000000000
	dc.w	%1111100000000000
	dc.w	%1111100000000000
	dc.w	%1100000000000000
	dc.w	%1111111100000000
	dc.w	%1111111100000000
	ds.w	1,0

	dc.w	%1111111100000000	; F
	dc.w	%1111111100000000
	dc.w	%0000000000000000
	dc.w	%1111100000000000
	dc.w	%1111100000000000
	dc.w	%1100000000000000
	dc.w	%1100000000000000
	dc.w	%1100000000000000
	ds.w	1,0

	dc.w	%1111111100000000	; G
	dc.w	%1111111100000000
	dc.w	%1100000000000000
	dc.w	%1100011100000000
	dc.w	%1100001100000000
	dc.w	%1100001100000000
	dc.w	%1111111100000000
	dc.w	%1111111100000000
	ds.w	1,0

	dc.w	%1100011000000000	; H
	dc.w	%1100011000000000
	dc.w	%0000000000000000
	dc.w	%1100011000000000
	dc.w	%1111111000000000
	dc.w	%1100011000000000
	dc.w	%1100011000000000
	dc.w	%1100011000000000
	ds.w	1,0

	dc.w	%0001100000000000	; I
	dc.w	%0001100000000000
	dc.w	%0000000000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	ds.w	1,0

	dc.w	%0000001100000000	; J
	dc.w	%0000001100000000
	dc.w	%0000000000000000
	dc.w	%0000001100000000
	dc.w	%0000001100000000
	dc.w	%1100011100000000
	dc.w	%1111111000000000
	dc.w	%1111110000000000
	ds.w	1,0

	dc.w	%1100001100000000	; K
	dc.w	%1100011000000000
	dc.w	%0000110000000000
	dc.w	%1111100000000000
	dc.w	%1111100000000000
	dc.w	%1100110000000000
	dc.w	%1100011000000000
	dc.w	%1100001100000000
	ds.w	1,0

	dc.w	%1100000000000000	; L
	dc.w	%1100000000000000
	dc.w	%0000000000000000
	dc.w	%1100000000000000
	dc.w	%1100000000000000
	dc.w	%1100000000000000
	dc.w	%1111111100000000
	dc.w	%1111111100000000
	ds.w	1,0

	dc.w	%1100001100000000	; M
	dc.w	%1110011100000000
	dc.w	%0000000000000000
	dc.w	%1111111100000000
	dc.w	%1101101100000000
	dc.w	%1100001100000000
	dc.w	%1100001100000000
	dc.w	%1100001100000000
	ds.w	1,0

	dc.w	%1100001100000000	; N
	dc.w	%1110001100000000
	dc.w	%1111000000000000
	dc.w	%1111101100000000
	dc.w	%1101111100000000
	dc.w	%1100111100000000
	dc.w	%1100011100000000
	dc.w	%1100001100000000
	ds.w	1,0

	dc.w	%1111111100000000	; O
	dc.w	%1111111100000000
	dc.w	%0000000000000000
	dc.w	%1100001100000000
	dc.w	%1100001100000000
	dc.w	%1100001100000000
	dc.w	%1111111100000000
	dc.w	%1111111100000000
	ds.w	1,0

	dc.w	%1111111100000000	; P
	dc.w	%1111111100000000
	dc.w	%0000001100000000
	dc.w	%1100001100000000
	dc.w	%1111111100000000
	dc.w	%1100000000000000
	dc.w	%1100000000000000
	dc.w	%1100000000000000
	ds.w	1,0

	dc.w	%1111111100000000	; Q
	dc.w	%1111111100000000
	dc.w	%0000000000000000
	dc.w	%1100011100000000
	dc.w	%1100111000000000
	dc.w	%1101110000000000
	dc.w	%1111111100000000
	dc.w	%1111111100000000
	ds.w	1,0

	dc.w	%0001111100000000	; R
	dc.w	%0011111100000000
	dc.w	%0110000000000000
	dc.w	%1100001100000000
	dc.w	%1111111100000000
	dc.w	%1100110000000000
	dc.w	%1100011000000000
	dc.w	%1100001100000000
	ds.w	1,0

	dc.w	%1111111100000000	; S
	dc.w	%1111111100000000
	dc.w	%0000000000000000
	dc.w	%1111110000000000
	dc.w	%0111111000000000
	dc.w	%0000001100000000
	dc.w	%1111111100000000
	dc.w	%1111111100000000
	ds.w	1,0

	dc.w	%1111111000000000	; T
	dc.w	%1111111000000000
	dc.w	%0000000000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	ds.w	1,0

	dc.w	%1100001100000000	; U
	dc.w	%1100001100000000
	dc.w	%0000000000000000
	dc.w	%1100001100000000
	dc.w	%1100001100000000
	dc.w	%1100001100000000
	dc.w	%1111111100000000
	dc.w	%1111111100000000
	ds.w	1,0

	dc.w	%1100001100000000	; V
	dc.w	%1100001100000000
	dc.w	%0000000000000000
	dc.w	%1100011000000000
	dc.w	%1100110000000000
	dc.w	%1101100000000000
	dc.w	%1111000000000000
	dc.w	%1110000000000000
	ds.w	1,0

	dc.w	%1100001100000000	; W
	dc.w	%1100001100000000
	dc.w	%0000000000000000
	dc.w	%1100001100000000
	dc.w	%1101101100000000
	dc.w	%1111111100000000
	dc.w	%1110011100000000
	dc.w	%1100001100000000
	ds.w	1,0

	dc.w	%1100001100000000	; X
	dc.w	%1100001100000000
	dc.w	%0000000000000000
	dc.w	%0011110000000000
	dc.w	%0011110000000000
	dc.w	%1110011100000000
	dc.w	%1100001100000000
	dc.w	%1100001100000000
	ds.w	1,0

	dc.w	%1100011000000000	; Y
	dc.w	%1100011000000000
	dc.w	%0000000000000000
	dc.w	%0011110000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	ds.w	1,0

	dc.w	%1111111100000000	; Z (was not original font)
	dc.w	%1111111100000000
	dc.w	%0000000000000000
	dc.w	%0011111100000000
	dc.w	%0111111000000000
	dc.w	%1100000000000000
	dc.w	%1111111100000000
	dc.w	%1111111100000000
	ds.w	1,0

	ds.w	9,0	; [
	ds.w	9,0	; \
	ds.w	9,0	; ]
	ds.w	9,0	; ^
	ds.w	9,0	; _
	ds.w	9,0	; `

	dc.w	%0000000000000000	; a
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0011111100000000
	dc.w	%0000001100000000
	dc.w	%1111111100000000
	dc.w	%1100001100000000
	dc.w	%1111111100000000
	ds.w	1,0

	dc.w	%1100000000000000	; b
	dc.w	%1100000000000000
	dc.w	%1100000000000000
	dc.w	%1111111100000000
	dc.w	%1100000100000000
	dc.w	%1100000100000000
	dc.w	%1100000100000000
	dc.w	%1111111100000000
	ds.w	1,0

	dc.w	%0000000000000000	; c
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1111111000000000
	dc.w	%1100000000000000
	dc.w	%1100000000000000
	dc.w	%1100000000000000
	dc.w	%1111111100000000
	ds.w	1,0

	dc.w	%0000001000000000	; d
	dc.w	%0000001000000000
	dc.w	%0000001000000000
	dc.w	%1111101000000000
	dc.w	%1100001000000000
	dc.w	%1100001000000000
	dc.w	%1100001000000000
	dc.w	%1111111000000000
	ds.w	1,0

	dc.w	%0000000000000000	; e
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1111111100000000
	dc.w	%1100000000000000
	dc.w	%1111111100000000
	dc.w	%1100000000000000
	dc.w	%1111111100000000
	ds.w	1,0
	
	dc.w	%0011111100000000	; f
	dc.w	%0011000000000000
	dc.w	%0011000000000000
	dc.w	%1111111100000000
	dc.w	%0011000000000000
	dc.w	%0011000000000000
	dc.w	%0011000000000000
	dc.w	%0011000000000000
	ds.w	1,0

	dc.w	%0000000000000000	; g (9x8)
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1111111100000000
	dc.w	%1100001100000000
	dc.w	%1111111100000000
	dc.w	%0000001100000000
	dc.w	%0000001100000000
	dc.w	%1111111100000000

	dc.w	%1100000000000000	; h
	dc.w	%1100000000000000
	dc.w	%1100000000000000
	dc.w	%1111111100000000
	dc.w	%1100001100000000
	dc.w	%1100001100000000
	dc.w	%1100001100000000
	dc.w	%1100001100000000
	ds.w	1,0

	dc.w	%0000000000000000	; i
	dc.w	%0001100000000000
	dc.w	%0000000000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	ds.w	1,0

	dc.w	%0000000000000000	; j (9x8)
	dc.w	%0000001000000000
	dc.w	%0000000000000000
	dc.w	%0000001000000000
	dc.w	%0000001000000000
	dc.w	%0000001000000000
	dc.w	%1100001000000000
	dc.w	%1100001000000000
	dc.w	%1111111000000000

	dc.w	%1100000000000000	; k
	dc.w	%1100000000000000
	dc.w	%1100000000000000
	dc.w	%1100111100000000
	dc.w	%1101000000000000
	dc.w	%1101000000000000
	dc.w	%1100100000000000
	dc.w	%1100011100000000
	ds.w	1,0

	dc.w	%0001100000000000	; l
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	dc.w	%0001100000000000
	ds.w	1,0

	dc.w	%0000000000000000	; m
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1111111100000000
	dc.w	%1101101100000000
	dc.w	%1101101100000000
	dc.w	%1100001100000000
	dc.w	%1100001100000000
	ds.w	1,0

	dc.w	%0000000000000000	; n
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1111111000000000
	dc.w	%1100001100000000
	dc.w	%1100001100000000
	dc.w	%1100001100000000
	dc.w	%1100001100000000
	ds.w	1,0

	dc.w	%0000000000000000	; o
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1111111100000000
	dc.w	%0000001100000000
	dc.w	%1100001100000000
	dc.w	%1100001100000000
	dc.w	%1111111100000000
	ds.w	1,0

	dc.w	%0000000000000000	; p (9x8)
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1111111100000000
	dc.w	%1100001100000000
	dc.w	%1111111100000000
	dc.w	%1100000000000000
	dc.w	%1100000000000000
	dc.w	%1100000000000000

	dc.w	%0000000000000000	; q (9x8)
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%0111111100000000
	dc.w	%0100001100000000
	dc.w	%0111111100000000
	dc.w	%0000001100000000
	dc.w	%0000001100000000
	dc.w	%0000001100000000

	dc.w	%0000000000000000	; r
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1111111100000000
	dc.w	%1100001100000000
	dc.w	%1100000000000000
	dc.w	%1100000000000000
	dc.w	%1100000000000000
	ds.w	1,0

	dc.w	%0000000000000000	; s
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1111111100000000
	dc.w	%1100000000000000
	dc.w	%1111111100000000
	dc.w	%0000001100000000
	dc.w	%1111111100000000
	ds.w	1,0

	dc.w	%0000000000000000	; t
	dc.w	%0010000000000000
	dc.w	%0010000000000000
	dc.w	%1111100000000000
	dc.w	%0010000000000000
	dc.w	%0010000000000000
	dc.w	%0010000000000000
	dc.w	%0011111100000000
	ds.w	1,0

	dc.w	%0000000000000000	; u
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1100001100000000
	dc.w	%1100001100000000
	dc.w	%1100001100000000
	dc.w	%1100001100000000
	dc.w	%0111111100000000
	ds.w	1,0

	dc.w	%0000000000000000	; v
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1100001100000000
	dc.w	%1100001100000000
	dc.w	%0110011000000000
	dc.w	%0011110000000000
	dc.w	%0001100000000000
	ds.w	1,0

	dc.w	%0000000000000000	; w
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1100001100000000
	dc.w	%1100001100000000
	dc.w	%1101101100000000
	dc.w	%1110011100000000
	dc.w	%1100001100000000
	ds.w	1,0

	dc.w	%0000000000000000	; x
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1110011100000000
	dc.w	%0011110000000000
	dc.w	%0001100000000000
	dc.w	%0011110000000000
	dc.w	%1110011100000000
	ds.w	1,0

	dc.w	%0000000000000000	; y (9x8)
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1100001100000000
	dc.w	%1100001100000000
	dc.w	%0111111100000000
	dc.w	%0000001100000000
	dc.w	%0000001100000000
	dc.w	%0111111100000000

	dc.w	%0000000000000000	; z
	dc.w	%0000000000000000
	dc.w	%0000000000000000
	dc.w	%1111111100000000
	dc.w	%0000011000000000
	dc.w	%0001100000000000
	dc.w	%0110000000000000
	dc.w	%1111111100000000
	ds.w	1,0
fontend


*------	COPER ------------------------------------------------------------*

coper	moveq	#$0010,d0			; delete coper request bit
	move.w	d0,$9c(a6)			;
	move.w	d0,$9c(a6)			; 3 times? https://amycoders.org/tutorials/frametime.html
	move.w	d0,$9c(a6)			;

	if lsptiming
	move.w	#$0334,$180(a6)			;
	endif
	bsr	lspplay				;
		
	movem.l	(a7)+,a0-a6/d0-d7		;
	rte					;


*------	IRQ3 -------------------------------------------------------------*

irq3	movem.l	a0-a6/d0-d7,-(a7)		;
	lea	custom,a6			;
	move.w	$1e(a6),d0			; read interrupt request bits
	btst	#4,d0				;
	bne	coper				;

	lea	vars(pc),a5			;

	moveq	#$0030,d0			; delete vertb and coper request bit
	move.w	d0,$9c(a6)			; 
	move.w	d0,$9c(a6)			; 3 times? https://amycoders.org/tutorials/frametime.html
	move.w	d0,$9c(a6)			;
		
	move	#$2200,sr			; allow other (coper) level 3 interrupts

	bsr	play				;

	move.w	v_actors(a5),d7			; process actors
	btst	#actormain,d7			; main actor?
	beq	.notmain			;

	move.l	v_dbplane1a(a5),d0		;
	move.l	d0,d1				;
	move.l	d0,$e0(a6)			;

	add.l	#($91-$2c)*pwidth,d0		;
	move.l	b_clist(pc),a2			;
	move.l	a2,a3				;
	add.w	#bpltext-clist+2+4,a2		; init text bitplane overlapping logo
	move.w	d0,(a2)				;
	swap	d0				;
	move.w	d0,-4(a2)			;

	add.l	#($100-$2c)*pwidth,d1		;
	add.w	#bpltext2-clist+2+4,a3		; init text bitplane below logo
	move.w	d1,(a3)				;
	swap	d1				;
	move.w	d1,-4(a3)			;
	
	bsr	cls				;
	
	move.l	v_draw(a5),d0			;
	beq	.drawnothing			;
	move.l	d0,a0				;
	jsr	(a0)				; draw
.drawnothing

.notmain
	move.w	v_actors(a5),d7			; process actors
	btst	#actorfadeindkd,d7		; actor fade in dkd logo?
	beq	.2				;
	lea	fadedkdcols(pc),a0		;
	add.w	v_fadecolindex(a5),a0		;
	move.l	b_clist2(pc),a1			;
	add.w	#colsdkd-clist2+2,a1		;
	moveq	#numdkdcols-1,d6		;
.set	move.w	(a0),(a1)			;
	add.w	#numfadesteps*2,a0		;
	addq.w	#4,a1				;
	dbf	d6,.set				;
	
	addq.w	#2,v_fadecolindex(a5)		;
	cmp.w	#numfadesteps*2,v_fadecolindex(a5) ;
	bne	.ndone				;
	sub.w	#1<<actorfadeindkd,v_actors(a5)	; stop actor
	subq.w	#2,v_fadecolindex(a5)		; prepare fade out	
.ndone

.2	btst	#actorfadeoutdkd,d7		; actor fade out dkd logo?
	beq	.3				;
	lea	fadedkdcols(pc),a0		;
	add.w	v_fadecolindex(a5),a0		;
	move.l	b_clist2(pc),a1			;
	add.w	#colsdkd-clist2+2,a1		;
	moveq	#numdkdcols-1,d6		;
.set2	move.w	(a0),(a1)			;
	add.w	#numfadesteps*2,a0		;
	addq.w	#4,a1				;
	dbf	d6,.set2			;
	subq.w	#2,v_fadecolindex(a5)		;
	bge	.ndone2				;	
	sub.w	#1<<actorfadeoutdkd,v_actors(a5) ; stop actor
	clr.w	v_fadecolindex(a5)		; prepare logo fade in
.ndone2

.3	btst	#actorfadeinlogo,d7		; actor fade in logo?
	beq	.4				;
	lea	fadedlogooncols(pc),a0		;
	add.w	v_fadecolindex(a5),a0		;
	move.l	b_clist(pc),a1			;
	move.w	(a0),cl_logocolor2-clist+2(a1)	; $0182
	add.w	#cl_logocolors-clist+2,a1 	;
	moveq	#numlogocols-1-1,d6		; all except $0182
.set3	add.w	#numfadesteps*2,a0		;
	move.w	(a0),(a1)			;
	addq.w	#4,a1				;
	dbf	d6,.set3			;
	addq.w	#2,v_fadecolindex(a5)		;
	cmp.w	#numfadesteps*2,v_fadecolindex(a5) ;
	bne	.ndone3				;
	sub.w	#1<<actorfadeinlogo,v_actors(a5) ; stop actor
.ndone3

.4
	
	if numbers
	moveq	#0,d0				; number
	move.w	v_frame(a5),d0			;
;	move.l	$200.w,d0			;
	moveq	#8-1,d1				; pos
	bsr	printnumber			;

	moveq	#0,d0				; number
	
	if numbers&testing	
	move.l	v_number2(a5),d0		;
	endif

	move.l	#(fontend-font)/16*onecharcodesize*16,d0
;	moveq	#1,d0				; value
;	move.l	v_temp(a5),d0
	move.l	#10*pwidth+8-1,d1		; pos
	bsr	printnumber			;

	addq.w	#1,v_frame(a5)			; advance frame number
	endif


	movem.l	v_db(a5),d0-d2			; triple buffering  active, clearing, buffer
	exg	d0,d2				; active <-> buffer
	exg	d1,d2				; clearing <-> active
	movem.l	d0-d2,v_db(a5)			;

	btst	#6,$bfe001			; left mouse button pressed?
	bne	.noquit				;
	st	v_quit(a5)			;
.noquit
	if timing
	move.w	#$0020,$180(a6)			; dark green color indicates free capacity
	endif

	movem.l	(a7)+,a0-a6/d0-d7		;
	rte					;
	

*------	DRAW SPRIAL ------------------------------------------------------*

drawspiral
	lea	sintab(pc),a3			;
	
	move.l	b_textspiral(pc),a4		;
	move.w	#$07ff,d3			; const
	move.l	v_dbplane1b(a5),a2		; const
	addq.w	#2,a2				; "center" 4

	moveq	#0,d4				; lame y translation
	movem.w	v_x(a5),d5/d6			;
	move.w	v_numletters(a5),d7		; (mind -1 for dbf see below)
.loop	move.l	(a4)+,d2			; is it a space?
	beq	.nodraw				;  -> do not draw
	move.l	d2,a1				;

	and.w	d3,d5				;
	move.w	(a3,d5.w),d0			;
	asr.w	#1,d0				;
	subq.w	#8,d0				; centering (stupid/slow)
	
	and.w	d3,d6				;
	move.w	(a3,d6.w),d1			;
	asr.w	#2,d1				;
	
	move.w	d4,d2				;
	asr.w	#2,d2				; 1
	add.w	d2,d1				;

;	cmp.w	#256,d1				; out of screen?
;	bgt	.nodraw				;

	moveq	#0,d2				;
	move.w	d1,d2				; y
	lsl.l	#6,d2				;
	
	lea	(a2,d2.w),a0			;

	move.w	d0,d2				; x
	asr.w	#4,d2				; == asr.w #3,d2
	asl.w	#1,d2				; == and.w #$fffe,d2

	add.w	d2,a0				;

	moveq	#0,d2		
	move.w	d0,d2				;
	and.w	#$000f,d2			;
	asl.l	#6,d2				; * 64

	add.l	d2,a1				;
	jsr	(a1)				; draw char

.nodraw	sub.w	#32,d5				; 32 40
	sub.w	#32,d6				; 32 40
	addq.w	#1,d4				; "lame" y effect
	dbf	d7,.loop			;

	addq.w	#8,v_x(a5)			; 8
	add.w	#10,v_y(a5)			; 10

	cmp.w	#textsend-texts-1-1,v_numletters(a5) ; -1-1 for dbf
	bgt	.numl				;
	addq.w	#1,v_numletters(a5)		;
.numl	rts					;
	
	
*------	PRECALC TEXT -----------------------------------------------------*
	
precalctext
.loop	moveq	#0,d0				;
	move.b	(a0)+,d0			;
	sub.b	#' ',d0				;
	bne	.1				;
	clr.l	(a1)+				; it's a space
	bra	.2				;

.1	swap	d0				; * 1024 (16 * onecharcodesize)
	lsr.l	#6,d0				;
	move.l	b_drawcharcode(pc),a4		;
	add.l	d0,a4				;
	move.l	a4,(a1)+			;
.2	dbf	d7,.loop			;
	rts					;


*------	DRAW WAVE --------------------------------------------------------*

spacing	equ	13
dx	equ	20
dy	equ	0
lines	equ	17

drawwave
	move.l	v_textscreen(a5),d0		;
	beq	.quit				;
	move.l	d0,a4				;

	move.l	v_dbplane1b(a5),a2		; const
	add.w	#pwidth-2,a2			; 1 line safety zone, -2 centering

	lea	sintab(pc),a3			;
	moveq	#0,d4				; y translation
	moveq	#lines-1,d3			;
	movem.w	v_x(a5),d5/d6			;
.oloop	and.w	#$07ff,d5			;
	move.w	(a3,d5.w),d0			; x
	asr.w	#4,d0				; 512 -> 16
	addq.w	#4,d0				; centering (stupid/slow)

	moveq	#27-1,d7			; 26 chars
.loop	move.l	(a4)+,d2			; is it a space?
	beq	.nodraw				;  -> do not draw
	move.l	d2,a1				;

	and.w	#$07ff,d6			;
	move.w	(a3,d6.w),d1			;
	asr.w	#4,d1				;
	
	add.w	d4,d1				; global line shift
	
	moveq	#0,d2				;
	move.w	d1,d2				; y
	lsl.l	#6,d2				;

	lea	(a2,d2.w),a0			;

	move.w	d0,d2				;
	asr.w	#4,d2				; == asr.w #3,d2
	asl.w	#1,d2				; == and.w #$fffe,d2
	add.w	d2,a0				;

	moveq	#0,d2				;
	move.w	d0,d2				;
	and.w	#$000f,d2			;
	asl.l	#6,d2				; * 64

	add.l	d2,a1				;
	jsr	(a1)				; draw char

.nodraw	add.w	#12,d0				; next x pos
	add.w	#16,d6				; next y (sin) pos
	dbf	d7,.loop			;

	add.w	#dx,d5
	sub.w	#27*16+dy,d6			; compensate add.w #16,d6 above
	add.w	#spacing,d4			;
	dbf	d3,.oloop			;

	addq.w	#8,v_x(a5)			; prepare next frame
	add.w	#16,v_y(a5)			;
.quit	rts					;
	
	
*------	PLAYER -----------------------------------------------------------*

play	tst.b	v_wait(a5)		;
	beq	.donotwait		;
	subq.b	#1,v_wait(a5)		;
	rts				;

.donotwait
	move.l	v_cmdspointer(a5),a0	;
.loop	move.b	(a0)+,d0		; cmd_wait (0)?
	bne	.1			;
	move.b	(a0)+,v_wait(a5)	; duration
	move.l	a0,v_cmdspointer(a5)	;
	rts				;

.1	subq.b	#1,d0			; cmd_textcolor (1)?
	bne	.2			;
	moveq	#0,d1			; color value
	move.b	(a0)+,d1		;
	asl.w	#8,d1			; merge bytes to word
	move.b	(a0)+,d1		;

	move.l	b_clist(pc),a1		;
	move.w	d1,cl_textcolor2-clist+2(a1) ;
	add.w	#cl_textcolor-clist+2,a1 ;	
	moveq	#(cl_textcolorend-cl_textcolor)/4-1,d7
.loop1	move.w	d1,(a1)			;
	addq.w	#4,a1			; next color
	dbf	d7,.loop1		;
	bra	.loop			;

.2	subq.b	#1,d0			; cmd_textscreen (2)?
	bne	.3			;
	moveq	#0,d1			; text value
	move.b	(a0)+,d1		;
	add.b	v_hidden(a5),d1		;
	lea	b_text1(pc),a1		;
	move.l	(a1,d1.w),v_textscreen(a5) ;
	bra	.loop			;

.3	subq.b	#1,d0			; cmd_logo (3)?
	bne	.4			;
	move.l	b_logoon(pc),d1		; default
	lea	logooncolors(pc),a1	;
	move.w	#$0277,d2		; color of $0182

	tst.b	(a0)+			; which logo (on or off)?
	bne	.3on			;
	move.l	b_logooff(pc),d1	;
	lea	logooffcolors(pc),a1	;
	move.w	#$0111,d2		; color of $0182
.3on	move.l	b_clist(pc),a2		;
	move.l	a2,a3			; used below
	add.w	#bpllogo-clist+2+4,a2	; init planet bitplanes
	moveq	#4-1,d7			; 4 bitplanes
.initbpl
	move.w	d1,(a2)			;
	swap	d1			;
	move.w	d1,-4(a2)		;
	swap	d1			;
	add.l	#logoonsize/4,d1	; /4 bitplanes
	addq.w	#8,a2			;
	dbf	d7,.initbpl		;
	
	move.w	d2,cl_logocolor2-clist+2(a3)
	add.w	#cl_logocolors-clist+2,a3 ;
	
	moveq	#(logocolorsend-logooncolors)/2-1,d7
.initcolors
	move.w	(a1)+,(a3)		;
	addq.w	#4,a3			;
	dbf	d7,.initcolors		;
	bra	.loop			;

.4	subq.b	#1,d0			; cmd_rewind (4)
	bne	.5			;
	lea	playrewind(pc),a0	;
	bra	.loop			;

.5	subq.b	#1,d0			; cmd_draw (5)
	bne	.6			;
	
	clr.l	v_x(a5)			; reset v_x AND v_y
		
	move.b	(a0)+,d1		; cmd parameter
	beq	.5n			; 0 (par_drawnothing)?
	subq.b	#1,d1			;
	beq	.5s			; 1 (par_drawspiral)?
	lea	drawwave(pc),a1		; it's 2 (par_drawwave)
	move.l	a1,v_draw(a5)		;
	bra	.loop			;
.5n	clr.l	v_draw(a5)		; draw nothing
	bra	.loop			;
.5s	lea	drawspiral(pc),a1	;
	move.l	a1,v_draw(a5)		;
	clr.w	v_numletters(a5)	;
	bra	.loop			;

.6	subq.b	#1,d0			; cmd_actor (6)?
	bne	.7			;
	moveq	#0,d1			;
	move.b	(a0)+,d1		; actor
	move.w	v_actors(a5),d2		;
	bset	d1,d2			;
	move.w	d2,v_actors(a5)		;
	bra	.loop			;

.7	subq.b	#1,d0			; cmd_mainclist (7)?
	bne	.8			;
	move.l	b_clist(pc),$80(a6)	; activate main clist (next frame)
.8	bra	.loop			;


logooncolors ; ($0184 until $019e)
	dc.w	$02aa,$0233,$0155,$0fff
	dc.w	$0313,$0505,$03cc,$0355
	dc.w	$0727,$0747,$0818,$08ff
	dc.w	$0a6b,$0c8d
logocolorsend
; $0182=$0277

logooffcolors ; ($0184 until $019e)
	dc.w	$0222,$0155,$0266,$0233
	dc.w	$0444,$0133,$0288,$0244
	dc.w	$0011,$03bb,$0566,$08a8
	dc.w	$07ee,$0cee	
; $0182=$0111

; commands and parameters
cmd_wait	equ 	0
cmd_textcolor	equ	1

cmd_textscreen	equ	2
par_textscreen1	equ	0*entrysize
par_textscreen2	equ	1*entrysize
par_textscreen3	equ	2*entrysize

cmd_logo	equ	3
par_logoon	equ	1
par_logooff	equ	0

cmd_rewind	equ	4

cmd_draw	equ	5
par_drawnothing	equ	0
par_drawspiral	equ	1
par_drawwave	equ	2

cmd_actor	equ	6
cmd_mainclist	equ	7

; actor bits
actorfadeindkd	equ	0
actorfadeoutdkd	equ	1
actormain	equ	2
actorfadeinlogo	equ	3

playcmds
	dc.b	cmd_wait,50
	dc.b	cmd_actor,actorfadeindkd
	dc.b	cmd_wait,50
	dc.b	cmd_actor,actorfadeoutdkd
	dc.b	cmd_wait,15+150

	dc.b	cmd_logo,par_logoon
	dc.b	cmd_actor,actormain, cmd_mainclist

	dc.b	cmd_actor,actorfadeinlogo
	dc.b	cmd_wait,15

	dc.b	cmd_wait,250
playrewind
	dc.b	cmd_textscreen,par_textscreen1
	dc.b	cmd_textcolor,$06,$cc
	dc.b	cmd_draw,par_drawwave
		
	dc.b	cmd_logo,par_logooff ; 1

	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff ; 2
	
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff ; 3

	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff ; 4
	
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff ; 5
	
	dc.b	cmd_wait,250 ; 12 secs
	dc.b	cmd_wait,250
	dc.b	cmd_wait,100

	dc.b	cmd_textcolor,$0c,$8c
	dc.b	cmd_textscreen,par_textscreen2

	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon ; 1
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff
	
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon ; 2
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff

	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon ; 3
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff
	
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon ; 4
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff
	
	dc.b	cmd_wait,250 ; 12 secs
	dc.b	cmd_wait,250
	dc.b	cmd_wait,100

	dc.b	cmd_textcolor,$06,$cc
	dc.b	cmd_textscreen,par_textscreen3
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon ; 1
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff
	
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon ; 2
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff

	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon ; 3
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff
	
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon ; 4
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff
	
	dc.b	cmd_wait,250 ; 12 secs
	dc.b	cmd_wait,250
	dc.b	cmd_wait,100
	
	; spiral

	dc.b	cmd_wait,220 ; sync with drums (was 250)
		
	dc.b	cmd_textcolor,$06,$cc
	dc.b	cmd_draw,par_drawspiral
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon ; 1
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff
	
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon ; 2
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff

	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon ; 3
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff
	
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon ; 4
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff
	
	dc.b	cmd_wait,250 ; 6 secs
	dc.b	cmd_wait,50

	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon ; 1
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff
	
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff

	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff
	
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff
	
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon
	dc.b	cmd_wait,2
	dc.b	cmd_logo,par_logooff

	dc.b	cmd_wait,250 ; 6 secs
	dc.b	cmd_wait,50

	dc.b	cmd_logo,par_logoon ; 1
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff
	
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon ; 2
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff

	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon ; 3
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff
	
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon ; 4
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff
	
	dc.b	cmd_wait,250 ; 6 secs
	dc.b	cmd_wait,50

	dc.b	cmd_textcolor,$0c,$8c
	
	dc.b	cmd_logo,par_logoon ; 1
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff
	
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon ; 2
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff

	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon ; 3
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff
	
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon ; 4
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff
	
	dc.b	cmd_wait,250 ; 6 secs
	dc.b	cmd_wait,50

	dc.b	cmd_logo,par_logoon ; 1
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff
	
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon ; 2
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff

	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon ; 3
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff
	
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logoon ; 4
	dc.b	cmd_wait,4
	dc.b	cmd_logo,par_logooff
	
	dc.b	cmd_wait,250 ; 6 secs
	dc.b	cmd_wait,50

	dc.b	cmd_logo,par_logoon ; 1
	dc.b	cmd_textcolor,$0c,$8c, cmd_wait,2
	dc.b	cmd_textcolor,$0b,$7b, cmd_wait,2
	dc.b	cmd_logo,par_logooff
	dc.b	cmd_textcolor,$0a,$7a, cmd_wait,2
	dc.b	cmd_textcolor,$09,$69, cmd_wait,2

	dc.b	cmd_logo,par_logoon ; 2
	dc.b	cmd_textcolor,$08,$68, cmd_wait,2
	dc.b	cmd_textcolor,$07,$57, cmd_wait,2
	dc.b	cmd_logo,par_logooff
	dc.b	cmd_textcolor,$06,$46, cmd_wait,2
	dc.b	cmd_textcolor,$05,$35, cmd_wait,2

	dc.b	cmd_logo,par_logoon ; 3
	dc.b	cmd_textcolor,$04,$34, cmd_wait,2
	dc.b	cmd_textcolor,$03,$23, cmd_wait,2
	dc.b	cmd_logo,par_logooff
	dc.b	cmd_textcolor,$02,$22, cmd_wait,2
	dc.b	cmd_textcolor,$01,$11, cmd_wait,2

	dc.b	cmd_logo,par_logoon ; 4
	dc.b	cmd_textcolor,$00,$00
	dc.b	cmd_draw,par_drawnothing

	dc.b	cmd_wait,250 ; 6 secs
	dc.b	cmd_wait,50

	dc.b	cmd_rewind

	even


*------	COPPER INSTRUCTION LIST ------------------------------------------*

clist	dc.w	$1007,$fffe ; chance for player to update clist in time

	dc.w	$008e,$2c81 ; DIWSTRT normal=$2c81 wide=$71
	dc.w	$0090,$2cc1 ; DIWSTOP normal=$2cc1 wide=$d1
	dc.w	$0092,$0038
	dc.w	$0094,$00d0

	dc.w	$0100,$1200
	dc.w	$0102,$0000
	dc.w	$0104,$0000 ; sprites have no priority
	dc.w	$0108,inviswidth
	dc.w	$010a,inviswidth
	
	dc.w	$0180,$0000
	
cl_logocolors
	dc.w	$0184,0,$0186,0,$0188,0,$018a,0
	dc.w	$018c,0,$018e,0,$0190,0,$0192,0
	dc.w	$0194,0,$0196,0,$0198,0,$019a,0
	dc.w	$019c,0,$019e,0	

cl_textcolor ; 17 colors in total
	dc.w	$0182,$06cc ; not overlapping logo
	dc.w	$01a0,$06cc,$01a2,$06cc,$01a4,$06cc,$01a6,$06cc ; overlapping logo
	dc.w	$01a8,$06cc,$01aa,$06cc,$01ac,$06cc,$01ae,$06cc
	dc.w	$01b0,$06cc,$01b2,$06cc,$01b4,$06cc,$01b6,$06cc
	dc.w	$01b8,$06cc,$01ba,$06cc,$01bc,$06cc,$01be,$06cc
cl_textcolorend


; sprites off
	dc.w	$0144,$0000,$0146,$0000 ; data 0
	dc.w	$014c,$0000,$014e,$0000 ; data 1
	dc.w	$0154,$0000,$0156,$0000 ; data 2
	dc.w	$015c,$0000,$015e,$0000 ; data 3
	dc.w	$0164,$0000,$0166,$0000 ; data 4
	dc.w	$016c,$0000,$016e,$0000 ; data 5
	dc.w	$0174,$0000,$0176,$0000	; data 6
	dc.w	$017c,$0000,$017e,$0000 ; data 7

lspline	equ $30
	dc.b	lspline,$07,$ff,$fe
	dc.w	$009c,$8010 ; trigger coper interrupt
	dc.b	lspline+11,$07,$ff,$fe

	if lsptiming
	dc.w	$0180,$00f0
	endif
lspdmacon
	dc.b	$00,$96,$80,0

	dc.w	$9107,$fffe
bpllogo	dc.w	$00e0,0,$00e2,0 ; bitplane 1
	dc.w	$00e4,0,$00e6,0 ; bitplane 2
	dc.w	$00e8,0,$00ea,0 ; bitplane 3
	dc.w	$00ec,0,$00ee,0 ; bitplane 4
bpltext	dc.w	$00f0,0,$00f2,0 ; bitplane 5  text

	dc.w	$0100,$5200
cl_logocolor2
	dc.w	$0182,$0277

	dc.w	$ffdf,$fffe ; $00,$07,$ff,$fe

bpltext2
	dc.w	$00e0,0,$00e2,0 ; bitplane 1

	dc.w	$0100,$1200
	
cl_textcolor2	
	dc.w	$0182,$06cc
	dc.w	$ffff,$fffe
clistend


*------	COPPER INSTRUCTION LIST 2 (DKD LOGO) -----------------------------*

dkdlogoheight	equ 142

clist2	dc.b	$00,$8e,$5a,$81
	dc.b	$00,$90,$5a+dkdlogoheight,$c1
	dc.w	$0092,$0038
	dc.w	$0094,$00d0
	dc.w	$0108,$0000
	dc.w	$010a,$0000
	dc.w	$0100,$3200
	dc.w	$0102,$0000
	dc.w	$0104,$0000 ; sprites have no priority
	
bpldkd	dc.w	$00e0,0,$00e2,0
	dc.w	$00e4,0,$00e6,0
	dc.w	$00e8,0,$00ea,0

	dc.w	$0180,$0000
colsdkd	dc.w	$0182,0,$0184,0,$0186,0
	dc.w	$0188,0,$018a,0,$018c,0,$018e,0

	dc.w	$0190,$0000
	dc.w	$0192,$0000
	dc.w	$0194,$0000
	dc.w	$0196,$0000
	dc.w	$0198,$0000
	dc.w	$019a,$0000
	dc.w	$019c,$0000
	dc.w	$019e,$0000
	dc.w	$01a0,$0000
	dc.w	$01a2,$0000
	dc.w	$01a4,$0000
	dc.w	$01a6,$0000
	dc.w	$01a8,$0000
	dc.w	$01aa,$0000
	dc.w	$01ac,$0000
	dc.w	$01ae,$0000
	dc.w	$01b0,$0000
	dc.w	$01b2,$0000
	dc.w	$01b4,$0000
	dc.w	$01b6,$0000
	dc.w	$01b8,$0000
	dc.w	$01ba,$0000
	dc.w	$01bc,$0000
	dc.w	$01be,$0000

; sprites off
	dc.w	$0144,$0000,$0146,$0000 ; data 0
	dc.w	$014c,$0000,$014e,$0000 ; data 1
	dc.w	$0154,$0000,$0156,$0000 ; data 2
	dc.w	$015c,$0000,$015e,$0000 ; data 3
	dc.w	$0164,$0000,$0166,$0000 ; data 4
	dc.w	$016c,$0000,$016e,$0000 ; data 5
	dc.w	$0174,$0000,$0176,$0000	; data 6
	dc.w	$017c,$0000,$017e,$0000 ; data 7

	dc.b	lspline,$07,$ff,$fe
	dc.w	$009c,$8010 ; trigger coper interrupt
	dc.b	lspline+11,$07,$ff,$fe

	if lsptiming
	dc.w	$0180,$00f0
	endif
lspdmacon2
	dc.b	$00,$96,$80,0

	dc.w	$ffff,$fffe
clist2end


*------	COLORS -----------------------------------------------------------*

numfadesteps	equ	15

numlogocols	equ	1+14
fadedlogooncols
	dc.w	$0000,$0111,$0111,$0122,$0122,$0133,$0133,$0144,$0244,$0255,$0255,$0266,$0266,$0277,$0277 ; $0182

	dc.w	$0000,$0111,$0122,$0133,$0133,$0144,$0155,$0155,$0266,$0277,$0288,$0288,$0299,$02aa,$02aa ; $0184
	dc.w	$0000,$0111,$0111,$0111,$0111,$0111,$0122,$0122,$0222,$0222,$0233,$0233,$0233,$0233,$0233 ; $0186
	dc.w	$0000,$0011,$0111,$0111,$0122,$0122,$0133,$0133,$0133,$0144,$0144,$0144,$0155,$0155,$0155 ; $0188
	dc.w	$0000,$0111,$0333,$0444,$0555,$0666,$0777,$0888,$0999,$0aaa,$0bbb,$0ccc,$0ddd,$0eee,$0fff ; $018a
	dc.w	$0000,$0101,$0111,$0111,$0111,$0111,$0212,$0212,$0212,$0212,$0313,$0313,$0313,$0313,$0313 ; $018c
	dc.w	$0000,$0101,$0101,$0101,$0202,$0202,$0303,$0303,$0303,$0404,$0404,$0404,$0505,$0505,$0505 ; $018e
	dc.w	$0000,$0111,$0122,$0133,$0144,$0155,$0266,$0266,$0277,$0288,$0399,$03aa,$03bb,$03cc,$03cc ; $0190
	dc.w	$0000,$0111,$0111,$0111,$0122,$0122,$0233,$0233,$0233,$0244,$0344,$0344,$0355,$0355,$0355 ; $0192
	dc.w	$0000,$0111,$0111,$0212,$0212,$0313,$0313,$0414,$0424,$0525,$0525,$0626,$0626,$0727,$0727 ; $0194
	dc.w	$0000,$0111,$0111,$0212,$0222,$0323,$0323,$0424,$0434,$0535,$0535,$0646,$0646,$0747,$0747 ; $0196
	dc.w	$0000,$0101,$0212,$0212,$0313,$0313,$0414,$0414,$0515,$0616,$0616,$0717,$0717,$0818,$0818 ; $0198
	dc.w	$0000,$0111,$0233,$0244,$0355,$0366,$0477,$0488,$0599,$06aa,$06bb,$07cc,$07dd,$08ee,$08ff ; $019a
	dc.w	$0000,$0111,$0212,$0323,$0324,$0434,$0535,$0536,$0647,$0747,$0858,$0859,$096a,$0a6b,$0a6b ; $019c
	dc.w	$0000,$0111,$0222,$0323,$0434,$0535,$0646,$0647,$0758,$0869,$096a,$0a7b,$0b7c,$0c8c,$0c8d ; $019e

numdkdcols	equ	7
fadedkdcols
	dc.w	$0000,$0111,$0112,$0122,$0223,$0233,$0234,$0234,$0345,$0346,$0356,$0457,$0467,$0468,$0468 ; $0182
	dc.w	$0000,$0111,$0122,$0223,$0234,$0334,$0345,$0346,$0457,$0467,$0568,$0579,$067a,$068b,$068b ; $0184
	dc.w	$0000,$0111,$0111,$0112,$0122,$0123,$0123,$0123,$0234,$0234,$0235,$0245,$0246,$0246,$0246 ; $0186
	dc.w	$0000,$0111,$0223,$0334,$0445,$0456,$0567,$0678,$0789,$079a,$08ab,$09bc,$0acd,$0bce,$0bdf ; $0188
	dc.w	$0000,$0111,$0222,$0233,$0344,$0345,$0456,$0467,$0578,$0679,$068a,$079b,$07ac,$08bc,$08bd ; $018a
	dc.w	$0000,$0111,$0233,$0344,$0455,$0566,$0677,$0788,$0899,$09aa,$0abb,$0bcc,$0cdd,$0cee,$0dff ; $018c
	dc.w	$0000,$0111,$0333,$0444,$0555,$0666,$0777,$0888,$0999,$0aaa,$0bbb,$0ccc,$0ddd,$0eee,$0fff ; $018e


*------	CLEAR SCREEN -----------------------------------------------------*

cls	move.l	v_dbplane1c(a5),a0		;
	bsr	waitblitter			;
	move.l	#$01000000,$40(a6)		; bltcon0 bltcon1
	move.l	a0,$54(a6)			; destination D
	move.w	#inviswidth,$66(a6)		; modulo D
	move.w	#pheight<<6+(pwidth-inviswidth)>>1,$58(a6) ; bltsize and start
	rts					;


*------	MEMORY MANAGEMENT -----------------------------------------*

BESTMEMORY	equ	0
MEMF_CHIP	equ	1<<1
MEMF_CLEAR	equ	1<<16

lspbanksize	equ	lspbankend-lspbank
clistsize	equ	clistend-clist
clist2size	equ	clist2end-clist2
logoonsize	equ	logoonend-logoon
logooffsize	equ	logooffend-logooff
logodkdsize	equ	logodkdend-logodkd

memtable
b_clist		dc.l	0,MEMF_CHIP,clistsize
b_clist2	dc.l	0,MEMF_CHIP,clist2size
b_lspbank	dc.l	0,MEMF_CHIP,lspbanksize
b_logoon	dc.l	0,MEMF_CHIP,logoonsize	; DO NOT CHANGE ORDER
b_logooff	dc.l	0,MEMF_CHIP,logooffsize ; DO NOT CHANGE ORDER
b_logodkd	dc.l	0,MEMF_CHIP,logodkdsize

memtable2
b_bitplanes	dc.l	0,MEMF_CHIP+MEMF_CLEAR,3*psize
b_drawcharcode	dc.l	0,BESTMEMORY+MEMF_CLEAR,(fontend-font)/16*16*onecharcodesize
b_text1		dc.l	0,BESTMEMORY,(text1end-text1)*4 ; *4 = longs  DO NOT CHANGE ORDER
b_text2		dc.l	0,BESTMEMORY,(text2end-text2)*4 ;             DO NOT CHANGE ORDER       
b_text3		dc.l	0,BESTMEMORY,(text3end-text3)*4 ;             DO NOT CHANGE ORDER
b_text4		dc.l	0,BESTMEMORY,(text4end-text4)*4 ;             DO NOT CHANGE ORDER
b_text5		dc.l	0,BESTMEMORY,(text5end-text5)*4 ;             DO NOT CHANGE ORDER       
b_text6		dc.l	0,BESTMEMORY,(text6end-text6)*4 ;             DO NOT CHANGE ORDER
b_textspiral	dc.l	0,BESTMEMORY,(textsend-texts)*4
;b_testoutofmem	dc.l	0,MEMF_CHIP,600000
memtableend

entrysize 	equ	12 ; one entry in the memtable is 12 bytes large (3 longwords)
entries		equ	(memtableend-memtable)/entrysize
entrieschip	equ	(memtable2-memtable)/entrysize

alloc	lea	clist(pc),a1			;
	move.l	AbsExecBase.w,a6		;
	jsr	TypeOfMem(a6)			;
	btst	#1,d0				; chipmem?
	beq	.notchipmem			;

	lea	clist(pc),a0			; mark data that is in chipmen already
	lea	b_clist(pc),a1			;
	move.l	a0,(a1)				;

	lea	clist2(pc),a0			;
	lea	b_clist2(pc),a1			;
	move.l	a0,(a1)				;

	lea	base(pc),a0			;
	add.l	#lspbank-base,a0		;
	lea	b_lspbank(pc),a1		;
	move.l	a0,(a1)				;

	lea	logoon(pc),a0			;
	lea	b_logoon(pc),a1			;
	move.l	a0,(a1)				;

	lea	logooff(pc),a0			;
	lea	b_logooff(pc),a1		;
	move.l	a0,(a1)				;

	lea	base(pc),a0			;
	add.l	#logodkd-base,a0		;
	lea	b_logodkd(pc),a1		;
	move.l	a0,(a1)				;
.notchipmem
	lea	memtable(pc),a5			;
	moveq	#entries-1,d7			;
.loop	tst.l	(a5)				; not to be allocated?
	bne	.noalloc			;
	move.l	8(a5),d0			; bytesize
	move.l	4(a5),d1			; requirements
	move.l	AbsExecBase.w,a6		;
	jsr	AllocMem(a6)			;
	move.l	d0,(a5)				;
	beq	.printerrorandfreemem		; out of memory
.noalloc	
	add.w	#entrysize,a5			; next entry
	dbf	d7,.loop			;
	bsr	initmemory			;
	moveq	#0,d0				; ok, all entries allocated
	rts					;

.printerrorandfreemem
	bsr	printoutofmemory		;
dealloc	move.l	AbsExecBase.w,a6		;
	jsr	TypeOfMem(a6)			;
	lea	memtable(pc),a5			;
	moveq	#entries-1,d7			;
	btst	#1,d0				; chipmem?
	beq	.loop				; we are not in chipmem so free all entries
	lea	memtable2(pc),a5		;
	moveq	#entries-entrieschip-1,d7	;
.loop	tst.l	(a5)				; end of memtable?
	beq	.done				;
	move.l	(a5),a1				; address of memory block
	move.l	8(a5),d0			; bytesize
	move.l	AbsExecBase.w,a6		;
	jsr	FreeMem(a6)			;
	add.w	#entrysize,a5			;
	dbf	d7,.loop			;
.done	moveq	#-1,d0				; alloc error
	rts					;

initmemory
	lea	vars(pc),a5			;
	
	lea	clist(pc),a0			; copy clist to chip memory
	move.l	b_clist(pc),a1			;
	move.w	#clistsize-1,d7			;
.clcopy	move.b	(a0)+,(a1)+			;
	dbf	d7,.clcopy			;

	lea	clist2(pc),a0			; copy clist2 to chip memory
	move.l	b_clist2(pc),a1			;
	move.w	#clist2size-1,d7		;
.cl2copy
	move.b	(a0)+,(a1)+			;
	dbf	d7,.cl2copy			;

	lea	base(pc),a0			; copy lspbank to chip memory
	add.l	#lspbank-base,a0		;
	move.l	b_lspbank(pc),a1		;
	move.l	#lspbanksize,d0			;
.copylspbank
	move.b	(a0)+,(a1)+			;
	subq.l	#1,d0				;
	bne	.copylspbank			;

	move.l	b_bitplanes(pc),a0		; init db bitplanes
	lea	v_db(a5),a1			;
	moveq	#3*numplanes-1,d7		; active, clearing, buffer
.initplanes
	move.l	a0,(a1)+			;
	add.l	#psize,a0			;
	dbf	d7,.initplanes			;

	lea	logoon(pc),a0			; copy logo on to chip memory
	move.l	b_logoon(pc),a1			;
	move.w	#logoonsize/2-1,d7		;
.copyl	move.w	(a0)+,(a1)+			;
	dbf	d7,.copyl			;

	lea	logooff(pc),a0			; copy logo off to chip memory
	move.l	b_logooff(pc),a1		;
	move.w	#logooffsize/2-1,d7		;
.copyl2	move.w	(a0)+,(a1)+			;
	dbf	d7,.copyl2			;

	lea	base(pc),a0			; copy logo dkd to chip memory
	add.l	#logodkd-base,a0		;
	move.l	b_logodkd(pc),a1		;
	move.w	#logodkdsize/2-1,d7		;
.copyl3	move.w	(a0)+,(a1)+			;
	dbf	d7,.copyl3			;	

	move.l	b_logoon(pc),d0			; init logo bitplanes pointers (TODO player can do that)
	move.l	b_clist(pc),a2			;
	add.w	#bpllogo-clist+2+4,a2		; init logo bitplanes
	moveq	#4-1,d7				; 4 bitplanes
.initbpl
	move.w	d0,(a2)				;
	swap	d0				;
	move.w	d0,-4(a2)			;
	swap	d0				;
	add.l	#logoonsize/4,d0		; /4 bitplanes
	addq.w	#8,a2				;
	dbf	d7,.initbpl			;

	move.l	b_logodkd(pc),d0		; init dkd logo bitplanes pointers
	move.l	b_clist2(pc),a2			;
	add.w	#bpldkd-clist2+2+4,a2		; init logo bitplanes
	moveq	#3-1,d7				; 3 bitplanes
.initbpl2
	move.w	d0,(a2)				;
	swap	d0				;
	move.w	d0,-4(a2)			;
	swap	d0				;
	add.l	#logodkdsize/3,d0		;
	addq.w	#8,a2				;
	dbf	d7,.initbpl2			;
	rts					;


*------	SINE TABLE 1024 STEPS *290 +290 ----------------------------------*

sintab	dc.w	$0122,$0124,$0126,$0127,$0129,$012b,$012d,$012e
	dc.w	$0130,$0132,$0134,$0136,$0137,$0139,$013b,$013d
	dc.w	$013e,$0140,$0142,$0144,$0145,$0147,$0149,$014b
	dc.w	$014d,$014e,$0150,$0152,$0154,$0155,$0157,$0159
	dc.w	$015b,$015c,$015e,$0160,$0162,$0163,$0165,$0167
	dc.w	$0168,$016a,$016c,$016e,$016f,$0171,$0173,$0174
	dc.w	$0176,$0178,$017a,$017b,$017d,$017f,$0180,$0182
	dc.w	$0184,$0185,$0187,$0189,$018a,$018c,$018e,$018f
	dc.w	$0191,$0193,$0194,$0196,$0198,$0199,$019b,$019c
	dc.w	$019e,$01a0,$01a1,$01a3,$01a4,$01a6,$01a8,$01a9
	dc.w	$01ab,$01ac,$01ae,$01af,$01b1,$01b2,$01b4,$01b6
	dc.w	$01b7,$01b9,$01ba,$01bc,$01bd,$01bf,$01c0,$01c2
	dc.w	$01c3,$01c5,$01c6,$01c8,$01c9,$01ca,$01cc,$01cd
	dc.w	$01cf,$01d0,$01d2,$01d3,$01d4,$01d6,$01d7,$01d9
	dc.w	$01da,$01db,$01dd,$01de,$01df,$01e1,$01e2,$01e3
	dc.w	$01e5,$01e6,$01e7,$01e9,$01ea,$01eb,$01ed,$01ee
	dc.w	$01ef,$01f0,$01f2,$01f3,$01f4,$01f5,$01f6,$01f8
	dc.w	$01f9,$01fa,$01fb,$01fc,$01fe,$01ff,$0200,$0201
	dc.w	$0202,$0203,$0204,$0206,$0207,$0208,$0209,$020a
	dc.w	$020b,$020c,$020d,$020e,$020f,$0210,$0211,$0212
	dc.w	$0213,$0214,$0215,$0216,$0217,$0218,$0219,$021a
	dc.w	$021b,$021c,$021d,$021d,$021e,$021f,$0220,$0221
	dc.w	$0222,$0223,$0223,$0224,$0225,$0226,$0227,$0227
	dc.w	$0228,$0229,$022a,$022a,$022b,$022c,$022d,$022d
	dc.w	$022e,$022f,$022f,$0230,$0231,$0231,$0232,$0232
	dc.w	$0233,$0234,$0234,$0235,$0235,$0236,$0236,$0237
	dc.w	$0238,$0238,$0239,$0239,$0239,$023a,$023a,$023b
	dc.w	$023b,$023c,$023c,$023d,$023d,$023d,$023e,$023e
	dc.w	$023e,$023f,$023f,$023f,$0240,$0240,$0240,$0241
	dc.w	$0241,$0241,$0241,$0242,$0242,$0242,$0242,$0242
	dc.w	$0243,$0243,$0243,$0243,$0243,$0243,$0243,$0244
	dc.w	$0244,$0244,$0244,$0244,$0244,$0244,$0244,$0244
	dc.w	$0244,$0244,$0244,$0244,$0244,$0244,$0244,$0244
	dc.w	$0244,$0244,$0243,$0243,$0243,$0243,$0243,$0243
	dc.w	$0243,$0242,$0242,$0242,$0242,$0242,$0241,$0241
	dc.w	$0241,$0241,$0240,$0240,$0240,$023f,$023f,$023f
	dc.w	$023e,$023e,$023e,$023d,$023d,$023d,$023c,$023c
	dc.w	$023b,$023b,$023a,$023a,$0239,$0239,$0239,$0238
	dc.w	$0238,$0237,$0236,$0236,$0235,$0235,$0234,$0234
	dc.w	$0233,$0232,$0232,$0231,$0231,$0230,$022f,$022f
	dc.w	$022e,$022d,$022d,$022c,$022b,$022a,$022a,$0229
	dc.w	$0228,$0227,$0227,$0226,$0225,$0224,$0223,$0223
	dc.w	$0222,$0221,$0220,$021f,$021e,$021d,$021d,$021c
	dc.w	$021b,$021a,$0219,$0218,$0217,$0216,$0215,$0214
	dc.w	$0213,$0212,$0211,$0210,$020f,$020e,$020d,$020c
	dc.w	$020b,$020a,$0209,$0208,$0207,$0206,$0204,$0203
	dc.w	$0202,$0201,$0200,$01ff,$01fe,$01fc,$01fb,$01fa
	dc.w	$01f9,$01f8,$01f6,$01f5,$01f4,$01f3,$01f2,$01f0
	dc.w	$01ef,$01ee,$01ed,$01eb,$01ea,$01e9,$01e7,$01e6
	dc.w	$01e5,$01e3,$01e2,$01e1,$01df,$01de,$01dd,$01db
	dc.w	$01da,$01d9,$01d7,$01d6,$01d4,$01d3,$01d2,$01d0
	dc.w	$01cf,$01cd,$01cc,$01ca,$01c9,$01c8,$01c6,$01c5
	dc.w	$01c3,$01c2,$01c0,$01bf,$01bd,$01bc,$01ba,$01b9
	dc.w	$01b7,$01b6,$01b4,$01b2,$01b1,$01af,$01ae,$01ac
	dc.w	$01ab,$01a9,$01a8,$01a6,$01a4,$01a3,$01a1,$01a0
	dc.w	$019e,$019c,$019b,$0199,$0198,$0196,$0194,$0193
	dc.w	$0191,$018f,$018e,$018c,$018a,$0189,$0187,$0185
	dc.w	$0184,$0182,$0180,$017f,$017d,$017b,$017a,$0178
	dc.w	$0176,$0174,$0173,$0171,$016f,$016e,$016c,$016a
	dc.w	$0168,$0167,$0165,$0163,$0162,$0160,$015e,$015c
	dc.w	$015b,$0159,$0157,$0155,$0154,$0152,$0150,$014e
	dc.w	$014d,$014b,$0149,$0147,$0145,$0144,$0142,$0140
	dc.w	$013e,$013d,$013b,$0139,$0137,$0136,$0134,$0132
	dc.w	$0130,$012e,$012d,$012b,$0129,$0127,$0126,$0124
	dc.w	$0122,$0120,$011e,$011d,$011b,$0119,$0117,$0116
	dc.w	$0114,$0112,$0110,$010e,$010d,$010b,$0109,$0107
	dc.w	$0106,$0104,$0102,$0100,$00ff,$00fd,$00fb,$00f9
	dc.w	$00f7,$00f6,$00f4,$00f2,$00f0,$00ef,$00ed,$00eb
	dc.w	$00e9,$00e8,$00e6,$00e4,$00e2,$00e1,$00df,$00dd
	dc.w	$00dc,$00da,$00d8,$00d6,$00d5,$00d3,$00d1,$00d0
	dc.w	$00ce,$00cc,$00ca,$00c9,$00c7,$00c5,$00c4,$00c2
	dc.w	$00c0,$00bf,$00bd,$00bb,$00ba,$00b8,$00b6,$00b5
	dc.w	$00b3,$00b1,$00b0,$00ae,$00ac,$00ab,$00a9,$00a8
	dc.w	$00a6,$00a4,$00a3,$00a1,$00a0,$009e,$009c,$009b
	dc.w	$0099,$0098,$0096,$0095,$0093,$0092,$0090,$008e
	dc.w	$008d,$008b,$008a,$0088,$0087,$0085,$0084,$0082
	dc.w	$0081,$007f,$007e,$007c,$007b,$007a,$0078,$0077
	dc.w	$0075,$0074,$0072,$0071,$0070,$006e,$006d,$006b
	dc.w	$006a,$0069,$0067,$0066,$0065,$0063,$0062,$0061
	dc.w	$005f,$005e,$005d,$005b,$005a,$0059,$0057,$0056
	dc.w	$0055,$0054,$0052,$0051,$0050,$004f,$004e,$004c
	dc.w	$004b,$004a,$0049,$0048,$0046,$0045,$0044,$0043
	dc.w	$0042,$0041,$0040,$003e,$003d,$003c,$003b,$003a
	dc.w	$0039,$0038,$0037,$0036,$0035,$0034,$0033,$0032
	dc.w	$0031,$0030,$002f,$002e,$002d,$002c,$002b,$002a
	dc.w	$0029,$0028,$0027,$0027,$0026,$0025,$0024,$0023
	dc.w	$0022,$0021,$0021,$0020,$001f,$001e,$001d,$001d
	dc.w	$001c,$001b,$001a,$001a,$0019,$0018,$0017,$0017
	dc.w	$0016,$0015,$0015,$0014,$0013,$0013,$0012,$0012
	dc.w	$0011,$0010,$0010,$000f,$000f,$000e,$000e,$000d
	dc.w	$000c,$000c,$000b,$000b,$000b,$000a,$000a,$0009
	dc.w	$0009,$0008,$0008,$0007,$0007,$0007,$0006,$0006
	dc.w	$0006,$0005,$0005,$0005,$0004,$0004,$0004,$0003
	dc.w	$0003,$0003,$0003,$0002,$0002,$0002,$0002,$0002
	dc.w	$0001,$0001,$0001,$0001,$0001,$0001,$0001,$0000
	dc.w	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	dc.w	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	dc.w	$0000,$0000,$0001,$0001,$0001,$0001,$0001,$0001
	dc.w	$0001,$0002,$0002,$0002,$0002,$0002,$0003,$0003
	dc.w	$0003,$0003,$0004,$0004,$0004,$0005,$0005,$0005
	dc.w	$0006,$0006,$0006,$0007,$0007,$0007,$0008,$0008
	dc.w	$0009,$0009,$000a,$000a,$000b,$000b,$000b,$000c
	dc.w	$000c,$000d,$000e,$000e,$000f,$000f,$0010,$0010
	dc.w	$0011,$0012,$0012,$0013,$0013,$0014,$0015,$0015
	dc.w	$0016,$0017,$0017,$0018,$0019,$001a,$001a,$001b
	dc.w	$001c,$001d,$001d,$001e,$001f,$0020,$0021,$0021
	dc.w	$0022,$0023,$0024,$0025,$0026,$0027,$0027,$0028
	dc.w	$0029,$002a,$002b,$002c,$002d,$002e,$002f,$0030
	dc.w	$0031,$0032,$0033,$0034,$0035,$0036,$0037,$0038
	dc.w	$0039,$003a,$003b,$003c,$003d,$003e,$0040,$0041
	dc.w	$0042,$0043,$0044,$0045,$0046,$0048,$0049,$004a
	dc.w	$004b,$004c,$004e,$004f,$0050,$0051,$0052,$0054
	dc.w	$0055,$0056,$0057,$0059,$005a,$005b,$005d,$005e
	dc.w	$005f,$0061,$0062,$0063,$0065,$0066,$0067,$0069
	dc.w	$006a,$006b,$006d,$006e,$0070,$0071,$0072,$0074
	dc.w	$0075,$0077,$0078,$007a,$007b,$007c,$007e,$007f
	dc.w	$0081,$0082,$0084,$0085,$0087,$0088,$008a,$008b
	dc.w	$008d,$008e,$0090,$0092,$0093,$0095,$0096,$0098
	dc.w	$0099,$009b,$009c,$009e,$00a0,$00a1,$00a3,$00a4
	dc.w	$00a6,$00a8,$00a9,$00ab,$00ac,$00ae,$00b0,$00b1
	dc.w	$00b3,$00b5,$00b6,$00b8,$00ba,$00bb,$00bd,$00bf
	dc.w	$00c0,$00c2,$00c4,$00c5,$00c7,$00c9,$00ca,$00cc
	dc.w	$00ce,$00d0,$00d1,$00d3,$00d5,$00d6,$00d8,$00da
	dc.w	$00dc,$00dd,$00df,$00e1,$00e2,$00e4,$00e6,$00e8
	dc.w	$00e9,$00eb,$00ed,$00ef,$00f0,$00f2,$00f4,$00f6
	dc.w	$00f7,$00f9,$00fb,$00fd,$00ff,$0100,$0102,$0104
	dc.w	$0106,$0107,$0109,$010b,$010d,$010e,$0110,$0112
	dc.w	$0114,$0116,$0117,$0119,$011b,$011d,$011e,$0120
			

*------	PRINT OUT OF MEMORY ----------------------------------------------*

printoutofmemory
	lea	.dos(pc),a1			;
	move.l	AbsExecBase.w,a6		;
	jsr	OldOpenLibrary(a6)		;
	move.l	d0,a6				;
	beq	.error				;
	jsr	Output(a6)			;
	move.l	d0,d1				;
	beq	.error				;
	moveq	#.textend-.text,d3		; length
	lea	.text(pc),a1			;
	move.l	a1,d2				;
	jsr	Write(a6)			;
	tst.l	d0				;
	beq	.error				;
	move.l	a6,a1				;
	move.l	AbsExecBase.w,a6		;
	jsr	CloseLibrary(a6)		;
.error	moveq	#0,d0				;
	rts					;

.dos	dc.b	"dos.library",0
.text	dc.b	"Error: Could not allocate enough memory",10
.textend
	even


;*****************************************************************
;
;	Light Speed Player v1.05 (modified by Depeche)
;	Fastest Amiga MOD player ever
;	Written By Arnaud Carré (aka Leonard / OXYGENE)
;	https://github.com/arnaud-carre/LSPlayer
;	twitter: @leonard_coder
;
;*****************************************************************

lspplay	lea	LSPVars(pc),a1
	move.l	(a1),a0				; byte stream
.process
	moveq	#0,d0
.cloop	move.b	(a0)+,d0
	bne	.swCode
	add.w	#$0100,d0
	bra	.cloop
.swCode	add.w	d0,d0
	move.l	m_codeTableAddr(a1),a2		; code table
	move.w	(a2,d0.w),d0			; code
	beq	.noInst
	cmp.w	m_escCodeRewind(a1),d0
	beq	.r_rewind
	cmp.w	m_escCodeSetBpm(a1),d0
	beq	.r_chgbpm

	add.b	d0,d0
	bcc	.noVd
	move.b	(a0)+,$d9(a6)
.noVd	add.b	d0,d0
	bcc	.noVc
	move.b	(a0)+,$c9(a6)
.noVc	add.b	d0,d0
	bcc	.noVb
	move.b	(a0)+,$b9(a6)
.noVb	add.b	d0,d0
	bcc	.noVa
	move.b	(a0)+,$a9(a6)
.noVa	move.l	a0,(a1)+			; store byte stream ptr
	move.l	(a1),a0				; word stream
	tst.b	d0
	beq	.noPa
	add.b	d0,d0
	bcc	.noPd
	move.w	(a0)+,$d6(a6)
.noPd	add.b	d0,d0
	bcc	.noPc
	move.w	(a0)+,$c6(a6)
.noPc	add.b	d0,d0
	bcc	.noPb
	move.w	(a0)+,$b6(a6)
.noPb	add.b	d0,d0
	bcc	.noPa
	move.w	(a0)+,$a6(a6)
.noPa	tst.w	d0
	beq	.noInst

	moveq	#0,d1
	move.l	m_lspInstruments-4(a1),a2	; instrument table
	lea	resetv+12(pc),a4
	lea	$d0(a6),a5
	moveq	#4-1,d2
.vloop	add.w	d0,d0
	bcs	.setIns
	add.w	d0,d0
	bcc	.skip
	move.l	(a4),a3
	move.l	(a3)+,(a5)
	move.w	(a3)+,4(a5)
	bra	.skip
.setIns	add.w	(a0)+,a2
	add.w	d0,d0
	bcc	.noReset
	bset	d2,d1
	move.w	d1,$96(a6)
.noReset
	move.l	(a2)+,(a5)
	move.w	(a2)+,4(a5)
	move.l	a2,(a4)
.skip	subq.w	#4,a4
	sub.w	#$10,a5
	dbf	d2,.vloop

	move.l	m_dmaconPatch-4(a1),a3		;
	move.b	d1,(a3)				; dmacom main clist		
	move.l	m_dmaconPatch2-4(a1),a3		;
	move.b	d1,(a3)				; dmacom clist2

.noInst	move.l	a0,(a1)				; store word stream (or byte stream if coming from early out)
	rts

.r_rewind
	move.l	m_byteStreamLoop(a1),a0
	move.l	m_wordStreamLoop(a1),m_wordStream(a1)
	bra	.process

.r_chgbpm	
	move.b	(a0)+,m_currentBpm+1(a1)	; BPM
	bra	.process

lspinit	lea	base(pc),a0			; a0: music data (any mem) + 10
	add.l	#lspmusic-base,a0		;
	move.l	b_lspbank(pc),a1		; a1: sound bank data (chip mem)

	lea	LSPVars(pc),a3
	move.w	(a0)+,m_currentBpm(a3)		; default BPM
	move.w	(a0)+,m_escCodeRewind(a3)
	move.w	(a0)+,m_escCodeSetBpm(a3)
	move.l	(a0)+,-(a7)			; who cares? replace with addq.w #4,a0?

	move.l	b_clist(pc),a2			; clist
	lea	lspdmacon+3-clist(a2),a2	;
	move.l	a2,m_dmaconPatch(a3)		;

	move.l	b_clist2(pc),a2			; clist2
	lea	lspdmacon2+3-clist2(a2),a2	;
	move.l	a2,m_dmaconPatch2(a3)		;

	move.w	(a0)+,d0			; instrument count
	lea	-12(a0),a2			; LSP data has -12 offset on instrument tab (to win 2 cycles in fast player)
	move.l	a2,m_lspInstruments(a3)		; instrument tab addr (minus 4)
	subq.w	#1,d0
	move.l	a1,d1
.relocLoop
	bset.b	#0,3(a0)			; bit0 is relocation done flag
	bne	.relocated
	add.l	d1,(a0)
	add.l	d1,6(a0)
.relocated
	lea	12(a0),a0
	dbf	d0,.relocLoop
	move.w	(a0)+,d0			; codes count (+2)
	move.l	a0,m_codeTableAddr(a3)		; code table
	add.w	d0,d0
	add.w	d0,a0
	movem.l	(a0)+,d0-d2			; word stream size, byte stream loop point, word stream loop point
	move.l	a0,m_wordStream(a3)
	lea	(a0,d0.l),a1			; byte stream
	move.l	a1,m_byteStream(a3)
	add.l	d2,a0
	add.l	d1,a1
	move.l	a0,m_wordStreamLoop(a3)
	move.l	a1,m_byteStreamLoop(a3)
	lea	m_currentBpm(a3),a0
	move.l	(a7)+,d0			; music len in frame ticks? who cares? REMOVE?
	rts

	rsreset
m_byteStream		rs.l	1	;  0 byte stream
m_wordStream		rs.l	1	;  4 word stream
m_dmaconPatch		rs.l	1	;  8 m_lfmDmaConPatch
m_codeTableAddr		rs.l	1	; 12 code table addr
m_escCodeRewind		rs.w	1	; 16 rewind special escape code
m_escCodeSetBpm		rs.w	1	; 18 set BPM escape code
m_lspInstruments	rs.l	1	; 20 LSP instruments table addr
m_relocDone		rs.w	1	; 24 reloc done flag
m_currentBpm		rs.w	1	; 26 current BPM
m_byteStreamLoop	rs.l	1	; 28 byte stream loop point
m_wordStreamLoop	rs.l	1	; 32 word stream loop point
m_dmaconPatch2		rs.l	1	; 40 m_lfmDmaConPatch2, added
sizeof_LSPVars		rs.w	0

LSPVars		ds.b	sizeof_LSPVars
	even			
resetv		dc.l	0,0,0,0


*------	LOGOS ------------------------------------------------------------*

logoon	incbin	"logo-on"
logoonend

logooff	incbin	"logo-off"
logooffend

logodkd	incbin	"logo-dkd"
logodkdend


*------	MUSIC ------------------------------------------------------------*

	even
lspbank	incbin	"lord_remix_68k.lsbank"
lspbankend

	even ; very important
lspmusic
	incbin	"lord_remix_68k.lsmusic",10	; skip header (10 bytes)
