; ------------------INITIALISATION

					; for use in basic
bascm:db	0x00			; communication with basic (<to >from)
					; <0=save,<1=load,<&hff=quit,
					; >&h20=cancel requester
					; flags (bits)
flags:db	0x00			; 7=quit,6=mouse,5=rect,4=dis gen input,
					; 3=paste box,2=char map,1=edit char
	db	0x00			; 7=grid trigger					
swscr:jp	clsrestscr		; called from basic, show the screen to BSAVE
					; start and return
return:				; entry point when returning from basic
	ld	a,(bascm)		; get basic communication byte
	cp	0x01			; was a load?
	jr	z,sstr1		; if so, return initializing sprites
	cp	0x00			; was a save?
	jr	z,sstr2		; if so, return initializing sprites all
	call	clsrestscr		; if canceled, restore drawing
	jr	sstr3			; return without initializing sprites
start:
	call	checkmouse		; check if there is a mouse
	ld	a,32			; 32 columns
	ld	(LINL32),a		; put on the variable
	call	init32		; initialize screen 1
	call	erafnk		; hide function keys display
	ld	a,(scrat)		; get fore color attribute
	ld	(FORCLR),a		; put fore color on memory
	ld	a,(scrat+1)		; get back color attribute
	ld	(BAKCLR),a		; put back color on memory
	ld	a,(scrat+2)		; get bord color attribute
	ld	(BDRCLR),a		; put bord color on memory
	call	chgclr		; change screen colors
	call	setundo		; copy screen for undo
sstr2:call	initsprmem		; auto fill sprite VRAM addresses in RAM table
	ld	a,(scrat+3)		; get character color attribute
	ld	(editoncl),a	; save in working char buffer (prep for high color)	
sstr1:call	initsprites		; copy sprites attributes and patters from RAM
sstr3:ld	hl,CSRSW		; disable text cursor
	ld	(hl),0xff		;
	ld	a,(curat+6)		; get current character
	call	getcharshape	; put character shape on sprite
	call	getcharcolor	; put character color on sprite
	jp	mainloop		; go to main loop

initsprmem:				; auto fill sprite VRAM addresses in RAM table
	ld	a,(splastal+8)	; get sprite amount
	ld	b,a			; put in decremental counter
	ld	c,0			; reset sprite number counter
	ld	de,spfrstal		; get RAM attributes start, y pos
linme:inc	de			; go to x pos
	inc	de			; go to sprite number
	ld	a,c			; get sprite number counter
	ld	(de),a		; save in RAM
	inc	de			; go to color
	inc	de			; go to sprite pattern pos low byte
	push	de			; save the RAM address
	call	calpat		; get VRAM sprite pattern address
	pop	de			; restore RAM address
	ld	a,l			; get VRAM address low byte
	ld	(de),a		; save in RAM
	inc	de			; go to sprite pattern pos high byte
	ld	a,h			; get VRAM address high byte
	ld	(de),a		; save in RAM
	ld	a,c			; get sprite number counter
	push	de			; save RAM address
	call	calatr		; get VRAM sprite attribute address
	pop	de			; restore RAM address
	inc	de			; go to sprite attribute pos low byte
	ld	a,l			; get VRAM address low byte
	ld	(de),a		; save in RAM
	inc	de			; go to sprite attribute pos high byte
	ld	a,h			; get VRAM address high byte
	ld	(de),a		; save in RAM
	ld	a,0x0a		; amount to jump
	call	add8to16		; jump 7 RAM addresses
	ld	a,c			; get sprite number counter
	inc	c			; increment it
	djnz	linme			; decremental counter, if not 0, loop
	ret

initsprites:			; initialize the sprites
					; send all sprites to 290 y pos (hide all)
	ld	b,0x20		; get all sprites attributes
	ld	hl,0x1b00		; get VRAM attribute address (pos y)
lisp3:ld	(hl),209		; pos y to 209. hide
	inc	hl			; increment to pos x
	inc	hl			; increment to pattern number
	inc	hl			; increment to color
	inc	hl			; increment to pos y
	djnz	lisp3			; do until finished
					; copy sprites attributes to VRAM
	ld	a,(splastal+8)	; get sprite amount
	ld	b,a			; put in B
	ld	hl,0x1b00		; get VRAM attribute address
	ld	de,spfrstal		; get RAM attributes start
	ld	c,0x04		; amount to copy
lisp1:call	copytovram		; copy to VRAM updating HL and DE
	ld	a,0x0d		; amount of RAM to jump over
	call	add8to16		; add to DE
	djnz	lisp1			; loop
					; copy sprites patterns to VRAM
	ld	a,(splastal+8)	; get sprite amount
	ld	b,a			; put in B
	ld	hl,0x3800		; get VRAM pattern address
	ld	de,spfrstal+9	; get RAM pattern start
	ld	c,0x08		; amount to copy
lisp2:call	copytovram		; copy to VRAM updating HL and DE
	ld	a,0x09		; amount of RAM to jump over
	call	add8to16		; add to DE
	djnz	lisp2			; loop
					; update cursor colors
	ld	b,0x00		; cursor color offset none
	call	cursorcolor		; color the cursors
	ret

checkmouse:				; check if a mouse is present
	ld	b,0xff		; try for 255 cycles
lchmo:call	getmouse		; get mouse offset
	ld	a,h			; get x offset
	and	l			; and with Y offset
	cp	0xff			; are both 255?
	jr	z,turnoffmouse	; if so, turn off the mouse
	djnz	lchmo			; decrease counter and loop
	ret
turnoffmouse:			; turn off mouse
	ld	b,0b01000000	; use bit 6
	jp	z,toggleflags	; toggle it
	ret
