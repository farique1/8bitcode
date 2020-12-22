; ------------------EDIT CHARACTER

showeditchrscr:			; edit character set
					; save the contents of the VRAM
					; screen
	ld	hl,0x1800		; VRAM position from
	ld	de,VRBUF4		; VRAM position to
	ld	bc,0x0300		; length
	call	streamvv		; copy
					; patterns
	ld	hl,0x0000		; VRAM position from
	ld	de,VRBUF5		; VRAM position to
	ld	bc,0x0800		; length
	call	streamvv		; copy
					; colors
	ld	hl,0x2000		; VRAM position from
	ld	de,VRBUF6		; VRAM position to
	ld	bc,0x0800		; length
	call	streamvv		; copy
	call	initworkchrs	; initialize working characters
					; draw screen elements
	ld	a,(curat+6)		; get the current character
	call	initgrid		; draw the editing grid
	ld	hl,0x1883		; top left position for char map
	call	createcharmap	; draw the character map
					; set flags
	ld	hl,flags		; get flags byte
	set	1,(hl)		; set char map flag
	set	4,(hl)		; set dis gen input flag
	call	kilbuf		; clear keyboard buffer
	ret

initworkchrs:			; initialize edit character screen
					; backup chars used for working
					; patterns
	ld	a,(editwork)	; get first working character
	call	getpatternad	; get its pattern address
	ld	de,editbkch		; get the backup address
	ld	bc,0x18		; get the amount to backup
	call	ldirmv		; copy
					; colors
	ld	a,(editwork)	; get first working character
	call	getcolorfrchr	; get its color address
	ld	de,editbkcl		; get the backup address
	ld	bc,0x01		; length to backup (prepare for high color)
	call	ldirmv		; copy
					; create the working characters
	ld	a,(editwork)	; get first working character
	call	getpatternad	; get its pattern address
	ld	hl,editonch		; working chars RAM pattern address
	ld	bc,0x18		; length
	call	ldirvm		; copy
					; color the working characters
	ld	a,(editwork)	; get first working character
	call	getcolorfrchr	; get its color address
	ld	d,h			; put in DE
	ld	e,l 			;
	ld	hl,editoncl		; get its color address
	ld	bc,0x01		; length (prepare for high color)
	call	ldirvm		; write
					; clear the screen w custom char
	ld	a,(editwork+2)	; choose a background character
	ld	hl,0x1800		; start of the screen address
	ld	bc,0x300		; screen length
	call	filvrm		; fill the screen
	ret

restworkchrs:			; restore chars used for working
					; patterns
	ld	a,(editwork)	; get first working character
	call	getpatternad	; get its pattern address
	ld	hl,editbkch		; get the backup address
	ld	bc,0x18		; get the amount to restore
	call	ldirvm		; copy
					; colors
	ld	a,(editwork)	; get first working character
	call	getcolorfrchr	; get its color address
	ld	d,h			; put in DE
	ld	e,l 			;
	ld	hl,editbkcl		; get the backup address
	ld	bc,0x01		; length to restore (prepare for high color)
	call	ldirvm		; copy
	ret

swapwrkchrs:			; swap working characters
	call	restworkchrs	; restore chars used for working
					; swap the working characters information
	ld	hl,(editwork+4)	; get the address of the other set
	ld	de,editwork		; get the address of the active set
	ld	bc,0x06		; get the amount to copy
	ldir				; copy
	call	initworkchrs	; initialize the working characters
					; redraw the screen
	ld	a,(edtch)		; get the editing character
	call	drawgrid		; draw the editing grid
	ld	hl,0x1883		; top left position for char map
	call	createcharmap	; draw the character map
	call	kilbuf		; clear keyboard buffer
	ret

savewrkchrs:			; save working characters. grid and background
					; create the working characters
	ld	a,(editwork)	; get first working character
	call	getpatternad	; get its pattern address
	ld	de,editonch		; working chars RAM pattern address
	ld	bc,0x18		; length
	call	ldirmv		; copy
					; color the working characters
	ld	a,(editwork)	; get first working character
	call	getcolorfrchr	; get its color address
	ld	de,editoncl		; get the backup address
	ld	bc,0x01		; length to restore (prepare for high color)
	call	ldirmv		; copy
	call	kilbuf		; clear keyboard buffer
	ret

clearmapcurs:			; clear grid cursor when cursor not above
	ld	a,209			; y position to turn off sprite
	ld	hl,(spcharac+6)	; character cursor VRAM attribute pos y
	call	wrtvrm		; write
	ld	hl,(spbckgrd+6)	; background cursor VRAM attribute pos y
	call	wrtvrm		; write
	ret

insidecharmap:			; cursor inside character map
	call	testmaparea		; test if inside map area
	jr	nc,clearmapcurs	; if not, leave
	call	chosefrommap	; show choosing cursor
	ret

cleargridcurs:			; clear map char cursor when cursor not above
	ld	a,209			; y position to turn off sprite
	ld	hl,(speditch+6)	; grid cursor VRAM attribute pos y
	call	wrtvrm		; write
	ret

insideeditarea:			; cursor inside editing grid
	call	testgridrea		; test if inside grid area
	jr	nc,cleargridcurs	; if not, leave
	ld	a,(curat)		; get pos y
	add	4			; offset cursor
	and	0b11111000		; step by 8 increments
	dec	a			; adjust for y sprite offset
	ld	hl,(speditch+6)	; character cursor VRAM attribute pos y
	call	wrtvrm		; write pos y to VRAM

	ld	a,(curat+1)		; get pos x
	add	4			; offset cursor
	and	0b11111000		; step by 8 increments
	ld	hl,(speditch+6)	; character cursor VRAM attribute pos y
	inc	hl			; inc to pos x
	call	wrtvrm		; write pos y to VRAM
	ret

canceledit:				; cancel character edit. restore all
	ld	a,(curat+6)		; get the current character
	ld	(edtch),a		; put in editing character
					; restore the contents of the VRAM
					; patterns
	ld	hl,VRBUF5		; VRAM position from
	ld	de,0x0000		; VRAM position to
	ld	bc,0x0800		; length
	call	streamvv		; copy
					; colors
	ld	hl,VRBUF6		; VRAM position from
	ld	de,0x2000		; VRAM position to
	ld	bc,0x0800		; length
	call	streamvv		; copy
acceptedit:				; accept character edit. restore screen only
	call	restworkchrs	; restore chars used for working
					; restore cursors
	ld	a,(edtch)		; get the editing character
	ld	(curat+6),a		; put in current character
	call	getcharshape	; put character shape on sprite
	call	getcharcolor	; put character color on sprite
	call	cleargridcurs	; remove grid cursor
					; restore screen
	ld	hl,VRBUF4		; VRAM position from
	ld	de,0x1800		; VRAM position to
	ld	bc,0x0300		; length
	call	streamvv		; copy
					; reset flags
	ld	hl,flags		; get flags byte
	res	1,(hl)		; reset rectangle flag
	res	4,(hl)		; reset disable gen input flag
	call	kilbuf		; clear keyboard buffer
	ret

enterredit:				; trigger with keyboard (to kill key buffer)
	call	triggeredit		; call trigger
	call	kilbuf		; clear keyboard buffer 
	ret

triggeredit:			; check if trigger inside map or grid
	call	testmaparea		; test if cursor inside the char map area
	jp	c,triggermap	; if so, perform char map area actions
	call	testgridrea		; test if cursor inside the grid area
	jp	c,triggergrid	; if so, perform grid area actions
	ret

triggermap:				; trigger inside char map
	ld	hl,(curat)		; get xy cursor position
	call	vrampos		; get VRAM cursor position
	call	rdvrm			; get character below
initgrid:				; entry point for initializing
	ld	(edtch),a		; save in editing character
	call edtsetundo		; copy char to undo buffer
	ld	a,(edtch)		; get editing character
	call	drawgrid		; draw an editing grid with character
	ret

triggergrid:			; toggle grid bits
					; do not toggle bit if trig already pressed
	ld	hl,flags+1		; get flags byte
	bit	7,(hl)		; is trigger flag set?
	jr	nz,stgg1		; if it is, skip
					; if not already pressed, toggle bit
	set	7,(hl)		; set trigger flag
	ld	hl,(curat)		; get xy cursor position
	call	vrampos		; get VRAM cursor position
	call	rdvrm			; get character below
	ld	ix,editwork+3	; get toggle byte
	xor	a,(ix)		; toggle on/off
	ld	(edtch+1),a		; save on grid char
					; write bit
stgg1:ld	hl,(curat)		; get cursor xy position
	call	vrampos		; get VRAM cursor position
	ld	a,(edtch+1)		; get grid character
	call	wrtvrm		; write it
	ld	a,(edtch)		; get editing character
	call	updatechar		; update the character from the grid
	ret

updatechar:				; update character from grid
					; input: A=character
	call	getpatternad	; get character pattern address
	ld	hl,0x1895		; get grid top left corner
	ld	c,0x08		; rows/bytes counter
	ld	b,0x08		; columns/bits counter
lupc2:push	bc			; save rows/bytes counter
lupc1:call	rdvrm			; get first grid character
	ld	ix,editwork+1
	sbc	a,(ix)		; subtract from empty one index (bigger)
	rl	c			; rotate left, put carry result on bit 0
	inc	hl			; increment columns/bits
	djnz	lupc1			; do until no more columns/bits
	ld	a,c			; get resulting byte on A
	ex	hl,de			; get character pattern address
	call	wrtvrm		; write created byte
	inc	hl			; increment pattern byte
	ex	hl,de			; get grid address back
	push	de			; save pattern address
	ld	de,0x18		; get amount to next row
	add	hl,de			; add to grid screen address
	pop	de			; get character pattern address back
	pop	bc			; restore rows/bytes counter
	dec	c			; decrement
	jr	nz,lupc2		; loop until no more rows/bytes
	ret

drawgrid:				; draw the editing grid with the character
					; input: A=character
	ld	hl,0x1855		; get editing char position
	call	wrtvrm		; write to screen
					; get character pattern VRAM address
	ld	h,0x00		; put character index in HL
	ld	l,a			;
	add	hl,hl			; multiply by 8 to get the pattern
	add	hl,hl			;
	add	hl,hl			;
	ld	d,h			; put in DE
	ld	e,l			;
	ld	hl,0x1895		; grid top left screen pos
	ld	c,0x08		; grid rows/bytes length
					; rows/bytes
lgrd1:ex	de,hl			; put the pattern address in HL
	call	rdvrm			; get it
	ex	de,hl			; put back in DE
	ld	b,0x08		; grid columns/bits length
					; columns/bits
lgrd2:rla				; put the left bit on the carrier
	push	af			; save the byte being read
	jr	c,sgrd2		; if the bit is 1, get ON char
	ld	a,(editwork+1)	; else get the OFF character
sgrd1:call	wrtvrm		; put it on the screen
	pop	af			; get the byte being read back
	inc	hl			; increment grid column
	djnz	lgrd2			; do until the byte is finished
	inc	de			; increment to next pattern byte
	push	de			; save the pattern address
	ld	de,0x18		; get amount to next row position
	add	hl,de			; add to current position
	pop	de			; get back the pattern address
	dec	c			; decrement rows/bytes counter
	jr	nz,lgrd1		; do while there are rows/bytes
	ret
sgrd2:ld	a,(editwork)	; get ON character
	jr	sgrd1			; return

testmaparea:			; test if cursor inside the char map area
	ld	a,(curat)		; get cursor y pos
	cp	0x1c			; compare with top position
	jr	c,stma1		; if lower, jump
	cp	0x9c			; compare with bottom position
	jr	nc,stma1		; if higher, jump
	ld	a,(curat+1)		; get cursor x pos
	cp	0x14			; compare with left position
	jr	c,stma1		; if lower, jump
	cp	0x94			; compare with right position
	jr	nc,stma1		; if higher, jump
	scf				; set carrier flag
	ret
stma1:scf				; set carrier flag
	ccf				; invert carrier flag
	ret

testgridrea:			; test if cursor inside the grid ares
					; check if inside grid
	ld	a,(curat)		; get cursor y pos
	cp	0x1c			; compare with top position
	jr	c,stga1		; if lower, jump
	cp	0x5c			; compare with bottom position
	jr	nc,stga1		; if higher, jump
	ld	a,(curat+1)		; get cursor x pos
	cp	0xa4			; compare with left position
	jr	c,stga1		; if lower, jump
	cp	0xe4			; compare with right position
	jr	nc,stga1		; if higher, jump
	scf				; set carrier flag
	ret
stga1:scf				; set carrier flag
	ccf				; invert carrier flag
	ret

getchrfromarea:			; get character depending if on grid or map
	call	testmaparea		; test if inside the char map
	ld	a,(edtch)		; get the current character
	ret	nc			; if not inside the char map, skip
	ld	hl,(curat)		; get the cursor xy position
	call	vrampos		; get the cursor VRAM address
	call	rdvrm			; read it
	ret


edtrollchar:			; cycle current character
	call	testgridrea		; is inside the grid?
	ret	nc			; if not, return
	ld	a,(edtch)		; get the current character from variable
	add	a,b			; add offset
	ld	(edtch),a		; put back at variable
	call	edtsetundo		; copy char to undo buffer
	ld	a,(edtch)		; get editing character
	call	drawgrid		; draw the grid
	call	kilbuf		; clear keyboard buffer
	ret

edtsetundo:				; save character to undo buffer
					; copy pattern to undo buffer
	ld	a,(edtch)		; get editing character
	call	getpatternad	; get pattern address
	ld	de,edtundoch	; get RAM undo buffer address
	ld	bc,0x08		; get char length
	call	ldirmv		; save in undo buffer
					; copy color to undo buffer
	ld	a,(edtch)		; get editing character
	call	getcolorfrchr	; get its color address
	ld	de,edtundocl	; get undo buffer
	ld	bc,0x01		; length of 1 byte (ready for hi color mode)
	call	ldirmv		; copy
	call	kilbuf
	ret

edtundo:				; undo last character modifications
					; undo color
	ld	a,(edtch)		; get editing character
	call	getcolorfrchr	; get its color address
	ld	d,h			; put in DE
	ld	e,l 			;
	ld	hl,edtundocl	; get undo buffer address
	ld	bc,0x01		; length of 1 byte (ready for hi color mode)
	call	ldirvm		; copy
					; undo pattern
	ld	a,(edtch)		; get editing character
	call	getpatternad	; get its pattern address
	ld	hl,edtundoch	; get the undo buffer address
	ld	bc,0x08		; whole character
	call	ldirvm		; copy
	ld	a,(edtch)		; get editing character
	call	drawgrid		; draw editing grid
	call	kilbuf		; clear keyboard buffer 
	ret

edtcopy:				; copy character pattern and color
	call	getchrfromarea	; get caracter according to area
					; copy color
seco1:push	af			; save the character
	call	getcolorfrchr	; get its color address
	ld	de,edtcopycl	; get copy buffer address
	ld	bc,0x01		; length of 1 byte (ready for hi color mode)
	call	ldirmv		; copy
					; copy pattern
	pop	af			; restore the character
	call	getpatternad	; get its pattern address
	ld	de,edtcopych	; get the copy buffer address
	ld	bc,0x08		; whole character
	call	ldirmv		; copy
	call	kilbuf		; clear keyboard buffer 
	ret

edtpaste:				; paste character pattern and color
	call	getchrfromarea	; get caracter according to area
sepa3:push	af			; save the character
	dec	b			; decrement paste type byte
	push	bc			; save paste type
	jr	z,sepa1		; if zero skip color	
					; paste color
	call	getcolorfrchr	; get its color address
	ld	d,h			; put in DE
	ld	e,l 			;
	ld	hl,edtcopycl	; get copy buffer address
	ld	bc,0x01		; length of 1 byte (ready for hi color mode)
	call	ldirvm		; copy
sepa1:pop	bc			; restore paste byte
	pop	af			; restore the character
	dec	b			; decrement paste type byte
	jr	z,sepa2		; if zero skip to end
					; paste pattern
	call	getpatternad	; get its pattern address
	ld	hl,edtcopych	; get the copy buffer address
	ld	bc,0x08		; whole character
	call	ldirvm		; copy
sepa2:call	testmaparea		; test if inside the char map
	jr	c,sepa4		; if it is skip draw grid
	ld	a,(edtch)		; get editing character
	call	drawgrid		; draw editing grid
sepa4:call	kilbuf		; clear keyboard buffer 
	ret

edtforecolor:			; edit a character block foreground color
	call	getchrfromarea	; get caracter according to area
sefc1:call	getcolorfrchr	; get its full color (fore and back)
	push	af			; save it
	srl	a			; send high nibble to the low position
	srl	a			; 
	srl	a			;
	srl	a			;
	add	a,b			; add with offset
	sll	a			; send low nibble to the high position
	sll	a			;
	sll	a			;
	sll	a			;
	and	0b11110000		; discard the low nibble
	ld	b,a			; put in B
	pop	af			; restore full color
	and	0b00001111		; discard original high nibble
	or	b			; mix with new high nibble
	call	wrtvrm		; write it
	call	kilbuf		; clear keyboard buffer
	ret

edtbackcolor:			; edit a character b
	call	getchrfromarea	; get caracter according to area
sebc1:call	getcolorfrchr	; get its full color (fore and back)
	push	af			; save it
	add	a,b			; add with offset
	and	0b00001111		; discard high nibble
	ld	b,a			; put in B
	pop	af			; restore full color
	and	0b11110000		; discard original low nibble
	or	b			; mix with new low nibble
	call	wrtvrm		; write it
	call	kilbuf		; clear keyboard buffer
	ret

getcolorfrchr:			; get character color from VRAM table
					; input: A=character
					; output: A=color:HL=address
	srl	a			; divide by 2
	srl	a			; divide by 4
	srl	a			; divide by 8
	ld	h,0x20		; H=high VRAM color table byte
	ld	l,a			; L=low VRAM color table byte
	call	rdvrm			; read color to A
	ret

getpatternad:			; get the pattern address of a character
					; input: A=character
					; output: HL,DE=address
	ld	h,0x00		; put character index in HL
	ld	l,a			;
	add	hl,hl			; multiply by 8 to get the pattern
	add	hl,hl			;
	add	hl,hl			;
	ld	d,h			; put in DE
	ld	e,l			;
	ret

edtinit:				; get editing char and pattern address
					; common header for the editing routines
	call	getchrfromarea	; get caracter according to area
	call	getpatternad	; get its pattern address
	ret

edtfnish:				; get editing char and update the grid
					; common footer for the editing routines
	ld	a,(edtch)		; get editing character
	call	drawgrid		; draw the grid
	call	kilbuf		; clear keyboard buffer
	ret

edtinvert:				; invert grid
	call	edtinit		; get editing char and pattern address
	ld	b,0x08		; set length to whole character
leinv:call	rdvrm			; read pattern address byte
	cpl				; invert it
	call	wrtvrm		; write it back
	inc	hl			; increment pattern address
	djnz	leinv			; repeat until done
	call	edtfnish		; draw the grid with editing char
	ret

edtcheckb:
	call	edtinit		; get editing char and pattern address
	ld	b,0x04		; set length to half character
leckbd:ld	a,0xaa		; create even pattern
	call	wrtvrm		; write it
	inc	hl			; increment pattern address
	ld	a,0x55		; create odd pattern
	call	wrtvrm		; write it
	inc	hl			; increment pattern address
	djnz	leckbd		; repeat until done
	call	edtfnish		; draw the grid with editing char
	ret

edtfill:				; fill grid
	call	edtinit		; get editing char and pattern address
	ld	bc,0x08		; set length to whole character
	ld	a,0xff		; set all bits
	call	filvrm		; fill editing character
	call	edtfnish		; draw the grid with editing char
	ret

edtclear:				; clear grid
	call	edtinit		; get editing char and pattern address
	ld	bc,0x08		; set length to whole character
	ld	a,0x00		; reset all bits
	call	filvrm		; fill editing character
	call	edtfnish		; draw the grid with editing char
	ret

edtscrlft:				; scroll grid left
	call	edtinit		; get editing char and pattern address
	ld	b,0x08		; set length to whole character
lescl:call	rdvrm			; get first pattern byte
	rlc	a			; rotate bits left
	call	wrtvrm		; write it back
	inc	hl			; go to next byte
	djnz	lescl			; do until finished
	call	edtfnish		; draw the grid with editing char
	ret

edtscrlrgt:				; scroll grid right
	call	edtinit		; get editing char and pattern address
	ld	b,0x08		; set length to whole character
lescr:call	rdvrm			; get first pattern byte
	rrc	a			; rotate bits right
	call	wrtvrm		; write it back
	inc	hl			; go to next byte
	djnz	lescr			; do until finished
	call	edtfnish		; draw the grid with editing char
	ret

edtscrup:				; scroll grid up
	call	edtinit		; get editing char and pattern address
	call	rdvrm			; get first pattern byte
	inc	hl			; goto next byte
	ld	bc,0x07		; set length of 7 bytes
	call	streamvv		; copy the lower byte to the higher byte
	dec	hl			; go back to the last byte
	call	wrtvrm		; write first pattern byte on last address
	call	edtfnish		; draw the grid with editing char
	ret

edtscrdwn:				; scroll grid down
	call	edtinit		; get editing char and pattern address
	ld	de,0x07		; set for the last (7th) byte
	add	hl,de			; add it to the pattern address
	ld	b,0x07		; set length of 7 bytes
	call	rdvrm			; get the last (7th) byte
	push	af			; save it
lescd:dec	hl			; decrement patter address
	call	rdvrm			; get pattern
	inc	hl			; increment patter address
	call	wrtvrm		; write pattern
	dec	hl			; decrement pattern address
	djnz	lescd			; repeat until finished
	pop	af			; restore last pattern byte
	call	wrtvrm		; write on first address
	call	edtfnish		; draw the grid with editing char
	ret

edtflipvrt:				; flip grid vertically
	call	edtinit		; get editing char and pattern address
	ld	c,0x08		; rows/bytes length
lefv2:ld	b,0x08		; columns/bits length
	call	rdvrm			; read pattern
	ld	d,a			; save to D
lefv1:rl	d			; rotate its bits left and put on the carrier
	rra				; put the carrier on the A right bit
	djnz	lefv1			; repeat until all columns/bits are done
	call	wrtvrm		; write it back
	inc	hl			; go to next pattern byte
	dec	c			; decrement rows/bytes counter
	jr	nz,lefv2		; do until all rows/bytes are done
	call	edtfnish		; draw the grid with editing char
	ret

edtfliphrz:				; flip grid horizontally
	call	edtinit		; get editing char and pattern address
	push	de			; save pattern address
	ld	b,0x08		; set rows/ bytes counter
	ld	de,edthelpch+7	; get the temp char last (7th) pattern address
lefh1:call	rdvrm			; read the first editing char pattern address
	ld	(de),a		; put first editing byte on last temp byte
	inc	hl			; increment editing
	dec	de			; decrement temp
	djnz	lefh1			; do until character finished
	ld	hl,edthelpch	; get address of temp character
	pop	de			; restore address of editing char pattern
	ld	bc,0x08		; set the length to all character
	call	ldirvm		; copy temp character to editing character
	call	edtfnish		; draw the grid with editing char
	ret

edtrotrgt:				; rotate grid right
	call	edtinit		; get editing char and pattern address
	ld	c,0x08		; set rows/bytes counter
lerr1:ld	b,0x08		; set columns/bits counter
	push	hl			; save editing char pattern address
	call	rdvrm			; read pattern byte
	ld	hl,edthelpch	; get temp char pattern address 
lerr2:rla				; rotate editing char pttrn byte left and get carrier
	rr	(hl)			; rotate right temp char pttrn byte inserting carrier 
	inc	hl			; increment temp char pattern byte
	djnz	lerr2			; repeat until finished
	pop	hl			; restore editing char pattern address
	inc	hl			; increment it
	dec	c			; decrement rows/bytes counter
	jr	nz,lerr1		; repeat until finished
	ld	hl,edthelpch	; get temp char pattern address
	ld	bc,0x08		; set length to whole character
	call	ldirvm		; put on editing character
	call	edtfnish		; draw the grid with editing char
	ret

edtrotlft:				; rotate grid left
	call	edtinit		; get editing char and pattern address
	ld	c,0x08		; set rows/bytes counter
lerl1:ld	b,0x08		; set columns/bits counter
	push	hl			; save editing char pattern address
	call	rdvrm			; read pattern byte
	ld	hl,edthelpch	; get temp char pattern address 
lerl2:rra				; rotate editing char pttrn byte right and get carrier
	rl	(hl)			; rotate left temp char pttrn byte inserting carrier 
	inc	hl			; increment temp char pattern byte
	djnz	lerl2			; repeat until finished
	pop	hl			; restore editing char pattern address
	inc	hl			; increment it
	dec	c			; decrement rows/bytes counter
	jr	nz,lerl1		; repeat until finished
	ld	hl,edthelpch	; get temp char pattern address
	ld	bc,0x08		; set length to whole character
	call	ldirvm		; put on editing character
	call	edtfnish		; draw the grid with editing char
	ret
