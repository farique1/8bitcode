; ------------------RECTANGLE OPERATIONS

preparerect:			; prepare to get a rectangle
	ld	a,b			; get the rectangle type
	ld	(rectt),a		; save it
	call	checkcurspos	; check if cursor is inside the screen
	ret	c			; return if not
	call	preprectcurs	; prepare rectangle cursors
					; define y position to horizontal box cursors
					; * no DEC A y adjustment in rect routine to avoid going >0 *
	ld	hl,curat+2		; get mem pos to y1
	ld	a,(curat)		; get y
	add	4			; offset cursor
	and	0b11111000		; step by 8 increments
	ld	(hl),a		; save
					; define x position to vertical box cursors
	ld	hl,curat+3		; get mem pos to x1
	ld	a,(curat+1)		; get x
	add	4			; offset cursor
	and	0b11111000		; step by 8 increments
	ld	(hl),a		; copy x to x1
					; set flags
	ld	hl,flags		; get flags byte
	set	4,(hl)		; set rectangle flag
	set	5,(hl)		; set disable gen input flag
	call	kilbuf		; clear keyboard buffer
	ret

changerect:				; change the box mode
	ld	a,(rectt)		; get the current box mode
	cp	0x06			; is the paste box?
	jr	z,schr1		; if so, skip
	inc	a			; increment
	cp	0x05			; is past last mode?
	jr	nz,schr1		; if not, skip
	ld	a,0x00		; set the first box mode
schr1:ld	(rectt),a		; assign box mode
	call	preprectcurs	; reconfigure cursors
	call	kilbuf		; clear keyboard buffer
	ret

showrectcurs:			; draw rectangle cursors on the screen
					; input: CURAT - cursor positions
					; * no DEC A y adjustment in rect routine to avoid going >0 *
	ld	a,(curat)		; get pos y
	add	4			; offset cursor
	and	0b11111000		; step by 8 increments
	ld	hl,(spboxlin+6)	; sprite 3 VRAM attribute pos y
	call	wrtvrm		; write pos y to VRAM
	ld	hl,(spboxadd+6)	; sprite 4 VRAM attribute pos y
	call	wrtvrm		; write pos y to VRAM

	ld	a,(curat+1)		; get pos x
	add	4			; offset cursor
	and	0b11111000		; step by 8 increments
	ld	hl,(spboxhol+6)	; sprite 2 VRAM attribute pos y
	inc	hl			; inc to pos x
	call	wrtvrm		; write pos y to VRAM
	ld	hl,(spboxadd+6)	; sprite 4 VRAM attribute pos y
	inc	hl			; inc topos x
	call	wrtvrm		; write pos y to VRAM

	ld	a,(curat+2)		; get pos y1
	add	4			; offset cursor
	and	0b11111000		; step by 8 increments
	ld	hl,(spboxfil+6)	; sprite 1 VRAM attribute pos y
	call	wrtvrm		; write pos y1 to VRAM
	ld	hl,(spboxhol+6)	; sprite 2 VRAM attribute pos y
	call	wrtvrm		; write pos y1 to VRAM

	ld	a,(curat+3)		; get pos x1
	add	4			; offset cursor
	and	0b11111000		; step by 8 increments
	ld	hl,(spboxfil+6)	; sprite 1 VRAM attribute pos y
	inc	hl			; inc to pos x
	call	wrtvrm		; write pos x1 to VRAM
	ld	hl,(spboxlin+6)	; sprite 3 VRAM attribute pos y
	inc	hl			; inc to pos x
	call	wrtvrm		; write pos x1 to VRAM
	ret

activaterect:			; accept rectangle and prepare to draw
					; check if is out of screen
	call	checkcurspos	; check if cursor is inside screen
	ret	c			; return if not
					; adjust y position
					; * no DEC A y adjustment in rect routine to avoid going >0 *
	ld	a,(curat)		; get y
	ld	(curat+4),a		; save copy
	add	4			; offset cursor
	and	0b11111000		; step by 8 increments
	ld	(curat),a		; save
					; adjust x position
	ld	a,(curat+1)		; get x
	ld	(curat+5),a		; save copy
	add	4			; offset cursor
	and	0b11111000		; step by 8 increments
	ld	(curat+1),a		; save

	call	setundo		; copy screen for undo
	call	getvramrectpos	; convert x,y,x1,y1 (curat) to memory positions (RECTT)

	ld	a,(rectt)		; get the rectangle type
	cp	0x01			; compare with 1 (filled box)
	call	z,drawboxhol
	ld	a,(rectt)		; get the rectangle type
	cp	0x00			; compare with 0 (hollow box)
	call	z,drawboxfil
	ld	a,(rectt)		; get the rectangle type
	cp	0x02			; compare with 2 (line box)
	call	z,drawboxlin
	ld	a,(rectt)		; get the rectangle type
	cp	0x03			; compare with 3 (line add box)
	call	z,drawboxlin
	ld	a,(rectt)		; get the rectangle type
	cp	0x04			; compare with 4 (line add box)
	call	z,drawboxchr
	ld	a,(rectt)		; get the rectangle type
	cp	0x05			; compare with 5 (copy)
	call	z,getpastebox

	ld	hl,(curat+4)	; get saved x,y cursor pos
	ld	(curat),hl		; put on current variable

	call	cancelrect		; cancel rectangle
	call	waittrgrls		; wait for trigger release
	ret

cancelrect:				; cancel the rectangle
	call	hideboxcur		; hide box cursors
					; reset flags
	ld	hl,flags		; get flags byte
	res	4,(hl)		; reset disable gen input flag
	res	5,(hl)		; reset rectangle flag
	call	kilbuf		; clear keyboard buffer
	ret

preprectcurs:			; prepare the rectangle cursors
					; change rectangle cursors patterns
	ld	a,(rectt)		; get rectangle type for the sprite shape
	inc	a			; increment 1 to sync shape with type
	ld	hl,(spboxfil+6)	; get box fil sprite y pos
	inc	hl			; inc to pattern number
	inc	hl			;
	call	wrtvrm		; assign
	ld	hl,(spboxhol+6)	; get box hol sprite y pos
	inc	hl			; inc to pattern number
	inc	hl			;
	call	wrtvrm		; assign
	ld	hl,(spboxlin+6)	; get box line sprite y pos
	inc	hl			; inc to pattern number
	inc	hl			;
	call	wrtvrm		; assign
	ld	hl,(spboxadd+6)	; get box line add sprite y pos
	inc	hl			; inc to pattern number
	inc	hl			;
	call	wrtvrm		; assign
	call	hidecharcur		; hide character cursor
	ret

getvramrectpos:			; get VRAM data from rectangle cursor positions
					; input: CURAT0-3 cursor variable (y,x,y1,x1) pixels (255,191)
					; output: RECTT1-4 rectangle variables (start addr,width,height) VRAM bytes

					; swap x<>x1, y<>y1 if the former is bigger
	ld	a,(curat)		; get cursor pos y (current)
	ld	hl,curat+2		; get location for cursor pos y1 (anchor)
	cp	(hl)			; compare y with y1
	jr	c,acniy		; if y1 greater than y jump over swap
	ld	b,a			; put y in B
	ld	a,(curat+2)		; get y1
	ld	(curat),a		; put in previous y pos
	ld	a,b			; get y back from B
	ld	(curat+2),a		; put in previous y1 pos
acniy:ld	a,(curat+1)		; get cursor pos x (current)
	ld	hl,curat+3		; get location for cursor pos x1 (anchor)
	cp	(hl)			; compare x with x1
	jr	c,acnix		; if x1 greater then x jump over swap
	ld	b,a			; put x in B
	ld	a,(curat+3)		; get x1
	ld	(curat+1),a		; put in previous x1 pos
	ld	a,b			; get x back from B
	ld	(curat+3),a		; put in previous x1 pos
					; get VRAM pos,w,h from x,y,x1,y1
acnix:ld	hl,(curat)		; get x,y (read bytes pos inverted)
	call	vrampos		; convert to VRAM position
	ld	(rectt+1),hl	; save to rectangle RAM
					; get height from y and y1
	ld	a,(curat)		; get y
	ld	b,a			; send to B
	ld	a,(curat+2)		; get y1
	sub	b			; subtract y from y1
	rrca				; divide by 2
	rrca				; divide by 4
	rrca				; divide by 8
	inc	a			; add another position
	ld	(rectt+3),a		; save height in bytes
					; get width from x and x1
	ld	a,(curat+1)		; get x
	ld	b,a			; send to B
	ld	a,(curat+3)		; get x1
	sub	b			; subtract x from x1
	rrca				; divide by 2
	rrca				; divide by 4
	rrca				; divide by 8
	inc	a			; add another position
	ld	(rectt+4),a		; save width in bytes
	ret

pasteoldbuf:			; paste from existing buffer content
					; check if copy in buffer
	ld	a,(buffcont1)	; get buffer 1 height
	ld	b,a			; put in B
	ld	a,(buffcont1+1)	; get buffer 1 width
	or	b			; combine height with width
	cp	0x00			; is any of them 0?
	ret	z			; if so, return
	call	setundo		; copy screen for undo
					; define area and activate paste mode
	ld	hl,(buffcont1)	; get buffer 1 content dimensions
	ld	(rectt+3),hl	; put on rectangle variable
	call	getpasteold		; activate paste mode
	call	kilbuf		; clear keyboard buffer
	ret

getpastebox:			; copy the rectangle area
	ld	hl,(curat)		; get current x,y cursor pos
	ld	(curat+4),hl	; put on saved to keep the tpo left corner
	ld	de,buffer1		; get RAM area to copy to
	call	boxvrb1		; copy VRAM to RAM
getpasteold:			; entry point for getting existing buffer
	ld	a,0x06		; get number of paste sprite (ninus 1)
	ld	(rectt),a		; put in memory
	call	preprectcurs	; prepare rectangle cursors
					; set flags
	ld	hl,flags		; get flags byte
	set	4,(hl)		; set dis gen input flag
	set	3,(hl)		; set paste box flag
	ret

showpastebox:			; show the content to be pasted
	ld	hl,(curat)		; get current cursor position
	call	vrampos		; convert to VRAM pos
	ld	(rectt+1),hl	; save in memory
					; save screen area
	ld	de,buffer2		; get the RAM area to copy to
	call	boxvrb2		; save VRAM area under cursor to RAM
					; draw paste content on the screen
	ld	hl,buffer1		; get copied data RAM area
	call	boxrv			; show on the screen at the cursor position
					; do stuff while the content is shown on screen
					; update y1 position
	ld	a,(curat)		; get y
	ld	b,a			; copy to B
	ld	a,(rectt+3)		; get height
	dec	a			; decrement 1 row
	rlca				; multiply by 2
	rlca				; multiply by 4
	rlca				; multiply by 8
	add	b			; add to y
	ld	hl,curat+2		; get y1 mem pos
	ld	(hl),a		; save y to y1
					; update x1 position
	ld	a,(curat+1)		; get x
	ld	b,a			; copy to B
	ld	a,(rectt+4)		; get width
	dec	a			; decrement 1 column
	rlca				; multiply by 2
	rlca				; multiply by 4
	rlca				; multiply by 8
	add	b			; add to x
	ld	hl,curat+3		; get x1 mem pos
	ld	(hl),a		; save x to x1

	call	showrectcurs	; show the rectangle cursors
					; put original content back on the screen
	ld	hl,buffer2		; get the RAM area to copy from
	call	boxrv			; restore area under the cursor
	ret

pastethebox:			; actually paste the copied area
	ld	hl,(curat)		; get current cursor position
	call	vrampos		; convert to VRAM pos
	ld	(rectt+1),hl	; save in memory
	ld	hl,buffer1		; get copied data RAM area
	call	boxrv			; paste to the screen at the cursor position
	ret

erasewithbox:			; erase screen area with the paste box
	ld	hl,(curat)		; get current cursor position
	call	vrampos		; convert to VRAM pos
	ld	(rectt+1),hl	; save in memory
	ld	a,(curat+6)		; get current char
	push	af			; save for later
	ld	a,(curat+8)		; get erase char
	ld	(curat+6),a		; put in current char
	call	drawboxfil		; draw a filled box with it
	pop	af			; get saved char
	ld	(curat+6),a		; put back in current
	ret

cancelpaste:			; cancel the paste box
	call	hideboxcur		; hide box cursors
					; reset flags
	ld	hl,flags		; get flags byte
	res	3,(hl)		; reset dis gen input flag
	res	4,(hl)		; reset paste box flag
	call	kilbuf		; clear keyboard buffer
	ret

drawboxchr:				; draw a filled box with sequential chars
	ld	hl,(rectt+1)	; get initial VRAM position
	ld	a,(rectt+4)		; get width
	ld	b,a			; put in B
	sub	0x20			; subtract from screen width
	neg				; negative it cause screen is bigger
	ld	e,a			; put in DE
	ld	d,0x00		;
	ld	a,(rectt+3)		; get height
	ld	c,a			; put in C
	ld	a,(curat+6)		; get current character
ldbc2:push	bc			; save counters
ldbc1:call	wrtvrms		; write character
	inc	hl			; increment column
	inc	a			; increment character index
	djnz	ldbc1			; do whole width
	pop	bc			; restore counters
	add	hl,de			; go to next screen line
	dec	c			; decrement one height unit
	jr	nz,ldbc2		; if not 0, loop
	ret

drawboxfil:				; draw a filled box with the curr char
	ld	hl,(rectt+1)	; get initial VRAM position
	ld	a,(rectt+4)		; get width
	ld	b,a			; put in B
	ld	a,(rectt+3)		; get height
	ld	c,a			; put in C
	push	hl			; save initial VRAM position
ldbfl:call	drawhorizline	; draw line
	pop	hl			; restore VRAM position
	ld	de,0x20		; get VRAM line advance amount
	add	hl,de			; add one screen line
	push	hl			; save VRAM position
	ld	a,(rectt+4)		; restore width
	ld	b,a			; put in B
	dec	c			; decrement one height unit
	jr	nz,ldbfl		; if not 0, loop
	pop	hl			; clear remnant stack push
	ret

drawboxhol:				; draw a hollow box with the cur char
					; upper line
	ld	hl,(rectt+1)	; get initial VRAM pos
	ld	a,(rectt+4)		; get width
	ld	b,a			; put in B
	call	drawhorizline	; draw line
					; right line (use updated HL pos)
	dec	hl			; back VRAM one x position
	ld	a,(rectt+3)		; get height
	ld	b,a			; put in B
	call	drawvertline	; draw line	ret
					; left line
	ld	hl,(rectt+1)	; get initial VRAM pos
	ld	a,(rectt+3)		; get height
	ld	b,a			; put in B
	call	drawvertline	; draw line
					; lower line (use updated HL pos and assigned DE)
	sbc	hl,de			; back VRAM one line
	ld	a,(rectt+4)		; get width
	ld	b,a			; put in B
	call	drawhorizline	; draw line
	ret

drawboxlin:				; draw a line box with or without adding the lines
					; if box = 1x1 draw a cross
	ld	hl,(rectt+3)	; get w,h
	ld	de,0x0101		; see if both are 1
	call	dcompr		; compare them
	jr	nz,sdbl1		; if any is different, skip
	ld	hl,(rectt+1)	; get initial VRAM pos
	ld	a,0x15		; get cross line character
	call	wrtvrms		; write on the screen
	ret
					; if box has no width draw vert line
sdbl1:ld	a,0x01		; get 1 to compare
	cp	h			; compare with width
	jr	nz,sdbl2		; if is not, skip
	ld	b,l			; if it is put height on counter
	ld	a,0x16		; get the vertical bar character
	ld	(curat+6),a		; put on current character
	ld	hl,(rectt+1)	; get initial VRAM pos
	call	drawvertline	; draw line
	ret
					; if box has no height draw horiz line
sdbl2:ld	a,0x01		; get 1 to compare
	cp	l			; compare with height
	jr	nz,sdbl3		; if is not, skip
	ld	b,h			; if it is put width on counter
	ld	a,0x17		; get the horizontal bar character
	ld	hl,(rectt+1)	; get initial VRAM pos
	ld	(curat+6),a		; put on current character
	call	drawhorizline	; draw line
	ret
sdbl3:ld	a,(curat+6)		; get current character
	push	af			; save it to retrieve at the end
					; upper line
	ld	hl,(rectt+1)	; get initial VRAM pos
	ld	a,0x18		; get top left corner character
	call	drawrectch		; write it according to type
	inc	hl			; go to next column to spare the corner
	ld	a,(rectt+4)		; get width
	dec	a			; decrease 2 to spare the corner
	dec	a			;
	ld	b,a			; put in B
	ld	a,0x17		; get horizontal bar character
	ld	(curat+6),a		; put on current character
	call	drawhorizline	; draw line
	ld	a,0x19		; get top right corner character
	call	drawrectch		; write it according to type
					; right line (use updated HL pos)
	ld	a,(rectt+3)		; get height
	dec	a			; decrease 2 to spare the corner
	dec	a			;
	ld	b,a			; put in B
	ld	de,0x20		; increment for advancing screen line
	add	hl,de			; go to next line to spare the corner
	ld	a,0x16		; get vertical bar character
	ld	(curat+6),a		; put on current character
	call	drawvertline	; draw line
	ld	a,0x1b		; get bottom right corner character
	call	drawrectch		; write it according to type
					; left line
	ld	hl,(rectt+1)	; get initial VRAM pos
	add	hl,de			; go to next line to spare the corner
	ld	a,(rectt+3)		; get height
	dec	a			; decrease 2 to spare the corner
	dec	a			;
	ld	b,a			; put in B
	call	drawvertline	; draw line
	ld	a,0x1a		; get bottom left corner character
	call	drawrectch		; write it according to type
					; lower line (use updated HL pos and assigned DE)
	ld	a,(rectt+4)		; get width
	dec	a			; decrease 2 to spare the corner
	dec	a			;
	ld	b,a			; put in B
	inc	hl			; go to next column to spare the corner
	ld	a,0x17		; get horizontal bar character
	ld	(curat+6),a		; put on current character
	call	drawhorizline	; draw line
	pop	af			; restore current character
	ld	(curat+6),a		; put it back on memory
	ret

drawvertline:			; draw a vertical or horizontal line
					; input: HL=VRAM start,B=height,CURAT+6=cur char,RECTT=rect type(for DRAWRECTCH)
	ld	de,0x20		; increment for advancing VRAM pos
	jr	sdrli			; skip to draw routine
drawhorizline:			; entry point for horizontal line
	ld	de,0x01		; increment for advancing VRAM pos
					; check if has any length
sdrli:ld	a,0x00		; get 0 to compare
	cp	b			; compare to length
	ret	z			; if match, go back
ldrli:ld	a,(curat+6)		; get current character
	call	drawrectch		; write it according to type
	add	hl,de			; increase VRAM pos
	djnz	ldrli			; decrease length, if not 0, loop
	ret

drawrectch:				; draw character depending on rectangle type
					; input: HL=VRAM pos,A=curr char,RECTT=rectangle type
	push	af			; save current character
	ld	a,(rectt)		; get rectangle type
	cp	0x03			; is line add?
	jr	nz,sdrr1		; if not, skip to next test
					; draw characters adding lines
					; check character already on the screen
	call	rdvrm			; get char on the screen
	cp	0x10			; compare with lower lines character
	jr	c,sdrr1		; if lower, skip everything
	cp	0x1c			; compare with higher lines character
	jr	nc,sdrr1		; if higher, skip everything
					; draw according to what already is on the screen position
	pop	af			; remove old AF from stack
	push	hl			; save VRAM position
	exx				; save BC,DE,HL
					; replace characters using a Look Up Table
					; calculate LUT row to use
	ld	b,0x15		; the lowest possible curr char number -1
	sub	b			; subtract from the current char number
	ld	b,a			; put result in B for the counter
	ld	a,0			; prepare A for the count
	ld	e,11			; get the number to advance a LUT row
ldrre:add	a,e			; advance a row in the LUT
	djnz	ldrre			; decrement counter and loop if not 0
	sub	e			; go to the start of the current LUT row
	ld	e,a			; put calculated LUT row in E
					; calculate LUT column
	pop	hl			; restore VRAM position
	call	rdvrm			; get character in it
	ld	b,0x11		; the lowest line char number minus
	sub	b			; subtract from the screen char number
	add	a,e			; add to the LUT row
	ld	d,0			; put LUT position in DE
	ld	e,a			;
	ld	hl,lineaddlut	; get LUT start address
	add	hl,de			; add to calculated number
	ld	a,(hl)		; get character in that address
	push	af			; save character from LUT
	exx				; restore BC,DE,HL
sdrr1:pop	af			; restore saved char
	call	wrtvrms		; write on the screen
	ret
