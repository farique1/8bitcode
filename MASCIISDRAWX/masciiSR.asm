; ------------------SUPPORT ROUTINES

setundo:				; copy screen to undo buffer
	ld	hl,0x1800		; screen address
	ld	de,VRBUF2		; undo buffer
	ld	bc,0x300		; screen size
	call	streamvv		; copy screen for undo
	call	kilbuf
	ret

copyscrtobuf2:			; save screen in buffer 2
	ld	hl,0x1800		; initial VRAM pos
	ld	de,buffer2		; buffer to use
	ld	bc,0x0300		; the whole screen
	call	ldirmv		; put VRAM on RAM
	ret

copytovram:				; copy amount to VRAm and increase HL and DE
					; input: C=amount,DE=RAM,HL=VRAM
					; output: HL+C,DE+C
	push	bc			; save B
	ld	b,c			; copy amount to B
lpctv:ld	a,(de)		; get byte
	call	wrtvrm		; write to VRAM
	inc	hl			; next VRAM address
	inc	de			; next RAM address
	djnz	lpctv			; loop
	pop	bc			; restore B
	ret

swapvv:				; swap between VRAM areas
					; input: HL=area 1,DE=area 2,BC=length
	call	rdvrm			; read char from orig
	push	af
	ex	de,hl			; exchange orig x dest
	call	rdvrm			; read char from orig
	ex	af,af'
	pop	af
	call	wrtvrm		; write char to dest
	ex	af,af'
	ex	de,hl			; exchange back
	call	wrtvrm		; write char to dest
	inc	de			; increment destination
	inc	hl			; increment origin
	dec	bc			; decrement length counter
	ld	a,b			; put high byte in A
	or	c			; compare with C
	jp	nz,swapvv		; if both not 0, loop
	call	kilbuf		; clear keyboard buffer
	ret

streamvv:				; stream from VRAM to VRAM
					; input: HL=origin,DE=dest,BC=length
	push	af			; save A
lstv1:call	rdvrm			; read char from orig
	ex	de,hl			; exchange orig x dest
	call	wrtvrm		; write char to dest
	ex	de,hl			; exchange back
	inc	de			; increment destination
	inc	hl			; increment origin
	dec	bc			; decrement length counter
	ld	a,b			; put high byte in A
	or	c			; compare with C
	jp	nz,lstv1		; if both not 0, loop
	pop	af			; restore A
	ret

createcharmap:			; create a 16x16 character map
					; input: HL=top left corner
	ld	de,0x10		; get VRAM advance amount for next row
	ld	b,0x10		; get columns amount
	ld	c,b			; put also in rows amount
	ld	a,0			; define character as 0
lsec1:call	wrtvrm		; write character on the screen
	inc	a			; go to the next character
	inc	hl			; go to the next screen column
	djnz	lsec1			; dec columns counter, if not 0, loop
	add	hl,de			; advance to the start of next row
	ld	b,0x10		; reset columns counter
	dec	c			; decrement lines counter
	jr	nz,lsec1		; if not 0, loop
	ret

drawlogosmall:			; draw a small MASCIIDRAWX logo
					; input: HL=screen pos
	ld	a,0x80		; get first character
	ld	b,0x08		; get length
ldls1:call	wrtvrm		; write char on screen	
	inc	a			; next char
	inc	hl			; next screen pos
	djnz	ldls1			; do until finished
	ret

boxrv:				; copy rectangle from RAM to VRAM
					; input: HL=buffer,RECTT variable +1=VRAM pos,+3=height,+4=width
	ld	de,(rectt+1)	; get initial VRAM pos 
	ld	a,(rectt+3)		; get height
	ld	b,a			; put in B
lbxrv:push	bc			; save height
	ld	a,(rectt+4)		; get width
	ld	b,0			; put in BC
	ld	c,a			;
	push	bc			; save width
	push	hl			; save origin RAM
	push	de			; save destination VRAM
					; prevent pasting beyond screen area
	push	de			; save VRAM address
	ex	de,hl			; exchange with HL
	add	hl,bc			; add to width
	dec	hl			; decrement 1 to adjust
	ld	a,h			; get VRAM address high byte
	cp	0x1b			; compare to the end of screen
	ex	de,hl			; exchange back
	pop	de			; restore VRAM address
	jr	nc,sbxrv		; if higher, skip

	call	ldirvm		; fill VRAM width
sbxrv:pop	hl			; get VRAM
	ld	de,32			; get 32
	add	hl,de			; add 32 to VRAM (next line)
	ex	de,hl			; DE=VRAM >< HL=32
	pop	hl			; get RAM
	pop	bc			; get width
	push	de			; save VRAM
	ld	d,b			; DE = width
	ld	e,c			;
	add	hl,de			; add width to RAM
	pop	de			; get VRAM
	pop	bc			; get height
	djnz	lbxrv			; decrease and loop until 0
	ret

boxvrb1:				; copy rectangle from VRAM to RAM
					; send to BOXVRB2 to preserv buffer 1 content dimensions
					; input: DE=buffer,RECTT variable +1=VRAM pos,+3=height,+4=width
	ld	hl,(rectt+3)	; get box dimensions
	ld	(buffcont1),hl	; save in buffer 1 content dimensions
boxvrb2:				; entry point for buffer 2
	ld	hl,(rectt+1)	; get initial VRAM pos 
	ld	a,(rectt+3)		; get height
	ld	b,a			; put in B
lbxvr:push	bc			; save height
	ld	a,(rectt+4)		; get width
	ld	b,0			; put in BC
	ld	c,a			;
	push	bc			; save width
	push	de			; save RAM
	push	hl			; save VRAM
	call	ldirmv		; fill VRAM width
	pop	hl			; get VRAM
	ld	de,0x20		; increment for advancing screen line
	add	hl,de			; add 32 to VRAM (lext line)
	pop	de			; get RAM
	ex	de,hl			; DE=VRAM >< HL=RAM
	pop	bc			; get width
	push	de			; save VRAM
	ld	d,b			; DE = width
	ld	e,c			; 
	add	hl,de			; add width to RAM
	pop	de			; get VRAM
	ex	de,hl			; DE=RAM >< HL=VRAM
	pop	bc			; get height
	djnz	lbxvr			; decrease and loop until 0
	ret

hidecur:				; hide the cursor
	ld	a,209			; get 209 for position
	ld	hl,(spcursor+6)	; get cursor sprite y VRAM pos
	call	wrtvrm		; assign
	ret

hidecharcur:			; hide the character cursors
	ld	a,209			; get 209 for position
	ld	hl,(spcharac+6)	; get character sprite y VRAM pos
	call	wrtvrm		; assign
	ld	hl,(spbckgrd+6)	; get background sprite y VRAM pos
	call	wrtvrm		; assign
	ret

hideboxcur:				; hide the rectangle cursors
	ld	a,209			; get 209 for y pos
	ld	hl,(spboxfil+6)	; get box fil sprite y pos
	call	wrtvrm		; assign
	ld	hl,(spboxhol+6)	; get box hollow sprite y pos
	call	wrtvrm		; assign
	ld	hl,(spboxlin+6)	; get box line sprite y pos
	call	wrtvrm		; assign
	ld	hl,(spboxadd+6)	; get box line add sprite y pos
	call	wrtvrm		; assign
	ret

chbordcolor:			; change the border color
					; input: A=color
	ld (BDRCLR),a		; set border color variable (for outside use)
	push	af			; save border color
	ld	a,(7)			; A = Main-ROM slot
	ld	c,a			; put in C
	inc	c			; C = CPU port connected to the VDP writing port #1
 
	di				; Interrupts must be disabled here
	pop	af			; restore border color
	out	(c),a			; Write the color
	ld 	b,0x07+0x80		; pick border color register 
	out	(c),b			; Write register 7 (with the bit 7 (0x80) always set)
	ei				; Interrupts can be enabled here
	ret

add8to16:				; add 8 bits to 16 bits
					; input: DE=16bits,A=8bits
					; output: DE+A
	add	a,e			; add A to E
	ld	e,a			; put A in E
	adc	a,d			; add A to D with carry
	sub	e			; subtract E from A
	ld	d,a			; put A in D
	ret

toggleflags:			; toggle bits on the flag byte
					; input B as bitmask (1 toggle, 0 stay)
	ld	a,(flags)		; read flags byte
	xor	b 			; toggle bits
	ld	(flags),a		; save flags byte
	call	kilbuf		; clear keyboard buffer
	ret

checkcurspos:			; chech current cursor position (CURAT)
					; output: C if beyond VRAM screen area
					; alter: A
	ld	hl,(curat)		; get x,y (read bytes pos inverted)
	call	vrampos		; get the VRAM position
	ld	a,0x1a		; get end of screen VRAM high byte
	cp	h			; compare with character pos high byte
	ret

wrtvrms:				; Write to VRAM safe
					; do not write under &h1800 or over &h1aff
					; input: H=RAM position high byte,A=character
	push	af			; save character
	ld	a,0x17		; high byte of address before the screen area
	cp	h			; compare to current high byte
	jr	nc,swtvr		; if equal or lower, skip writing
	ld	a,0x1a		; get end of screen VRAM high byte
	cp	h			; compare with character pos high byte
	jr	c,swtvr		; if higher, skip writing
	pop	af			; restore character
	call	wrtvrm		; write to VRAM
	ret
swtvr:pop	af			; restore character
	ret

vrampos:				; calc character screen position
					; input: h=x,l=y (191,255)
					; output: hl = VRAM position (&h1800-&h1aff)
					; alter: a, hl
	push	hl			; save HL with x,y information
	ld	a,l			; get y pos
	add	4			; offset cursor
	and	0b11111000		; step by 8 increments
	ld	h,0			; pos y to L
	ld	l,a			;
	add	hl,hl			; hl * 2
	add	hl,hl			; hl * 4

	ex	de,hl			; preserve HL excanging with DE
	pop	hl			; restore previous HL with x,y information
	ld	a,h			; get x pos
	ex	de,hl			; restore original HL and DE values

	add	4			; offset cursor
	and	0b11111000		; step by 8 increments
	rrca				; divide by 2
	rrca				; divide by 4
	rrca				; divide by 8

	add	a,l 			; add l to x pos
	ld	l,a			; put in l
	ld	a,0x18		; put &h18 in a
	add	a,h			; add h to a
	ld	h,a			; put in h
	ret
