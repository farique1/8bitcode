; ------------------MISCELLANEOUS

; ------------------IO

fileinput:				; load drawing
	call	kilbuf		; clear keyboard buffer
	ld	hl,CSRSW		; enable text cursor
	ld	(hl),0x00		;
	ld	hl,bascm		; get basic communication byte
	ld	(hl),0x01		; insert load flag
	ld	hl,flags		; get flags byte
	set	7,(hl)		; set quit flag
	call	clssavescr		; save current drawing
	ret

fileoutput:				; save drawing
	call	kilbuf		; clear keyboard buffer
	ld	hl,CSRSW		; enable text cursor
	ld	(hl),0x00		;
	ld	hl,bascm		; get basic communication byte
	ld	(hl),0x00		; insert load flag
	ld	hl,flags		; get flags byte
	set	7,(hl)		; set quit flag
	call	clssavescr		; save current drawing
	ret

setquit:				; prepare to exit the ML program
	ld	hl,bascm		; get basic communication byte
	ld	(hl),0xff		; insert quit flag
	ld	de,textquit		; RAM position of text
	ld	hl,0x194a		; text VRAM position
	call	areyousure		; call confirmation
	ret	nz			; if not, return
	ld	hl,flags		; get flags byte
	set	7,(hl)		; set quit flag
	call	kilbuf		; clear keyboard buffer
	ret

; ------------------CHARACTER MAP

showcharmap:			; show a character map to choose from
					; save VRAM content under character map
	ld	hl,0x1867		; get start VRAM position for box
	ld	(rectt+1),hl	; save in mem
	ld	hl,0x1212		; get width(H) and height(L) 
	ld	(rectt+3),hl	; save in mem (reversed)
	ld	hl,(rectt+1)	; VRAM position from
	ld	de,VRBUF4		; VRAM position to
	ld	bc,0x0250		; length
	call	streamvv		; copy
					; save current character, draw border and restore char
	ld	a,(curat+6)		; get current character
	push	af			; save it
	ld	a,0xff		; get new character
	ld	(curat+6),a		; save as current
	call	drawboxhol		; draw box border with new char
	pop	af			; restore current character
	ld	(curat+6),a		;
	ld	hl,(curat)		; get current cursor position
	ld	(curat+4),hl	; save for later
	ld	hl,0x8060		; get middle of the screen
	ld	(curat),hl		; put on current cursor position
					; draw character map
	ld	hl,0x1888		; top left position for char map
	call	createcharmap	; draw the character map
					; set flags
	ld	hl,flags		; get flags byte
	set	2,(hl)		; set char map flag
	set	4,(hl)		; set dis gen input flag
	call	kilbuf		; clear keyboard buffer
	ret

chosefrommap:			; choose from the character map
					; put shape of selected char on cursor
	ld	hl,(curat)		; get x,y (read bytes pos inverted)
	call	vrampos		; get the VRAM position
	call	rdvrm			; read VRAM from position
	call	getcharshape	; change the character cursor shape
					; put color of selected char on cursor, inverted
	call	getcolorfrchr	; get color from VRAM table
	push	af			; save it
	ld	hl,(spcharac+6)	; character cursor VRAM attribute pos y
	inc	hl			; inc to color
	inc	hl			;
	inc	hl			;
	and	0b00001111		; clip foreground color
	call	wrtvrm		; apply
	ld	hl,(spbckgrd+6)	; background cursor VRAM attribute pos y
	inc	hl			; inc to color
	inc	hl			;
	inc	hl			;
	pop	af			; restore character color
	and	0b11110000		; clip background color
	srl	a			; slide to low nibble
	srl	a			;
	srl	a			;
	srl	a			;
	call	wrtvrm		; apply

	ld	a,(curat)		; get pos y
	add	4			; offset cursor
	and	0b11111000		; step by 8 increments
	dec	a			; adjust for y sprite offset
	ld	hl,(spcharac+6)	; character cursor VRAM attribute pos y
	call	wrtvrm		; write pos y to VRAM
	ld	hl,(spbckgrd+6)	; background cursor VRAM attribute pos y
	call	wrtvrm		; write pos y to VRAM

	ld	a,(curat+1)		; get pos x
	add	4			; offset cursor
	and	0b11111000		; step by 8 increments
	ld	hl,(spcharac+6)	; character cursor VRAM attribute pos y
	inc	hl			; inc to pos x
	call	wrtvrm		; write pos y to VRAM
	ld	hl,(spbckgrd+6)	; character cursor VRAM attribute pos y
	inc	hl			; inc to pos x
	call	wrtvrm		; write pos y to VRAM
	ret

geterasechar:			; get erase char from character map
	ld	bc,curat+8		; get adrress for erase character
	jr	sgfm1			; skip get current character
getfrommap:				; get a new character from the char map
	ld	bc,curat+6		;  get adrress for current character
sgfm1:ld	hl,(curat)		; get cursor xy position
	call	vrampos		; get cursor VRAM address
	call	rdvrm			; read char beneath it
	ld	(bc),a		; put on current or erase character address
cancelcharmap:			; cancel char map keeping the original character
	ld	a,(curat+6)		; get current character
	call	getcharshape	; restore chracter cursor shape
	call	getcharcolor	; restore chracter cursor color
					; restore cursor position	
	ld	hl,(curat+4)	; get saved x,y cursor pos
	ld	(curat),hl		; put on current variable
					; restore background area
	ld	hl,VRBUF4		; VRAM position from
	ld	de,(rectt+1)	; VRAM position to
	ld	bc,0x0250		; length
	call	streamvv		; copy
					; reset flags
	ld	hl,flags		; get flags byte
	res	2,(hl)		; set char map flag
	res	4,(hl)		; set dis gen input flag
	call	waittrgrls		; wait for trigger release
	call	kilbuf		; clear keyboard buffer
	ret

; ------------------TEXT

entertext:				; type text on the screen
	call	setundo		; copy screen for undo
	ld	hl,CSRSW		; enable text cursor
	ld	(hl),0x00		;
	call	hidecur		; hide the cursor
	call	hidecharcur		; hide the character cursors

	ld	hl,(curat)		; get cursor x,y
	ld	a,l			; put cursor x in A
	add	4			; offset cursor
	and	0b11111000		; step by 8 increments
	rrca				; divide by 2
	rrca				; divide by 4
	rrca				; divide by 8
	inc	a			; compensate for POSIT
	ld	l,a			; put back to L
	ld	a,h			; put cursor y in A
	add	4			; offset cursor
	and	0b11111000		; step by 8 increments
	rrca				; divide by 2
	rrca				; divide by 4
	rrca				; divide by 8
	inc	a			; compensate for POSIT
	ld	h,a			; put back to H

	call	posit			; locate on the screen
	call	kilbuf		; clear keyboard buffer
	call	inlin			; enter text
	ld	hl,CSRSW		; disable text cursor
	ld	(hl),0xff		;
	ret

getcharkey:				; type a key to get a character
	call	kilbuf		; clear keyboard buffer
	call	hidecur		; hide the cursor

	ld	a,(curat)		; get pos y
	add	4			; offset cursor
	and	0b11111000		; step by 8 increments
	dec	a			; decrement to compensate sprite offset
	ld	hl,(sptextent+6)	; text entry sprite VRAM attribute pos y
	call	wrtvrm		; write pos y to VRAM
	ld	a,(curat+1)		; get pos x
	add	4			; offset cursor
	and	0b11111000		; step by 8 increments
	ld	hl,(sptextent+6)	; text entry sprite VRAM attribute pos y
	inc	hl			; inc to pos x
	call	wrtvrm		; write pos y to VRAM

	call	chget			; get a character
	push	af			; save it
	ld	a,209			; get 209 as y pos to hide the text entry sprite 
	ld	hl,(sptextent+6)	; sprite 3 VRAM attribute pos y
	call	wrtvrm		; write pos y to VRAM
	pop	af			; get the character back
	cp	0x1b			; is ESC?
	ret	z			; if it is, return
	ld	(curat+6),a		; save the character as the current character
	call	getcharshape	; put the shape on the cursor
	call	getcharcolor	; put the color on the cursor
	ret

writetext:				; write text on screen
					; input: DE=RAM pos,HL=VRAM pos
					; special chars: 0xff=new line,0x01-0x1f=space amount,0x00=end
	dec	de			; dec RAM pos to compensate first RAM advance
	push	hl			; save initial VRAM pos
lwrtx:inc	de			; advance one RAM text position
	ld	b,1			; assign 1 for the looping character amount
	ld	a,(de)		; get first character
	cp	0x00			; is 0?
	jr	z,swext		; if so, jump to end text routine
	cp	0xff			; is 255?
	jr	nz,swret		; if not, jump over new line routine
	pop	hl			; get start of line position
	push	af			; save the character value
	ld	a,0x20		; get 32 (down one line on the screen VRAM)
	ex	de,hl			; exchange RAM<>VRAM positions for the add routine
	call	add8to16		; add 32 to VRAM position (now on DE)
	ex	de,hl			; exchange RAM<>VRAM back
	pop	af			; restore character value
	push	hl			; save start of line position
	jr	lwrtx			; loop to get new character
swret:cp	0x20			; compare to 31
	jr	nc,lwspc		; if greater, jump over spaces routine
	ld	b,a			; get amount of spaces (byte value)
	ld	a,0x20		; define character as space
lwspc:call	wrtvrm		; write on the screen
	inc	hl			; advance screen position
	djnz	lwspc			; decrease character amount and print another if not 0
	jr	lwrtx			; loop to get new character
swext:pop	hl			; restore HL (to tidy the stack) before returning
	ret

showhelp:				; show the help screen
	call	clssavescr		; clear screen saving the drawing
	ld	hl,0x180c		; get text VRAM position
	call	drawlogosmall	; draw a small MASCIISDRAWX logo
lshh2:ld	ix,hlptxt		; get start pos of help page text indexes
	ld	b,0x01		; reset page counter (not 0 because DJNZ)
lshh1:push	bc			; save page counter
	call	kilbuf		; clear keyboard buffer
					; clear screen from logo down
	ld	a,0x20		; get space char
	ld	bc,0x2c0		; get amount to clear
	ld	hl,0x1840		; get initial position to clear
					; show screens
	call	filvrm		; fill area with spaces
	ld	a,(ix)		; get low byte of content of text index
	ld	e,a			; put in E
	ld	a,(ix+1)		; get high byte of content of text index
	ld	d,a			; put in D
	ld	hl,0x1840		; get VRAM position
	call	writetext		; write text
	pop	bc			; restore page counter
	call	chget			; wait for a character
	cp	0x1c			; is right arrow?
	jr	nz,sshh1		; if not, skip ahead
	inc	ix			; increment help page text index twice
	inc	ix			;
	inc	b			; increment counter
	ld	a,b			; put in A
	cp	0x0a			; is over the last page (EDIT)
	jr	z,lshh2		; it it is, loop back to restart
	jr	lshh1			; loop to next page
sshh1:cp	0x1d			; is key left arrow?
	jr	nz,sshh2		; if not, skip ahead
	dec	ix			; decrement help page text index twice
	dec	ix			;
	djnz	lshh1			; dec counter, if !< 0, loop to next screen
sshh3:ld	b,0x09		; put last page on counter (EDIT)
	ld	ix,hlptxt+16	; get last help page text index (EDIT)
	jr	lshh1			; loop to next page
sshh2:cp	0x1e			; is key up arrow?
	jr	z,lshh2		; if so, loop back to restart
	cp	0x1f			; is key down arrow?
	jr	z,sshh3		; if so, loop back to last page
	call	clsrestscr		; restore drawing
	ret

; ------------------REQUESTERS

areyousure:				; make an "are you sure" requester and process answer
					; SURE text is put centered two rows down from label pos
					; input: DE=RAM label address,HL=VRAm label pos
	push	hl			; save text VRAM pos
	push	de			; save text RAM pos
	call	clssavescr		; generate a standard background screen
	ld	hl,0x180c		; get text VRAM position
	call	drawlogosmall	; draw a small MASCIISDRAWX logo
	pop	de			; restore text VRAM pos
	pop	hl			; restore text RAM pos
	call	writetext		; write it
	ld	a,l			; get VRAM text position low byte
	and	0b11110000		; round to decimal place
	ld	l,a			; put back on VRAM text position
	ld	de,0x49		; down 2 rows plus 9 column position
	add	hl,de			; add to VRAM text position
	ld	de,textsure		; get the SURE text
	call	writetext		; write it
	call	kilbuf		; erase keyboard buffer
	call	chget			; get a key press
	cp	0x0d			; is it ENTER?
	jr	z,says1		; if so, skip next
	cp	0x20			; is it SPACE?
	jr	z,says1		; if so, skip next
	cp	0x59			; is it "Y"?
	jr	z,says1		; if so, skip next
	cp	0x79			; is it "y"?
says1:push	af			; save answer
	call	clsrestscr		; cancel the standard background screen
	pop	af			; restore answer
	ret

getrequlook:			; save drawing and get the requester look
	ld	a,(BDRCLR)		; get border color
	ld	(scrat+5),a		; save border color bkp
					; save drawing patterns on VRAM buffer
	ld	hl,0x0108		; VRAM position from - from ! to ~
	ld	de,VRBUF5		; VRAM position
	ld	bc,0x0348		; length
	call	streamvv		; copy
					; save drawing color on VRAM buffer
	ld	hl,0x2004		; VRAM position from
	ld	de,VRBUF6		; VRAM position to
	ld	bc,0x000d		; length
	call	streamvv		; copy
					; copy interface font from RAM to VRAM
	ld	a,0x00		; make space character blank
	ld	hl,0x0100		; VRAM position to copy
	ld	bc,0x08		; length
	call	filvrm		; copy
	ld	hl,fontint1		; font RAM position
	ld	de,0x0108		; VRAM position to copy - ! to `
	ld	bc,0x01ff		; length
	call	ldirvm		; copy
	ld	hl,fontint2		; font RAM position
	ld	de,0x0308		; VRAM position to copy - A to Z
	ld	bc,0x00d0		; length
	call	ldirvm		; copy
	ld	hl,fontint3		; font RAM position
	ld	de,0x03d8		; VRAM position to copy - { to ~
	ld	bc,0x0068		; length
	call	ldirvm		; copy
					; assign color
	ld	a,(scrat+3)		; color to copy
	ld	hl,0x2004		; VRAM position to copy
	ld	bc,0x000d		; length
	call	filvrm		; copy
	ld	a,0x1e		; color to copy
	ld	hl,0x200c		; VRAM position to copy
	ld	bc,0x0004		; length
	call	filvrm		; copy
	ld	a,(scrat+2)		; get def border color
	call	chbordcolor		; change it
	ret

droprequlook:			; restore drawing look
					; restore font patterns
	ld	hl,VRBUF5		; VRAM position from
	ld	de,0x0108		; VRAM position to
	ld	bc,0x0348		; length
	call	streamvv		; copy
					; restore color
	ld	hl,VRBUF6		; RAM position from
	ld	de,0x2004		; VRAM position to
	ld	bc,0x000d		; length
	call	streamvv		; copy

	ld	a,(scrat+5)		; get saved border color
	call	chbordcolor		; change it
	ret

clssavescr:				; save drawing and show the standard screen
					; copy screen contents
	ld	hl,0x1800		; VRAM position from
	ld	de,VRBUF4		; VRAM position to
	ld	bc,0x0300		; length
	call	streamvv		; copy
					; create requester
	call	cls			; clear the screen
	call	hidecur		; hide cursor
	call	hidecharcur		; hide the character cursors

	call	getrequlook		; get requ look and save original to VRAM
	ret

clsrestscr:				; drop standard screen and restore drawing
	xor	a			; CLS needs Z reseted
	call	cls			; clear the screen
	call	droprequlook	; restore original look
					; restore screen contents
	ld	hl,VRBUF4		; VRAM position from
	ld	de,0x1800		; VRAM position to
	ld	bc,0x0300		; length
	call	streamvv		; copy
	ret

; ------------------CHARACTER

drawchar:				; draw a character on the screen
	ld	hl,(curat)		; get x,y (read bytes pos inverted)
	call	vrampos		; get VRAM position
	ld	a,(curat+6)		; get current character
	call	wrtvrms		; write to VRAM safe
	ret

erasechar:				; erase a character on the screen
	ld	hl,(curat)		; get x,y (read bytes pos inverted)
	call	vrampos		; get VRAM position
	ld	a,(curat+8)		; get current character
	call	wrtvrms		; write to VRAM safe
	ret

rollchar:				; choose a current character
	ld	a,(curat+6)		; get the current character from variable
	add	a,b			; add offset
	ld	(curat+6),a		; put back at variable
	call	getcharshape	; put character shape on sprite
	call	getcharcolor	; put character color on sprite
	call	kilbuf		; clear keyboard buffer
	ret

pickchar:				; get character under the cursor
	call	checkcurspos	; get cursor pos and check if is inside screen
	ret	c			; return if not
	call	rdvrm			; read VRAM from position
	ld	(curat+6),a		; put in current character variable
	call	getcharshape	; put character shape on sprite
	call	getcharcolor	; put character color on sprite
	call	kilbuf		; clear keyboard buffer
	ret		

; ------------------CURSOR

drawcsr:				; draw the cursor sprite
	ld	hl,(spcursor+6)	; cursor VRAM pos y address
	ld	a,(curat)		; get RAM pos y
	dec	a			; adjust for offset
	call	wrtvrm		; write pos y to VRAM
	inc	hl			; move to VRAM attibute pos x
	ld	a,(curat+1)		; get pos x
	call	wrtvrm		; write pos x to VRAM
	ret

drawchcsr:				; draw the character cursor sprites
	ld	a,(curat)		; get pos y
	add	4			; offset cursor
	and	0b11111000		; step by 8 increments
	dec	a			; adjust for y sprite offset
	ld	hl,(spcharac+6)	; character cursor VRAM attribute pos y
	call	wrtvrm		; write pos y to VRAM
	ld	hl,(spbckgrd+6)	; background cursor VRAM attribute pos y
	call	wrtvrm		; write pos y to VRAM

	ld	a,(curat+1)		; get pos x
	add	4			; offset cursor
	and	0b11111000		; step by 8 increments
	ld	hl,(spcharac+6)	; character cursor VRAM attribute pos y
	inc	hl			; inc to pos x
	call	wrtvrm		; write pos y to VRAM
	ld	hl,(spbckgrd+6)	; character cursor VRAM attribute pos y
	inc	hl			; inc to pos x
	call	wrtvrm		; write pos y to VRAM
	ret

getcharshape:			; put char pattern value to char cursor
					; input: A=character
	ld	h,0x00		; put current character value in HL
	ld	l,a			; idem
	add	hl,hl			; multiply by 8 to get VRAM pos
	add	hl,hl			; idem
	add	hl,hl			; idem
	ld	de,(spcharac+4)	; character sprite RAM pattern
	ld	bc,0x08		; 8 bytes of data to copy
	call	streamvv		; copy
	ret

getcharcolor:			; put char color value to char and BG cursor
					; input: A=character
	call	getcolorfrchr	; get character color
					; put on the background sprite
	push	af			; save color
	and	0b00001111		; discard higher than 15
	ld	hl,(spbckgrd+6)	; background cursor VRAM attribute pos y
	inc	hl			; inc to color
	inc	hl			;
	inc	hl			;
	call	wrtvrm		; apply
					; put on the character sprite
	pop	af			; restore color
	rrca				; send bits to lower nibble
	rrca				; send bits to lower nibble
	rrca				; send bits to lower nibble
	rrca				; send bits to lower nibble
	and	0b00001111		; discard higher than 15
	ld	hl,(spcharac+6)	; character cursor VRAM attribute pos y
	inc	hl			; inc to color
	inc	hl			;
	inc	hl			;
	call	wrtvrm		; apply
	ret

cursorcolor:			; change the cursor color
	ld	a,(curat+9)		; get cursor color
	add	b			; add/sub color
	and	0b00001111		; discard high nibble
	ld	(curat+9),a		; save cursor color
	ld	hl,splastcu+8	; get position of last cursor number
	ld	b,(hl)		; put in counter
	ld	de,spcursor+6	; get RAM pos for 1st sprite attribute table
lcuco:ld	a,(de)		; get low byte
	ld	l,a			; put in L
	inc	de			; go to the high byte
	ld	a,(de)		; get high byte
	ld	h,a			; put in H
	inc	hl			; move HL to color attribute
	inc	hl			; idem
	inc	hl			; idem
	ld	a,(curat+9)		; get cursor color
	call	wrtvrm		; send to VRAM color attribute
	ld	a,0x10		; amount to advance RAM addresses
	call	add8to16		; advance to next sprite RAM attribute table pos
	djnz	lcuco			; decrement counter, if not 0, loop
	call	kilbuf		; clear keyboard buffer
	ret

; ------------------SCREEN

undolast:				; undo last action
	ld	hl,VRBUF2		; undo buffer address
	ld	de,0x1800		; get screen start address
	ld	bc,0x300		; get amount to copy
	call	swapvv		; swap screen with undo buffer
	ret

swapbuffers:			; swap between 2 screens
	ld	hl,0x1800		; screen start pos
	ld	de,VRBUF3		; swap screen start pos
	ld	bc,0x300		; amount to copy
	call	swapvv		; swap screens
	ret

cyclebordcol:			; cycle the border color
					; input: B=0x00 or 0xff
	ld	a,(BDRCLR)		; get the current border color
	add	b			; add or subtract
	call	chbordcolor		; change the color
	call	kilbuf		; clear keyboard buffer
	ret

clearscr:				; clear the screen
	ld	de,textclear	; text message address
	ld	hl,0x194a		; text VRAM position
	call	areyousure		; call confirmation
	ret	nz			; if not, return
	call	setundo		; copy screen for undo
	call	cls			; clear the screen
	call	kilbuf		; clear keyboard buffer
	ret

clearscrchar:			; clear the screen with the current character
	ld	de,textclear	; text message address
	ld	hl,0x194a		; text VRAM position
	call	areyousure		; call confirmation
	ret	nz			; if not, return
	call	setundo		; copy screen for undo
	ld	a,(curat+6)		; get current character from variable
	ld	bc,0x300		; get the screen VRAM size
	ld	hl,0x1800		; get the screen VRAM start address
	call	filvrm		; fill the area with the character
	call	kilbuf		; clear keyboard buffer
	ret

clearscrinit:			; initialize the screen and tables
	ld	de,textreset	; text message address
	ld	hl,0x194a		; text VRAM position
	call	areyousure		; call confirmation
	ret	nz			; if not, return
	call	init32		; initialize the screen
	call	initsprites		; initialize the sprites
	ld	a,(curat+6)		; get current character
	call	getcharshape	; put character shape on sprite
	call	getcharcolor	; put character color on sprite
	call	kilbuf		; clear keyboard buffer
	ret

scrollup:				; scroll the screen up
	call	copyscrtobuf2	; copy whole screen to buffer 2
					; get buffer minus first row and put on VRAM
	ld	de,0x1800		; initial VRAM pos
	ld	hl,buffer2+0x20	; the buffer to use
	ld	bc,0x02e0		; the whole screen
	call	ldirvm		; put RAM on VRAM
					; get first buffer row and put on last VRAM row
	ld	de,0x1ae0		; initial VRAM pos
	ld	hl,buffer2		; pos after last RAM row
	ld	bc,0x0020		; amount to copy. one row
	call	ldirvm		; put VRAM on RAM
	call	kilbuf		; clear keyboard buffer
	ret

scrolldown:				; scroll the screen down
	call	copyscrtobuf2	; copy whole screen to buffer 2
					; get buffer minus last row and put on VRAM second row
	ld	de,0x1820		; initial VRAM pos
	ld	hl,buffer2		; the buffer to use
	ld	bc,0x02e0		; the whole screen
	call	ldirvm		; put RAM on VRAM
					; get last buffer row and put on first VRAM row
	ld	de,0x1800		; initial VRAM pos
	ld	hl,buffer2+0x02e0	; pos after last RAM row
	ld	bc,0x0020		; amount to copy. one row
	call	ldirvm		; put VRAM on RAM
	call	kilbuf		; clear keyboard buffer
	ret

scrollright:			; scroll the screen right
	call	copyscrtobuf2	; copy whole screen to buffer 2
					; copy to start of VRAM screen skipping first byte
	ld	de,0x1801		; initial VRAM pos skipping 1 byte
	ld	hl,buffer2		; buffer to use
	ld	bc,0x02ff		; whole screen minus 1 byte
	call	ldirvm		; put RAM on VRAM
					; raise the first column 1 row up
	ld	b,0x18		; put amount of rows in counter
	ld	hl,0x1800		; initial VRAM pos. top of first column
	ld	de,buffer2+0x1f	; the top of last column on RAM
	call	fixscrollcol	; put on first VRAM column and move up
	call	kilbuf		; clear keyboard buffer
	ret

scrollleft:				; scroll the screen left
	call	copyscrtobuf2	; copy whole screen to buffer 2
					; copy to start of VRAM screen minus first byte from RAM
	ld	de,0x1800		; initial VRAM pos
	ld	hl,buffer2+0x01	; buffer to use minus first byte
	ld	bc,0x02ff		; whole screen minus 1 byte
	call	ldirvm		; put RAM on VRAM
					; raise the last column 1 row up
	ld	b,0x18		; put amount of rows in counter
	ld	hl,0x181f		; initial VRAM pos. top of last column
	ld	de,buffer2		; top of first column on RAM
	call	fixscrollcol	; put on last VRAM column and move up
	call	kilbuf		; clear keyboard buffer
	ret

fixscrollcol:			; raise a screen column 1 row up
	ld	a,(de)		; get character from buffer position
	call	wrtvrm		; write to VRAM
	push	bc			; save rows counter
	ld	bc,0x20		; amount to move down one line
	add	hl,bc			; move down VRAM address
	ex	de,hl			; exchange RAM<>VRAM
	add	hl,bc			; move down RAM address
	ex	de,hl			; exchange back RAM<>VRAM
	pop	bc			; restore rows counter
	djnz	fixscrollcol	; decrement, if not 0, loop
	ret

flipscrvert:			; flip the screen vertically
	push	bc			; save flip type
	call	copyscrtobuf2	; copy whole screen to buffer 2
					; copy rows exchanging top with bottom
	ld	hl,0x1ae0		; initial VRAM pos - last row
	ld	de,buffer2		; buffer to use
	ld	bc,0x2018		; counters: B=&h20cols,C=&h18rows
lfsv1:ld	a,(de)		; get character from buffer
	call	wrtvrm		; put on VRAM
	inc	de			; increment 1 RAM column
	inc	hl			; increment 1 VRAM column
	djnz	lfsv1			; decrement counter, if not 0, loop 
	ld	b,0x20		; restore cols counter
	push	bc			; save counters
	ld	bc,0x40		; amount to go to the start of above row
	sbc	hl,bc			; adjust VRAM pointer
	pop	bc			; restore counters
	dec	c			; decrement row counter
	jr	nz,lfsv1		; if not 0, loop
					; test flip type
	pop	bc			; restore flip type
	ld	a,b			; put in A
	cp	0x00			; compare to don't flip patterns type
	jr	z,sfsv1		; if match, skip flipping patterns
					; flip patterns
	ld	a,0xff		; get last character index
lfsv2:push	af			; save it
	call	getpatternad	; get its pattern address
	push	de			; save pattern address
	ld	b,0x08		; set rows/ bytes counter
	ld	de,edthelpch+7	; get the temp char last (7th) pattern address
lfsv3:call	rdvrm			; read the first editing char pattern address
	ld	(de),a		; put first editing byte on last temp byte
	inc	hl			; increment editing
	dec	de			; decrement temp
	djnz	lfsv3			; do until character finished
	ld	hl,edthelpch	; get address of temp character
	pop	de			; restore address of editing char pattern
	ld	bc,0x08		; set the length to all character
	call	ldirvm		; copy temp character to editing character
	pop	af			; restore character index
	dec	a			; decrement it
	jr	nz,lfsv2		; if not 0, loop
sfsv1:call	kilbuf		; clear keyboard buffer
	ret

flipscrhorz:			; flip the screen horizontally
	push	bc			; save flip type
	call	copyscrtobuf2	; copy whole screen to buffer 2
					; copy columns exchanging left with right
	ld	bc,0x1820		; counters: B=&h20cols,C=&h18rows
	ld	hl,0x181f		; initial VRAM pos. top of last column
	ld	de,buffer2		; top of first column on RAM
lfsh1:ld	a,(de)		; get character from buffer position
	call	wrtvrm		; write to VRAM
	push	bc			; save counters
	ld	bc,0x20		; amount to back one row up
	add	hl,bc			; move down VRAM address
	ex	de,hl			; exchange RAM<>VRAM
	add	hl,bc			; move down RAM address
	ex	de,hl			; exchange back RAM<>VRAM
	pop	bc			; restore counters
	djnz	lfsh1			; decrement, if not 0, loop
	ld	b,0x18		; restore rows counter
	push	bc			; save counters
	ld	bc,0x300		; amount to move back to top of column
	sbc	hl,bc			; move there
	dec	hl			; move one column back
	ex	de,hl			; exchange RAM<>VRAM
	sbc	hl,bc			; move back to top of column
	inc	hl			; advance one column
	ex	de,hl			; exchange back RAM<>VRAM
	pop	bc			; restore counters
	dec	c			; decrement cols counter
	jr	nz,lfsh1		; if not 0, loop
					; test flip type
	pop	bc			; restore flip type
	ld	a,b			; put in A
	cp	0x00			; compare to don't flip patterns type
	jr	z,sssh1		; if match, skip flipping patterns
					; flip patterns
	ld	hl,0x0000		; pattern address
	ld	de,0x0800		; pattern length
lssh2:ld	b,0x08		; columns/bits length
	call	rdvrm			; read pattern
	ld	c,a			; save to D
lssh3:rl	c			; rotate its bits left and put on the carrier
	rra				; put the carrier on the A right bit
	djnz	lssh3			; repeat until all columns/bits are done
	call	wrtvrm		; write it back
	inc	hl			; go to next pattern byte
	dec	de			; decrement rows/bytes counter
	ld	a,d			; put high byte in A
	or	e			; compare with C
	jr	nz,lssh2		; do until all rows/bytes are done
sssh1:call	kilbuf		; clear keyboard buffer
	ret

; ------------------CONTROLS

checktrigger:			; check mouse button 1 or space
					; output: NZ if pressed
	ld	a,(TRGFLG)		; get trigger flags
	and	0b00010001		; mask unwanted bite
	xor	0b00010001		; compare if TRIG or SPC pressed
	ret	nz			; if pressed, return
	ld	hl,flags+1		; if not. reset trigger flag
	res	7,(hl)		; 
	ret

checkbutton2:			; check mouse button 2
					; output: NZ if pressed
	ld	a,(TRGFLG)		; get trigger flags
	and	0b00100000		; mask unwanted bite
	xor	0b00100000		; compare if TRIG2 pressed
	ret

waittrgrls:				; wait until trigger release
	call	checktrigger	; check trigger
	jr	nz,waittrgrls	; if pressed, loop
	call	checkbutton2	; check trigger
	jr	nz,waittrgrls	; if pressed, loop
	ret

movecursor:				; move cursor with the mouse
	ld	a,(curat+1)		; get sprite pos x
	sub	h			; subtract from mouse offset
	ld	(curat+1),a		; save sprite pos x

	ld	a,(curat)		; get sprite pos y
	sub	l 			; subtract from mouse offset
	call	fixycoord		; prevent access to higher than 191 y screen pos
	ld	(curat),a		; save sprite pos y
	ret

movcurhor:				; move the cursor horizontally with the keyboard
	ld	hl,curat+1		; get cursor x pos address
	ld	a,(hl)		; get cursor position
	and	a,0b11111000	; step by 8 increments
	add	a,b			; add movement offset
	ld	(hl),a		; save cursor position
	call	kilbuf		; clear keyboard buffer
	ret

movcurver:				; move the cursor vertically with the keyboard
	ld	hl,curat		; get cursor y pos address
	ld	a,(hl)		; get cursor position
	and	a,0b11111000	; step by 8 increments
	add	a,b			; add movement offset
	call	fixycoord		; prevent access to higher than 191 y screen pos
	ld	(hl),a		; save cursor position
	call	kilbuf		; clear keyboard buffer
	ret

fixycoord:				; prevent access to higher than 191 y screen pos
	cp	0xbc			; compare with screen bottom (192-4)
	ret	c			; if less, its inside screen, return
	cp	0xc1			; compare with 192, past screen bottom
	jr	nc,sfycoo		; if bigger, skip
	ld	a,0xfd		; if not put on screen top (0-4)
	ret				; and return
sfycoo:cp	0xfc			; compare with screen top (0-4)
	ret	nc			; if bigger, still inside screen, return
	ld	a,0xbb		; if not. put on screen bottom
	ret

getmouse: 				; Read the mouse (found on the interwebs)
					; Input: D/E=mouse number Output: H=X-offset, L=Y-offset
					; Note that the routine will output H=L=255 if no mouse is present!
	ld	de,0x9310		; mouse 1
	call	gtofst		; Read bit 7-4 of the x-offset
	and	0xf
	rlca
	rlca
	rlca
	rlca
	ld	c,a
	call	gtofst		; Read bit 3-0 of the x-offset
	and	0xf
	or	c
	ld	h,a			; Store combined x-offset
	call	gtofst		; Read bit 7-4 of the y-offset
	and	0xf
	rlca
	rlca
	rlca
	rlca
	ld	c,a
	call	gtofst		; Read bit 3-0 of the y-offset
	and	0xf
	or	c
	ld	l,a			; Store combined y-offset
					; filter random 15 pixels mouse jump
	ld	a,15
	cp	h
	jr	z,sgtm1
	ld	a,240
	cp	h
	jr	z,sgtm1
	ld	a,15
	cp	l
	jr	z,sgtm2
	ld	a,240
	cp	l
	jr	z,sgtm2
	ret
sgtm1:ld	h,0
	ret
sgtm2:ld	l,0
	ret

gtofst:
	ld	a,15            ; Read psg register 15 for mouse
	out	(0xa0),a
	ld	a,d
	out	(0xa1),a
	xor	e
	ld	d,a

	ld	a,14
	out	(0xa0),a
	in	a,(0xa2)
	ret
