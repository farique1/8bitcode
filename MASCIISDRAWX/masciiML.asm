; ------------------MAIN LOOP

mainloop:				; - always do section
	call	chsns			; check keyboard buffer
	call	nz,readmainkeys	; if keys check main keys
	call	drawcsr		; draw cursor sprite
					; - general input section
	ld	a,(flags)		; look for flag byte
	bit	4,a			; test bit 5
	jr	nz,smgin		; if 1, no general input 
	call	drawchcsr		; draw character sprites
	call	checktrigger	; check button/SPACE - output NZ if pressed
	call	nz,drawchar		; if pressed draw character
	call	checkbutton2	; check button 2
	call	nz,erasechar	; if pressed cancel rectangle
	call	chsns			; check keyboard buffer
	call	nz,readgenkeys	; if key check general which
smgin:				; - mouse section
	ld	a,(flags)		; look for flag byte
	bit	6,a			; test bit 6
	jr	nz,smmse		; if 1, no mouse
	call	getmouse		; if set, mouse - output HL
	call	movecursor		; move the cursors - input HL
smmse:				; - rectangle section
	ld	a,(flags)		; look for flag byte
	bit	5,a			; test bit 5
	jr	z,smrct		; if 0, no rectangle 
	call	showrectcurs	; get rectangle data
	call	chsns			; check keyboard buffer
	call	nz,readrectkeys	; if keys check rectangle keys
	call	checktrigger	; check button/SPACE - output NZ if pressed
	call	nz,activaterect	; if pressed activate rectangle
	call	checkbutton2	; check button 2
	call	nz,cancelrect	; if pressed cancel rectangle
smrct:				; - paste box section
	ld	hl,flags		; get flags byte
	bit	3,(hl)		; test bit 3
	jr	z,smpbx		; if 0, no paste box 
	set	4,(hl)		; force set disable general input
	call	showpastebox	; show paste box
	call	chsns			; check keyboard buffer
	call	nz,readpastbxkeys	; if keys check paste box keys
	call	checktrigger	; check button/SPACE - output NZ if pressed
	call	nz,pastethebox	; if pressed paste the paste box
	call	checkbutton2	; check button 2
	call	nz,erasewithbox	; if pressed cancel paste box
smpbx:				; - character map section
	ld	hl,flags		; get flags byte
	bit	2,(hl)		; test bit 3
	jr	z,smcmp		; if 0, no character map
	call	chosefrommap	; choose character from map
	call	chsns			; check keyboard buffer
	call	nz,readchmapkeys	; if keys check character map keys
	call	checktrigger	; check button/SPACE - output NZ if pressed
	call	nz,getfrommap	; if pressed get the selected character
	call	checkbutton2	; check button 2
	call	nz,geterasechar	; if pressed cancel character map
smcmp:				; - edit characters section
	ld	hl,flags		; get flags byte
	bit	1,(hl)		; test bit 3
	jr	z,smedc		; if 0, no edit char
	call	insidecharmap	; function according to screen location
	jr	z,smedc		; if 0, no edit char
	call	insideeditarea	; function according to screen location
	call	chsns			; check keyboard buffer
	call	nz,readedchkeys	; if keys check character map keys
	call	checktrigger	; check button/SPACE - output NZ if pressed
	call	nz,triggeredit	; if pressed get the selected character
	call	checkbutton2	; check button 2
	call	nz,savewrkchrs	; if pressed cancel character map
smedc:				; - quit or loop
	ld	hl,flags		; look for flag byte
	bit	7,(hl)		; test bit 7
	jp	z,mainloop		; if 0, loop
	call	hidecur		; hide the cursor
	call	hidecharcur		; hide the character cursor
	ret
