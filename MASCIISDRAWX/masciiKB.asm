; ------------------KEYBOARD READ

readgenkeys:
	ld	a,(NEWKEY+6)	; keyboard matrix: F3 F2 F1 CODE CAPS GRAPH CTRL SHIFT
	ld	d,a			; store in d

	ld	a,(NEWKEY+2)	; keyboard matrix: B A DEAD /? .> ,< `~ '"
	cp	0xff			; pressed key on this row?
	jr	z,ngek2		; if not, jump to next
	and	d			; combine with alt keys result
	ld	b,0x00		; filled rectangle
	cp	0b01111111		; b
	jp	z,preparerect
	cp	0b10111111		; a
	jp	z,scrollleft
	cp	0b11101110		; / + SHIFT
	jp	z,getcharkey
	cp	0b11101111		; /
	jp	z,showcharmap
	cp	0b11110111		; .
	jp	z,showeditchrscr
ngek2:
	ld	a,(NEWKEY+3)	; keyboard matrix: J I H G F E D C
	cp	0xff			; pressed key on this row?
	jr	z,ngek3		; if not, jump to next
	and	d			; combine with alt keys result
	cp	0b10111111		; i
	jp	z,fileinput
	cp	0b11011111		; h
	jp	z,showhelp
	cp	0b11110111		; f
	jp	z,swapbuffers
	ld	b,0x00		; don't flip patterns
	cp	0b11111011		; e
	jp	z,flipscrhorz
	ld	b,0x01		; flip patterns
	cp	0b11111010		; e + SHIFT
	jp	z,flipscrhorz
	cp	0b11111101		; d
	jp	z,scrollright
	ld	b,0x05		; copy area rectangle type
	cp	0b11111110		; c
	jp	z,preparerect
ngek3:
	ld	a,(NEWKEY+4)	; keyboard matrix: R Q P O N M L K
	cp	0xff			; pressed key on this row?
	jr	z,ngek4		; if not, jump to next
	and	d			; combine with alt keys result
	ld	b,0x00		; don't flip patterns
	cp	0b10111111		; q
	jp	z,flipscrvert
	ld	b,0x01		; flip patterns
	cp	0b10111110		; q + SHIFT
	jp	z,flipscrvert
	cp	0b10111101		; q + CTRL
	jp	z,setquit
	cp	0b11101111		; o
	jp	z,fileoutput
	cp	0b11110111		; n
	jp	z,pickchar
ngek4:
	ld	a,(NEWKEY+5)	; keyboard matrix: Z Y X W V U T S
	cp	0xff			; pressed key on this row?
	jr	z,ngek5		; if not, jump to next
	and	d			; combine with alt keys result
	ld	b,0xf1		; -16 chracters
	cp	0b01111110		; z + SHIFT
	jp	z,rollchar
	ld	b,0xff		; -1 character
	cp	0b01111111		; z
	jp	z,rollchar
	cp	0b10111111		; y
	jp	z,setundo
	ld	b,0x0f		; +16 chracters
	cp	0b11011110		; x + SHIFT
	jp	z,rollchar
	ld	b,0x01		; +1 character
	cp	0b11011111		; x
	jp	z,rollchar
	cp	0b11101111		; w
	jp	z,scrollup
	cp	0b11110111		; v
	jp	z,pasteoldbuf
	cp	0b11111011		; u
	jp	z,undolast
	cp	0b11111101		; t
	jp	z,entertext
	cp	0b11111110		; s
	jp	z,scrolldown
ngek5:
	ld	a,(NEWKEY+7)	; keyboard matrix: RET SELECT BS STOP TAB ESC F5 F4
	cp	0xff			; pressed key on this row?
	jr	z,ngek7		; if not, jump to next
	and	d			; combine with alt keys result
	cp	0b01111111		; ENTER
	jp	z,drawchar
	cp	0b11011111		; BS
	jp	z,erasechar
	cp	0b11111011		; ESC
	jp	z,clearscr
	cp	0b11111010		; ESC + SHIFT
	jp	z,clearscrchar
	cp	0b11111001		; ESC + CTRL
	jp	z,clearscrinit
ngek7:
	ret				; if no key found return without killing buffer

readmainkeys:
	ld	a,(NEWKEY+6)	; keyboard matrix: F3 F2 F1 CODE CAPS GRAPH CTRL SHIFT
	ld	d,a			; store in d

	ld	a,(NEWKEY+0)	; keyboard matrix: 7& 6^ 5% 4$ 3# 2@ 1! 0)
	cp	0xff			; pressed key on this row?
	jr	z,nmak0		; if not, jump to next
	ld	b,0xff		; decrease color
	cp	0b01111111		; 7
	jp	z,cursorcolor
	ld	b,0x01		; increase color
	cp	0b11111110		; 0
	jp	z,cyclebordcol
nmak0:
	ld	a,(NEWKEY+1)	; keyboard matrix: ;: ]} [{ \¦ =+ -_ 9( 8*
	cp	0xff			; pressed key on this row?
	jr	z,nmak1		; if not, jump to next
	ld	b,0xff		; decrease color
	cp	0b11111101		; 9
	jp	z,cyclebordcol
	ld	b,0x01		; increase color
	cp	0b11111110		; 8
	jp	z,cursorcolor
nmak1:
	; ld	a,(NEWKEY+3)	; keyboard matrix: J I H G F E D C
	; cp	0xff			; pressed key on this row?
	; jr	z,nmak3		; if not, jump to next
	; rlca
	; and	d			; combine with alt keys result
; nmak3:
	ld	a,(NEWKEY+4)	; keyboard matrix: R Q P O N M L K
	cp	0xff			; pressed key on this row?
	jr	z,nmak4		; if not, jump to next
	ld	b,0b01000000	; flip bit 6
	cp	0b11111011		; m
	jp	z,toggleflags
nmak4:
	ld	a,(NEWKEY+8)	; keyboard matrix: → ↓ ↑ ← DEL INS HOME SPACE
	cp	0xff			; pressed key on this row?
	jr	z,nmak8		; if not, jump to next
	or	0b00000001		; mask SPACE to avoid conflict with SHIFT
	and	d			; combine with alt keys result
	ld	b,0x20		; +32 pixels (4 char)
	cp	0b01111110		; → + SHIFT
	jp	z,movcurhor
	cp	0b10111110		; ↓ + SHIFT
	jp	z,movcurver
	ld	b,0xe0		; -32 pixels (4 char)
	cp	0b11011110		; ↑ + SHIFT
	jp	z,movcurver
	cp	0b11101110		; ← + SHIFT
	jp	z,movcurhor
	ld	b,0x08		; +8 pixels (1 char)
	cp	0b01111111		; →
	jp	z,movcurhor
	cp	0b10111111		; ↓
	jp	z,movcurver
	ld	b,0xf8		; -8 pixels (1 char)
	cp	0b11011111		; ↑
	jp	z,movcurver
	cp	0b11101111		; ←
	jp	z,movcurhor
nmak8:
	ret				; if no key found return without killing buffer

readrectkeys:
	ld	a,(NEWKEY+2)	; keyboard matrix: B A DEAD /? .> ,< `~ '"
	cp	0xff			; pressed key on this row?
	jr	z,nrek2		; if not, jump to next
	cp	0b01111111		; b
	jp	z,changerect
nrek2:
	ld	a,(NEWKEY+3)	; keyboard matrix: J I H G F E D C
	cp	0xff			; pressed key on this row?
	jr	z,nrek3		; if not, jump to next
	cp	0b11111110		; c
	jp	z,cancelrect
nrek3:
; 	ld	a,(NEWKEY+5)	; keyboard matrix: Z Y X W V U T S
; 	cp	0xff			; pressed key on this row?
; 	jr	z,nrek5		; if not, jump to next
; nrek5:
	ld	a,(NEWKEY+7)	; keyboard matrix: RET SELECT BS STOP TAB ESC F5 F4
	cp	0xff			; pressed key on this row?
	jr	z,nrek7		; if not, jump to next
	cp	0b01111111		; ENTER
	jp	z,activaterect
	cp	0b11011111		; BS
	jp	z,cancelrect
	cp	0b11111011		; ESC
	jp	z,cancelrect
nrek7:
	ret				; if no key found return without killing buffer

readpastbxkeys:
	ld	a,(NEWKEY+3)	; keyboard matrix: J I H G F E D C
	cp	0xff			; pressed key on this row?
	jr	z,npak3		; if not, jump to next
	cp	0b11110111		; f
	jp	z,swapbuffers
	cp	0b11111110		; c
	jp	z,cancelpaste
npak3:
	ld	a,(NEWKEY+5)	; keyboard matrix: Z Y X W V U T S
	cp	0xff			; pressed key on this row?
	jr	z,npak5		; if not, jump to next
	cp	0b10111111		; y
	jp	z,setundo
	cp	0b11110111		; v
	jp	z,cancelpaste
	cp	0b11111011		; u
	jp	z,undolast
npak5:
	ld	a,(NEWKEY+7)	; keyboard matrix: RET SELECT BS STOP TAB ESC F5 F4
	cp	0xff			; pressed key on this row?
	jr	z,npak7		; if not, jump to next
	cp	0b01111111		; ENTER
	jp	z,pastethebox
	cp	0b11011111		; BS
	jp	z,erasewithbox
	cp	0b11111011		; ESC
	jp	z,cancelpaste
npak7:
	ret				; if no key found return without killing buffer

readchmapkeys:
	ld	a,(NEWKEY+2)	; keyboard matrix: B A DEAD /? .> ,< `~ '"
	cp	0xff			; pressed key on this row?
	jr	z,ncmk2		; if not, jump to next
	cp	0b11101111		; /
	call	z,cancelcharmap
ncmk2:
	ld	a,(NEWKEY+7)	; keyboard matrix: RET SELECT BS STOP TAB ESC F5 F4
	cp	0xff			; pressed key on this row?
	jr	z,ncmk7		; if not, jump to next
	cp	0b01111111		; ENTER
	call	z,getfrommap
	cp	0b11011111		; BS
	jp	z,geterasechar
	cp	0b11111011		; ESC
	call	z,cancelcharmap
ncmk7:
	ret				; if no key found return without killing buffer

readedchkeys:
	ld	a,(NEWKEY+6)	; keyboard matrix: F3 F2 F1 CODE CAPS GRAPH CTRL SHIFT
	ld	d,a			; store in d

	ld	a,(NEWKEY+0)	; keyboard matrix: 7& 6^ 5% 4$ 3# 2@ 1! 0)
	cp	0xff			; pressed key on this row?
	jr	z,neck0		; if not, jump to next
	ld	b,0x01
	cp	0b11101111		; 4
	jp	z,edtbackcolor
	ld	b,0xff
	cp	0b11110111		; 3
	jp	z,edtbackcolor
	ld	b,0x01
	cp	0b11111011		; 2
	jp	z,edtforecolor
	ld	b,0xff
	cp	0b11111101		; 1
	jp	z,edtforecolor
neck0:
	ld	a,(NEWKEY+2)	; keyboard matrix: B A DEAD /? .> ,< `~ '"
	cp	0xff			; pressed key on this row?
	jr	z,neck2		; if not, jump to next
	cp	0b10111111		; a
	jp	z,edtscrlft
	cp	0b11110111		; .
	jp	z,acceptedit
neck2:
	ld	a,(NEWKEY+3)	; keyboard matrix: J I H G F E D C
	cp	0xff			; pressed key on this row?
	jr	z,neck3		; if not, jump to next
	cp	0b01111111		; j
	jp	z,edtcheckb
	cp	0b11011111		; h
	jp	z,edtinvert
	cp	0b11110111		; f
	jp	z,edtrotlft
	cp	0b11111011		; e
	jp	z,edtfliphrz
	cp	0b11111101		; d
	jp	z,edtscrlrgt
	cp	0b11111110		; c
	jp	z,edtcopy
neck3:
	ld	a,(NEWKEY+4)	; keyboard matrix: R Q P O N M L K
	cp	0xff			; pressed key on this row?
	jr	z,neck4		; if not, jump to next
	cp	0b01111111		; r
	jp	z,edtrotrgt
	cp	0b10111111		; q
	jp	z,edtflipvrt
	cp	0b11111101		; l
	jp	z,edtclear
	cp	0b11111110		; k
	jp	z,edtfill
neck4:
	ld	a,(NEWKEY+5)	; keyboard matrix: Z Y X W V U T S
	cp	0xff			; pressed key on this row?
	jr	z,neck5		; if not, jump to next
	and	d
	ld	b,0xf1		; -16 chracters
	cp	0b01111110		; z + SHIFT
	jp	z,edtrollchar
	ld	b,0xff		; -1 character
	cp	0b01111111		; z
	jp	z,edtrollchar
	cp	0b10111111		; y
	jp	z,edtsetundo
	ld	b,0x0f		; +16 chracters
	cp	0b11011110		; x + SHIFT
	jp	z,edtrollchar
	ld	b,0x01		; +1 character
	cp	0b11011111		; x
	jp	z,edtrollchar
	cp	0b11101111		; w
	jp	z,edtscrup
	ld	b,0x00
	cp	0b11110111		; v
	jp	z,edtpaste
	ld	b,0x01
	cp	0b11110110		; v + SHIFT
	jp	z,edtpaste
	ld	b,0x02
	cp	0b11110101		; v + CTRL
	jp	z,edtpaste
	cp	0b11111011		; u
	jp	z,edtundo
	cp	0b11111110		; s
	jp	z,edtscrdwn
neck5:
	ld	a,(NEWKEY+7)	; keyboard matrix: RET SELECT BS STOP TAB ESC F5 F4
	cp	0xff			; pressed key on this row?
	jr	z,neck7		; if not, jump to next
	cp	0b01111111		; ENTER
	jp	z,enterredit
	cp	0b11011111		; BS
	jp	z,savewrkchrs
	cp	0b11110111		; TAB
	jp	z,swapwrkchrs
	cp	0b11111011		; ESC
	jp	z,canceledit
neck7:
	ret				; if no key found return without killing buffer
