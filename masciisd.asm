; ------------------MASCIISDRAWX
;	MASCIISDRAWX
;	(C) Fred Rique (farique) 2020
;	github/farique1/8bitcodes
;	/Users/Farique/Desktop/Projects/Assembly/Z80/zasm/zasm -uwy ascdrw.asm ascdrw.bin
; ------------------------

; ------------------DOCS
; KEYS:
	; H					help
	;    ARROWS				   navigate
	;    ANY KEY			   exit
	; ARROWS MOUSE			move
	;  + SHIFT				move * 4
	; SPACE BUTTON1			draw
	; BS BUTTON2			erase (default space char)
	; M					toggle mouse
	; Z X					cycle characters
	;  + SHIFT				cycle characters * 16
	; 7 8					cycle cursor color
	; 9 0					cycle border color
	; N 					pick character under cursor
	; B					cycle box modes:
	;					 filled box w/ curr char
	;					 hollow box with curr char
	;					 box with lines
	;					 box with lines adding them
	;					 box with sequential chars
	;    ENTER BUTTON1 SPACE	   draw
	;    ESC BS C BUTTON2		   cancel
	; C					copy area of screen / enter paste
	; V					paste previous copy
	;    ENTER BUTTON1 SPACE	   paste
	;    BS BUTTON2			   erase area
	;    C V				   cancel
	; /					show character map picker
	;    ENTER BUTTON1 SPACE	   pick and leave
	;    BS BUTTON2			   pick erase char
	;    ESC / 				   leave
	; ?					change the current character by typing
	;    ESC				   leave without changing
	; T					enter text by typing
	;    ENTER CTRL + STOP		    leave
	; W A S D				scroll up left down right
	; Q E					flip vertical horizontal
	;  + SHIFT				also flip patterns
	; F					swap screen buffers
	; U					undo box or text
	; Y					quick save (lose undo)
	; O I					save load file
	;					   ^ = more files
	;    ARROWS				   choose a file
	;    ENTER(2x)			   pick and save or load the file
	;    SPACE				   write a name
	;    E				   change the extension to list
	;    D				   delete the file (ENTER confirm)
	;    ESC				   cancel
	; ESC					clear screen
	;  + SHIFT				clear screen w/ curr char
	;  + CTRL				reset screen
	; Q + CTRL				quit
	; .					edit characters mode
	;    ARROWS MOUSE			   move
	;     + SHIFT			   move * 4
	;    SPACE BUTTON1		   toggle bit/choose character
	;    Z X				   cycle characters
	;     + SHIFT			   cycle characters * 16
	;    1 2				   cycle foreground color
	;    3 4				   cycle background color
	;    W A S D			   scroll
	;    Q E				   flip vert horz
	;    R F				   rotate
	;    H				   invert
	;    J				   create checkerboard
	;    K				   clear
	;    L				   fill
	;    C				   copy
	;    V				   paste all
	;     + SHIFT			   paste pattern
	;	+ CTRL			   paste color				
	;    U				   undo
	;    Y				   quick save (lose undo)
	;    TAB				   swap working characters
	;    BS				   save working characters
	;    ESC				   exit without saving
	;    .				   exit
; TODO
	; shift constrain to straight lines
	; key to show position of cursor. VRAM, x and y
	; basic, error detection for file not found
	; basic, mouse support
	; basic, file filter instead of extensiom
	; make draw box use its own ram variable for the character
	; use COPYTOVRAM more, see char map fonts
	; check all potential writings outside the screen
	; replace all WRTVRM with WRTVRMS adjusting routines accordingly
; IDEAS
	; crop and flip the paste buffer
	; swap to a second paste buffer if available
; ------------------------

; ------------------HEADER
	org	0xb000		; start memory location

	db	0xfe			; indicate it is a bin file
	dw	bascm			; start address
	dw	_end			; end address
	dw	start			; execute address
; ------------------------

; ------------------BODY
#include "masciiIN.asm"		; initializations
#include "masciiML.asm"		; mail loop
#include "masciiMI.asm"		; misc routines
#include "masciiRE.asm"		; rectangle operations
#include "masciiEC.asm"		; edit characters
#include "masciiKB.asm"		; keyboard reading
#include "masciiSR.asm"		; support routines
#include "masciiDT.asm"		; data
; ------------------------
