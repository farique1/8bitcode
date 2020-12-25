# MASCIISDRAWX  
**A Screen 1 drawing program in Assembly**  
Screen 1.5<sup>[1](#references)</sup> coming soon.  
**`MSX`**  
  
I always bragged that I knew **Assembly** but I've never coded anything more than a few small **helper routines** for may **Basic** programs. No more. I decided to make a complete and "useful" program in Assembly to call my own. Also I wanted to explore the undocumented **Screen 1.5** mode on the **MSX** and do something nice with it.  
  
It started wit a redesign of the [Change Graph Kit](temp) interface that I didn't want to do with Basic and `PRINT`ing, a slow, cumbersome and not intuitive way to do a redesign. Combine it with me stumbling on some pages about the Screen 1 with Screen 2 colors (something I was always curious about), add some more curiosity about the mouse on the MSX, finish with the Assembly thing and this thing was born.  
  
**MASCIISDRAWX** is a Screen 1 paint program (soon to be upgraded to Screen 1.5<sup>[1](#references)</sup>, that's the whole point) made completely in Assembly (except for the disk routines, of course, life is too short) with a Basic loader and a lot of mouse functionality. Most of the commands are key driven but using the mouse is a breeze and highly recommended.  
  
As a tile mapped screen, the workflow is to edit the character patterns and colors and assemble them on the screen, **MASCIISDRAWX** has extensive functionality on both regards.  
  
Designing a screen character by character is not an easy task so  **MASCIISDRAWX** will shortly feature an importer for character patterns, probably integrated with **[Fontoraw](temp)**, to kickstart the creation process. For now its picking crumbs all the way.  
  
**MASCIISDRAWX** was the first Assembly program I ever made and every single line is commented. So apart from the fact it could probably have half the size and double the speed I imagine it is a nice program to go through if you are learning. Not a lot of fancy techinics, instead, a simple, easy to follow code that works as it should.  
  
SCREEENSHOT_OF_CHARACTER_EDITING_SCREEN  
VIDEO_OF_CGK_EDITING  
  
**Keys and mouse controls:**  
  
	H				Help  
	   ARROWS			  Navigate  
	   ANY KEY			  Exit  
	ARROWS MOUSE			Move  
	 +SHIFT				Move * 4  
	SPACE BUTTON1			Draw  
	BS BUTTON2			Erase  
	M				Toggle mouse  
	Z X				Cycle characters  
	 +SHIFT				Cycle characters * 16  
	7 8				Cycle cursor color  
	9 0				Cycle border color  
	N 				Pick character under cursor  
	B				Cycle box modes:  
					 Filled box with current character  
					 Hollow box with current character  
					 Box with lines  
					 Box with lines adding them  
					 Box with sequential characters  
	   ENTER BUTTON1 SPACE	  	  Draw  
	   ESC BS C BUTTON2	 	  Cancel  
	C				Copy area and enter paste mode  
	V				Paste previous copy  
	   ENTER BUTTON1 SPACE		  Paste  
	   BS BUTTON2			  Erase area  
	   C V				  Cancel  
	/				Show character map picker  
	   ENTER BUTTON1 SPACE		  Pick and leave  
	   BS BUTTON2			  Pick erase character and leave  
	   ESC /			  Leave  
	?				Get current character by typing  
	   ESC				  Leave without changing  
	T				Enter text by typing  
	   ENTER CTRL+STOP		  Leave  
	W A S D				Scroll up left down right  
	Q E				Flip vertical horizontal  
	 +SHIFT				Also flip patterns  
	F				Swap screen buffers  
	U				Undo (some functions)  
	Y				Quick save (lose undo)  
	ESC				Clear screen  
	 +SHIFT				Clear screen w/ current character  
	 +CTRL				Reset screen  
	Q+CTRL				Quit  
 	
	.				Enter edit character mode  
	   ARROWS MOUSE			  Move  
	    +SHIFT			  Move * 4  
	   SPACE BUTTON1	   	  Toggle bit / Choose character  
	   Z X				  Cycle characters  
	    +SHIFT			  Cycle characters * 16  
	   1 2				  Cycle foreground color  
	   3 4				  Cycle background color  
	   W A S D			  Scroll character  
	   Q E				  Flip character vertical horzontal  
	   R F				  Rotate character  
	   H				  Invert character  
	   J				  Create checkerboard  
	   K				  Clear character  
	   L				  Fill character  
	   C				  Copy character  
	   V			   	  Paste all  
	    +SHIFT			  Paste pattern  
	    +CTRL			  Paste color  
	   U				  Undo  
	   Y				  Quick save (lose undo)  
	   TAB				  Swap working characters location  
	   BS				  Save working characters  
	   ESC				  Exit without saving  
	   .				  Exit  
 	
	O I				Save / Load file  
					  ^ = There are more files  
	   ARROWS			  Choose a file  
	   ENTER(2x)			  Pick and save or load the file  
	   SPACE			  Write a name (ENTER blank to leave)  
	   E				  Change the extension to list  
	   D				  Delete the file (ENTER Y confirm)  
	   ESC				  Cancel  
  	
	* A lot of the tools functions on the edit character screen  
	  depend on where the cursor is, the character map or grid area.  
	* Working characters are the grid and background characters  
	  on the Edit Character screen. They can be edited and saved for  
	  the current session.  
  
**Some planned features and ideas:**  
  
	Support for Screen 1.5  
	Importer for character pattern data  
	Shift to constrain movement to straight lines  
	Key to show position of cursor. VRAM, x and y  
	Crop, flip and rotate the paste box  
	Swap to a second paste buffer if available  
	Basic: Error detection for file operations  
	Basic: Mouse support  
	Basic: File filter instead of extension filter  
  
  
--------  
###### References
>**[1]** Screen 1.5 is how I'm calling the Screen 1 hack to color the characters per pattern byte instead of blocks of 8 characters.  
