
# Yayasg!  
**Yet Another Yet Another Snake Game!**  
**`MSX`  `CoCo`**  
  
It occurred to me the  **Snake Games**  are the  **Hello World!**  of game design and I’ve never done one so I took this opportunity to explore not only the programming of this kind go game, trying for the  **fastest speed**  I can achieve (while keeping the code  **readable**), but also to explore some mythical (to me) characteristics of the computers involved, the “**undocumented**”  **graphic modes**.  
  
The best way to achieve a good performance while keeping the code readable is to have the least amount of operations possible. So in three of the versions the method I came up to erase the tail of the snake was to create an unidimensional array (`DIM`) mimicking the screen addresses. Each location holds the next address to go and when the tail erasing character come to a position, it reads its content and is sent there in the next turn, performing basically no calculation. When the snake eats a worm it just skips one update of the array, basically performing no action at all (it still has to run the code to create a new worm). The collision detection is made by directly `PEEK`ing the screen and the worms random positions are pre calculated on another array. This method was enough to cause the need of a slow down routine and enable a feature to make the snake go faster the bigger it gets.  
  
On the **Semigraphics 24** **CoCo** version I had not one but two problems using this method. First, the screen address space is too big to mimic on an array in Basic (6144 bytes instead of the 512 on a conventional text screen). Second, the fastest method to put graphics on a Semigraphic screen in Basic is with `GET`/`PUT` (this speed was what make me think the whole thing was possible) and it not only do not work with memory addresses, using a `X` `Y` coordinate, but it is also too big (256x192) for an array. Both this things made me compromise on an array (two arrays in fact, to hold `X` and `Y` components) representing the snake as it grows. In this case, each position in the arrays represent the size of the snake and point to the next coordinate. The collision detection is made by directly `PPOINT`ing the screen, the worms random positions are easily calculable so they are done on the fly. This works and is somewhat fast but the snake has a maximum size (the arrays size). I intend to do a reset of the arrays when they reach the end on the guise of the snake resting for a little but for now it just tires and its game over.  
  
The programs were made using **[Basic Dignified](https://github.com/farique1/msx-basic-dignified)** (`.bad`) and these are the commented version but there are of course `.asc` and `.bas` versions as well. There are `.dsk` files available on the **Releases** section.  
  
## **`MSX`**  
  
The **MSX** versions are basically the same with changes only on the graphic routines. Both versions read the playfield pre built from the disk but if it is not found, the program will ask to redraw the screen from the scratch.  
  
You control the game with the `cursor keys` and `enter` and you can, at the start, set the speed and amount of worms.  
  
The **Screen 1** version is `Yayasg.bas` and the **Screen 1.5** is `YayasgFC.bas`.  
  
Normal and Screen 1.5 versions  
SCREENSHOT_NORMAL -  SCREENSHOT_HIGH_COLOR  
  
## **`CoCo`**  
  
The **CoCo** versions are two different beasts but they share the same structure. Both versions read the playfield pre built from the disk but if it is not found, the program will ask to redraw the screen from the scratch.  
  
You control the game with the `cursor keys` and `enter` and you can, at the start, set the speed and amount of worms.  
  
The **normal** version is `Yayasg.bas` and the **Semigraphics 24** is `YayasgSG.bas`.  
  
Normal and Semigraphics 24 versions  
SCREENSHOT_NORMAL - SCREENSHOT_SEMIGRAPHICS  
