# 8 Bit Code  
  
 A bunch of projects for the computers of the **8 bit** era, mostly **MSX 1** and **CoCo 2.**  
  
## [Yayasg!](https://github.com/farique1/8bitcode/tree/main/Yayasg)  
**Yet Another Yet Another Snake Game!**  
**`MSX` `CoCo`**  
  
It occurred to me the **Snake Games** are the **Hello World!** of game design and I've never done one so I took this opportunity to explore not only the programming of this kind go game, trying for the **fastest speed** I can achieve (while keeping the code **readable**), but also to explore some mythical (to me) characteristics of the computers involved, the "**undocumented**" **graphic modes**.  
  
## [MASCIISDRAWX](https://github.com/farique1/8bitcode/tree/main/MASCIISDRAWX)  
**A Screen 1 drawing program in Assembly**  
Screen 1.5<sup>[1](#references)</sup> coming soon.  
**`MSX`**  
  
I always bragged that I knew **Assembly** but I've never coded anything more than a few small **helper routines** for may **Basic** programs. No more. I decided to make a complete and "useful" program in Assembly to call my own. Also I wanted to explore the undocumented **Screen 1.5** mode on the **MSX** and do something nice with it.  
  
## [Semidraw24](https://github.com/farique1/8bitcode/tree/main/Semidraw24)  
**A Semigraphics 24 drawing program with a touch of animation**  
**`CoCo`**  
  
The **elusive** and criminally underused **Semigraphics** modes on the **CoCo** always fascinated me. I had as given that you needed **Assembly** to access them but when I studied a little more and found out that they were **perfectly accessible** and usable in **Basic** this little exploration program came out.  
  
## [Change Graph Kit](https://github.com/farique1/Change-Graph-Kit)  
**Edit game graphics**  
**`MSX`**  
  
Back in the olden days I managed to **edit** the **graphics** on **commercial MSX games** by the way of three very small **Basic** programs that could live in memory along them, one at a time, without conflict. They worked but the whole thing was a mess. Recently I decided to know **how far** I could go **remaking** it. It also served as a tool to **test** and **debug** my **[Basic Dignified](https://github.com/farique1/msx-basic-dignified)** and **[Sublime Tools](https://github.com/farique1/MSX-Sublime-Tools)** projects.  

--------  
###### Acknowledgments  
  
Most of the Basic programs were made using **[MSX Basic Dignified](https://github.com/farique1/msx-basic-dignified)** or **[CoCo Basic Dignified](https://github.com/farique1/coco-basic-dignified)** (`.bad`) and these are the commented version but there are of course `.asc` and `.bas` versions as well.
There are `.dsk` and/or `.cas` files available on the **Releases** section for most of the programs.  
    
###### References  
**[1]** Screen 1.5 is how I'm calling the Screen 1 hack to color the characters per pattern byte instead of blocks of 8 characters. Here I need to send ANOTHER thank you to [Giovanni Nunes](https://github.com/plainspooky) for pointing me in the right direction.  
