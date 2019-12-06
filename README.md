```                                                                                                           
                                                            ,...        ,,                            ,,    
                                                          .d' ""      '7MM                     mm   '7MM    
                                                          dM'           MM                     MM     MM    
 .P"Ybmmm  ,6"Yb. '7MMpMMMb.pMMMb.  .gP"Ya     ,pW"Wq.   mMMmm     ,M""bMM   .gP"Ya   ,6"Yb. mmMMmm   MMpMMMb.
:MI  I8   8)   MM   MM    MM    MM ,M'   Yb   6W'   'Wb   MM     ,AP    MM  ,M'   Yb 8)   MM   MM     MM    MM
 WmmmP"    ,pm9MM   MM    MM    MM 8M""""""   8M     M8   MM     8MI    MM  8M""""""  ,pm9MM   MM     MM    MM
8M        8M   MM   MM    MM    MM YM.    ,   YA.   ,A9   MM     'Mb    MM  YM.    , 8M   MM   MM     MM    MM
 YMMMMMb  'Moo9^Yo.JMML  JMML  JMML.'Mbmmd'    'Ybmd9'  .JMML.    'Wbmd"MML. 'Mbmmd' 'Moo9^Yo. 'Mbmo.JMML  JMML
6'     dP
Ybmmmd'
``` 

## ABOUT

Game of Death is a toy [Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) implementation implemented in [Typed Racket](https://docs.racket-lang.org/ts-guide/). I call this a toy implementation because it's built on a naive [quadtree](https://en.wikipedia.org/wiki/Quadtree) data structure rather than the state-of-the-art [Hashlife](https://en.wikipedia.org/wiki/Hashlife). However, building it taught me a bit about Typed Racket, and it was a blast to put together.

## FEATURES

It has some basic support for [Run Length Encoding](https://www.conwaylife.com/wiki/Run_Length_Encoded), and will import and export via that format.

GIFs can be exported, but beware, it's slow and might crash your system. GIFs will be sized according to the viewport, so if you're full screen, you're probably going to have a bad time.

The rule governing birth and death of cells can be modified to your liking. Colors are customizable. Importing and exporting to file is available. There are several sizes of brush.

See the in-game help dialog for more. (Or just look at the code.)

## USAGE

These can be compiled and run with the [Racket programming language](https://racket-lang.org/).

## SCREENSHOTS

![image](https://user-images.githubusercontent.com/1585385/70302868-c20e4c80-17c3-11ea-8f95-ecee4247a54e.png)

![image](https://user-images.githubusercontent.com/1585385/70303049-39dc7700-17c4-11ea-820a-cfcf230d2be5.png)

## VERSIONS

1.0.0. First release!

## CONTRIBUT(ORS|ING)

So far, just me (rob@robertlavery.com). Pull requests are welcome.

## TODO

I should clean up and better comment this code. If there's interest I might compile, sign and host binaries, for those who don't want to install Racket. (I really do recommend Racket, though.)
