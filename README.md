```                                                                                                           
                                                                ,...      ,,                            ,,    
                                                              .d' ""    '7MM                     mm   '7MM    
                                                              dM'         MM                     MM     MM    
 .P"Ybmmm  ,6"Yb. '7MMpMMMb.pMMMb.  .gP"Ya     ,pW"Wq.   mMMmm     ,M""bMM   .gP"Ya   ,6"Yb. mmMMmm   MMpMMMb.
:MI  I8   8)   MM   MM    MM    MM ,M'   Yb   6W'   'Wb   MM     ,AP    MM  ,M'   Yb 8)   MM   MM     MM    MM
 WmmmP"    ,pm9MM   MM    MM    MM 8M""""""   8M     M8   MM     8MI    MM  8M""""""  ,pm9MM   MM     MM    MM
8M        8M   MM   MM    MM    MM YM.    ,   YA.   ,A9   MM     'Mb    MM  YM.    , 8M   MM   MM     MM    MM
 YMMMMMb  'Moo9^Yo.JMML  JMML  JMML.'Mbmmd'    'Ybmd9'  .JMML.    'Wbmd"MML. 'Mbmmd' 'Moo9^Yo. 'Mbmo.JMML  JMML
6'     dP
Ybmmmd'
``` 

Game of Death is a toy [Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) implementation implemented in Typed Racket. I call this a toy implementation because it's using a naive quadtree algorithm rather than the state-of-the-art Hashlife. However, building it taught me a bit about Typed Racket, and it was a blast to put together.

It has some basic support for [Run Length Encoding](https://www.conwaylife.com/wiki/Run_Length_Encoded), and will import and export via that format.

GIFs can be exported, but beware, it's slow and might crash your system. GIFs will be sized according to the viewport, so if you're full screen, you're probably going to have a bad time.

The rule governing birth and death of cells can be modified to your liking. Colors are customizable. Importing and exporting to file is available. There are several sizes of brush.

These can be compiled and run with the [Racket programming language](https://racket-lang.org/).

VERSIONS

I guess this is 0.0.1.

CONTRIBUT(ORS|ING)

So far, just me (rob@robertlavery.com). Pull requests are welcome.
