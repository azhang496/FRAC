# FRAC: Recursive Art Compiler
------------------------------

Group Members:
- Kunal Kamath (kak2211)
- Annie Zhang (az2350)
- Calvin Li (ctl2124)
- Justin Chiang (jc4127)


##Build:
Before you can create GIFs in FRAC, you will first need to install two GIF processing tools:

  1. Gifsicle: https://www.lcdf.org/gifsicle/
  2. GraphicsMagick: http://www.graphicsmagick.org/


After downloading and unzipping, you will need to run:

  ```
  ./configure
  make
  make install
  ```

in the top-level directory of both folders.


To execute your FRAC program, simply run:

  `make` in the FRAC directory and  
  `./run.sh [your_file.frac]` for the output  


##Other Folders:

1. demo  
Contains our demo presentation

2. documentation  
Contains our project proposal, LRM, and final report in HTML files

3. images  
Contains .bmp and .gif fractal examples generated in our language

4. lib  
Contains the turtle graphics files we use to draw fractals. Credit to Mike Lam for the code.

5. tests  
Contains our test suite for the project. Run `./testing.sh` to check all available tests
