#!/bin/bash

#builds necessary config files
cd config/
cd gifsicle/
./config
make
make install
cd ..
cd GraphicsMagick/
./config
make
make install
cd ../..
