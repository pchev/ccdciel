#!/bin/bash

# Run this script to update all the translations after modification of a 
# resource string in u_translation.pas and compilation of the program.
#
# The command updatepofiles is run two time to avoid error with comment on top of the file
#
# Update first the path to your Lazarus installation and run "make" in lazarus/tools
#

rstconv -i src/units/x86_64-linux-qt5/u_translation.rsj -o data/language/ccdciel.pot
/home/compiler/lazarus/tools/updatepofiles data/language/ccdciel.pot
/home/compiler/lazarus/tools/updatepofiles data/language/ccdciel.pot

rstconv -i src/units/x86_64-linux-qt5/u_hints.rsj -o data/language/ccdciel_hints.pot
/home/compiler/lazarus/tools/updatepofiles data/language/ccdciel_hints.pot
/home/compiler/lazarus/tools/updatepofiles data/language/ccdciel_hints.pot

mv data/language/ccdciel.pot data/language/ccdciel.po
mv data/language/ccdciel_hints.pot data/language/ccdciel_hints.po
