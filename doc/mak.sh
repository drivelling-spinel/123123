#! /bin/bash

# Usage of the works is permitted provided that this
# instrument is retained with the works, so that any entity
# that uses the works is notified of this instrument.
#
# DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

echo -ne "\\U Source Control Revision ID\n\nhg $(hg id -i), from commit on at $(hg log -r . --template="{date|isodatesec}\n")\n\nIf this is in ecm's repository, you can find it at \\W{https://hg.pushbx.org/ecm/tsr/rev/$(hg log -r . --template "{node|short}")}{https://hg.pushbx.org/ecm/tsr/rev/$(hg log -r . --template "{node|short}")}\n" > screvid.but
halibut --precise tsr.but screvid.but --html --text --pdf 2>&1 \
  | grep -Ev 'warning\: code paragraph line is [0-9]+ chars wide, wider than body width [0-9]+'
unix2dos tsr.txt
