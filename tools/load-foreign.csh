#!/bin/csh -fx

# script run by load-foreign to do OS-specific symbol-table extraction.

if ( `uname` == HP-UX ) then
   ld -a archive -N -A $argv[1] -R $argv[2] -o $argv[3] $argv[5-] /lib/dyncall.o
   if ($status != 0) exit 1

   /bin/nm -xp $argv[3] | sed -e '/ [tdb] /d' -e 's/^0x//' -e 's/  / /' > $argv[4]
   if ($status != 0) exit 2
else
   ld -N -A $argv[1] -T $argv[2] -o $argv[3] $argv[5-]
   if ($status != 0) exit 1

   nm -gp $argv[3] > $argv[4]
   if ($status != 0) exit 2
endif

exit 0
