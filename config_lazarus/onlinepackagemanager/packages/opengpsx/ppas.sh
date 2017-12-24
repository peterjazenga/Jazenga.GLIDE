#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Linking /home/prajuab/Lazarus/OpenGPS/GPSTestProj
OFS=$IFS
IFS="
"
/usr/bin/ld -b elf32-i386 -m elf_i386  --dynamic-linker=/lib/ld-linux.so.2    -L. -o /home/prajuab/Lazarus/OpenGPS/GPSTestProj /home/prajuab/Lazarus/OpenGPS/link.res
if [ $? != 0 ]; then DoExitLink /home/prajuab/Lazarus/OpenGPS/GPSTestProj; fi
IFS=$OFS
