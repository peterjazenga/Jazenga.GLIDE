#!/bin/bash
# diffuse inc extended files

directory=`dirname $0`
echo "Working in $directory"
cd $directory
rm ../Ancestromania/Packages/AncestroComponents/*.inc
ln -s $directory/extends.inc ../Ancestromania/Packages/AncestroComponents/extends.inc
ln -s $directory/dlcompilers.inc ../Ancestromania/Packages/AncestroComponents/dlcompilers.inc
rm ../ManFrames/*.inc
ln -s $directory/extends.inc ../ManFrames/extends.inc
ln -s $directory/dlcompilers.inc ../ManFrames/dlcompilers.inc
rm ../XMLFrames/*.inc
ln -s $directory/extends.inc ../XMLFrames/extends.inc
ln -s $directory/dlcompilers.inc ../XMLFrames/dlcompilers.inc

