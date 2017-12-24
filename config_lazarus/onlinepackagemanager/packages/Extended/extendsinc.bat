rem diffuse inc extended files

cd %CD%

cd ..

del Ancestromania\Packages\AncestroComponents\*.inc
del ManFrames\*.inc
del XMLFrames\*.inc

copy Extended\*.inc Ancestromania\Packages\AncestroComponents\
copy Extended\*.inc ManFrames\
copy Extended\*.inc XMLFrames\

