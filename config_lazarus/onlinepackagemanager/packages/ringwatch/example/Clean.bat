@echo off
echo.
echo.
echo Deleting temporary and backup files
echo
del *.~*
del *.ppu
del *.o
del *.or
rem cd Backup
del backup\*.~*
rem cd..
rmdir Backup
echo.
echo Cleaning completed