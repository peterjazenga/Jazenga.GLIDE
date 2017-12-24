REM Adjust the path to yours path
SET FPC_VERSION=3.0.4
if "%1"=="" SET FPC_BIN_PATH=%~dp0
if NOT "%1"=="" SET FPC_BIN_PATH=%1

REM Create fpc.cfg
%FPC_BIN_PATH%\fpc\%FPC_VERSION%\bin\i386-win32\fpcmkcfg.exe -d basepath=%FPC_BIN_PATH%\fpc\%FPC_VERSION% -o %FPC_BIN_PATH%\fpc\%FPC_VERSION%\bin\i386-win32\fpc.cfg

REM Set FPC Path
Set Path=%FPC_BIN_PATH%\fpc\%FPC_VERSION%\bin\i386-win32

REM Make lazarus
make clean all

REM Strip
strip.exe lazbuild.exe
strip.exe lazarus.exe
strip.exe startlazarus.exe
