@echo off
call Clean.bat
@echo on
strip --strip-all *.exe
upx --best *.exe
