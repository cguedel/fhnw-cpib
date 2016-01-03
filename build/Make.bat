@echo off
mkdir tmp

for /R ..\src %%f in (*.hs) do copy %%f tmp\
cd tmp
ghc --make -Wall Program.hs -o iml.exe
copy iml.exe ..
cd ..

rmdir /S /Q tmp
