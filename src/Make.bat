@echo off
mkdir tmp

for /R . %%f in (*.hs) do copy %%f tmp\
cd tmp
ghc --make Program.hs -o iml.exe
copy iml.exe ..
cd ..

rmdir /S /Q tmp
