mkdir obj
ghc --make Main.hs -odir obj -hidir obj -o benchmark.exe -i.. -O2
