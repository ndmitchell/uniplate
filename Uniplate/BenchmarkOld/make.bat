mkdir obj
set comp=ghc --make -O2 -odir obj -hidir obj -i. -i.. -fglasgow-exts %1
%comp% OperationsData.hs
%comp% OperationsTypeable.hs
%comp% OperationsAll.hs
%comp% OperationsManual.hs
%comp% Main.hs -o benchmark.exe
