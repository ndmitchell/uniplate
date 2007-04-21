mkdir obj
set comp=ghc --make -odir obj -hidir obj -i. -i..
%comp% OperationsData.hs
%comp% OperationsTypeable.hs
%comp% OperationsAll.hs
%comp% Main.hs -o benchmark.exe
