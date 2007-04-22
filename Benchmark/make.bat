mkdir obj
set comp=ghc --make -odir obj -hidir obj -i. -i.. %1
%comp% OperationsData.hs
%comp% OperationsTypeable.hs
%comp% OperationsAll.hs
%comp% OperationsManual.hs
%comp% Main.hs -o benchmark.exe
