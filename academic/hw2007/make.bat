mkdir obj
copy *.* obj\*.*
chdir obj
lhs2tex play.tex -o final.tex
bibtex final
texify final.tex
cd ..
del play.dvi
copy obj\final.dvi play.dvi
