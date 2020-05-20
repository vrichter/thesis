$ENV{'TEXINPUTS'}='./texmf//:';

$pdflatex = 'pdflatex -shell-escape -interaction=nonstopmode';
$lualatex = 'lualatex -shell-escape -interaction=nonstopmode';
$pdf_mode = 4;

$biber = 'biber --validate-datamodel --isbn-normalise --isbn13 %O %S';

$max_repeat = 15;

#add synctex extensions so they are cleaned
push @generated_exts, 'synctex', 'synctex.gz';

add_cus_dep('glo', 'gls', 0, 'makeglossaries');
add_cus_dep('acn', 'acr', 0, 'makeglossaries');
sub makeglossaries {
    system("makeglossaries \"$_[0]\"");
}
#add generated extensions so they are cleaned correctly
push @generated_exts, 'glo', 'gls', 'glg';
push @generated_exts, 'acn', 'acr', 'alg';
$clean_ext .= ' %R.ist %R.xdy %R.lol %R.bbl %R.synctex.gz(busy) %R.run.xml %R.thm _minted-%R';

add_cus_dep('py', 'pdf', 0, 'py2pdf');
sub py2pdf {
    system("python2 \"$_[0].py\" \"$_[0].pdf\"");
}

add_cus_dep('xml', 'pdf', 0, 'drawio2pdf');
sub drawio2pdf {
    system("drawio-batch -f pdf \"$_[0].xml\" \"$_[0].pdf\"");
}

add_cus_dep('xcf', 'png', 0, 'xcf2png');
sub xcf2png {
    system("xcf2png \"$_[0].xcf\" > \"$_[0].png\"");
}

add_cus_dep('quexml', 'tex', 0, 'quexml2tex');
sub quexml2tex {
    system("quexmltolatex -p \"${\basename($_[0])}\" \"$_[0].quexml\" > \"$_[0].tex\"");
}

$cleanup_includes_cusdep_generated = 1;
