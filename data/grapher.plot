set logscale y
set title "Pointer Analysis, fact database sizes"
set xlabel "Number of facts in the CFG of a file"
set ylabel "size (bytes)"
set key top left
plot 'size-POINTER.csv' using 1:3 title "Brute Force estimate - input size",'size-POINTER.csv' using 1:2 title "Variaibility-aware - input size", 'outputsize-POINTER.csv' using 1:3 title "Brute Force estimate - output size",'outputsize-POINTER.csv' using 1:2 title "Variaibility-aware - output size"


