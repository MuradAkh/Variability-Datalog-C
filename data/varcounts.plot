set title "Number of features in busybox files"
set xlabel "Number of features in the CFG of a file"
set ylabel "Number of files"
binwidth = 1
bin(x, width) = width*floor(x/width)
set tics out nomirror
set style fill transparent solid 0.5 border lt -1
set xtics binwidth
set boxwidth binwidth

set xrange [-0.5:33.5]
plot 'varcounts.csv' using (bin($1,binwidth)):(1.0) smooth freq with boxes notitle