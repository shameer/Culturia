set term png small size 1980,1024
set output "benchmark.png"

set ylabel "RPS write"
set y2label "RPS read"

set ytics nomirror
set y2tics nomirror in

set yrange [0:*]
set y2range [0:*]

plot "benchmark.log" using 2 with lines axes x1y1 title "RPS write", \
     "benchmark.log" using 3 with lines axes x1y2 title "RPS read"