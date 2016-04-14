clear
: push-10 10 ;
: hoge
    postpone push-10 ; immediate

: a hoge ;
.s
a 
.s
: foo [ 1 ] literal ;
\ ." fooo abaa "

: hoge ." foo"  ;

." aaa"
hoge hoge