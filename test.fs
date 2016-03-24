: push-10 10 ;
: hoge POSTPONE push-10 ; immediate
: a hoge ; ( a push-10 )
.s
a 
.s
a
