clear
: push-10 10 ;

: hoge POSTPONE push-10 ; immediate

: a hoge ; ( a push-10 )



: elsif-test
    dup 10 <
    if  ." smaller than 10"
    else
	dup 20 <
	if ." smaller than 20"
	else
	    dup 30 <
	    if ." smaller than 30"
	    then
	then    
    then ." finish" ;

0  elsif-test

10 elsif-test

20 elsif-test

30 elsif-test

: hoge ." foo"  ;


." foo"

: hoge + * ;
: hello POSTPONE then ; immediate compile-only
: hoge POSTPONE if ; immediate