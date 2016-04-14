: push-10 10 ;
: hoge POSTPONE push-10 ; immediate
: a hoge ; ( a push-10 )

: elsif
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
    then drop ." finish ";

