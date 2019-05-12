#! /bin/bash

for f in `ls ./../samples/*.aps`
do
	echo $f " -> "
	./../toProlog $f
	echo  "type du programme :"
  	./../toProlog  $f| swipl -s ./../check.pl -g main_stdin
done

"Typechecking succesfully ended"


