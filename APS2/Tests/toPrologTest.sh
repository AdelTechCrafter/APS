#! /bin/bash

for f in `ls ./../samples/*.aps`
do
	echo $f " : "
	./../toProlog $f
	echo -e
done
echo "toProlog successfully ended"
