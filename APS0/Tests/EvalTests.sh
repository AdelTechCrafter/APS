#! /bin/bash

for f in `ls ./../samples/*.aps`
do
  echo $f " : "
	./../evaluateur $f
	echo -e
done
echo "Eval succesfully ended"
