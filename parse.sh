#!/bin/sh
if [ "$#" -lt 1 ]; then
	echo Needs one argument
	exit 1
fi

for ARG in $@
do
	echo "\$ cat $ARG | ./toy"
	cat $ARG | ./toy

	OUTPUT=${ARG%.*}.o
	echo "\n\$ mv output.o $OUTPUT"
	mv output.o $OUTPUT
	echo "Moved output to $OUTPUT\n\n"
done
