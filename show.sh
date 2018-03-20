#!/bin/bash

# The small file
SMALL_FILE="small.dec"
BIG_FILE="big.dec"

CIPHERS=("CES" "PLF" "VIG" "RFC" "RTS")

for cipher in ${CIPHERS[@]}
do
	echo "**************$cipher**********"
	cat $cipher/$SMALL_FILE
	read
	cat $cipher/$BIG_FILE
	read
done
