#!/bin/bash

# The line to run
RUNLINE='./run.sh'

# The ciphers
CIPHERS=( "CES" "PLF" "VIG" "RTS" "RFC" )

# The keys 
KEYS=( "5" "data" "data" "1342" "5" )

cp ../small.txt ./
cp ../big.txt ./
cp ../GradingTemplate.docx ./
cp ../show.sh ./

for((index=0; index<5; ++index))
do
	# Get the cipher and the key
	CIPHER=${CIPHERS[index]}
	KEY=${KEYS[index]}
	
	echo "$CIPHER $KEY"
	
	
	
	mkdir -p $CIPHER
	
	echo "$RUNLINE $CIPHER "$KEY" ENC small.txt small.enc"
	$RUNLINE $CIPHER "$KEY" ENC small.txt small.enc
	echo "$RUNLINE $CIPHER "$KEY" DEC small.enc small.dec"
	$RUNLINE $CIPHER "$KEY" DEC small.enc small.dec

	read
	
	echo "$RUNLINE $CIPHER "$KEY" ENC big.txt big.enc"
	$RUNLINE $CIPHER "$KEY" ENC big.txt big.enc
	echo "$RUNLINE $CIPHER "$KEY" DEC big.enc big.dec"
	$RUNLINE $CIPHER "$KEY" DEC big.enc big.dec
	
	read
	
	mv small.dec $CIPHER
	mv big.dec $CIPHER
done

# Print all the small file encryptions
for((index=0; index<5;++index))
do

	# Get the cipher
	CIPHER=${CIPHERS[index]}
	
	echo "$CIPHER= `cat $CIPHER/small.dec`"
		
done

ls -l | grep txt
ls -l "CES/"
ls -l "PLF/"
ls -l "VIG/"
ls -l "RTS/"
ls -l "RFC/"

libreoffice5.4 --writer GradingTemplate.docx &
