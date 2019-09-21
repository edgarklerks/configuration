#!/bin/zsh
rm /tmp/inputs
# Grab the mouse dev id
xinput list | grep keyboard |  grep -E -v '(Webcam|Video|Virtual|Button)' |  perl -ne 's/.*id=([0-9]+).*/\1/; print' | while read i; do
    (
         xinput test $i | head -n 5 | tail -n 1 >>| /tmp/inputs
	rm /tmp/dump.jpeg
	xtrlock &
	streamer -f jpeg -o /tmp/dump.jpeg
	feh --zoom fill -F /tmp/dump.jpeg &
	killall -9 xinput

	 ) &
done
