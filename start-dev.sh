#!/bin/sh

NODENAME="couch_image_resizer"

cd `dirname $0`
exec erl -connect_all false -pa ebin -pa deps/*/ebin -sname "$NODENAME" -s reloader -s couch_image_resizer
