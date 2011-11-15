#!/bin/sh

NODENAME="couch_image_resizer"

cd `dirname $0`

if [ -z "$1" ]; then
    CONFIG_FILE="config/couch_image_resizer.erlenv"
else
    CONFIG_FILE="$1"
fi

SASL_CONFIG="$(erl -sname $NODENAME -noshell -pa ebin -eval "error_logger:tty(false), couch_image_resizer_app:read_config(\"$CONFIG_FILE\"), io:format(\"~s~n\",[couch_image_resizer:get_app_env(sasl_config)])" -run init stop)"

export HEART_COMMAND="$(erl -sname $NODENAME -noshell -pa ebin -eval "error_logger:tty(false), couch_image_resizer_app:read_config(\"$CONFIG_FILE\"), io:format(\"~s~n\",[couch_image_resizer:get_app_env(heart_command)])" -run init stop)"

exec erl -detached -heart -connect_all false -pa ebin -pa deps/*/ebin -sname "$NODENAME" -boot start_sasl -config "$SASL_CONFIG" -run couch_image_resizer start "$CONFIG_FILE"
