#!/bin/sh

# This program transforms the standard input by applying the keystrokes given as
# arguments to every line as if they were made in vim.
# When given -s as the first parameter, it will apply the given keystrokes only
# once instead of once for every line.

# We use three temporary files in this script:
#   - the first one, $VOUTPUTFILE, will be used to store our final output that
#     will be printed back to the user.
#   - the second one, $VINFOFILE, will be used to store the keystrokes we want to
#     apply to our input in the 'v' register. This file will be loaded by vim as
#     it's viminfo file (-i).
#   - the third one, $VSCRIPTFILE, will be used to apply execute the macro in
#     the v register. It will either apply the macro once to the whole file or
#     to every line, depending on wether the -s flag is present. Then, it will
#     write the file to $VOUTPUTFILE.
#     This will be loaded by vim as it's script file (-s).


# Create our temporary files
VUUID=$(uuidgen)
VSCRIPTFILE="/tmp/$VUUID-script"
VINFOFILE="/tmp/$VUUID-info"
VOUTPUTFILE="/tmp/$VUUID"

# Literal Return and Escape characters (produced with "C-v <key>")
RETURN=""
ESCAPE=""

# Write the $VSCRIPTFILE according to the presence of the -s flag
if [ "$1" = '-s' ]; then
    echo ":norm! @v$RETURN$ESCAPE:wq $VOUTPUTFILE" > "$VSCRIPTFILE"
    shift
else
    echo ":%norm! @v$RETURN$ESCAPE:wq $VOUTPUTFILE" > "$VSCRIPTFILE"
fi

# Write the $VINFOFILE to store the given keystrokes in the 'v' register
PARSED_SCRIPT="$(echo "$@" | sed "s/<cr>/$RETURN/g" | sed "s/<esc>/$ESCAPE/g")"
echo "\"v@	CHAR	0
	$PARSED_SCRIPT" > "$VINFOFILE"

# Pipe the stdin to vim
#   -X: don't talk to the X server (speeds up the startup time a bit)
#   -i: Use $VINFOFILE as the viminfo file
#   -s: Use the $VSCRIPTFILE as the script file executed when vim starts
# We pipe stdout and stderr to /dev/null to suppress vim's "Reading from
# stdin..." message
cat | vim - -X -i "$VINFOFILE" -s "$VSCRIPTFILE" 2>/dev/null 1>/dev/null

# Print back the $VOUTPUTFILE and then remove our temporary files
cat "$VOUTPUTFILE"
rm "$VSCRIPTFILE"
rm "$VINFOFILE"
rm "$VOUTPUTFILE"
