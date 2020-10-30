#!/bin/sh

font="Lucida MAC"

# Colours
background='#000000'
titleColour='^fg(#00AAAA)'
asideColour='^fg(#666666)'
headingColour='^fg(#FFFFFF)'
keyColourSuper='^fg(#AAAA00)'
keyColourHyper='^fg(#AA88FF)'
keyColourMedia='^fg(#FF8888)'
descColour='^fg(#AAAAAA)'

# Patterns to replace
keyLinesSuper='\(M4-\|Super\)[^ ]*'
keyLinesHyper='M3-[^ ]*'
keyLinesMedia='\(Print\|XF86\|C-\)[^ ]*'
headings='>>'

# Replacement Variables
super="${keyColourSuper}Super(Windows\/Space)${titleColour}"
hyper="${keyColourHyper}Hyper(Caps Lock)${titleColour}"
title="${titleColour}XMonad Keybindings (with the $super or $hyper key)"\
"${asideColour}        -    Click to close"

# Screen dimensions, for positioning calculations
screenXY=`xdpyinfo | awk '/dimensions:/ { print $2 }'`
screenX=${screenXY%x*}
screenY=${screenXY#*x}

# Dimensions
lineHeight=20
lines=32
replaceSeparator="s/SeparatorPlaceholder/    /g"
width=1200
height=`expr ${lineHeight} \* \( ${lines} + 1 \)`

# Position
xPos=`expr \( ${screenX} - ${width} \) / 2`
yPos=`expr \( ${screenY} - ${height} \) / 2`

# Dzen behaviour
eventActions='onstart=uncollapse'\
';button1=exit;button3=exit;key_Escape=exit'\
';button4=scrollup;button5=scrolldown'

# Replace placeholders
replaceTitle="s/TitlePlaceholder/${title}/g"
replaceSuperTap="s/C-Escape/Super   /g"
replaceShift="s/Shift-\([^ ]*\)/S-\1    /g"
replaceSlash="s/slash/\/    /g"
replacePlaceholders="${replaceTitle};${replaceM4};${replaceSuperTap}
;${replaceShift};${replaceSlash};${replaceSeparator}"

# Format colour
colourKeyLinesSuper="s/${keyLinesSuper}/${keyColourSuper}&${descColour}/g"
colourKeyLinesHyper="s/${keyLinesHyper}/${keyColourHyper}&${descColour}/g"
colourKeyLinesMedia="s/${keyLinesMedia}/${keyColourMedia}&${descColour}/g"
colourHeadings="s/${headings}/${headingColour}&/g"
formatColour="${colourKeyLinesSuper};${colourKeyLinesHyper};${colourKeyLinesMedia};${colourHeadings}"

# Remove redundancies
removeM4="s/M4-\([^ ]*\)/\1   /g"
removeM3="s/M3-\([^ ]*\)/\1   /g"
screen="s/ S \(.\)/ \1  /g"
removeRedundancies="${removeM4};${removeM3};${screen}"

addMargin="/[^<${title}>]/s/^/  /g"

sed "${replacePlaceholders};${formatColour};${addMargin};${removeRedundancies}" \
    | dzen2 -p \
            -bg $background \
            -h "$lineHeight" -w "$width" -l "$lines" \
            -x "$xPos" -y "$yPos" \
            -fn $font \
            -e $eventActions
