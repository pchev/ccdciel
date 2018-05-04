#!/bin/bash

# Example script to switch on the light panel light when starting a Flat series
# Change the device properties accordingly to the device you use
# In Preference/Flat Light ON script set:
# bash -c /path-to-script/lightpanelon.sh

# do nothing by default
exit

# Here the light switch is on the arduino socket 1
indi_setprop "Arduino Switcher.SOCKET 1.SOCKET01"=On

# With a flip-flat we also need to close the cover
indi_setprop "Flip Flat.CAP_PARK.PARK"=On 
sleep 5
indi_setprop "Flip Flat.FLAT_LIGHT_CONTROL.FLAT_LIGHT_ON"=On

echo light ON

# wait enough for light intensity to stabilize
sleep 5

