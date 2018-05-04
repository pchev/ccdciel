#!/bin/bash

# Example script to switch off the light panel light at the end of a Flat series
# Change the device properties accordingly to the device you use

# do nothing by default
exit

# Here the light switch is on the arduino socket 1
indi_setprop "Arduino Switcher.SOCKET 1.SOCKET01"=Off

# With a flip-flat we also need to reopen the cover
indi_setprop "Flip Flat.FLAT_LIGHT_CONTROL.FLAT_LIGHT_ON"=Off
sleep 1
indi_setprop "Flip Flat.CAP_PARK.PARK"=Off                                

echo light OFF

sleep 5

