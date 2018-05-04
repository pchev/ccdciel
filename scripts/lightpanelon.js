// Example script to switch on the light panel light when starting a Flat series
// Change the device properties accordingly to the device you use
// In Preference/Flat Light ON script set:
// cscript C:\path-to-script\lightpanelon.js

// Replace by real switch driver
var S = new ActiveXObject("ASCOM.Simulator.Switch");
S.Connected = true;

//Replace by switch number and required value 
S.SetSwitchValue(2,100);

