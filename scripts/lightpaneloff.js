// Example script to switch off the light panel light at the end of a Flat series
// Change the device properties accordingly to the device you use
// In Preference/Flat Light OFF script set:
// cscript C:\path-to-script\lightpaneloff.js

// Replace by real switch driver
var S = new ActiveXObject("ASCOM.Simulator.Switch");
S.Connected = true;

//Replace by switch number
S.SetSwitchValue(2,0);
