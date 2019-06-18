unit u_hints;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


resourcestring
  rsConnectAllDe = 'Connect all devices';
  rsAutoguiderSt = 'Autoguider status';
  rsExposureTime = 'Exposure time in seconds';
  rsStartTheCapt = 'Start the capture sequence';
  rsIncrementSte = 'Increment step for the inward or outward movement';
  rsSetAbsoluteF = 'Set absolute focuser position';
  rsVCurveLearni = 'V curve learning';
  rsMoveFocuserI = 'Move focuser inward';
  rsMoveFocuserO = 'Move focuser outward';
  rsStartX = 'Start X';
  rsStartY = 'Start Y';
  rsCurrentTeles = 'Current telescope Right Ascension';
  rsCurrentTeles2 = 'Current telescope Declination';
  rsCurrentTeles3 = 'Current telescope side of pier';
  rsTimeFromMeri = 'Time from meridian transit (hour angle)';
  rsPlanetariumS = 'Planetarium status';
  rsCameraISO = 'Camera ISO';
  rsCameraGain = 'Camera gain';
  rsCameraBinnin = 'Camera binning';
  rsStartOnePrev = 'Start one preview exposure';
  rsLoopPreviewE = 'Loop preview exposures';
  rsStackThePrev = 'Stack the preview frames to a single image.%sA new stack '
    +'is started when you start a loop or if you uncheck this button.';
  rsLoadASequenc = 'Load a sequence file';
  rsCreateANewSe = 'Create a new sequence';
  rsStartTheSequ = 'Start the sequence execution';
  rsStopTheSeque = 'Stop the sequence execution';
  rsIfCheckedNoC = 'If checked no confirmation dialog will be presented and '
    +'failed operation will abort the current step.';
  rsCopyTheSeque = 'Copy the sequence to a new one. Completion status is '
    +'reinitialized';
  rsDeleteTheSeq = 'Delete the sequence file';
  rsClearTheSequ = 'Clear the sequence completion status';

implementation

end.

