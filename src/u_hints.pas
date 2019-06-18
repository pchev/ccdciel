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
  rsTheSelectedS = 'The selected star luminosity profile';
  rsHistoryOfThe = 'History of the last FWHM and Intensity measurement';
  rsTheHalfFluxD = 'The Half Flux Diameter value in pixels';
  rsTheMaximumIn = 'The maximum intensity value';
  rsTheSignalNoi = 'The Signal/Noise ratio';
  rsTheFullWidth = 'The Full Width at Half Maximum value in pixels / arc '
    +'seconds';
  rsKeepTheGraph = 'Keep the graph visible after autofocus is completed';
  rsInspectTheRe = 'Inspect the repartition of the HFD across the image';
  rsStartImageLo = 'Start image loop centered on selected star';
  rsStartTheAuto = 'Start the auto-focus procedure';
  rsHistogramOfT = 'Histogram of the current image.%sClick and move to adjust '
    +'the visualisation threshold.';
  rsShowOfTheHis = 'Show %s of the histogram';
  rsCreateANewEm = 'Create a new empty profile.%sWarning this also clear every'
    +' program preference for this new profile.%sUse the Copy button if you '
    +'want to configure a different equipement but keep the program options.';
  rsZoomToAdjust = 'Zoom to adjust in window';
  rsFixTheVisual = 'Fix the visualisation threshold to the image data range, 1'
    +'00% of the histogram.';
  rsZoomTwoTime = 'Zoom two time';
  rsZoomToOrigin = 'Zoom to original scale';
  rsZoomToHalfSi = 'Zoom to half size';
  rsShowBullsEye = 'Show bulls eye';
  rsShowHighligh = 'Show highlight and shadow clipping';
  rsGammaOfTheIm = 'Gamma of the image display';
  rsInvertImageD = 'Invert image display';
  rsDeleteThisPr = 'Delete this profile and associated program preference.';
  rsCopyTheCurre = 'Copy the current profile to a new one.%sThis also copy '
    +'every program preference to the new profile.';
  rsMakeTestToDe = 'Make test to determine the most performant option '
    +'depending on your computer.%s(Network is mandatory if the camera is '
    +'connected to a remote Indi server)';
  rsTheTemporary = 'The temporary RAM disk used to transfert the images from '
    +'the Indi server';
  rsDoNotMoveThe = 'Do not move the telescope before to start imaging.';
  rsGetTheCoordi = 'Get the coordinates and object name from the planetarium';
  rsUsePlateSolv = 'Use plate solving to increase the accuracy of the '
    +'telescope position.';
  rsSolveTheCurr = 'Solve the current image and get the center coordinates';
  rsForMovingObj = 'For moving objects, asteroids or comets, you can update '
    +'the coordinates from the planetarium before to slew to the target.%sBe '
    +'sure the planetarium is connected, configured to follow the system time,'
    +' and can find the object name as it is typed here.';
  rsYouCanAvoidT = 'You can avoid to move to a focus star if this target field'
    +' include a suitable star.%sBeware the auto-focus will fail if the star '
    +'is too faint.';
  rsApplyThisSet = 'Apply this setting to all the targets';
  rsGetTheCoordi2 = 'Get the coordinates from the internal annotation database';
  rsStartAPrevie = 'Start a preview loop when waiting for the delay to expire.';
  rsSolveTheCurr2 = 'Solve the current image and get the rotation of the image';
  rsClearTimeCon = 'Clear time constraint';
  rsRepeatTheWho2 = 'Repeat the whole list a number of time.';
  rsInfiniteNumb = 'Infinite number of repetition';
  rsActivateTheS = 'Activate the start time';
  rsWaitToThisTi = 'Wait to this time before to start to process the list.';
  rsSetTheStartT = 'Set the start time at astronomical dusk, recomputed for '
    +'the date you run the sequence.';
  rsActivateTheS2 = 'Activate the stop time';
  rsStopAtThisTi = 'Stop at this time even if the processing is not finished.';
  rsSetTheStopTi = 'Set the stop time at astronomical dawn, recomputed for the'
    +' date you run the sequence.';
  rsThisAllowToR = 'This allow to restart an interupted sequence at the point '
    +'it was stopped';
  rsControlHowCo = 'Control how complete target are managed when the whole '
    +'list is repeated';
  rsAddAnObjectT = 'Add an object to the list';
  rsDeleteTheSel = 'Delete the selected row from the list.';
  rsAddAScriptTo = 'Add a script to the list.';
  rsAddAFlatSequ = 'Add a flat sequence to the list.';
  rsTheListOfTar = 'The list of target to process in sequence.';
  rsSaveTheListA = 'Save the list and close this window.';
  rsSaveTheListW = 'Save the list with a new name and close this window.';

implementation

end.

