unit u_hints;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


resourcestring
  rsConnectAllDe = 'Connect all devices';
  rsAutoguiderSt = 'Autoguider status';
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
  rsPlanetariumS2 = 'Planetarium status';
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
  rsClearTheSequ = 'Clear the sequence completion status';
  rsTheSelectedS = 'The selected star luminosity profile';
  rsHistoryOfThe = 'History of the last FWHM and Intensity measurement';
  rsTheHalfFluxD = 'The Half Flux Diameter value in pixels';
  rsTheMaximumIn = 'The maximum intensity value';
  rsTheSignalNoi = 'The Signal/Noise ratio';
  rsTheFullWidth = 'The Full Width at Half Maximum value in pixels / arc '
    +'seconds';
  rsKeepTheGraph = 'Keep the graph visible after autofocus is completed';
  rsStartImageLo = 'Start image loop centered on selected star';
  rsStartTheAuto = 'Start the auto-focus procedure';
  rsHistogramOfT = 'Histogram of the current image.%sClick and move to adjust '
    +'the visualisation threshold.';
  rsCreateANewEm = 'Create a new empty profile.%sWarning this also clear every'
    +' program preference for this new profile.%sUse the Copy button if you '
    +'want to configure a different equipment but keep the program options.';
  rsZoomToAdjust = 'Zoom to adjust in window';
  rsZoomTwoTime = 'Zoom two time';
  rsZoomToOrigin = 'Zoom to original scale';
  rsZoomToHalfSi = 'Zoom to half size';
  rsShowBullsEye = 'Show crosshairs';
  rsShowHighligh = 'Show highlight and shadow clipping';
  rsGammaOfTheIm = 'Gamma of the image display';
  rsInvertImageD = 'Invert image display';
  rsDeleteThisPr = 'Delete this profile and associated program preference.';
  rsCopyTheCurre = 'Copy the current profile to a new one.%sThis also copy '
    +'every program preference to the new profile.';
  rsMakeTestToDe = 'Make test to determine the most performant option '
    +'depending on your computer.%s(Network is mandatory if the camera is '
    +'connected to a remote Indi server)';
  rsTheTemporary = 'The temporary RAM disk used to transfer the images from '
    +'the Indi server';
  rsUsePlateSolv = 'Use astrometric (plate) solving to improve telescope pointing accuracy';
  rsStartAPrevie = 'Start a preview loop when waiting for the delay to expire.';
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
  rsThisAllowToR = 'This allow to restart an interrupted sequence at the point '
    +'it was stopped';
  rsControlHowCo = 'Control how complete target are managed when the whole '
    +'list is repeated';
  rsDeleteTheSel = 'Delete the selected row from the list.';
  rsTheListOfTar = 'The list of target to process in sequence.';
  rsSaveTheListA = 'Save the list and close this window.';
  rsSaveTheListW = 'Save the list with a new name and close this window.';
  rsUseDynamic2 = 'Dynamic can be use with an absolute or relative position focuser,%s this method require the starting position is near the focus position.%s This is the preferred method.';
  rsUseVcurveWi2 = 'Vcurve can only work with an absolute position focuser, it require to learn the curve.';
  rsUseIterati2 = 'Use Iterative if the first two options do not work for you or if you start with really bad focus.%s This is the slowest and less precise method.';
  rsTheDirection = 'The direction the focuser will always finish to move.%sIf '
    +'using auto-focus set the same move direction.';
  rsActivateBack = 'Activate backlash compensation.%sLet this disabled if your'
    +' focuser driver include backlash compensation.';
  rsThePreferedF = 'The preferred focuser direction for autofocus.';
  rsTryToCorrect = 'Try to correct for focuser slippage since the last Vcurve '
    +'learning.%sBeware you must set the focuser temperature correction and '
    +'the filters offset before to use this option.';
  rsTheMagnitude = 'The magnitude of the star used to run the autofocus.%'
    +'sUsing a faint star can minimize the telescope movement but prefer the '
    +'brightest to avoid trouble with nearby stars.';
  rsTheElbrusIma = 'The Elbrus Images folder as set in the Elbrus parameters.';
  rsTheUnixPathE = 'The Unix path equivalent to the Elbrus Images folder.';
  rsStartTheVcur = 'Start the Vcurve learning process';
  rsIfTheImageIs = 'If the image is now focused, click this button to get the '
    +'current focuser position';
  rsAfterLearnin = 'After learning is completed you can use this cursor to '
    +'better adjust to the linear part of the curve.%sThe fit must be '
    +'particularly good near the horizontal blue line position.';
  rsTheObjectNam = 'The object name for use in FITS header and file name';
  rsTheNumberOfI = 'The number of images to take';
  rsTheTypeOfFra = 'The type of frame';
  rsTheNumberOfI2 = 'The number of image to take before dither';
  rsTheNumberOfI3 = 'The number of image to take before autofocus';
  rsAutofocusIfT = 'Autofocus if the temperature change is greater than the '
    +'configured value';
  rsAutofocusIfHFD = 'Autofocus if the HFD % change is greater than the '
    +'configured value';
  rsEditTheCurre = 'Edit the current sequence';
  rsListOfProfil = 'List of profile for every different equipment configuration';
  rsGlobalINDISe = 'Global INDI server parameters';
  rsGlobalALPACA = 'Global ALPACA server parameters';
  rsTheBaseFolde = 'The base folder where the images are recorded';
  rsATemporaryDi = 'A temporary directory for work files. Be sure it contain '
    +'only ASCII character and no space';
  rsSelectTheInf = 'Select the information used to make the image file name';
  rsSelectTheInf2 = 'Select the information used to make the image directory '
    +'name';
  rsTheObserverN = 'The observer name for inclusion in the FITS header';
  rsTheObservato = 'The observatory name for inclusion in the FITS header';
  rsTheTelescopeNameForI =
    'The telescope name for inclusion in the FITS header';
  rsTheHorizonPr = 'The horizon profile used to compute object visibility. '
    +'This file is the same as used in Cartes du Ciel';
  rsTheMinimalOb = 'The minimal object elevation to start or end observing';
  rsBeSureToPaus = 'Be sure to pause guiding when using a OAG. You can continue guiding when using a separate guide scope.';
  rsDragDropToCh = 'Drag&drop to change order';
  rsClickToSortB = 'Click to sort by %s';
  rsClickToSetVa = 'Click to set value for all the targets';
  rsStepPlanFor  = 'Step plan for capturing the images.%sThe step plan details are specified in the list below';
  rsWaitForFullD = 'Wait for a moonless night';
  rsDonTWaitForT = 'Process the other targets in the list instead of waiting for the start condition(s) to be met. To be used with option "Repeat the whole list"';
  rsADescription = 'A description for this step';
  rsTheNumberOfI4 = 'The number of image for this step';
  rsAutofocusAtT = 'Autofocus at the start of this step';
  rsRedoAutofocu = 'Redo autofocus after a number of frames';
  rsDitherAfterT = 'Dither on the specified frame interval';
  rsSetHereTheAc = 'Set here the action to take after the sequence is terminated';
  rsPixelBinning = 'Pixel binning';
  rsTheTargetNam = 'The target name to be used for capturing the images';
  rsTargetPositi = 'Target position (J2000)';
  rsCameraPositi = 'Camera position angle';
  rsStartTimeCap = 'Start time capturing (MC=Meridian crossing)';
  rsStopTimeCapt = 'Stop time capturing (MC=Meridian crossing)';
  rsStayAtTheTar = 'Stay at the target position for autofocus.%sA medium bright star should be visible.%sElse slew to a bright database star.';
  rsPriorToSlewi = 'Prior to slewing retrieve actual comet or asteroid coordinates from planetarium program.%sPlanetarium program should be connected and following system time.%sTarget name should be searchable within the planetarium.';
  rsRepeatThePla = 'Repeat the plan for this object.%sSet to zero to not process this target.';
  rsLetBlankForD = 'Let blank for default system search path';
  rsActiveOnlyIf = 'Active only if the target use astrometry positioning%sand the exposure time is longer than the astrometry timeout';
  rsPauseTheSequ = 'Pause the sequence after the current exposure is complete';
  rsShowCompleti = 'Show completion status';
  rsShowLastCapt = 'Show captured image';
  rsShowGuidingS = 'Show guiding statistics';
  rsGuidingHisto = 'Guiding history in arcsecond %s - blue: RA error %s - red: Dec error %s - yellow: Star mass';
  rsDoNotSetThis = 'Do not set this option if you don''t know exactly what you do and without careful testing';
  rsImageLuminos = 'Image luminosity';
  rsCameraOffset = 'Camera offset';
  rsIfNotChecked = 'If not checked the program will never change the gain/offset value set in the driver setup';
  rsHistogramFul = 'Histogram full range or clipped to available data';
  rsSaveIndividu = 'Save individual images before stacking';
  rsFlatProcessi = 'Flat processing also require a Dark';
  rsSetTheTemper = 'Set the temperature threshold in the Preferences';
  rsSetTheHFD = 'Set the HFD % change threshold in the Preferences. Measurements must also be enabled.';
  rsThisOptionIs = 'This option is normally unchecked, use it only if you manage the pointing and autoguiding by script.';

implementation

end.

