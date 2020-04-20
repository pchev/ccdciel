unit u_translation;

{$mode objfpc}{$H+}

{
Copyright (C) 2019 Patrick Chevalley

http://www.ap-i.net
pch@ap-i.net

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. 

}

interface

uses
  gettext, translations, u_global, u_utils,
  LazUTF8, LazFileUtils, Classes, SysUtils;

procedure GetDefaultLanguage(var buf1, buf2: string);
function Translate(lang: string = ''): string;

resourcestring
  rsFile = 'File';
  rsBadPixelMap = 'Bad pixel map';
  rsClearBadPixe = 'Clear bad pixel map';
  rsApplyToCurre = 'Apply to current image';
  rsFocuserCalib = 'Focuser calibration';
  rsOpenFITSFile = 'Open FITS file%s';
  rsSaveFITSFile = 'Save FITS file%s';
  rsOpenReferenc = 'Open reference image';
  rsClearReferen = 'Clear reference image';
  rsQuit = 'Quit';
  rsEdit = 'Edit';
  rsPreferences = 'Preferences%s';
  rsINDISettings = 'INDI settings';
  rsViewHeader = 'View header';
  rsTools = 'Tools';
  rsConnection = 'Connection';
  rsPreview = 'Preview';
  rsAutoguider = 'Autoguider';
  rsPlanetarium = 'Planetarium';
  rsScript = 'Script';
  rsFocuser = 'Focuser';
  rsStarProfile = 'Star profile';
  rsCapture = 'Capture';
  rsFilters = 'Filters';
  rsFrame = 'Frame';
  rsRotator = 'Rotator';
  rsSensorTemperatu = 'Sensor temperature';
  rsTelescopeMou = 'Telescope mount';
  rsSequence = 'Sequence';
  rsVideo = 'Video';
  rsVisualisatio = 'Visualisation';
  rsMessages = 'Messages';
  rsClock = 'Clock';
  rsResetToDefau = 'Reset to default';
  rsConnect = 'Connect';
  rsStart = 'Start';
  rsBegin = 'Begin';
  rsLoop = 'Loop';
  rsCalibrate = 'Calibrate';
  rsGuide = 'Guide';
  rsDither = 'Dither';
  rsNewTarget = 'New target';
  rsRun = 'Run';
  rsStop = 'Stop';
  rsNew = 'New';
  rsZoom = 'Zoom';
  rsAdjustToWind = 'Adjust to window';
  rsFocus = 'Focus';
  rsMoveInward = 'Move inward';
  rsMoveOutward = 'Move outward';
  rsFocusAid = 'Focus aid';
  rsSet = 'Set';
  rsReset = 'Reset';
  rsRotate = 'Rotate';
  rsPark = 'Park';
  rsTrack = 'Track';
  rsLoad = 'Load';
  rsStartRecord = 'Start record';
  rsStopRecord = 'Stop record';
  rsHelp = 'Help';
  rsPDFDocumenta = 'PDF documentation';
  rsOnlineDocume = 'Online documentation';
  rsShowCurrentL = 'Show current log';
  rsBrowseLogFil = 'Browse log files';
  rsReportAProbl = 'Report a problem';
  rsDownloadLate = 'Download latest version';
  rsAbout = 'About';
  rsDevicesSetup = 'Devices Setup%s';
  rsCompiledWith = 'Compiled with';
  rsStartNewLog = 'Start new log';
  rsConnected = '%s connected';
  rsDisconnected = '%s disconnected';
  rsGuiding = '%s guiding';
  rsDevices = 'Device';
  rsConfiguratio = 'Configuration saved';
  rsTheCameraIsC = 'The camera is connected. Do you want to exit the program '
    +'now?';
  rsDisconnectin = 'Disconnecting devices';
  rsCoverTheCame = 'Cover the camera and set the exposure time and binning in '
    +'the Preview pane now.';
  rsClickContinu = 'Click Continue when ready.';
  rsBadPixelDete = 'Bad pixel detection found %s hot pixels.';
  rsTooManyHotPi = 'Too many hot pixel found!';
  rsPleaseIncrea = 'Please increase the threshold.';
  rsExposureFail = 'Exposure fail!';
  rsBadPixelMapC = 'Bad pixel map cleared.';
  rsPleaseConfig = 'Please configure your';
  rsCamera = 'Camera';
  rsFilterWheel = 'Filter wheel';
  rsMount = 'Mount';
  rsWatchdog = 'Watchdog';
  rsAreYouSureYo = 'Are you sure you want to disconnect all the devices now?';
  rsDisconnect = 'Disconnect';
  rsDesiredTempe = 'Desired temperature';
  rsExposureTime = 'Exposure time in secondes';
  rsCurrentFocus = 'Current focuser absolute position';
  rsRelativeIncr = 'Relative increment for the inward or outward movement';
  rsConnecting = 'Connecting %s';
  rsExposureAbor = 'Exposure aborted!';
  rsCancel = 'Cancel';
  rsCameraCooler = 'Camera cooler %s';
  rsPleaseActiva = 'Please activate the log file in the preference.';
  rsInvalidNumer = 'Invalid numeric value';
  rsCannotGetFoc = 'Cannot get focuser absolute position';
  rsFromToBy = 'From: %s to %s by %s';
  rsFocuserTempe = 'Focuser temperature: %s';
  rsStopVcurveLe = 'Stop Vcurve learning';
  rsMeasurementH = 'Measurement %s hfd:%s peak:%s snr:%s';
  rsCannotDetect = 'Cannot detect star.';
  rsCannotReachN = 'Cannot reach near focus HFD, please increase Half Width or'
    +' better center the curve.';
  rsCannotReachS = 'Cannot reach start focus HFD, please increase Half Width '
    +'or decrease the start HFD';
  rsCannotFindAS = 'Cannot find a star at his position. Move to a bright star '
    +'or increase the preview exposure time, or the autofocus binning.';
  rsStartLearnin = 'Start learning V curve';
  rsEastPointing = 'East (Pointing West)';
  rsWestPointing = 'West (Pointing East)';
  rsUnknowPierSi = 'Unknown pier side';
  rsParked = 'Parked';
  rsUnparked = 'Unparked';
  rsStopGuiding = 'Stop guiding';
  rsDisconnectTh = 'Disconnect the camera before to change the configuration.';
  rsProfile = 'Profile: %s';
  rsOptions = 'Options :%s';
  rsFilterName = 'Filter name';
  rsFocuserOffse = 'Focuser offset';
  rsExposureFact = 'Exposure factor';
  rsAbortExposur = 'Abort exposure';
  rsInvalidExpos = 'Invalid exposure time %s';
  rsFocuserTempe2 = 'Focuser temperature: %s , adjust position by %s';
  rsInvalidBinni = 'Invalid binning %s';
  rsSetBinning = 'Set Binning';
  rsStopPreview = 'Stop preview';
  rsMeridianFlip = 'Meridian flip aborted!';
  rsCaptureStopp = 'Capture stopped during autofocus';
  rsAutofocusFai = 'Autofocus failed!';
  rsDithering = 'Dithering';
  rsNotAutoguidi = 'Not autoguiding! dithering ignored.';
  rsStartingExpo = 'Starting %s exposure %s for %s seconds';
  rsDownloading = 'Downloading';
  rsSeq = 'Seq:';
  rsExp = 'Exp:';
  rsSec = 'sec.';
  rsStopCapture = 'Stop capture';
  rsSeqFinished = 'Seq: %s Finished';
  rsStackOfFrame = 'stack of %s frames';
  rsEndPreview = 'End preview';
  rsSavedFile = 'Saved file %s';
  rsSaved = 'Saved %s';
  rsImageDebayer = 'Image Debayered';
  rsImageUnDebay = 'Image Un-debayered';
  rsNotConnected = '%s not connected!';
  rsTheFocuserDo = 'The focuser do not support Absolute or Relative movement, '
    +'autofocus calibration is not possible.';
  rsCannotRunCal = 'Cannot run calibration now, stop capture and retry';
  rsCannotRunCal2 = 'Cannot run calibration now, stop preview and retry';
  rsFocuserCalib2 = 'Focuser calibration started';
  rsSetFocusDire = 'Set focus direction inward';
  rsSetFocusDire2 = 'Set focus direction outward';
  rsRequestToSto = 'Request to stop focuser calibration';
  rsTheFocuserDo2 = 'The focuser do not move enough after 30 steps! please '
    +'check if the focuser is moving at all or increase the minimum movement';
  rsTheFocuserDo3 = 'The focuser do not move enough after 30 steps! please '
    +'check if the focuser is moving at all or decrease the maximum HFD';
  rsReachFocuser = 'Reach focuser range limit! please better position the '
    +'focuser half way on it''s mechanical travel or decrease the maximum HFD';
  rsSelectAStarF = 'Select a star first!';
  rsCannotStartM = 'Cannot start manual focus now, stop capture and retry';
  rsFocusAidStar = 'Focus aid started';
  rsFocusAidStop = 'Focus aid stopped';
  rsAutofocusAlr = 'Autofocus already running.';
  rsPleaseConfig2 = 'Please configure the Autofocus options.';
  rsPleaseRunVcu = 'Please run Vcurve learning for binning %s';
  rsPleaseRunThe = 'Please run the V-curve learning for this focuser direction first. Button V-learn.';
  rsAutofocusNow = 'Autofocus now';
  rsStopAutoguid = 'Stop autoguider';
  rsStayAtTheCur = 'Stay at the current position for autofocus';
  rsInPlaceAutof = 'In place autofocus failed!';
  rsSequenceWill = 'Sequence will continue with the current focuser position.';
  rsGetCurrentPo = 'Get current position from current target';
  rsGetCurrentPo2 = 'Get current position from last image';
  rsCannotSolveC = 'Cannot solve current image.';
  rsGetCurrentPo3 = 'Get current position from telescope';
  rsSlewToFocusS = 'Slew to focus star %s';
  rsCannotFindAF = 'Cannot find a focus star.';
  rsStarDatabase =
    'Star database is empty, check path to star database files!!';
  rsAutofocusFai2 = 'Autofocus failed, try with another star...';
  rsAutofocusFai3 = 'Autofocus failed after %s retries, continue without '
    +'focusing.';
  rsReturnToTarg = 'Return to target position';
  rsRestartAutog = 'Restart autoguider';
  rsPause = 'Pause';
  rsFailedToStar = 'Failed to start guiding!';
  rsCameraOrFocu = 'Camera or focuser are not connected';
  rsCannotStartA2 = 'Cannot start autofocus now, astrometry is running';
  rsCannotStartA3 = 'Cannot start autofocus now, stop capture and retry';
  rsAutoFocusSta = 'AutoFocus started, initial position: %s';
  rsAutoFocusSta2 = 'AutoFocus started';
  rsAutofocusCan = 'Autofocus cannot find a star!%sPlease adjust your '
    +'parameters';
  rsAutoFocusSuc = 'AutoFocus successful';
  rsAutoFocusErr = 'AutoFocus error';
  rsReturnTheFoc = 'Return the focuser to previous position %s';
  rsResolveSucce = '%s resolve successful.';
  rsResolveError = '%s resolve error.';
  rsCameraAndMou = 'Camera and mount must be connected!';
  rsCameraAndRot = 'Camera and rotator must be connected!';
  rsRotatorIsNot = 'Rotator is not connected';
  rsSendImageToP = 'Send image to planetarium';
  rsPlanetariumE = 'Planetarium error.';
  rsPlanetariumI = 'Planetarium is not connected';
  rsUnableToFind = 'Unable to find resolve data!';
  rsCCDFrameSent = 'CCD frame sent to planetarium.';
  rsConnected2 = 'Connected %s';
  rsDisconnected3 = 'Disconnected';
  rsMoveToNewPla = 'Move to new planetarium object %s';
  rsPleaseConfir = 'Please confirm you want to slew the telescope to %s at '
    +'coordinates %s/%s';
  rsPlanetariumT = 'Planetarium target set to %s';
  rsPlanetariumT2 = 'Planetarium target slew fail';
  rsInvalidCoord = 'Invalid coordinates';
  rsBeforeToUseT = 'Before to use this tool you must connect the camera, the '
    +'mount and the planetarium';
  rsOpenFile = 'Open file %s';
  rsInvalidOrUns = 'Invalid or unsupported FITS file %s';
  rsReceiveUnkno = 'Receive unknown message: %s';
  rsMeridianIn = 'Meridian in';
  rsMeridianSinc = 'Meridian since';
  rsWaitMeridian = 'Wait meridian flip for %s minutes';
  rsWaitMeridian2 = 'Wait meridian flip';
  rsMountIsNotRe = 'Mount is not reporting pier side, meridian flip can be '
    +'unreliable.';
  rsMeridianFlip2 = 'Meridian flip will occur now.';
  rsClickContinu2 = 'Click Continue when ready';
  rsMeridianFlip3 = 'Meridian flip canceled before flip';
  rsMeridianFlip4 = 'Meridian flip now';
  rsMeridianFlip5 = 'Meridian flip';
  rsMeridianFlip6 = 'Meridian flip error!';
  rsWait1Minute = 'Wait 1 minute ...';
  rsMeridianFlip7 = 'Meridian flip done';
  rsMeridianFlip9 = 'Meridian flip canceled after flip';
  rsRecenterOnLa = 'Recenter on last position';
  rsRecenterImag = 'Recenter image error!%sdistance: %s';
  rsRecenterOnLa2 = 'Recenter on last image';
  rsMeridianFlip10 = 'Meridian flip completed';
  rsTiltIndicati2 = 'Tilt indication[hfd]=%s';
  rsImageMedianH = 'Image median hfd=%s';
  rsNoStarDetect = 'No star detected. Is the image focused and sufficiently '
    +'exposed?';
  rsTCPIPServerS = 'TCP/IP server stopped';
  rsSocketErrorS = 'Socket error %s.  %s';
  rsTCPIPServerL = 'TCP/IP server listen on port: %s';
  rsDevicesConne = 'Devices connection';
  rsCam = 'Cam.';
  rsFil = 'Fil.';
  rsFoc = 'Foc.';
  rsRot = 'Rot.';
  rsMnt = 'Mnt.';
  rsWch = 'Wch.';
  rsGain = 'Gain';
  rsBin = 'Bin';
  rsObject = 'Object';
  rsCount = 'Count';
  rsType = 'Type';
  rsDitherEvery = 'Dither every';
  rsDitherEvery2 = 'Dither%severy';
  rsFocusEvery = 'Focus every';
  rsStartCapture = 'Start capture';
  rsCannotStartC = 'Cannot start capture now';
  rsStack = 'Stack';
  rsStartSingleP = 'Start single preview';
  rsCannotStartP = 'Cannot start preview now';
  rsStopLoop = 'Stop Loop';
  rsStartPreview = 'Start preview loop';
  rsCannotStartP2 = 'Cannot start preview loop now';
  rsStopPreviewL = 'Stop preview loop';
  rsTakeControlE = 'Take control exposure for %s seconds';
  rsCurrent = 'Current';
  rsCooler = 'Cooler';
  rsSetpoint = 'Setpoint';
  rsFilter = 'Filter';
  rsSpeed = 'Speed';
  rsTimer = 'Timer';
  rsIncr = 'Incr.';
  rsPos = 'Pos.';
  rsStep = 'Step';
  rsVLearn = 'V-learn';
  rsTemp = 'Temp.';
  rsTelescopePos = 'Telescope position';
  rsRA = 'RA';
  rsDec = 'Dec';
  rsMin = 'min.';
  rsParkTheTeles = 'Park the telescope now?';
  rsPA = 'PA';
  rsReverse = 'Reverse';
  rsHalt = 'Halt';
  rsCalibrated = 'Calibrated';
  rsUncalibrated = 'Uncalibrated';
  rsWarningRever = 'Warning, reversing the rotator will invalidated the '
    +'calibration. Do you want to continue?';
  rsRunScript = 'Run script';
  rsCopy = 'Copy';
  rsAnotherScrip = 'Another script is already running';
  rsPleaseSelect = 'Please select a script!';
  rsNoScriptAreR = 'No script are running.';
  rsDoYouWantToD = 'Do you want to delete file %s ?';
  rsCopyTo = 'Copy to ';
  rsScriptAlread = 'Script %s already exist. Do you want to replace this file?';
  rsNewScript = 'New script';
  rsScriptAlread2 = 'Script %s already exist. Do you want to edit this script?';
  rsScriptAlread3 = 'Script %s already exist. Do you want to replace this '
    +'custom script by the template?';
  rsPlan = 'Plan';
  rsEnd = 'End';
  rsDesc = 'Desc.';
  rsExp2 = 'Exp.';
  rsRepeat = 'Repeat';
  rsCurrentPlan = 'Current plan';
  rsTargets = 'Targets';
  rsRunUnattende = 'Run unattended';
  rsDelete = 'Delete';
  rsSequenceAlre = 'Sequence %s already exist. Do you want to replace this '
    +'file?';
  rsAutoguiderNo = 'Autoguider not connected';
  rsCannotConnec = 'Cannot connect to autoguider, sequence will run without '
    +'guiding! %sDo you want to continue anyway?';
  rsSequenceWill2 = 'Sequence will run without guiding!';
  rsSequenceAbor = 'Sequence aborted.';
  rsTryToConnect = 'Try to connect to autoguider';
  rsCameraNotCoo = 'Camera not cooling, set temperature to %s';
  rsCaptureAlrea = 'Capture already running! please stop it first if you want '
    +'to start a new sequence.';
  rsPleaseLoadOr = 'Please load or create a target list first.';
  rsAutoguidingR = 'Autoguiding restarted.';
  rsSequenceAbor2 = 'Sequence aborted because the autoguider was not '
    +'reconnected after %s minutes.';
  rsAutoguiderWa = 'Autoguider was still not guiding after %s minutes.';
  rsTryNextTarge = 'Try next target in sequence.';
  rsAutoguidingS = 'Autoguiding stopped for %s minutes, sequence timeout in %s'
    +' minutes.';
  rsHFD = 'HFD';
  rsIntensity = 'Intensity';
  rsFWHM = 'FWHM';
  rsImageInspect = 'Image inspection';
  rsManualFocusA = 'Manual focus aid';
  rsAutofocus = 'Autofocus';
  rsAutofocusSta3 = 'Autofocus start Vcurve';
  rsAutofocusSta4 = 'Autofocus start Dynamic curve';
  rsAutofocusSta5 = 'Autofocus start Iterative focus';
  rsAutofocusCan2 = 'Autofocus canceled because no star was selected.';
  rsAutofocusCan3 = 'Autofocus canceled because no star was found.';
  rsAutofocusCan4 = 'Autofocus canceled because the HFD cannot be measured';
  rsAutofocusMea = 'Autofocus mean frame %s/%s, hfd=%s peak:%s snr:%s';
  rsAutofocusCan5 = 'Autofocus canceled because of low SNR, POS=%s HFD=%s PEAK'
    +':%s SNR:%s';
  rsAutofocusFin = 'Autofocus finished, POS=%s HFD=%s PEAK:%s SNR:%s TEMP:%s';
  rsFocuserSlipp = 'Focuser slippage offset set to %s';
  rsAutofocusFin2 = 'Autofocus final HFD is higher than %s';
  rsAutofocusRun = 'Autofocus running, hfd=%s peak:%s snr:%s';
  rsStartFocusHF = 'Start focus HFD is outside of current V curve, please '
    +'decrease the start HFD value';
  rsAutofocusMov = 'Autofocus move to start position %s';
  rsAutofocusMov2 = 'Autofocus move to near focus %s';
  rsAutofocusMea2 = 'Autofocus measurement %s : HFD=%s position=%s';
  rsAutofocusMov3 = 'Autofocus move to focus position %s';
  rsNotEnoughPoi = 'Not enough points in or out of focus position after two '
    +'try. Start with a better focus position and run the focuser calibration '
    +'tool.';
  rsTheFocuserIs = 'The focuser is now positioned at the best observed HFD '
    +'position.';
  rsNotEnoughPoi2 = 'Not enough points in or out of focus position, retrying '
    +'now.';
  rsTooSmallHFDD = 'Too small HFD difference, try to increase the number of '
    +'point or the movement, or run the focuser calibration tool to measure '
    +'this parameters.';
  rsTooSmallHFDD2 = 'Too small HFD difference.';
  rsFocusQuality = 'Focus quality = %s';
  rsAutofocusFoc = 'Autofocus focus in by %s';
  rsAutofocusFoc2 = 'Autofocus focus out by %s';
  rsFocusPositio = 'Focus position';
  rsMaxOffset = 'Max offset';
  rsNumberOfStep = 'Number of steps';
  rsGetCurrent = 'Get current';
  rsFocusDirecti = 'Focus direction';
  rsFit = 'Fit';
  rsQuality = 'Quality:';
  rsSlope = 'Slope';
  rsCenter = 'Center';
  rsLearn = 'Learn';
  rsSave = 'Save';
  rsVCurveQualit = 'V curve quality is low. Do you really want to save this '
    +'curve?';
  rsFocusDirecti2 = 'Focus direction: ';
  rsIn = 'In';
  rsOut = 'Out';
  rsInterface = 'Interface';
  rsServer = 'Server';
  rsPort = 'Port';
  rsTimeout = 'Timeout';
  rsChoose = 'Choose';
  rsSetup = 'Setup';
  rsSensor = 'Sensor';
  rsMainSensor = 'Main sensor';
  rsGuiderSensor = 'Guider sensor;';
  rsLoadConfigur = 'Load configuration on startup';
  rsImageTransfe = 'Image transfer';
  rsNetwork = 'Network';
  rsRAMDisk = 'RAM Disk';
  rsDirectory = 'Directory';
  rsBeSureToConf = 'Be sure to configure the camera connection.';
  rsUseFilterWhe = 'Use filter wheel';
  rsNotImplement = 'Not implemented!';
  rsUseFocuser = 'Use focuser';
  rsUseRotator = 'Use rotator';
  rsUseMount = 'Use mount';
  rsHeartBeatThr = 'Heart beat threshold [minutes]';
  rsUseWatchdog = 'Use watchdog';
  rsASCOMInterfa = 'ASCOM interface is only available on Windows.';
  rsFoundDevices = 'Found %s devices';
  rsCopyCurrentP = 'Copy current profile to';
  rsCreateNewEmp = 'Create new empty profile';
  rsHeight = 'Height';
  rsWidth = 'Width';
  rsOptions2 = 'Options';
  rsOK = 'OK';
  rsFiles = 'Files';
  rsCaptureFolde = 'Capture folder';
  rsLogAllMessag = 'Log all messages to file ';
  rsTemporaryFol = 'Temporary folder';
  rsDefault = 'Default';
  rsFileNameOpti = 'File name options';
  rsFolderNameOp = 'Folder name options';
  rsAllowToGetPr = 'Allow to get program status from TCP/IP internal server';
  rsLanguage = 'Language';
  rsObservatory = 'Observatory';
  rsObserverName = 'Observer name';
  rsObservatoryN = 'Observatory name';
  rsTelescopeNam = 'Telescope name';
  rsLatitude = 'Latitude';
  rsLongitude = 'Longitude';
  rsHorizonProfi = 'Horizon profile';
  rsMinimumObser = 'Minimum observing elevation ';
  rsColorPreview = 'Color preview';
  rsBayerMatrixP = 'Bayer matrix pattern';
  rsDebayerThePr = 'Debayer the preview image';
  rsReferenceIma = 'Reference image';
  rsTreshold = 'Treshold :';
  rsVideoPreview = 'Video preview frame rate ';
  rsBadPixelsDet = 'Bad pixels detection';
  rsBadPixelThre = 'Bad pixel threshold';
  rsSigma = 'sigma';
  rsPreviewStack = 'Preview stacking';
  rsShowPreviewS = 'Show preview stack option';
  rsClippingIndi = 'Clipping indicator';
  rsShadowADU = 'Shadow ADU';
  rsHighlightADU = 'Highlight ADU';
  rsAutomaticCoo = 'Automatic cooling ';
  rsCoolDownWhen = 'Cool down when camera is connected';
  rsDegree = 'degree';
  rsMaximumTempe = 'Maximum temperature change ';
  rsLimitTempera = 'Limit temperature change';
  rsDegreesPerMi = 'Degrees %s per minute';
  rsFlat = 'Flat';
  rsSequenceAuto = 'Sequence automatic flat';
  rsFlatAutoExpo = 'Flat auto-exposure';
  rsUseFlatAutom = 'Use flat automatic exposure';
  rsExposureTime2 = 'Exposure time';
  rsMax = 'Max:';
  rsMin2 = 'Min:';
  rsFlatImageMea = 'Flat image mean level';
  rsAutomaticFla = 'Automatic flat exposure is required fot the twilight flat.';
  rsStarDetectio = 'Star detection window size ';
  rsFocusWindowS = 'Focus window size';
  rsFocuserCorre = 'Focuser correction';
  rsStabilizatio = 'Stabilization delay after move';
  rsDisable = 'Disable';
  rsBacklashComp = 'Backlash compensation';
  rsFilterOffset = 'Filter offset';
  rsFocuserTempe3 = 'Focuser temperature compensation';
  rsTemperatureC = 'Temperature coefficient in steps/%s';
  rsAutofocusMet = 'Autofocus method';
  rsSeconds = 'seconds';
  rsMoveDirectio = 'Move direction';
  rsSlippageCorr = 'Slippage correction ';
  rsEstimatedSli = 'Estimated slippage since last learning';
  rsNumberOfDyna = 'Number of dynamic points';
  rsMovementBetw = 'Movement between points';
  rsInitialMovem = 'Initial movement (steps)';
  rsFinalMovemen = 'Final movement (steps)';
  rsNearFocus = 'Near focus';
  rsBinning = 'Binning';
  rsAutofocusTol = 'Autofocus tolerance ';
  rsMinSNR = 'min.SNR';
  rsSlewWithAPre = 'Slew with a precision of ';
  rsArcmin = '[arcmin]';
  rsAstrometry = 'Astrometry';
  rsAstrometryOp = 'Astrometry options';
  rsFromTelescop = 'From telescope driver';
  rsFocaleLength = 'Focale length';
  rsStartFocus = 'Start focus';
  rsPixelSize = 'Pixel size';
  rsFromCameraDr = 'From camera driver';
  rsSoftware = 'Software';
  rsMaximumSearc = 'Maximum search radius';
  rsScaleToleran = 'Scale tolerance';
  rsDownsample = 'Downsample';
  rsMaximumSourc = 'Maximum sources';
  rsOtherOptions = 'Other options ';
  rsCygwinPath = 'Cygwin path';
  rsUseCustomScr = 'Use custom script';
  rsElbrusImages = 'Elbrus Images folder ';
  rsImagesFolder = 'Images folder Unix path';
  rsBeforeYouCan = 'Before you can use this option the Elbrus program must be '
    +'calibrated for your images. Then select File / E-Wait for message.%sAlso'
    +' be sure to set the parameter: "Add the WCS in the FITS header".';
  rsManyFunction = 'Many functions will not work reliably without an '
    +'astrometry software.';
  rsProgramFolde = 'Program folder ';
  rsWaitAfterSol = 'Wait after solve';
  rsSlewing = 'Slewing';
  rsPrecisionSle = 'Precision slewing options';
  rsTargetPrecis = 'Target precision [arcmin] ';
  rsMaximumNumbe = 'Maximum number of retry';
  rsExposureTime3 = 'Exposure time [seconds]';
  rsControlExpos = 'Control exposure';
  rsCorrectionMe = 'Correction method';
  rsDelayAfterTe = 'Delay after telescope slew [seconds] ';
  rsMeridian = 'Meridian';
  rsOnMeridianCr = 'On Meridian crossing';
  rsCanTrackPast = 'Can track past meridian for';
  rsMinutes = 'minutes';
  rsPauseBeforeM = 'Pause before meridian flip ';
  rsPauseAfterMe = 'Pause after meridian flip ';
  rsNoFlipUntilP = 'No flip until past meridian for';
  rsAutofocusAft = 'Autofocus after meridian flip';
  rsCalibrateAut = 'Calibrate autoguider after meridian flip';
  rsAutoGuiding = 'Auto-guiding';
  rsPixels = 'Pixels';
  rsRAOnly = 'RA only';
  rsSettleTolera = 'Settle tolerance';
  rsMinTime = 'Min. time';
  rsCalibrationD = 'Calibration %sdelay';
  rsStarLostReco = 'Star lost recovery';
  rsS = 's';
  rsRestartAfter = 'Restart after';
  rsAbortAfter = 'Abort after';
  rsSkychartOnLo = 'Skychart on local computer';
  rsTheProgramNe = 'The program need to be restarted';
  rsNearHFDMustB = 'Near HFD must be smaller than Start HFD!';
  rsDestroyAllBa = 'Destroy all Bad Pixel Map data?';
  rsCreateFromCa = 'Create from camera';
  rsCreateFromDa = 'Create from dark file';
  rsOpenDarkFile = 'Open Dark file';
  rsSubfolderByS = 'Subfolder by sequence name';
  rsSubfolderByF = 'Subfolder by frame type';
  rsSubfolderByO = 'Subfolder by object name';
  rsSubfolderByP = 'Subfolder by plan step';
  rsSubfolderByE = 'Subfolder by exposure time';
  rsSubfolderByB = 'Subfolder by binning';
  rsSubfolderByD = 'Subfolder by date (UT)';
  rsSubfolderByD2 = 'Subfolder by date (local time, change at noon)';
  rsObjectName = 'Object name';
  rsDateUTSequen = 'Date (UT) / Sequence';
  rsNorth = 'North';
  rsSouth = 'South';
  rsWest = 'West';
  rsEast = 'East';
  rsRed = 'red';
  rsGreen = 'green';
  rsBlue = 'blue';
  rsNone2 = 'None';
  rsTwilightSkyF = 'Twilight sky flat';
  rsVCurve = 'V curve';
  rsDynamic = 'Dynamic';
  rsIterative = 'Iterative';
  rsMountSync = 'Mount sync';
  rsPointingOffs = 'Pointing offset';
  rsDoNothing = 'Do nothing';
  rsAutomaticFli = 'Automatic flip';
  rsAbort = 'Abort';
  rsNotEnoughMea = 'Not enough measurement points! Try to increase the '
    +'maximum HFD or decrease the minimum start movement';
  rsPleaseConnec = 'Please connect the planetarium first';
  rsPlanetariumP = 'Planetarium position';
  rsCenterRA = 'Center RA';
  rsCenterDec = 'Center Dec';
  rsClickTheObje = 'Click the object in planetarium';
  rsEditTargetLi = 'Edit target list';
  rsNewObject = 'New object';
  rsSkyFlat = 'Sky Flat';
  rsRepeatTheWho = 'Repeat the whole list';
  rsStartAt = 'Start at ';
  rsStopAt = 'Stop at';
  rsDusk = 'dusk';
  rsDawn = 'dawn';
  rsPreviewWhenW = 'Preview when waiting to repeat';
  rsSeconds2 = '[seconds]';
  rsInterval = 'Interval';
  rsAnyTime = 'Any time';
  rsEndTime = 'End time';
  rsStartTime = 'Start time';
  rsRise = 'rise';
  rsSet2 = 'set';
  rsNoMove = 'No move';
  rsUseAstromet2 = 'Use astrometry to%srefine the position';
  rsCurrentImage = 'Current image';
  rsUpdateRADec2 = 'Update RA+Dec%sfrom Planetarium';
  rsStayInPlace2 = 'Stay in place%sfor autofocus';
  rsSetRotatorAn = 'Set rotator angle';
  rsFlatTime = 'Flat time';
  rsAtDusk = 'at dusk';
  rsAtDawn = 'at dawn';
  rsInvalidObjec = 'Invalid object coordinates!';
  rsThisObjectIs = 'This object is never below the requested elevation';
  rsThisObjectIs2 = 'This object is never above the requested elevation';
  rsPlanAlreadyE = 'Plan %s already exist. Do you want to replace this file?';
  rsPlanAlreadyE2 = 'Plan %s already exist. Do you want to edit this plan?';
  rsYouMustConfi = 'You must configure your flat preference.';
  rsCanOnlyAddOn = 'Can only add one flat serie at dusk and one at dawn';
  rsDeleteSequen = 'Delete sequence %s ?';
  rsThereIsAlrea = 'There is already a dusk flat plan';
  rsThereIsAlrea2 = 'There is already a dawn flat plan';
  rsEditPlan = 'Edit plan';
  rsDescription = 'Description';
  rsExposure = 'Exposure';
  rsRepeatThisSt = 'Repeat this step';
  rsAutofocusBef = 'Autofocus%sbefore to start';
  rsAutofocusEve = 'Autofocus%severy';
  rsAdd = 'Add';
  rsResolve = 'Resolve';
  rsResolveAndSl = 'Resolve and Slew to image center';
  rsResolveAndSl2 = 'Resolve and Slew to cursor';
  rsResolveAndSy = 'Resolve and Sync the mount';
  rsResolveAndRo = 'Resolve and Rotate';
  rsResolveAndSy2 = 'Resolve and Sync the rotator';
  rsResolveAndSh = 'Resolve and show image in planetarium';
  rsResolveAndSh2 = 'Resolve and show image frame in planetarium';
  rsViewLastReso = 'View last resolver log';
  rsStopAstromet = 'Stop astrometry resolver';
  rsPreviewDebay = 'Preview debayer';
  rsAstrometryRe = 'Astrometry resolver log';
  rsFileNotFound = 'File not found %s';
  rsClose = 'Close';
  rsFITSHeader = 'FITS header';
  rsLogFilesAreS = 'Log files are saved in %s';
  rsFocuserCalib3 = 'Focuser calibration started, please wait...';
  rsFocuserCalib4 = 'Focuser calibration completed, click Next to see the '
    +'result.';
  rsNext = 'Next';
  rsBack = 'Back';
  rsThisProcedur = 'This procedure will help you to  find the focuser '
    +'parameters for auto-focus.%sYou only need to run it once to set this '
    +'parameters.%sCenter a star with a high elevation and do a manual '
    +'focusing to be as close as possible of the focus point. Use the Manual '
    +'focus tool for that.%sUse the Star profile tool to check the star peak '
    +'intensity is about three fourth of the camera maximum ADU but not '
    +'saturated. %sAfter focusing, your focuser must be positioned about half '
    +'way of it''s mechanical travel. If this is not the case please adjust it'
    +' now.%sThe binning you set in the Preview now will be used for the '
    +'autofocusing operation. If you want to change the focusing binning later'
    +' you have to run this procedure again.';
  rsGlobalFocuse = 'Global focuser parameters.%sSet the focuser direction IN '
    +'or OUT to the one that give the best result for your setup. If you plan '
    +'to use temperature compensation, set the direction it need to move when '
    +'the temperature drop during the night.';
  rsPreferedFocu = 'Prefered focuser direction';
  rsIfYourFocuse = 'If your focuser driver do not offer this option, set here '
    +'the number of steps necessary to clean the backlash when the direction '
    +'change. This not need to be a precise value, it is better to set it '
    +'higher than the actual backlash.';
  rsNoBacklash = 'No backlash';
  rsClickNextToS = 'Click Next to start the measurement.';
  rsMaximumDefoc = 'Maximum defocused HFD the focuser can safely reach';
  rsMinimumStart = 'Minimum, starting point,  focuser movement';
  rsYouCanSetThe = 'You can set the parameters to define how the focuser will '
    +'move.%sTry first with the default values and adjust them only if you '
    +'receive an error message on the next step.';
  rsParameter = 'Parameter';
  rsValue = 'Value';
  rsClickNextToS2 = 'Click Next to save this values to the configuration for '
    +'this profile.';
  rsTheDataAre2 = 'The data are now recorded in the profile.%sYou can review '
    +'the other auto-focus options using the menu %sEdit / Preferences / Autofocus';
  rsStartPositio = 'Start position  pos:%s hfd:%s peak:%s snr:%s';
  rsMeasurementP = 'Measurement %s pos:%s step:%s hfd:%s peak:%s snr:%s';
  rsRunScript2 = 'Run script %s';
  rsScriptFinish = 'Script %s finished';
  rsScriptExecut = 'Script execution error, row %s: %s';
  rsCompilationE = 'Compilation error: %s';
  rsScriptError = 'Script error: %s';
  rsScriptTermin = 'Script terminating...';
  rsOutOfRange = '%s out of range';
  rsNotAnEqmodMo = '%s not an eqmod mount';
  rsScriptEditor = 'Script editor';
  rsStepInto = 'Step Into';
  rsStepOver = 'Step Over';
  rsRemoveAllBre = 'Remove all breakpoints';
  rsCannotFindAp = 'Cannot find approximate coordinates for this image.%sThe '
    +'astrometry resolution may take a very long time.';
  rsResolvingUsi = 'Resolving using %s ...';
  rsResolverAlre = 'Resolver already running, cannot start astrometry now!';
  rsStopAstromet2 = 'Stop astrometry resolver.';
  rsCenterAppare = 'Center, apparent coord. RA=%s DEC=%s PA=%s';
  rsNoResolverCo = 'No resolver configured!';
  rsDoSimpleSlew = 'Do simple slew to %s/%s';
  rsSlewToEQ = 'Slewing to %s %s/%s';
  rsSlewComplete = 'Slew is complete';
  rsResolveContr = 'Resolve control exposure';
  rsFailToResolv = 'Fail to resolve control exposure';
  rsDistanceToTa = 'Distance to target: %s arcmin';
  rsSyncToEQ = 'Sync to %s %s/%s';
  rsSlewWithOffs = 'Slew with offset %s/%s';
  rsPrecisionSle2 = 'Precision slew finished.';
  rsPrecisionSle3 = 'Precision slew failed!';
  rsErrorResult = 'Error result = %s';
  rsErrorStartin = 'Error starting command: %s';
  rsSetTemperatu = 'Set temperature %s';
  rsTemperatureR = 'Temperature ramp requested to stop';
  rsSetTemperatu2 = 'Set temperature ramp to %s started';
  rsTemperatureR2 = 'Temperature ramp canceled';
  rsSetTemperatu3 = 'Set temperature ramp finished';
  rsAlignmentSta = 'Alignment star lost';
  rsStackingWith = 'Stacking with alignment star at %s/%s';
  rsNoAlignmentS = 'No alignment star found for stacking';
  rsErrorUnknowT = 'Error: Unknown telescope focal length, set in telescope '
    +'driver or in PREFERENCE, ASTROMETRY.';
  rsFocuserMoveT = 'Focuser move to %s';
  rsFocuserMoveB = 'Focuser move by %s';
  rsRotatorMoveT = 'Rotator move to PA %s';
  rsRotatorSyncC = 'Rotator sync calibration = %s';
  rsConnected3 = 'Connected';
  rsConnectionEr = 'Connection error: %s';
  rsDisconnectio = 'Disconnection error: %s';
  rsError = 'Error: %s';
  rsStartExposur = 'Start exposure error: %s';
  rsSetBinningX = 'Set binning %sx%s';
  rsSetFrameType = 'Set frame type %s';
  rsSetFilterPos = 'Set filter position %s';
  rsSetCooler = 'Set cooler %s';
  rsCannotSyncWh = 'Cannot Sync when the mount is not tracking';
  rsStartTracking = 'start tracking';
  rsStopTelescop = 'Stop telescope motion.';
  rsError2 = 'Error';
  rsNoResponseFr = 'No response from server';
  rsTrue = 'True';
  rsFalse = 'False';
  rsUnpark = 'Unpark';
  rsMountCapabil = 'Mount capabilities: %s';
  rsCameraFrameX = 'Camera frame x=%s y=%s width=%s height=%s';
  rsWaitToStartS = 'Wait to start sequence %s at %s';
  rsSequenceCanc = 'Sequence %s canceled before start';
  rsStopTimeAlre = 'Stop time %s already passed';
  rsNoDuskToday = 'No dusk today';
  rsSequenceStar = 'Sequence %s start immediately';
  rsSequenceIgno = 'Sequence %s ignore stop at dawn';
  rsNoDawnToday = 'No dawn today';
  rsStartingSequ = 'Starting sequence %s';
  rsStartingSequ2 = 'Starting sequence %s repeat %s/%s';
  rsStopTheCurre = 'Stop the current sequence at %s';
  rsRequestToSto2 = 'Request to stop the current sequence';
  rsAbortTheCurr = 'Abort the current sequence';
  rsRequestToSto3 = 'Request to stop autofocus ...';
  rsSequenceStop = 'Sequence stopped by user request, no termination script '
    +'will be run.';
  rsNotRunningNo = 'Not running, nothing to do.';
  rsTryNextTarge2 = 'Try next target';
  rsStopPlan = 'Stop plan %s';
  rsScriptFailed = 'Script %s failed!';
  rsDoYouWantToR = 'Do you want to retry?';
  rsTargetInitia = 'Target initialisation failed!';
  rsSequenceFini = 'Sequence %s finished.';
  rsInitializeTa = 'Initialize target %s';
  rsNewCoordinat = 'New coordinates from planetarium: %s %s';
  rsWaitToStartA = 'Wait to start at %s';
  rsTargetCancel = 'Target %s canceled before start';
  rsNoSuitableDu = 'No suitable dusk for automatic flat today';
  rsNoSuitableDa = 'No suitable dawn for automatic flat today';
  rsWaitingForTw = 'Waiting for twilight at %s';
  rsStopTheCurre2 = 'Stop the current target at %s';
  rsStartAutogui = 'Start autoguider';
  rsAutoguiderSt2 = 'Autoguider Start';
  rsAutoguiderNo2 = 'Autoguider not guiding %s seconds after requested to '
    +'start.%sDo you want to retry?';
  rsSlewAbortedA = 'Slew aborted after 3 retries!';
  rsErrorMountNo = 'Error! Mount not connected';
  rsTelescopeSle = 'Telescope slew error: %s arcminutes.';
  rsTelescopeSle2 = 'Telescope slew';
  rsAfterTelesco = 'After telescope pointing to target the offset relative to '
    +'requested position is %s arcminutes.%sDo you want to retry the slew?';
  rsWaitSecondsB = 'Wait %s seconds before repeated target %s';
  rsContinueInSe = 'Continue in %s seconds';
  rsSequenceStop2 = 'Sequence %s stopped.';
  rsRepeatTarget = 'Repeat target%s/%s %s';
  rsObjectStartP = 'Object %s, start plan %s';
  rsStartPlan = 'Start plan %s';
  rsObjectPlanFi = 'Object %s, plan %s finished.';
  rsPlanFinished = 'Plan %s finished.';
  rsStartStep = 'Start step %s';
  rsWaitSecondsB2 = 'Wait %s seconds before repeated sequence %s';
  rsPlanStopped = 'Plan %s stopped.';
  rsRepeatStep = 'Repeat step %s/%s %s';
  rsShowINDILog = 'Show INDI log';
  rsNumberOfExpo = 'Number of exposure per point';
  rsSomeDefinedD = 'Some defined devices are not connected';
  rsOpenPictureF = 'Open FITS or picture file%s';
  rsPauseAutogui = 'Pause autoguider';
  rsResumeAutogu = 'Resume autoguider';
  rsMaximumADU = 'Maximum ADU';
  rsSensorsPrope = 'Sensors properties';
  rsTheMenuFileF = 'The menu "Tools/Focuser calibration" help to fill this values.';
  rsSlewToANearS = 'Slew to a near star of magnitude';
  rsFocusStarSel = 'Focus star selection';
  rsDefaultBehav = 'Default behavior';
  rsSlewToFocusS2 = 'Slew to focus star';
  rsStayInPlace = 'Stay in place';
  rsDetectMultip = 'Detect multiple stars in the image';
  rsUsingMedianO = 'Using median of %s stars';
  rsHYPERBOLACur = 'HYPERBOLA curve fitting focus at %s,  remaining curve fit'
    +' error %s,  iteration cycles %s';
  rsImageCleanup = 'Image cleanup';
  rsSaveConfigur = 'Save configuration now';
  rsWeAreProbabl = 'We are probably trying to focus on a hot pixel!';
  rsPleaseCreate =
    'Please create a new bad pixel map or select a brighter star';
  rsItIsSuggestT = 'It is suggest to slew to focus star when using Vcurve';
  rsSystemIsUnde = 'System is undersampled, accept 1 pixel star';
  rsMagnifyer = 'Magnifier';
  rsDomePanel = 'Dome/Panel';
  rsFlatLevel = 'Flat level=%s';
  rsReachConfigu = 'Reach configured minimum flat exposure time!';
  rsReachConfigu2 = 'Reach configured maximum flat exposure time!';
  rsAdjustFlatEx = 'Adjust flat exposure time to %s and retry.';
  rsStopFlatCapt = 'Stop flat capture';
  rsSkyIsStillTo = 'Sky is still too bright, waiting for dusk';
  rsWaitingForDu = 'Waiting for dusk';
  rsSkyIsStillTo2 = 'Sky is still too dark, waiting for dawn';
  rsWaitingForDa = 'Waiting for dawn';
  rsTelescopeAzi = 'Telescope azimuth';
  rsTelescopeEle = 'Telescope elevation';
  rsSlewTelescop2 = 'Slew telescope to flat panel position';
  rsErrorProcess = 'Error processing temporary file in %s';
  rsTelescopeSle3 = 'Telescope slew failed';
  rsTemporaryFol2 = 'Temporary folder must contain only ASCII characters';
  rsSolvedInSeco = 'Solved in %s seconds';
  rsOffset = 'offset';
  rsSummary = 'Summary';
  rsCommands = 'Commands';
  rsDetails = 'Details';
  rsUseExternalC = 'Use external command to light the panel on/off';
  rsLightON = 'Light ON';
  rsLightOFF = 'Light OFF';
  rsResolveAndPl = 'Resolve and plot DSO';
  rsSaveAs = 'Save as...';
  rsRemoveObject = 'Remove object';
  rsCannotComput = 'Cannot compute rise or set time.';
  rsDeletePlan = 'Delete plan';
  rsSavePlan = 'Save plan';
  rsSavePlanAs = 'Save plan as...';
  rsRemoveStep = 'Remove step';
  rsAddStep = 'Add step';
  rsThePlanIsMod = 'The plan %s is modified. Do you want to save the change ?';
  rsNoAstronomic = 'No astronomical twilight at this location today';
  rsDeleteStep = 'Delete step %s ?';
  rsStepDescript = 'Step description';
  rsTerminationO = 'Termination options';
  rsStopTelescop2 = 'Stop telescope tracking';
  rsParkTheTeles2 = 'Park the telescope';
  rsWarmTheCamer = 'Warm the camera';
  rsRunAScript = 'Run a script';
  rsTheTelescope = 'The telescope is parked!';
  rsExecutingThe = 'Executing the termination error actions.';
  rsExecutingThe2 = 'Executing the termination actions.';
  rsTheSequenceW = 'The sequence %s will be stopped at %s, in %s seconds.';
  rsDarkNight = 'Dark%snight';
  rsTargetName = 'Target%sname';
  rsSkipTarget = 'Skip target %s';
  rsWaitingForDa2 = 'Waiting for dark night';
  rsTargetWillBe = 'Target can run until %s, in %s seconds';
  rsContinue = 'Continue';
  rsCancelInSeco = 'Cancel in %s seconds.';
  rsTomorrow = 'tomorrow';
  rsNeedToWaitUn = 'Need to wait until %s';
  rsTimeAlreadyP = 'Time already passed %s';
  rsNoTerminatio = 'No termination actions configured.';
  rsElevation = 'Elevation';
  rsSetMountTime = 'Set mount time from computer';
  rsSetMountSite = 'Set mount site long/lat from configuration';
  rsUserGroup = 'User group';
  rsUsingConfigu = 'Using configuration file %s';
  rsPleaseSelect2 = 'Please select a temporary folder without space in the '
    +'path name.';
  rsGetSiteLongL = 'Get site long/lat from the mount';
  rsTimeSendToTe = 'Time send to telescope.';
  rsSiteSendToTe = 'Site send to telescope.';
  rsSiteSetFromT = 'Site set from the telescope.';
  rsFlipTheImageV = 'Flip the image vertically';
  rsFlipTheImageH = 'Flip the image horizontally';
  rsTracking = 'tracking';
  rsNotTracking = 'not tracking';
  rsOn = 'On';
  rsOff = 'Off';
  rsStatus = '%s status';
  rsMountTrackin = 'Mount tracking restarted';
  rsMountTrackin2 = 'Mount tracking stopped! sequence timeout in %s minutes.';
  rsMountWasStil = 'Mount was still not tracking after %s minutes.';
  rsNoResponseFr2 = 'No response from camera!';
  rsDisplay = 'Display';
  rsReadImage = 'Read image';
  rsUnknownStatu = 'Unknown status';
  rsReadCCD = 'Read CCD';
  rsWaitStart = 'Wait start';
  rsIdle = 'Idle';
  rsPleaseRunThe2 = 'Please run the Focuser Calibration procedure first';
  rsDarkFrame = 'Dark frame';
  rsLoadDarkFile = 'Load dark file';
  rsClearDarkFra = 'Clear Dark frame';
  rsWeatherStati = 'Weather station';
  rsSafetyMonito = 'Safety monitor';
  rsClearConditi = 'Clear condition';
  rsSafeForOpera = 'Safe for operation';
  rsCloudCover = 'Cloud cover';
  rsDewPoint = 'Dew point';
  rsHumidity = 'Humidity';
  rsPressure = 'Pressure';
  rsRainRate = 'Rain rate';
  rsSkyBrightnes = 'Sky brightness';
  rsSkyQuality = 'Sky quality';
  rsSkyTemperatu = 'Sky temperature';
  rsStarFWHM = 'Star FWHM';
  rsTemperature = 'Temperature';
  rsWindDirectio = 'Wind direction';
  rsWindGust = 'Wind gust';
  rsWindSpeed = 'Wind speed';
  rsMinimum = 'minimum';
  rsMaximum = 'maximum';
  rsDome = 'Dome';
  rsOpen = 'Open';
  rsSlaved = 'Slaved';
  rsShowPrompt = 'Show prompt';
  rsStopDomeSlav = 'Stop Dome slaving';
  rsParkDome = 'Park the Dome';
  rsCloseDome = 'Close the Dome shutter';
  rsAutoguiderSh = 'Autoguider shutdown';
  rsPlanetariumS = 'Planetarium shutdown';
  rsCallExternal = 'Call external command';
  rsExitProgram = 'Exit program';
  rsSafe = 'Safe';
  rsUnsafe = 'Unsafe';
  rsUnsafeCondit = 'Unsafe condition detected!';
  rsTheSafetyMon = 'The safety monitor report unsafe condition.%sSelect %s to run the safety actions now, or %s to ignore them.';
  rsUnsafeCondit2 = 'Unsafe condition ignored by user';
  rsTheFollowing = 'The following actions are run if the safety monitor report dangerous condition';
  rsAction = 'Action';
  rsUseDome = 'Use dome';
  rsUseWeatherSt = 'Use weather station';
  rsInterfaceTyp = 'Interface type';
  rsObservingCon = 'Observing condition';
  rsUseSafetyMon = 'Use safety monitor';
  rsParkAndClose = 'Park and close the dome';
  rsGood = 'Good';
  rsBad = 'Bad';
  rsWeatherMonit = 'Weather monitor report: %s';
  rsWeatherIssue = 'Weather issue: %s';
  rsSequencePaus = 'Sequence paused for bad weather ...';
  rsContinueSequ = 'Continue sequence execution';
  rsIgnoreWeathe = 'Ignore weather condition for image type=%s';
  rsWeatherCondi = 'Weather condition are bad, it is best if you stop the running capture now.';
  rsDomeShutter = 'Dome shutter: %s';
  rsAvailable = 'available';
  rsUnavailable = 'unavailable';
  rsDomeSlaving = 'Dome slaving: %s';
  rsWaitingForMi = 'Waiting for %s minutes of good weather before to restart the sequence';
  rsReadoutModes = 'Readout modes';
  rsRestartAfter2 = 'Restart after weather is good for';
  rsPauseSequenc = 'Pause sequence when the observing condition are out of the limits';
  rsFilter0 = 'No change';
  rsSuspendDomeS = 'Suspend dome slaving';
  rsTemperatureS = 'Temperature scale';
  rsRunAutoFocus = 'Auto-focus if temp. change by more than %s';
  rsLicenseAgree = 'License agreement';
  rsUnattendedEr = 'Unattended error script';
  rsInCamera = 'In camera';
  rsInMount = 'In mount';
  rsNoDevice = 'No device';
  rsProtocol = 'Protocol';
  rsApplyToAllDe = 'Apply to all devices';
  rsRemoteDevice = 'Remote device number';
  rsGet = 'Get';
  rsColumn = 'column';
  rsUserName = 'User name';
  rsPassword = 'Password';
  rsTheMountIndi = 'the mount indicate %s';
  rsDoNotRunAsAd = 'Do not run %s as %s!';
  rsAdministrato = 'administrator';
  rsDonTSwait = 'Don''t%swait';
  rs32bitdriver = 'This driver work only in 32bit, please install the 32bit version of CCDciel.';
  rsSlave = 'Slave';
  rsMountIsNotPa = 'Mount is not parked.%sPark the dome anyway?';
  rsCannotComput2 = 'Cannot compute meridian time';
  rsSearch = 'Search';
  rsWaitTime = 'Wait time';
  rsPulseDuratio = 'Pulse duration';
  rsNoAutoGuidin = 'No auto-guiding, only dither by sending pulse guide directly to the mount.';
  rsSetTheMeanDi = 'Set the mean dithering pulse duration in seconds.';
  rsDitherOnly = 'Dither only';
  rsDeviceIsConn = 'Device %s is connected.%sDo you want to disconnect now to '
    +'open the setup dialog?';
  rsSeparator = 'Separator';
  rsMean = 'Mean:';
  rsStdDev = 'Std.Dev:';
  rsImageStatist = 'Image statistic';
  rsMode = 'Mode:';
  rsKeepCompleti = 'Keep completion status';
  rsResetComplet = 'Reset completion status on repeat';
  rsThisSequence = 'This sequence %s contain completion status.';
  rsClearTheComp = 'Clear the completion status?';
  rsThisSequence2 = 'This sequence contain information about the steps already completed.%sNow you can continue after the last checkpoint.%sDo you want to clear the completion status to restart from the beginning?';
  rsItWillContin = 'It will continue after the last checkpoint.';
  rsGlobalRepeat = 'Global repeat:';
  rsDone = 'done';
  rsThisWillBeLo = 'This will be lost when saving the new file.%sDo you want to save with a new name to not loss this information in the original file?';
  rsOpenTheDomeS = 'Open the Dome shutter';
  rsUnparkTheDom = 'Unpark the Dome';
  rsUnparkTheTel = 'Unpark the telescope';
  rsStartTelesco = 'Start Telescope tracking';
  rsSlaveTheDome = 'Slave the Dome';
  rsWaitTimeBetw = 'Wait time between actions';
  rsParkTheTeles3 = 'Park the telescope and the dome';
  rsTelescopeAnd = 'Telescope and dome are now parked';
  rsUnparkTheTel2 = 'Unpark the telescope and the dome';
  rsTelescopeAnd2 = 'Telescope and dome are now unparked';
  rsAutomaticall = 'Automatically slave "Dome Park" to "Mount Park"';
  rsOpenDomeSequ = 'Open dome sequence when mount is asked to unpark';
  rsCloseDomeSeq = 'Close dome sequence when mount is asked to park';
  rsAbortMount = 'Abort mount%s';
  rsDomeNotAfter = 'Dome not %s after request!';
  rsTelescopeNot = 'Telescope not %s after request!';
  rsAbortDome = 'Abort dome %s';
  rsUnslaved = 'Unslaved';
  rsAllowToOpenT = 'Allow to open the dome when safety status is bad or unknown';
  rsDomeAlready = 'Dome already %s';
  rsFOV = 'FOV';
  rsFromPlateSol = 'From plate solving';
  rsImageScale = 'Image scale';
  rsImage = 'Image';
  rsTheCameraIma = 'The camera image format is';
  rsPleaseSetThe = 'Please set the format to %s before to start the sequence.';
  rsWarningImage = 'Warning! %s image received';
  rsAutofocusGra = 'Autofocus graph';
  rsCannotChange = 'Cannot change start time. Sequence will start in time for the dusk flat.';
  rsCannotChange2 = 'Cannot change stop time. Sequence will stop in time for the dawn flat.';
  rsFixedSequenc = 'Fixed sequence length';
  rsImportCdCObs = 'Import CdC observing list';
  rsShowHints = 'Show hints';
  rsVerboseDevic = 'Verbose device log';
  rsResolveAndPl2 = 'Resolve and plot Hyperleda';
  rsPauseGuiding = 'Pause guiding during autofocus';
  rsGoto = 'Goto';
  rsRetrievePosi = 'Retrieve position from deepsky database';
  rsAzimuth = 'Azimuth';
  rsInsertRows = 'Insert rows';
  rsDoYouWantToR2 = 'Do you want to run the termination action now?';
  rsYes = 'Yes';
  rsNo = 'No';
  rsRequestedToN = 'Requested to not run the termination action';
  rsPeriodicAuto = 'Periodic auto-focus';
  rsAutomaticall2 = 'Automatically run auto-focus every';
  rsAutofocusDue = 'Autofocus due in';
  rsOr = 'or';
  rsCommandPath = 'Command path';
  rsPixelCount = '%sPixel count:%s';
  rsMedian = '%sMedian:%s';
  rsRecenterSequ = 'Recenter sequence target that drift';
  rsRunAstrometr = 'Run astrometry on every image to search for drift';
  rsRecenterIfTh = 'Recenter if the drift is more than';
  rsTargetWillBe2 = 'Target will be recentered before next image';
  rsRecenterTarg = 'Recenter target now';
  rsSaturated = 'saturated';
  rsPhotometry = 'Photometry';
  rsMeasurement = 'Measurement';
  rsUseAsReferen = 'Use as reference star';
  rsSetMagnitude = 'Set magnitude';
  rsNoStarFound = 'No star found';
  rsCaptureStopp2 = 'Capture stopped unexpectedly';
  rsSimplifiedPh = 'Simplified photometry taking account for calibration, exposure time and airmass';
  rsAirmass = 'Airmass';
  rsFlux = 'Flux';
  rsSimplifiedPh2 = 'Simplified photometry, uncalibrated';
  rsSimplifiedPh3 = 'Simplified photometry taking account for only the calibration';
  rsStar = 'Star';
  rsMaximumInten = 'Maximum intensity';
  rsBackground = 'Background';
  rsMagnitude = 'Magnitude';
  rsApproximateC = 'Approximate coordinates and image scale';
  rsAllowToSelec = 'Allow to select a brighter star for high filter exposure factor';
  rsAdjustAutofo = 'Adjust autofocus star magnitude:%s, exposure factor:%s';
  rsPauseSequenc2 = 'Pause sequence';
  rsTheSequenceI = 'The sequence is now in pause.';
  rsClickCancelT = 'Click Cancel to abort the sequence.';
  rsSequenceWil2 = 'Sequence will be paused after current exposure is complete.';
  rsPauseSequenc3 = 'Pause sequence request canceled.';
  rsAutomaticHFD = 'Automatic HFD measurement for every sequence capture';
  rsStatus2 = 'Status';
  rsWhenAskedMov = 'When asked, move the mount manually only along the RA axis, always in the same direction, by about 15 to 45 degrees each time.';
  rsDoYouWantToC = 'Do you want to cancel the polar alignment?';
  rsCancelPolarA = 'Cancel polar alignment';
  rsPleaseWaitUn = 'Please wait until the measurements are complete';
  rsTheMountDoNo = 'The mount do not move enough!';
  rsMoveTheMount = 'Move the mount manually in RA, click Continue when ready';
  rsComputationR = 'Computation result:';
  rsMountRotatio = 'Mount rotation center';
  rsTotalPolarEr = 'Total polar error:';
  rsHorizontalCo = 'Horizontal correction :';
  rsMoveWestBy = 'move West by ';
  rsMoveEastBy = 'move East by ';
  rsVerticalCorr = 'Vertical correction :';
  rsMoveDownBy = 'move Down by ';
  rsMoveUpBy = 'move Up by ';
  rsMoveTheGreen = 'Move the green circle to a star using the mouse.';
  rsThenAdjustTh = 'Then adjust the mount polar adjustment knob to move the star to the pink circle at the other extremity of the line.';
  rsYouCanCloseT = 'You can close this window after you finish to move the mount polar axis.';
  rsPolarAlignme = 'Polar alignment';
  rsMakeAFirstPo = 'Adjust the mount RA axis manually to point to the pole the best as possible.%s'+
                   'Slew the telescope to point near the pole at a declination about 89 to 90 degrees with counterweight down.%s'+
                   'Select below if the telescope can already slew automatically or if it must be moved manually.%s'+
                   'If automatic, select the direction and movement step. Beware it will move two time this amount.';
  rsMovingMount = 'Moving mount';
  rsAutomatic = 'Automatic';
  rsManual = 'Manual';
  rsMountRotatio2 = 'Mount rotation direction';
  rsRotateBy = 'rotate by';
  rsRotateTelesc = 'Rotate the telescope';
  rsExposureS = 'Exposure %s';
  rsPlateSolveEx = 'Plate solve exposure %s';
  rsPolarAlignme2 = 'Polar alignment instruction';
  rsSaveAPNGFile = 'Save a bitmap file additionally to the standard FITS file';
  rsSavePictureS = 'Save picture%s';
  rsLockOverlay = 'Lock overlay';
  rsAz = 'Az';
  rsAlt = 'Alt';
  rsAutoguiderEr = 'Autoguider error';
  rsTestEmailFro = 'Test email from CCDciel';
  rsThisMessageC = 'This message confirm the email configuration is working.';
  rsEmailSentSuc = 'Email sent successfully, please check your inbox';
  rsUseDSLRColor = 'Use DSLR color balance';
  rsColorBalance = 'Color balance';
  rsCompressTheF = 'Compress the FITS files with fpack';
  rsStartNewExpo = 'Start new exposure as early as possible';
  rsNotification = 'Notifications';
  rsEmailConfigu = 'Email configuration';
  rsSMTPServerAd = 'SMTP server address';
  rsSMTPServerPo = 'SMTP server port';
  rsFromEmailAdd = 'From email address';
  rsDestinationE = 'Destination email address';
  rsSendTestEmai = 'Send test email';
  rsSequenceNorm = 'Sequence normal end';
  rsSequenceAbno = 'Sequence abnormal end';
  rsAutofocusErr2 = 'Autofocus error';
  rsMeridianFlip8 = 'Meridian flip error';
  rsAll = 'All';
  rsEmailOn = 'Email on :';
  rsPleaseSeeThe = 'Please see the documentation for important security information';
  rsInvalid = 'Invalid';
  rsFStop = 'F-stop';
  rsSecureSSLTLS = 'Secure SSL/TLS connection';
  rsSSLTLSNotSup = 'SSL/TLS not supported, try to uncheck the corresponding box.';
  rsUseAsFallbac = 'Use as fallback if other software fail';
  rsRetryWithAst = 'Retry with astrometry.net';
  rsPlanet = 'Planet';
  rsAutofocusSta6 = 'Autofocus start Planet focus';
  rsTargetInitia2 = 'Target initialisation error';
  rsCompute = 'Compute';
  rsWaitingDSeco = 'Waiting %d seconds before to restart the exposure';
  rsCancelExposu = 'Cancel exposure';
  rsRestartExpos = 'Restart exposure';
  rsImageSharpne = 'Image sharpness %s';
  rsSharpness = 'Sharpness';
  rsTemperatureF = 'Temperature from weather station';
  rsExposureTime4 = 'Exposure time too short for measurement, need %d seconds.';
  rsNoMeasuremen = 'No measurement, early start exposure not set.';
  rsNoCenteringM = 'No centering measurement, exposure time shorter than astrometry timeout.';
  rsNoCenteringM2 = 'No centering measurement, no sequence or target not using precision slew.';
  rsNoCenteringM3 = 'No centering measurement, astrometry not configured.';
  rsGlobalAction = 'Global actions when a sequence is running';
  rsMeasurementO = 'Measurement on new images';
  rsLimitedTo15x = '(limited to 1.5x the slew precision)';
  rsThisOptionsA = 'This options are in addition to the autofocus frequency available in the Plan definition';
  rsOffAxisAberr = 'Off-axis aberration[hfd]=%s';
  rsCannotStartW = 'Cannot start when capture is running';
  rsCoordinates = 'Coordinates';
  rsRiseSetCondi = 'Rise/Set condition';
  rsInformation = 'Information';
  rsSlot = 'Slot';
  rsNumberOfSlot = 'Number of slot';
  rsManualFilter = 'Manual filter change canceled';
  rsInteractionD = 'Interaction dialog';
  rsSequenceStep = 'Sequence steps';
  rsErrorMessage = 'Error messages';
  rsEmailNotific = 'Email notification text';
  rsVoiceConfigu = 'Voice configuration';
  rsTest = 'Test';
  rsPerformance = 'Performance';
  rsDisplaySpeed = 'Display speed';
  rsLowQualityIm = 'Low quality image scaling';
  rsDoNotDisplay = 'Do not display images during capture';
  rsAddFITSKeywo = 'Add FITS keyword for DSLR EXIF tags';
  rsAlignmentOn = 'Alignment on';
  rsTruePole = 'True pole';
  rsRefractedPol = 'Refracted pole';
  rsDiscover = 'Discover';
  rsDiscoveryPor = 'Discovery port';
  rsPower = 'Power';
  rsCollimation = 'Collimation';
  rsNumberOfCirc = 'Number of circles';
  rsCenterABrigh = 'Center a bright star. %s Defocus until the ring are clearly visible. %s Click Start button. %s Adjust the collimation. %s The star must be centered in full frame for the final adjustement.';
  rsTargetSCoord = 'Target %s coordinates not updated';
  rsUseOnlyStarN = 'Use only star near the image center';
  rsDATEOBSFromC = 'DATE-OBS from camera driver';

implementation

procedure GetDefaultLanguage(var buf1, buf2: string);
var
  i: integer;
 {$ifdef darwin}
  response: TStringList;
 {$endif}
begin
  GetLanguageIDs(buf1, buf2);
  i := pos('.', buf1);
  if i > 0 then
    buf1 := copy(buf1, 1, i - 1);
 {$ifdef darwin}
  if (trim(buf1) = '') and (trim(buf2) = '') then
  begin
    response := TStringList.Create;
    ExecProcess('defaults read -g AppleLocale', response);
    if response.Count > 0 then
    begin
      buf1 := response[0];
      i := pos('_', buf1);
      if i > 0 then
        buf2 := copy(buf1, 1, i - 1);
    end;
    response.Free;
  end;
 {$endif}
end;

function Translate(lang: string = ''): string;
var
  lang2, pofile: string;
begin
  lang2 := '';
  if lang = '' then
    GetDefaultLanguage(lang, lang2);
  pofile := format(slash(appdir) + slash('data') + slash('language') + 'ccdciel.%s.po', [lang]);
  if FileExists(pofile) then
    Result := lang
  else
  begin
    pofile := format(slash(appdir) + slash('data') + slash('language') +  'ccdciel.%s.po', [lang2]);
    if FileExists(pofile) then
      Result := lang2
    else
    begin
      pofile := format(slash(appdir) + slash('data') + slash('language') +  'ccdciel.%s.po', ['en']);
      Result := 'en';
    end;
  end;
  // translate messages
  TranslateUnitResourceStrings('u_translation', systoutf8(
    slash(appdir) + slash('data') + slash('language') + 'ccdciel.%s.po'), Result, '');
  // translate hints
  TranslateUnitResourceStrings('u_hints', systoutf8(
    slash(appdir) + slash('data') + slash('language') + 'ccdciel_hints.%s.po'), Result, '');
  // translate LCL messages
  TranslateUnitResourceStrings('LCLStrConsts', systoutf8(
    slash(appdir) + slash('data') + slash('language') + 'lclstrconsts.%s.po'), Result, '');
end;

end.
