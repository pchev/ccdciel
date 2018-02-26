unit u_translation;

{$mode objfpc}{$H+}

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
  rsCCDTemperatu = 'CCD Temperature';
  rsTelescopeMou = 'Telescope mount';
  rsSequence = 'Sequence';
  rsVideo = 'Video';
  rsVisualisatio = 'Visualisation';
  rsMessages = 'Messages';
  rsClock = 'Clock';
  rsResetToDefau = 'Reset to default';
  rsConnect = 'Connect';
  rsStart = 'Start';
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
  rsInitialized = 'initialized';
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
  rsUnknowPierSi = 'Unknow pier side';
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
  rsCannotStartA = 'Cannot start autofocus now, stop preview and retry';
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
  rsReceiveUnkno = 'Receive unknow message: %s';
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
  rsTiltIndicati = ' Tilt indication:%s%';
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
  rsCameraIsNotC = 'Camera is not connected';
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
  rsConnectAndGe = 'Connect and get devices list';
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
  rsFilterWheelI = 'Filter wheel in camera connection. (Restart required!)';
  rsUseFilterWhe = 'Use filter wheel';
  rsNotImplement = 'Not implemented!';
  rsUseFocuser = 'Use focuser';
  rsFocuserInMou = 'Focuser in mount connection. (Restart required!)';
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
  rsSubstractADr = 'Substract a drak frame';
  rsShowPreviewS = 'Show preview stack option';
  rsClippingIndi = 'Clipping indicator';
  rsShadowADU = 'Shadow ADU';
  rsHighlightADU = 'Highlight ADU';
  rsCCDTemperatu2 = 'CCD temperature';
  rsAutomaticCoo = 'Automatic cooling ';
  rsCoolDownWhen = 'Cool down when camera is connected';
  rsDegree = 'degree';
  rsMaximumTempe = 'Maximum temperature change ';
  rsLimitTempera = 'Limit temperature change';
  rsDegreesPerMi = 'Degrees per minute';
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
  rsTemperatureC = 'Temperature coefficient in steps/C';
  rsAutofocusMet = 'Autofocus method';
  rsSeconds = 'seconds';
  rsMoveDirectio = 'Move direction';
  rsRunVCurveLea = 'Run V curve learning again if you change any option.';
  rsSlippageCorr = 'Slippage correction ';
  rsEstimatedSli = 'Estimated slippage since last learning';
  rsNumberOfDyna = 'Number of dynamic points';
  rsMovementBetw = 'Movement between points';
  rsInitialMovem = 'Initial movement (steps)';
  rsFinalMovemen = 'Final movement (steps)';
  rsNearFocus = 'Near focus';
  rsNExp = 'n.exp';
  rsBinning = 'Binning';
  rsAutofocusTol = 'Autofocus tolerance ';
  rsMinSNR = 'min.SNR';
  rsAutofocusSta6 = 'Autofocus star list';
  rsBeforeAutoma = 'Before automatic focus slew to a near star of magnitude';
  rsSlewWithAPre = 'Slew with a precision of ';
  rsArcmin = '[arcmin]';
  rsYouCanUseThe = 'You can use the menu "File / Focuser calibration" to help '
    +'to fill the values on this page for your specific focuser.';
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
  rsCreatePlotOf = 'Create plot of the result';
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
  rsCalibrateAut = 'Calibrate autoguider after medidian flip';
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
  rsBacklashComp2 = 'Backlash compensation must be in the same direction as '
    +'auto-focus';
  rsInvalidNumbe = 'Invalid number %s';
  rsFocusWindowM = 'Focus window must be at least four time greater than Star '
    +'window!';
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
  rsUseAstrometr = 'Use astrometry to refine the position';
  rsCurrentImage = 'Current image';
  rsUpdateRADecF = 'Update RA+Dec from Planetarium';
  rsStayInPlaceF = 'Stay in place for autofocus';
  rsSetRotatorAn = 'Set rotator angle';
  rsRepeatThePla = 'Repeat the plan for this object';
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
  rsAutofocusBef = 'Autofocus before to start';
  rsAutofocusEve = 'Autofocus every';
  rsAdd = 'Add';
  rsPlanName = 'Plan name';

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
  // translate LCL messages
  TranslateUnitResourceStrings('LCLStrConsts', systoutf8(
    slash(appdir) + slash('data') + slash('language') + 'lclstrconsts.%s.po'), Result, '');
end;

end.
