unit pu_scriptengine;

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

{
 Script engine with specific function for CCDciel
}

interface

uses  u_global, u_utils, cu_fits, indiapi, cu_planetarium, fu_ccdtemp, fu_devicesconnection, pu_pause,
  fu_capture, fu_preview, fu_mount, cu_wheel, cu_mount, cu_camera, cu_focuser, cu_autoguider, cu_astrometry,
  fu_cover, cu_cover,
  Classes, SysUtils, FileUtil, uPSComponent, uPSComponent_Default, LazFileUtils,
  uPSComponent_Forms, uPSComponent_Controls, uPSComponent_StdCtrls, Forms, process,
  u_translation, Controls, Graphics, Dialogs, ExtCtrls;

type

  TNotifyOutput = procedure(msg:TStringList; r: integer) of object;

  TPythonThread = class(TThread)
  protected
    procedure Execute; override;
    procedure ShowOutput;
  public
    PyProcess: TProcess;
    pycmd, pyscript, pypath, args: string;
    debug: boolean;
    host,port: string;
    output: TStringList;
    rc: integer;
    FRunning: boolean;
    FOnShowOutput: TNotifyOutput;
    constructor Create;
  end;

  { Tf_scriptengine }

  Tf_scriptengine = class(TForm)
    PSImport_Classes1: TPSImport_Classes;
    PSImport_Controls1: TPSImport_Controls;
    PSImport_DateUtils1: TPSImport_DateUtils;
    PSImport_Forms1: TPSImport_Forms;
    PSImport_StdCtrls1: TPSImport_StdCtrls;
    ShutdownTimer: TTimer;
    TplPSScript: TPSScript;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ShutdownTimerTimer(Sender: TObject);
    procedure TplPSScriptCompile(Sender: TPSScript);
    procedure TplPSScriptExecute(Sender: TPSScript);
    procedure TplPSScriptAfterExecute(Sender: TPSScript);
    procedure TplPSScriptLine(Sender: TObject);
  private
    { private declarations }
    FScriptFilename: string;
    Ffits : TFits;
    Fdevicesconnection:Tf_devicesconnection;
    Fcapture: Tf_capture;
    Fpreview: Tf_preview;
    Fccdtemp: Tf_ccdtemp;
    f_mount: Tf_mount;
    f_cover: Tf_cover;
    Ffilter: T_wheel;
    Fmount: T_mount;
    Fcamera: T_camera;
    FFocuser: T_focuser;
    Fautoguider: T_autoguider;
    Fastrometry: TAstrometry;
    Fplanetarium: TPlanetarium;
    FonMsg: TNotifyMsg;
    FonStartSequence: TNotifyStr;
    FonScriptExecute: TNotifyEvent;
    FonScriptAfterExecute: TNotifyEvent;
    FonSaveFitsFile: TNotifyStr;
    FonOpenFitsFile: TNotifyStr;
    FonOpenReferenceImage: TNotifyStr;
    FonClearReferenceImage: TNotifyEvent;
    FonSlewImageCenter: TNotifyEvent;
    FonPlotDSO,FonPlotHyperleda: TNotifyEvent;
    FonAutofocus: TNotifyBool;
    FonAutomaticAutofocus: TNotifyBool;
    ilist: array of Integer;
    dlist: array of Double;
    slist: array of String;
    LastErr:string;
    strllist: array of TStringList;
    Waitrunning, cancelWait, ScriptCancel: boolean;
    FParamStr: TStringList;

    RunProcess: TProcess;
    procedure msg(str:string);
    function doGetS(varname:string; var str: string):Boolean;
    function doSetS(varname:string; str: string):Boolean;
    function doGetSL(varname:string; var strl: TStringList):Boolean;
    function doSetSL(varname:string; strl: TStringList):Boolean;
    function doSaveSL(fn:string; strl: TStringList):Boolean;
    function doGetI(varname:string; var i: Integer):Boolean;
    function doSetI(varname:string; i: Integer):Boolean;
    function doGetD(varname:string; var x: Double):Boolean;
    function doSetD(varname:string; x: Double):Boolean;
    function doGetB(varname:string; var x: Boolean):Boolean;
    function doOpenFile(fn:string):boolean;
    function doRun(cmdline:string):boolean;
    Function ExecPr(cmd: string; output: TStringList; ShowConsole:boolean=false): integer;
    function doRunWait(cmdline:string):boolean;
    function doRunOutput(cmdline:string; var output:TStringlist):boolean;
    Function doJDtoStr(var jd: Double) : string;
    Function doStrtoJD(dt:string; var jdt: Double) : boolean;
    Function doARtoStr(var ar: Double) : string;
    Function doDEtoStr(var de: Double) : string;
    Function doStrtoAR(str:string; var ar: Double) : boolean;
    Function doStrtoDE(str:string; var de: Double) : boolean;
    Procedure doEq2Hz(var ra,de,a,h : double);
    Procedure doHz2Eq(var a,h,ra,de : double);
    function doFormatFloat(Const Format : String; var Value : double) : String;
    function doFormat(Const Fmt : String; const Args : Array of const) : String;
    Procedure doStrtoFloatD(str:string; var defval: Double; var val: Double);
    function doStringReplace(str,s1,s2: String): string;
    function doIsNumber(str: String): boolean;
    function doMsgBox(const aMsg: string):boolean;
    Procedure doShowMessage(const aMsg: string);
    procedure doLogmsg(str:string);
    procedure doWait(wt:integer);
    function doWaitTill(hour:string; showdialog: boolean):boolean;
    function doDeleteFile(FileName: String): Boolean;
    function doRenameFile(OldName, NewName: String): Boolean;
    function doCreateDir(NewDir: String): Boolean;
    function Cmd(cname:string):string;
    function CmdArg(cname:string; var arg:Tstringlist):string;
    function CompileScripts: boolean;
    function doParamstr: Tstringlist;
    function doParamCount: Integer;
  public
    { public declarations }
    dbgscr: TPSScriptDebugger;
    scr: TPSScript;
    PythonScr: TPythonThread;
    PythonResult: integer;
    PythonOutput: TStringList;
    function cmd_DevicesConnection(onoff:string):string;
    function cmd_MountPark(onoff:string):string;
    function cmd_MountTrack:string;
    function cmd_MountSlew(RA,DE:string):string;
    function cmd_MountSlewAsync(RA,DE:string):string;
    function cmd_MountSync(RA,DE:string):string;
    function cmd_MountAbortMotion:string;
    function cmd_EqmodClearPoints:string;
    function cmd_EqmodClearSyncDelta:string;
    function cmd_EqmodStdSync:string;
    function cmd_EqmodAppendSync:string;
    function cmd_AutoguiderConnect:string;
    function cmd_AutoguiderCalibrate:string;
    function cmd_AutoguiderStartGuiding:string;
    function cmd_AutoguiderStopGuiding:string;
    function cmd_AutoguiderPause:string;
    function cmd_AutoguiderUnPause:string;
    function cmd_AutoguiderDither:string;
    function cmd_AutoguiderShutdown:string;
    function cmd_Wheel_GetFilter:string;
    function cmd_Wheel_SetFilter(num:string):string;
    function cmd_Wheel_GetFiltersName(var fl:TStringList):string;
    function cmd_Wheel_SetFiltersName(fl:TStringList):string;
    function cmd_Focuser_SetPosition(p:string):string;
    function cmd_Ccd_SetTemperature(t:string):string;
    function cmd_Preview_SetExposure(exp:string):string;
    function cmd_Preview_SetBinning(bin:string):string;
    function cmd_Preview_Single:string;
    function cmd_Preview_Loop:string;
    function cmd_Preview_WaitLoop:string;
    function cmd_Preview_Stop:string;
    function cmd_Capture_SetExposure(exp:string):string;
    function cmd_Capture_SetBinning(bin:string):string;
    function cmd_Capture_SetObjectName(obj:string):string;
    function cmd_Capture_SetCount(num:string):string;
    function cmd_Capture_SetFrameType(typ:string):string;
    function cmd_Capture_SetDither(num:string):string;
    function cmd_Capture_Start:string;
    function cmd_Capture_Stop:string;
    function cmd_AstrometrySolve:string;
    function cmd_AstrometrySync:string;
    function cmd_AstrometrySlewImageCenter:string;
    function cmd_AstrometryPlotDSO:string;
    function cmd_AstrometryPlotHyperleda:string;
    function cmd_PlanetariumConnect:string;
    function cmd_PlanetariumShowImage(fov:string=''):string;
    function cmd_PlanetariumShutdown:string;
    function cmd_ProgramShutdown:string;
    function cmd_SequenceStart(seq:string):string;
    function cmd_SaveFitsFile(fn:string):string;
    function cmd_OpenFitsFile(fn:string):string;
    function cmd_OpenReferenceImage(fn:string):string;
    function cmd_ClearReferenceImage:string;
    function cmd_AutoFocus:string;
    function cmd_AutomaticAutoFocus:string;
    function cmd_ListFiles(var lf:TStringList):string;
    function cmd_coverstatus: string;
    function cmd_coveropen: string;
    function cmd_coverclose: string;
    function cmd_calibratorstatus: string;
    function cmd_getcalibratorbrightness: integer;
    function cmd_calibratorlighton(value:string): string;
    function cmd_calibratorlightoff: string;
    function cmd_customheader_add(key,value:string): string;
    function cmd_customheader_del(key:string): string;
    function cmd_customheader_clear: string;
    function cmd_customheader(key:string): string;
    function ScriptType(fn: string): TScriptType;
    function  RunScript(sname,path,args: string):boolean;
    function ScriptRunning: boolean;
    function RunPython(pycmd, pyscript, pypath, args: string; debug:boolean=false): boolean;
    procedure StopPython;
    function PythonRunning: boolean;
    procedure ShowPythonOutput(output: TStringList; exitcode: integer);
    procedure StopScript;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
    property onStartSequence: TNotifyStr read FonStartSequence write FonStartSequence;
    property onScriptExecute: TNotifyEvent read FonScriptExecute write FonScriptExecute;
    property onScriptAfterExecute: TNotifyEvent read FonScriptAfterExecute write FonScriptAfterExecute;
    property onSaveFitsFile: TNotifyStr read FonSaveFitsFile  write FonSaveFitsFile;
    property onOpenFitsFile: TNotifyStr read FonOpenFitsFile  write FonOpenFitsFile;
    property onOpenReferenceImage: TNotifyStr read FonOpenReferenceImage write FonOpenReferenceImage;
    property onClearReferenceImage: TNotifyEvent read FonClearReferenceImage write FonClearReferenceImage;
    property onSlewImageCenter: TNotifyEvent read FonSlewImageCenter write FonSlewImageCenter;
    property onPlotDSO: TNotifyEvent read FonPlotDSO write FonPlotDSO;
    property onPlotHyperleda: TNotifyEvent read FonPlotHyperleda write FonPlotHyperleda;
    property onAutofocus: TNotifyBool read FonAutofocus write FonAutofocus;
    property onAutomaticAutofocus: TNotifyBool read FonAutomaticAutofocus write FonAutomaticAutofocus;
    property fits: TFits read Ffits write Ffits;
    property DevicesConnection: Tf_devicesconnection read Fdevicesconnection write Fdevicesconnection;
    property Ccdtemp: Tf_ccdtemp read Fccdtemp write Fccdtemp;
    property Preview: Tf_preview read Fpreview write Fpreview;
    property Capture: Tf_capture read Fcapture write Fcapture;
    property Fomount: Tf_mount read f_mount write f_mount;
    property Mount: T_mount read Fmount write Fmount;
    property Cover: Tf_cover read f_cover write f_cover;
    property Camera: T_camera read Fcamera write Fcamera;
    property Focuser: T_focuser read FFocuser write FFocuser;
    property Filter: T_wheel read Ffilter write Ffilter;
    property Autoguider: T_autoguider read Fautoguider write Fautoguider;
    property Astrometry: TAstrometry read Fastrometry write Fastrometry;
    property Planetarium: TPlanetarium read Fplanetarium write Fplanetarium;
    property ScriptFilename: string read FScriptFilename;
  end;

var
  f_scriptengine: Tf_scriptengine;

implementation

{$R *.lfm}

{ Tf_scriptengine }

procedure Tf_scriptengine.FormCreate(Sender: TObject);
var i: integer;
begin
  PythonOutput:=TStringList.Create;
  SetLength(ilist,10);
  SetLength(dlist,10);
  SetLength(slist,10);
  SetLength(strllist,10);
  for i:=0 to 9 do strllist[i]:=TStringList.Create;
  Waitrunning:=false;
  cancelWait:=false;
  scr:=TPSScriptDebugger.Create(self);
  scr.OnCompile:=@TplPSScriptCompile;
  scr.OnExecute:=@TplPSScriptExecute;
  scr.OnAfterExecute:=@TplPSScriptAfterExecute;
  scr.OnLine:=@TplPSScriptLine;
  scr.Plugins.Assign(TplPSScript.Plugins);
  dbgscr:=TPSScriptDebugger.Create(self);
  dbgscr.OnCompile:=@TplPSScriptCompile;
  dbgscr.OnLine:=@TplPSScriptLine;
  dbgscr.Plugins.Assign(TplPSScript.Plugins);
end;

procedure Tf_scriptengine.FormDestroy(Sender: TObject);
var i: integer;
begin
  PythonOutput.Free;
  scr.Free;
  dbgscr.Free;
  SetLength(ilist,0);
  SetLength(dlist,0);
  SetLength(slist,0);
  for i:=0 to 9 do strllist[i].Free;
  SetLength(strllist,0);
end;

procedure Tf_scriptengine.TplPSScriptLine(Sender: TObject);
begin
  if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
end;

   function Tf_scriptengine.doGetS(varname:string; var str: string):Boolean;
begin
  result:=true;
  varname:=uppercase(varname);
  if varname='LASTERROR' then str:=LastErr
  else if varname='DIRECTORYSEPARATOR' then str:=DirectorySeparator
  else if varname='APPDIR' then str:=Appdir
  else if varname='TMPDIR' then str:=TmpDir
  else if varname='CAPTUREDIR' then str:=config.GetValue('/Files/CapturePath',defCapturePath)
  else if varname='LIGHTDIR' then str:=Fcapture.FrameType.Items[ord(LIGHT)]
  else if varname='BIASDIR' then str:=Fcapture.FrameType.Items[ord(BIAS)]
  else if varname='DARKDIR' then str:=Fcapture.FrameType.Items[ord(DARK)]
  else if varname='FLATDIR' then str:=Fcapture.FrameType.Items[ord(FLAT)]
  else if varname='HOSTOS' then str:=hostOS
  else if varname='COVERSTATUS' then str:=cmd_coverstatus
  else if varname='CALIBRATORSTATUS' then str:=cmd_calibratorstatus
  else if varname='TELESCOPE_PIERSIDE' then str:=PierSideName[ord(mount.PierSide)]
  else if varname='STR1' then str:=slist[0]
  else if varname='STR2' then str:=slist[1]
  else if varname='STR3' then str:=slist[2]
  else if varname='STR4' then str:=slist[3]
  else if varname='STR5' then str:=slist[4]
  else if varname='STR6' then str:=slist[5]
  else if varname='STR7' then str:=slist[6]
  else if varname='STR8' then str:=slist[7]
  else if varname='STR9' then str:=slist[8]
  else if varname='STR10' then str:=slist[9]
  else result:=false;
end;

function Tf_scriptengine.doSetS(varname:string; str: string):Boolean;
begin
  result:=true;
  varname:=uppercase(varname);
  if varname='STR1' then slist[0]:=str
  else if varname='STR2' then slist[1]:=str
  else if varname='STR3' then slist[2]:=str
  else if varname='STR4' then slist[3]:=str
  else if varname='STR5' then slist[4]:=str
  else if varname='STR6' then slist[5]:=str
  else if varname='STR7' then slist[6]:=str
  else if varname='STR8' then slist[7]:=str
  else if varname='STR9' then slist[8]:=str
  else if varname='STR10' then slist[9]:=str
  else result:=false;
end;

function Tf_scriptengine.doGetSL(varname:string; var strl: TStringList):Boolean;
begin
result:=true;
varname:=uppercase(varname);
if varname='STRL1' then strl:=strllist[0]
else if varname='STRL2' then strl:=strllist[1]
else if varname='STRL3' then strl:=strllist[2]
else if varname='STRL4' then strl:=strllist[3]
else if varname='STRL5' then strl:=strllist[4]
else if varname='STRL6' then strl:=strllist[5]
else if varname='STRL7' then strl:=strllist[6]
else if varname='STRL8' then strl:=strllist[7]
else if varname='STRL9' then strl:=strllist[8]
else if varname='STRL10' then strl:=strllist[9]
else result:=false;
end;

function Tf_scriptengine.doSetSL(varname:string; strl: TStringList):Boolean;
begin
result:=true;
varname:=uppercase(varname);
if varname='STRL1' then strllist[0]:=strl
else if varname='STRL2' then strllist[1]:=strl
else if varname='STRL3' then strllist[2]:=strl
else if varname='STRL4' then strllist[3]:=strl
else if varname='STRL5' then strllist[4]:=strl
else if varname='STRL6' then strllist[5]:=strl
else if varname='STRL7' then strllist[6]:=strl
else if varname='STRL8' then strllist[7]:=strl
else if varname='STRL9' then strllist[8]:=strl
else if varname='STRL10' then strllist[9]:=strl
else result:=false;
end;

function Tf_scriptengine.doSaveSL(fn:string; strl: TStringList):Boolean;
var f: textfile;
    i: integer;
begin
// save a stringlist with Unix line ending
 try
 AssignFile(f,fn);
 SetTextLineEnding(f,lf);
 Rewrite(f);
 for i:=0 to strl.Count-1 do begin
   WriteLn(f,strl[i]);
 end;
 CloseFile(f);
 result:=true;
 except
   result:=false;
 end;
end;

function  Tf_scriptengine.doGetD(varname:string; var x: double):boolean;
begin
  result:=true;
  varname:=uppercase(varname);
  if varname='TELESCOPERA' then x:=f_mount.CurrentRA
  else if varname='TELESCOPEDE' then x:=f_mount.CurrentDec
  else if varname='CCDTEMP' then x:=TempDisplay(TemperatureScale,Fccdtemp.CurrentTemperature)
  else if varname='TIMENOW' then x:=now
  else if varname='OBS_LATITUDE' then x:=ObsLatitude
  else if varname='OBS_LONGITUDE' then x:=-ObsLongitude
  else if varname='OBS_ELEVATION' then x:=ObsElevation
  else if varname='DOUBLE1' then x:=dlist[0]
  else if varname='DOUBLE2' then x:=dlist[1]
  else if varname='DOUBLE3' then x:=dlist[2]
  else if varname='DOUBLE4' then x:=dlist[3]
  else if varname='DOUBLE5' then x:=dlist[4]
  else if varname='DOUBLE6' then x:=dlist[5]
  else if varname='DOUBLE7' then x:=dlist[6]
  else if varname='DOUBLE8' then x:=dlist[7]
  else if varname='DOUBLE9' then x:=dlist[8]
  else if varname='DOUBLE10' then x:=dlist[9]
  else result:=false;
end;

function Tf_scriptengine.doSetD(varname:string; x: Double):Boolean;
begin
  result:=true;
  varname:=uppercase(varname);
  if varname='DOUBLE1' then dlist[0]:=x
  else if varname='DOUBLE2' then dlist[1]:=x
  else if varname='DOUBLE3' then dlist[2]:=x
  else if varname='DOUBLE4' then dlist[3]:=x
  else if varname='DOUBLE5' then dlist[4]:=x
  else if varname='DOUBLE6' then dlist[5]:=x
  else if varname='DOUBLE7' then dlist[6]:=x
  else if varname='DOUBLE8' then dlist[7]:=x
  else if varname='DOUBLE9' then dlist[8]:=x
  else if varname='DOUBLE10' then dlist[9]:=x
  else result:=false;
end;

function  Tf_scriptengine.doGetI(varname:string; var i: Integer):Boolean;
begin
  result:=true;
  varname:=uppercase(varname);
  if varname='FOCUSERPOSITION' then i:=FFocuser.Position
  else if varname='CALIBRATORBRIGHTNESS' then i:=cmd_getcalibratorbrightness
  else if varname='INT1' then i:=ilist[0]
  else if varname='INT2' then i:=ilist[1]
  else if varname='INT3' then i:=ilist[2]
  else if varname='INT4' then i:=ilist[3]
  else if varname='INT5' then i:=ilist[4]
  else if varname='INT6' then i:=ilist[5]
  else if varname='INT7' then i:=ilist[6]
  else if varname='INT8' then i:=ilist[7]
  else if varname='INT9' then i:=ilist[8]
  else if varname='INT10' then i:=ilist[9]
  else result:=false;
end;

function Tf_scriptengine.doSetI(varname:string; i: Integer):Boolean;
begin
  result:=true;
  varname:=uppercase(varname);
  if varname='INT1' then ilist[0]:=i
  else if varname='INT2' then ilist[1]:=i
  else if varname='INT3' then ilist[2]:=i
  else if varname='INT4' then ilist[3]:=i
  else if varname='INT5' then ilist[4]:=i
  else if varname='INT6' then ilist[5]:=i
  else if varname='INT7' then ilist[6]:=i
  else if varname='INT8' then ilist[7]:=i
  else if varname='INT9' then ilist[8]:=i
  else if varname='INT10' then ilist[9]:=i
  else result:=false;
end;

function Tf_scriptengine.doGetB(varname:string; var x: Boolean):Boolean;
begin
  result:=true;
  varname:=uppercase(varname);
  if varname='TELESCOPE_CONNECTED' then x:=(mount.Status=devConnected)
  else if varname='TELESCOPE_PARKED' then x:=mount.Park
  else if varname='TELESCOPE_TRACKING' then x:=mount.Tracking
  else if varname='TELESCOPE_SLEWING' then x:=mount.MountSlewing
  else if varname='TELESCOPE_EQMOD' then x:=mount.IsEqmod
  else if varname='AUTOGUIDER_CONNECTED' then x:=(Autoguider.State<>GUIDER_DISCONNECTED)
  else if varname='AUTOGUIDER_RUNNING' then x:=Autoguider.Running
  else if varname='AUTOGUIDER_GUIDING' then x:=(Autoguider.State=GUIDER_GUIDING)
  else if varname='WHEEL_CONNECTED' then x:=(Filter.Status=devConnected)
  else if varname='FOCUSER_CONNECTED' then x:=(Focuser.Status=devConnected)
  else if varname='CAMERA_CONNECTED' then x:=(Camera.Status=devConnected)
  else if varname='PLANETARIUM_CONNECTED' then x:=Planetarium.Connected
  else if varname='PREVIEW_RUNNING' then x:=Preview.Running
  else if varname='PREVIEW_LOOP' then x:=Preview.Loop
  else if varname='CAPTURE_RUNNING' then x:=Capture.Running
  else result:=false;
end;

Procedure Tf_scriptengine.doEq2Hz(var ra,de,a,h : double);
begin
 cmdEq2Hz(ra,de,a,h);
end;

Procedure Tf_scriptengine.doHz2Eq(var a,h,ra,de : double);
begin
 cmdHz2Eq(a,h,ra,de);
end;

function Tf_scriptengine.doFormatFloat(Const Format : String; var Value : double) : String;
begin
  result:=FormatFloat(format, Value);
end;

Procedure Tf_scriptengine.doStrtoFloatD(str:string; var defval: Double; var val: Double);
begin
  val:=StrToFloatDef(str,defval);
end;

function Tf_scriptengine.doStringReplace(str,s1,s2: String): string;
begin
  result:=StringReplace(str,s1,s2,[rfReplaceAll]);
end;

function Tf_scriptengine.doFormat(Const Fmt : String; const Args : Array of const) : String;
begin
 result:=Format(Fmt,Args);
end;

function Tf_scriptengine.doIsNumber(str: String): boolean;
begin
  result:=IsNumber(str);
end;

function Tf_scriptengine.doMsgBox(const aMsg: string):boolean;
begin
  result:=MessageDlg(aMsg,mtConfirmation,mbYesNo,0)=mrYes;
end;

Procedure Tf_scriptengine.doShowMessage(const aMsg: string);
begin
  ShowMessage(aMsg);
end;

procedure Tf_scriptengine.msg(str:string);
begin
  doLogmsg(str);
end;

procedure Tf_scriptengine.doLogmsg(str:string);
begin
  if Assigned(FonMsg) then FonMsg(str);
end;

function Tf_scriptengine.doOpenFile(fn:string):boolean;
var i: integer;
begin
  i:=ExecuteFile(fn);
  result:=(i=0);
end;

function Tf_scriptengine.doRun(cmdline:string):boolean;
begin
  ExecNoWait(cmdline,'',true);
  wait(1);
  result:=true;
end;

Function Tf_scriptengine.ExecPr(cmd: string; output: TStringList; ShowConsole:boolean=false): integer;
const READ_BYTES = 2048;
var
  M: TMemoryStream;
  param: TStringList;
  n: LongInt;
  BytesRead: LongInt;
begin
M := TMemoryStream.Create;
RunProcess := TProcess.Create(nil);
param:=TStringList.Create;
result:=1;
try
  BytesRead := 0;
  SplitCmd(cmd,param);
  cmd:= param[0];
  param.Delete(0);
  RunProcess.Executable:=cmd;
  RunProcess.Parameters:=param;
  if ShowConsole then begin
     RunProcess.ShowWindow:=swoShowNormal;
     RunProcess.StartupOptions:=[suoUseShowWindow];
  end else begin
     RunProcess.ShowWindow:=swoHIDE;
  end;
  if output<>nil then RunProcess.Options := [poUsePipes, poStdErrToOutPut];
  RunProcess.Execute;
  while RunProcess.Running do begin
    if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
    if (output<>nil) and (RunProcess.Output<>nil) then begin
      M.SetSize(BytesRead + READ_BYTES);
      n := RunProcess.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
      if n > 0 then inc(BytesRead, n);
    end;
  end;
  result:=RunProcess.ExitStatus;
  if (output<>nil) and (result<>127)and(RunProcess.Output<>nil) then repeat
    M.SetSize(BytesRead + READ_BYTES);
    n := RunProcess.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
    if n > 0
    then begin
      Inc(BytesRead, n);
    end;
  until (n<=0)or(RunProcess.Output=nil);
  if (output<>nil) then begin
    M.SetSize(BytesRead);
    output.LoadFromStream(M);
  end;
  FreeAndNil(RunProcess);
  M.Free;
  param.Free;
except
  on E: Exception do begin
    result:=-1;
    if (output<>nil) then output.add(E.Message);
    FreeAndNil(RunProcess);
    M.Free;
    param.Free;
  end;
end;
end;

function Tf_scriptengine.doRunWait(cmdline:string):boolean;
var i: integer;
begin
  i:=ExecPr(cmdline,nil,false);
  wait(1);
  result:=(i=0);
end;

function Tf_scriptengine.doRunOutput(cmdline:string; var output:TStringlist):boolean;
var i: integer;
begin
  i:=ExecPr(cmdline,output,false);
  wait(1);
  result:=(i=0);
end;

Function Tf_scriptengine.doARtoStr(var ar: Double) : string;
begin
  // script do not work if a float parameter is not var.
  result:=ARtoStr3(ar);
end;

Function Tf_scriptengine.doDEtoStr(var de: Double) : string;
begin
  result:=DEtoStr3(de);
end;

Function Tf_scriptengine.doStrtoAR(str:string; var ar: Double) : boolean;
begin
  if trim(str)<>'' then begin
    ar:=Str3ToAR(str);
    result:=(ar<>0);
  end
  else result:=false;
end;

Function Tf_scriptengine.doStrtoDE(str:string; var de: Double) : boolean;
begin
  if trim(str)<>'' then begin
    str:=StringReplace(str,ldeg,'d',[rfReplaceAll]);
    str:=StringReplace(str,lmin,'m',[rfReplaceAll]);
    str:=StringReplace(str,lsec,'s',[rfReplaceAll]);
    de:=Str3ToDE(str);
    result:=(de<>0);
  end
  else result:=false;
end;

Function Tf_scriptengine.doJDtoStr(var jd: Double) : string;
begin
  result:=jddate(jd);
end;

Function Tf_scriptengine.doStrtoJD(dt:string; var jdt: Double) : boolean;
var sy,y,m,d,p: integer;
    h:double;
begin
result:=false;
sy:=1;
h:=0;
dt:=trim(dt);
if length(dt)>2 then begin
 if dt[1]='-' then begin sy:=-1; delete(dt,1,1); end;
 if dt[1]='+' then begin sy:=1; delete(dt,1,1); end;
end;
p:=pos('-',dt);
if p=0 then exit;
y:=sy*strtoint(trim(copy(dt,1,p-1)));
dt:=copy(dt,p+1,999);
p:=pos('-',dt);
if p=0 then exit;
m:=strtoint(trim(copy(dt,1,p-1)));
dt:=copy(dt,p+1,999);
p:=pos('T',dt);
if p=0 then p:=pos(' ',dt);
if p=0 then d:=strtoint(trim(dt))     // no time part
   else begin
    d:=strtoint(trim(copy(dt,1,p-1)));

   end;
jdt:=jd(y,m,d,h);
end;

procedure Tf_scriptengine.doWait(wt:integer);
var endt: TDateTime;
begin
  endt:=now+wt/secperday;
  try
  Waitrunning:=true;
  while now<endt do begin
    Sleep(100);
    if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
    if cancelWait then begin
      Waitrunning:=false;
      cancelWait:=false;
      exit;
    end;
  end;
  finally
    Waitrunning:=false;
  end;
end;

function Tf_scriptengine.doWaitTill(hour:string; showdialog: boolean):boolean;
begin
 result:=WaitTill(hour,showdialog);
end;

function Tf_scriptengine.doDeleteFile(FileName: String): Boolean;
var lf:TStringList;
    i: integer;
    dir:string;
begin
  result:=false;
  if pos('*',FileName)>1 then begin
    dir:=slash(ExtractFilePath(FileName));
    lf:=TStringList.Create;
    lf.Add(FileName);
    cmd_ListFiles(lf);
    if lf.Count>0 then begin
      result:=true;
      for i:=0 to lf.Count-1 do begin
        result:=result and DeleteFileUTF8(dir+lf[i]);
      end;
    end;
    lf.Free;
  end
  else begin
    result:=DeleteFileUTF8(FileName);
  end;
end;

function Tf_scriptengine.doRenameFile(OldName, NewName: String): Boolean;
begin
  result:=RenameFileUTF8(OldName, NewName);
end;

function Tf_scriptengine.doCreateDir(NewDir: String): Boolean;
begin
  result:=CreateDirUTF8(NewDir);
end;

function Tf_scriptengine.doParamstr: Tstringlist;
begin
  result:=FParamStr;
end;

function Tf_scriptengine.doParamCount: Integer;
begin
  if FParamStr=nil then
    result:=0
  else
    result:=FParamStr.Count;
end;

function Tf_scriptengine.ScriptType(fn: string): TScriptType;
var
 f: textfile;
 buf: string;
 p: integer;
begin
 result:=stUnknown;
 AssignFile(f,fn);
 Reset(f);
 repeat
   ReadLn(f,buf);
   p:=pos(';',buf);
   if p>0 then buf:=copy(buf,1,p-1);
   p:=pos('{',buf);
   if p>0 then buf:=copy(buf,1,p-1);
   p:=pos('//',buf);
   if p>0 then buf:=copy(buf,1,p-1);
   p:=pos('(*',buf);
   if p>0 then buf:=copy(buf,1,p-1);
   if LowerCase(trim(buf))='begin' then begin
     result:=stPascal;
     break;
   end;
   if pos('from ccdciel import ccdciel',buf)>0 then begin
     result:=stPython;
     break;
   end;
 until eof(f);
 CloseFile(f);
end;

function Tf_scriptengine.RunScript(sname,path,args: string):boolean;
var fn: string;
    i: integer;
    ok: boolean;
    st: TScriptType;
begin
 try
  result:=false;
  msg(Format(rsRunScript2, [sname]));
  FScriptFilename:=sname;
  fn:=slash(path)+sname+'.script';
  st:=ScriptType(fn);
  if st=stPython then begin
    ScriptCancel:=false;
    result:=RunPython(PythonCmd, fn, slash(ScriptsDir),args);
    result:=result and (not ScriptCancel);
    for i:=0 to PythonOutput.Count-1 do
       msg(PythonOutput[i]);
    if result then
       msg(Format(rsScriptFinish, [sname]))
    else begin
       msg(Format(rsScriptError,[inttostr(PythonResult)]));
       msg(Format(rsScriptFinish, [sname]));
    end;
  end
  else if st=stPascal then begin
    {$if defined(CPUARM) or defined(CPUAARCH64)}
      msg('Pascal language script are not supported on ARM processor');
      exit;
    {$endif}
    FParamStr:=TStringList.Create;
    FParamStr.Add(fn);
    if args<>'' then begin
      SplitCmdLineParams(args,FParamStr,true);
    end;
    scr.Script.LoadFromFile(fn);
    ok:=scr.Compile;
    ScriptCancel:=false;
    if ok then begin
      if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
      result:=scr.Execute;
      wait(2);
      result:=result and (not ScriptCancel);
      if result then
         msg(Format(rsScriptFinish, [sname]))
      else begin
         msg(Format(rsScriptExecut, [inttostr(scr.ExecErrorRow),
           scr.ExecErrorToString]));
         msg(Format(rsScriptFinish, [sname]));
      end;
    end else begin
      for i:=0 to scr.CompilerMessageCount-1 do begin
         msg(Format(rsCompilationE, [scr.CompilerErrorToStr(i)]));
      end;
      result:=false;
    end;
    FParamStr.Free;
  end
  else begin
    result:=false;
    msg('Unknown script language '+fn);
  end;
 except
   on E: Exception do begin
    msg(Format(rsScriptError, [E.Message]));
   end;
 end;
end;

function Tf_scriptengine.ScriptRunning: boolean;
begin
 result:= (scr.Running or PythonRunning);
end;

Procedure Tf_scriptengine.StopScript;
begin
  if Mount.MountSlewing then Mount.AbortMotion;
  if Astrometry.Busy then Astrometry.StopAstrometry;
  if Capture.Running then begin
     Camera.AbortExposure;
     Capture.Stop;
  end;
  if Preview.Running then begin
     Camera.AbortExposure;
     Preview.Stop;
  end;
  if Autoguider.Running then begin
    msg(rsStopAutoguid);
    Autoguider.Guide(false);
    Autoguider.WaitBusy(15);
  end;
  msg(rsScriptTermin);
  if scr.Running then begin
    ScriptCancel:=true;
    scr.Stop;
  end
  else if PythonRunning then begin
     ScriptCancel:=true;
     StopPython;
  end;
  if Waitrunning then cancelWait:=true;
  if WaitTillrunning then begin
    if f_pause<>nil
     then f_pause.BtnCancel.Click
     else cancelWaitTill:=true;
  end;
  if RunProcess<>nil then RunProcess.Active:=false;
end;

function Tf_scriptengine.CompileScripts: boolean;
begin
 result:=scr.Compile;
end;

procedure Tf_scriptengine.TplPSScriptExecute(Sender: TPSScript);
begin
 if assigned(FonScriptExecute) then FonScriptExecute(self);
end;

procedure Tf_scriptengine.TplPSScriptAfterExecute(Sender: TPSScript);
begin
 if assigned(FonScriptAfterExecute) then FonScriptAfterExecute(self);
end;

procedure Tf_scriptengine.TplPSScriptCompile(Sender: TPSScript);
begin
with Sender as TPSScript do begin
  comp.AddConstantN('deg2rad', 'extended').SetExtended(deg2rad);
  comp.AddConstantN('rad2deg', 'extended').SetExtended(rad2deg);
  comp.AddConstantN('msgOK', 'string').SetString(msgOK);
  comp.AddConstantN('msgFailed', 'string').SetString(msgFailed);
  AddMethod(self, @Tf_scriptengine.Cmd, 'function Cmd(cname:string):string;');
  AddMethod(self, @Tf_scriptengine.CmdArg, 'function CmdArg(cname:string; var arg:Tstringlist):string;');
  AddMethod(self, @Tf_scriptengine.doGetS, 'function GetS(varname:string; var str: string):Boolean;');
  AddMethod(self, @Tf_scriptengine.doSetS, 'function SetS(varname:string; str: string):Boolean;');
  AddMethod(self, @Tf_scriptengine.doSetSL, 'function SetSL(varname:string; strl: TStringList):Boolean;');
  AddMethod(self, @Tf_scriptengine.doGetSL, 'function GetSL(varname:string; var strl: TStringList):Boolean;');
  AddMethod(self, @Tf_scriptengine.doGetI, 'function GetI(varname:string; var i: Integer):Boolean;');
  AddMethod(self, @Tf_scriptengine.doSetI, 'function SetI(varname:string; i: Integer):Boolean;');
  AddMethod(self, @Tf_scriptengine.doGetD, 'function GetD(varname:string; var x: double):boolean;');
  AddMethod(self, @Tf_scriptengine.doSetD, 'function SetD(varname:string; x: Double):Boolean;');
  AddMethod(self, @Tf_scriptengine.doGetB, 'function GetB(varname:string; var x: Boolean):Boolean;');
  AddMethod(self, @Tf_scriptengine.doARtoStr, 'Function ARtoStr(var ar: Double) : string;');
  AddMethod(self, @Tf_scriptengine.doDEtoStr, 'Function DEtoStr(var de: Double) : string;');
  AddMethod(self, @Tf_scriptengine.doStrtoAR, 'Function StrtoAR(str:string; var ar: Double) : boolean;');
  AddMethod(self, @Tf_scriptengine.doStrtoDE, 'Function StrtoDE(str:string; var de: Double) : boolean;');
  AddMethod(self, @Tf_scriptengine.doJDtoStr, 'Function JDtoStr(var jd: Double) : string;');
  AddMethod(self, @Tf_scriptengine.doStrtoJD, 'Function StrtoJD(dt:string; var jdt: Double) : boolean;');
  AddMethod(self, @Tf_scriptengine.doEq2Hz, 'Procedure Eq2Hz(var ra,de,a,h : double);');
  AddMethod(self, @Tf_scriptengine.doHz2Eq, 'Procedure Hz2Eq(var a,h,ra,de : double);');
  AddMethod(self, @Tf_scriptengine.doFormatFloat, 'function FormatFloat(Const Format : String; var Value : double) : String;');
  AddMethod(self, @Tf_scriptengine.doStrtoFloatD, 'Procedure StrtoFloatD(str:string; var defval: Double; var val: Double);');
  AddMethod(self, @Tf_scriptengine.doStringReplace, 'function StringReplace(str,s1,s2: String): string;');
  AddMethod(self, @Tf_scriptengine.doFormat, 'Function Format(Const Fmt : String; const Args : Array of const) : String;');
  AddMethod(self, @Tf_scriptengine.doIsNumber, 'function IsNumber(str: String): boolean;');
  AddMethod(self, @Tf_scriptengine.doMsgBox,'function MsgBox(const aMsg: string):boolean;');
  AddMethod(self, @Tf_scriptengine.doShowMessage,'Procedure ShowMessage(const aMsg: string);');
  AddMethod(self, @Tf_scriptengine.doLogmsg,'procedure Logmsg(str:string);');
  AddMethod(self, @Tf_scriptengine.doOpenFile,'function OpenFile(fn:string):boolean;');
  AddMethod(self, @Tf_scriptengine.doRun,'function Run(cmdline:string):boolean;');
  AddMethod(self, @Tf_scriptengine.doRunWait,'function RunWait(cmdline:string):boolean;');
  AddMethod(self, @Tf_scriptengine.doRunOutput,'function RunOutput(cmdline:string; var output:TStringlist):boolean;');
  AddMethod(self, @Tf_scriptengine.doWait,'procedure Wait(wt:integer);');
  AddMethod(self, @Tf_scriptengine.doWaitTill,'function WaitTill(hour:string; showdialog: boolean):boolean;');
  AddMethod(self, @Tf_scriptengine.doDeleteFile,'function DeleteFile(FileName: String): Boolean;');
  AddMethod(self, @Tf_scriptengine.doRenameFile,'function RenameFile(OldName, NewName: String): Boolean;');
  AddMethod(self, @Tf_scriptengine.doCreateDir,'function CreateDir(NewDir: String): Boolean;');
  AddMethod(self, @Tf_scriptengine.doSaveSL,'function SaveSL(fn:string; strl: TStringList):Boolean;');
  AddMethod(self, @Tf_scriptengine.doParamstr,'function Paramstr: Tstringlist;');
  AddMethod(self, @Tf_scriptengine.doParamCount,'function ParamCount: Integer;');
end;
end;

function Tf_scriptengine.Cmd(cname:string):string;
begin
cname:=uppercase(cname);
result:=msgFailed;
if cname='TELESCOPE_ABORTMOTION' then result:=cmd_MountAbortMotion
else if cname='TELESCOPE_TRACK' then result:=cmd_MountTrack
else if cname='EQMOD_CLEARPOINTS' then result:= cmd_EqmodClearPoints
else if cname='EQMOD_CLEARSYNCDELTA' then result:= cmd_EqmodClearSyncDelta
else if cname='EQMOD_STDSYNC' then result:= cmd_EqmodStdSync
else if cname='EQMOD_APPENDSYNC' then result:= cmd_EqmodAppendSync
else if cname='AUTOGUIDER_CONNECT' then result:=cmd_AutoguiderConnect
else if cname='AUTOGUIDER_CALIBRATE' then result:=cmd_AutoguiderCalibrate
else if cname='AUTOGUIDER_STARTGUIDING' then result:=cmd_AutoguiderStartGuiding
else if cname='AUTOGUIDER_STOPGUIDING' then result:=cmd_AutoguiderStopGuiding
else if cname='AUTOGUIDER_PAUSE' then result:=cmd_AutoguiderPause
else if cname='AUTOGUIDER_UNPAUSE' then result:=cmd_AutoguiderUnPause
else if cname='AUTOGUIDER_DITHER' then result:=cmd_AutoguiderDither
else if cname='AUTOGUIDER_SHUTDOWN' then result:=cmd_AutoguiderShutdown
else if cname='WHEEL_GETFILTER' then result:=cmd_Wheel_GetFilter
else if cname='PREVIEW_SINGLE' then result:=cmd_Preview_Single
else if cname='PREVIEW_LOOP' then result:=cmd_Preview_Loop
else if cname='PREVIEW_WAITLOOP' then result:=cmd_Preview_WaitLoop
else if cname='PREVIEW_STOP' then result:=cmd_Preview_Stop
else if cname='CAPTURE_START' then result:=cmd_Capture_Start
else if cname='CAPTURE_STOP' then result:=cmd_Capture_Stop
else if cname='ASTROMETRY_SOLVE' then result:=cmd_AstrometrySolve
else if cname='ASTROMETRY_SYNC' then result:=cmd_AstrometrySync
else if cname='ASTROMETRY_SLEW_IMAGE_CENTER' then result:=cmd_AstrometrySlewImageCenter
else if cname='ASTROMETRY_PLOT_DSO' then result:=cmd_AstrometryPlotDSO
else if cname='ASTROMETRY_PLOT_HYPERLEDA' then result:=cmd_AstrometryPlotHyperleda
else if cname='PLANETARIUM_CONNECT' then result:=cmd_PlanetariumConnect
else if cname='PLANETARIUM_SHOWIMAGE' then result:=cmd_PlanetariumShowImage
else if cname='PLANETARIUM_SHUTDOWN' then result:=cmd_PlanetariumShutdown
else if cname='PROGRAM_SHUTDOWN' then result:=cmd_ProgramShutdown
else if cname='CLEAR_REFERENCE_IMAGE' then result:=cmd_ClearReferenceImage
else if cname='AUTOFOCUS' then result:=cmd_AutoFocus
else if cname='AUTOMATICAUTOFOCUS' then result:=cmd_AutomaticAutoFocus
else if cname='COVER_OPEN' then result:=cmd_coveropen
else if cname='COVER_CLOSE' then result:=cmd_coverclose
else if cname='CALIBRATOR_LIGHT_OFF' then result:=cmd_calibratorlightoff
else if cname='CUSTOMHEADER_CLEAR' then result:=cmd_customheader_clear
;
LastErr:='cmd('+cname+'): '+result;
end;

function Tf_scriptengine.CmdArg(cname:string; var arg:Tstringlist):string;
var i: integer;
begin
cname:=uppercase(cname);
for i:=arg.count to MaxCmdArg do arg.add('');
result:=msgFailed;
if cname='DEVICES_CONNECTION' then result:=cmd_DevicesConnection(arg[0])
else if cname='TELESCOPE_SLEW' then result:=cmd_MountSlew(arg[0],arg[1])
else if cname='TELESCOPE_SLEWASYNC' then result:=cmd_MountSlewAsync(arg[0],arg[1])
else if cname='TELESCOPE_SYNC' then result:=cmd_MountSync(arg[0],arg[1])
else if cname='TELESCOPE_PARK' then result:=cmd_MountPark(arg[0])
else if cname='WHEEL_SETFILTER' then result:=cmd_Wheel_SetFilter(arg[0])
else if cname='WHEEL_GETFILTERSNAME' then result:=cmd_Wheel_GetFiltersName(arg)
else if cname='WHEEL_SETFILTERSNAME' then result:=cmd_Wheel_SetFiltersName(arg)
else if cname='FOCUSER_SETPOSITION' then result:=cmd_Focuser_SetPosition(arg[0])
else if cname='CCD_SETTEMPERATURE' then result:=cmd_Ccd_SetTemperature(arg[0])
else if cname='PREVIEW_SETEXPOSURE' then result:=cmd_Preview_SetExposure(arg[0])
else if cname='PREVIEW_SETBINNING' then result:=cmd_Preview_SetBinning(arg[0])
else if cname='CAPTURE_SETEXPOSURE' then result:=cmd_Capture_SetExposure(arg[0])
else if cname='CAPTURE_SETBINNING' then result:=cmd_Capture_SetBinning(arg[0])
else if cname='CAPTURE_SETOBJECTNAME' then result:=cmd_Capture_SetObjectName(arg[0])
else if cname='CAPTURE_SETCOUNT' then result:=cmd_Capture_SetCount(arg[0])
else if cname='CAPTURE_SETFRAMETYPE' then result:=cmd_Capture_SetFrameType(arg[0])
else if cname='CAPTURE_SETDITHER' then result:=cmd_Capture_SetDither(arg[0])
else if cname='SEQUENCE_START' then result:=cmd_SequenceStart(arg[0])
else if cname='SAVE_FITS_FILE' then result:=cmd_SaveFitsFile(arg[0])
else if cname='OPEN_FITS_FILE' then result:=cmd_OpenFitsFile(arg[0])
else if cname='OPEN_REFERENCE_IMAGE' then result:=cmd_OpenReferenceImage(arg[0])
else if cname='LIST_FILES' then result:=cmd_ListFiles(arg)
else if cname='PLANETARIUM_SHOWIMAGE_FOV' then result:=cmd_PlanetariumShowImage(arg[0])
else if cname='CALIBRATOR_LIGHT_ON' then result:=cmd_calibratorlighton(arg[0])
else if cname='CUSTOMHEADER' then result:=cmd_customheader(arg[0])
else if cname='CUSTOMHEADER_ADD' then result:=cmd_customheader_add(arg[0],arg[1])
else if cname='CUSTOMHEADER_DEL' then result:=cmd_customheader_del(arg[0])
;
LastErr:='cmdarg('+cname+'): '+result;
end;

function Tf_scriptengine.cmd_DevicesConnection(onoff:string):string;
var connect: boolean;
begin
try
result:=msgFailed;
connect:=(onoff='ON');
if connect and (Fdevicesconnection.BtnConnect.Caption=rsConnect) then begin
 Fdevicesconnection.Connect;
 wait(1);
end;
if (not connect) and (Fdevicesconnection.BtnConnect.Caption=rsDisconnect) then begin
 Fdevicesconnection.Disconnect(false);
 wait(1);
end;
result:=msgOK;
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_MountPark(onoff:string):string;
var park: boolean;
begin
try
result:=msgFailed;
park:=(onoff='ON');
Fmount.park:=park;
wait(2);
result:=msgOK;
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_MountTrack:string;
begin
try
result:=msgFailed;
Fmount.Track;
wait(2);
result:=msgOK;
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_MountSlew(RA,DE:string):string;
var r,d: double;
begin
try
result:=msgFailed;
r:=StrToFloatDef(RA,9999);
d:=StrToFloatDef(DE,9999);
if (abs(r)<=24)and(abs(d)<=90) then begin
 if Fmount.Slew(r,d) then begin
   wait(2);
   result:=msgOK;
 end;
end
else result:=Format(rsOutOfRange, [msgFailed]);
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_MountSlewAsync(RA,DE:string):string;
var r,d: double;
begin
try
result:=msgFailed;
r:=StrToFloatDef(RA,9999);
d:=StrToFloatDef(DE,9999);
if (abs(r)<=24)and(abs(d)<=90) then begin
 if Fmount.SlewAsync(r,d) then begin
   wait(1);
   result:=msgOK;
 end;
end
else result:=Format(rsOutOfRange, [msgFailed]);
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_MountSync(RA,DE:string):string;
var r,d: double;
begin
try
result:=msgFailed;
r:=StrToFloatDef(RA,9999);
d:=StrToFloatDef(DE,9999);
if (abs(r)<=360)and(abs(d)<=90) then begin
 if Fmount.Sync(r,d) then begin
   wait(2);
   result:=msgOK;
 end;
end
else result:=Format(rsOutOfRange, [msgFailed]);
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_MountAbortMotion:string;
begin
try
Fmount.AbortMotion;
wait(2);
result:=msgOK;
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_EqmodClearPoints:string;
begin
try
  result:=msgFailed;
  if mount.IsEqmod then begin
     if mount.ClearAlignment then
       result:=msgOK;
  end
  else result:=Format(rsNotAnEqmodMo, [msgFailed]);
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_EqmodClearSyncDelta:string;
begin
try
  result:=msgFailed;
  if mount.IsEqmod then begin
     if mount.ClearDelta then
       result:=msgOK;
  end
  else result:=Format(rsNotAnEqmodMo, [msgFailed]);
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_EqmodStdSync:string;
begin
try
  result:=msgFailed;
  if mount.IsEqmod then begin
     mount.SyncMode:=alSTDSYNC;
     result:=msgOK;
  end
  else result:=Format(rsNotAnEqmodMo, [msgFailed]);
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_EqmodAppendSync:string;
begin
try
  result:=msgFailed;
  if mount.IsEqmod then begin
     mount.SyncMode:=alADDPOINT;
     result:=msgOK;
  end
  else result:=Format(rsNotAnEqmodMo, [msgFailed]);
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_AutoguiderConnect:string;
begin
try
 result:=msgFailed;
 if (Autoguider=nil)or(Autoguider.AutoguiderType<>agPHD) then exit;
 autoguider.Connect(config.GetValue('/Autoguider/PHDhostname','localhost'),
                    config.GetValue('/Autoguider/PHDport','4400'),
                    config.GetValue('/Autoguider/PHDpath',''),
                    config.GetValue('/Autoguider/PHDstart',false));
 result:=msgOK;
 wait(2);
except
   result:=msgFailed;
 end;
end;

function Tf_scriptengine.cmd_AutoguiderCalibrate:string;
begin
try
 result:=msgFailed;
 if Autoguider=nil then exit;
 Autoguider.Calibrate;
 if Autoguider.WaitBusy(CalibrationDelay+SettleMaxTime) then result:=msgOK;
 wait(2);
except
   result:=msgFailed;
 end;
end;

function Tf_scriptengine.cmd_AutoguiderStartGuiding:string;
begin
 try
 result:=msgFailed;
 if Autoguider=nil then exit;
 Autoguider.Guide(true);
 if Autoguider.WaitGuiding(CalibrationDelay+SettleMaxTime) then result:=msgOK;
 wait(2);
except
   result:=msgFailed;
 end;
end;

function Tf_scriptengine.cmd_AutoguiderStopGuiding:string;
begin
 try
 result:=msgFailed;
 if Autoguider=nil then exit;
 Autoguider.Guide(false);
 if Autoguider.WaitBusy(SettleMaxTime) then result:=msgOK;
 wait(2);
 except
   result:=msgFailed;
 end;
end;

function Tf_scriptengine.cmd_AutoguiderPause:string;
begin
 try
 result:=msgFailed;
 if Autoguider=nil then exit;
 Autoguider.Pause(true);
 wait(2);
 result:=msgOK;
 except
   result:=msgFailed;
 end;
end;

function Tf_scriptengine.cmd_AutoguiderUnPause:string;
begin
 try
 result:=msgFailed;
 if Autoguider=nil then exit;
 Autoguider.Pause(false);
 wait(2);
 result:=msgOK;
 except
   result:=msgFailed;
 end;
end;

function Tf_scriptengine.cmd_AutoguiderDither:string;
begin
 try
 result:=msgFailed;
 if Autoguider=nil then exit;
 Autoguider.Dither(DitherPixel, DitherRAonly, DitherWaitTime);
 if Autoguider.WaitDithering(SettleMaxTime) then result:=msgOK;
 wait(2);
 except
   result:=msgFailed;
 end;
end;

function Tf_scriptengine.cmd_AutoguiderShutdown:string;
begin
 result:=msgOK;
 if Autoguider=nil then exit;
 Autoguider.Shutdown;
end;

function Tf_scriptengine.cmd_Wheel_GetFilter:string;
var i:integer;
begin
try
i:=Ffilter.Filter;
result:=IntToStr(i);
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_Wheel_SetFilter(num:string):string;
var i,n:integer;
begin
try
result:=msgFailed;
val(num,i,n);
if n<>0 then exit;
Ffilter.Filter:=i;
if Ffilter.Filter=i then result:=msgOK;
wait(2);
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_Wheel_GetFiltersName(var fl:TStringList):string;
begin
try
result:=msgFailed;
fl.Clear;
if Ffilter.Status=devConnected then
  fl.Assign(Ffilter.FilterNames)
else begin
  fl.Assign(FilterList);
end;
fl.Delete(0); // remove no_change
result:=msgOK;
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_Wheel_SetFiltersName(fl:TStringList):string;
begin
try
result:=msgFailed;
Ffilter.FilterNames.Assign(fl);
result:=msgOK;
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_Focuser_SetPosition(p:string):string;
begin
try
result:=msgFailed;
FFocuser.Position:=StrToInt(p);
result:=msgOK;
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_Ccd_SetTemperature(t:string):string;
var n:integer;
    tt:double;
begin
try
result:=msgFailed;
val(t,tt,n);
if n<>0 then exit;
Camera.Temperature:=TempCelsius(TemperatureScale,tt);
result:=msgOK;
Ccdtemp.Setpoint.Value:=tt;
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_Preview_SetExposure(exp:string):string;
begin
try
result:=msgFailed;
Preview.ExpTime.Text:=exp;
result:=msgOK;
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_Preview_SetBinning(bin:string):string;
begin
try
result:=msgFailed;
Preview.Binning.Text:=bin;
result:=msgOK;
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_Preview_Single:string;
begin
try
result:=msgFailed;
if Preview.Running or Preview.Loop then exit;
Preview.Running:=true;
if Assigned(Preview.onStartExposure) then
  Preview.onStartExposure(Self)
 else
  exit;
if Preview.Running then begin
  if Assigned(FonMsg) then FonMsg(rsStartSingleP);
  while Preview.Running do begin
    sleep(10);
    if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
  end;
  wait(1);
  result:=msgOK;
end;
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_Preview_Loop:string;
begin
try
result:=msgFailed;
if Preview.Running then exit;
Preview.Loop:=True;
Preview.Running:=true;
if Assigned(Preview.onStartExposure) then
  Preview.onStartExposure(Self)
 else
  exit;
if Preview.Running then begin
 Preview.BtnLoop.Font.Color:=clGreen;
 Preview.BtnLoop.Caption:=rsStopLoop;
 if Assigned(FonMsg) then FonMsg(rsStartPreview);
end;
wait(1);
result:=msgOK;
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_Preview_WaitLoop:string;
begin
try
result:=msgFailed;
while Preview.Running and Preview.Loop do begin
  Sleep(100);
  if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
end;
result:=msgOK;
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_Preview_Stop:string;
begin
try
result:=msgFailed;
if not Preview.Running then exit;
Preview.Stop;
if Assigned(Preview.onAbortExposure) then
  Preview.onAbortExposure(Self);
wait(1);
result:=msgOK;
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_Capture_SetExposure(exp:string):string;
begin
try
result:=msgFailed;
Capture.ExposureTime:=StrToFloatDef(exp,Capture.ExposureTime);
result:=msgOK;
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_Capture_SetBinning(bin:string):string;
begin
try
result:=msgFailed;
Capture.Binning.Text:=bin;
result:=msgOK;
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_Capture_SetObjectName(obj:string):string;
begin
try
result:=msgFailed;
Capture.Fname.Text:=obj;
result:=msgOK;
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_Capture_SetCount(num:string):string;
begin
try
result:=msgFailed;
Capture.SeqNum.Value:=StrToInt(num);
result:=msgOK;
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_Capture_SetFrameType(typ:string):string;
begin
try
result:=msgFailed;
Capture.FrameType.ItemIndex:=ord(Str2Frametype(typ));
result:=msgOK;
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_Capture_SetDither(num:string):string;
var i,n: integer;
begin
try
result:=msgFailed;
val(num,i,n);
if n<>0 then exit;
Capture.CheckBoxDither.Checked:=(i>0);
if i>0 then Capture.DitherCount.Value:=i else Capture.DitherCount.value:=1;
result:=msgOK;
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_Capture_Start:string;
begin
try
result:=msgFailed;
if Capture.Running then exit;
Capture.BtnStart.Click;
wait(1);
result:=msgOK;
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_Capture_Stop:string;
begin
try
result:=msgFailed;
if not Capture.Running then exit;
Capture.BtnStart.Click;
wait(1);
result:=msgOK;
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_AstrometrySolve:string;
begin
try
result:=msgFailed;
if Astrometry.Busy then exit;
Astrometry.SolveCurrentImage(true);
if Astrometry.LastResult then begin
 wait(1);
 result:=msgOK;
end;
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_AstrometrySync:string;
begin
try
result:=msgFailed;
if Astrometry.Busy then exit;
Astrometry.SyncCurrentImage(true);
if Astrometry.LastResult then begin
 wait(1);
 result:=msgOK;
end;
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_AstrometrySlewImageCenter:string;
begin
  if Assigned(FonSlewImageCenter) then FonSlewImageCenter(self);
  result:=msgOK;
end;

function Tf_scriptengine.cmd_AstrometryPlotDSO:string;
begin
  if Assigned(FonPlotDSO) then FonPlotDSO(self);
  result:=msgOK;
end;

function Tf_scriptengine.cmd_AstrometryPlotHyperleda:string;
begin
  if Assigned(FonPlotHyperleda) then FonPlotHyperleda(self);
  result:=msgOK;
end;

function Tf_scriptengine.cmd_PlanetariumConnect:string;
var i: integer;
begin
try
result:=msgFailed;
i:=config.GetValue('/Planetarium/Software',ord(plaNONE));
case TPlanetariumType(i) of
  CDC:  planetarium.Connect(config.GetValue('/Planetarium/CdChostname','localhost'),
                   config.GetValue('/Planetarium/CdCport',''),
                   config.GetValue('/Planetarium/CdCpath',''),
                   config.GetValue('/Planetarium/CdCstart',false));
  SAMP: planetarium.Connect('','',config.GetValue('/Planetarium/SAMPpath',''),
                   config.GetValue('/Planetarium/SAMPstart',false));
  HNSKY: planetarium.Connect('','',config.GetValue('/Planetarium/HNSKYpath',''),
                   config.GetValue('/Planetarium/HNSKYstart',false));
  plaNONE: exit;
end;
wait(1);
result:=msgOK;
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_PlanetariumShowImage(fov:string=''):string;
var fovdeg: double;
begin
try
result:=msgFailed;
Astrometry.SolveCurrentImage(true);
if astrometry.LastResult and planetarium.Connected then begin
  fovdeg:=StrToFloatDef(trim(fov),0);
  if planetarium.ShowImage(slash(TmpDir)+'ccdcielsolved.fits',fovdeg) then
     result:=msgOK;
end;
wait(1);
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_PlanetariumShutdown:string;
begin
try
Planetarium.Shutdown;
result:=msgOK;
except
  result:=msgFailed;
end;
end;

function Tf_scriptengine.cmd_SequenceStart(seq:string):string;
begin
  if Assigned(FonStartSequence) then FonStartSequence(seq);
  result:=msgOK;
end;

function Tf_scriptengine.cmd_SaveFitsFile(fn:string):string;
begin
  if Assigned(FonSaveFitsFile) then FonSaveFitsFile(fn);
  result:=msgOK;
end;

function Tf_scriptengine.cmd_OpenFitsFile(fn:string):string;
begin
  if Assigned(FonOpenFitsFile) then FonOpenFitsFile(fn);
  result:=msgOK;
end;

function Tf_scriptengine.cmd_OpenReferenceImage(fn:string):string;
begin
  if Assigned(FonOpenReferenceImage) then FonOpenReferenceImage(fn);
  result:=msgOK;
end;

function Tf_scriptengine.cmd_ClearReferenceImage:string;
begin
  if Assigned(FonClearReferenceImage) then FonClearReferenceImage(self);
  result:=msgOK;
end;

function Tf_scriptengine.cmd_AutoFocus: string;
var ok:boolean;
begin
  result:=msgFailed;
  ok:=false;
  try
  if Assigned(FonAutofocus) then FonAutofocus(ok);
  if ok then result:=msgOK;
  finally
  end;
end;

function Tf_scriptengine.cmd_AutomaticAutoFocus: string;
var ok:boolean;
begin
  result:=msgFailed;
  ok:=false;
  try
  if Assigned(FonAutomaticAutofocus) then FonAutomaticAutofocus(ok);
  if ok then result:=msgOK;
  finally
  end;
end;

function Tf_scriptengine.cmd_ProgramShutdown:string;
begin
  ConfirmClose:=false;
  ShutdownTimer.Enabled:=true;
  result:=msgOK;
end;

procedure Tf_scriptengine.ShutdownTimerTimer(Sender: TObject);
begin
 ShutdownTimer.Enabled:=false;
 Application.MainForm.Close;
end;

function Tf_scriptengine.cmd_ListFiles(var lf:TStringList):string;
var fs : TSearchRec;
    i: integer;
    dir: string;
begin
 dir:=lf[0];
 lf.Clear;
 i:=FindFirstUTF8(dir,0,fs);
 while i=0 do begin
   lf.Add(fs.Name);
   i:=FindNextUTF8(fs);
 end;
 FindCloseUTF8(fs);
 if lf.Count>0 then
   result:=msgOK
 else
  result:=msgFailed;
end;

function Tf_scriptengine.cmd_coverstatus: string;
begin
 result:=CoverLabel[ord(f_cover.Cover)];
end;

function Tf_scriptengine.cmd_coveropen: string;
var timeout: double;
begin
  result:=msgFailed;
  if f_cover.Connected then begin
    timeout:=now+60/secperday;
    f_cover.BtnOpenCoverClick(nil);
    repeat
      wait(1);
      if now>timeout then break;
    until (f_cover.Cover<>covMoving);
    if f_cover.Cover=covOpen then result:=msgOK;
  end;
end;

function Tf_scriptengine.cmd_coverclose: string;
var timeout: double;
begin
  result:=msgFailed;
  if f_cover.Connected then begin
    timeout:=now+60/secperday;
    f_cover.BtnCloseCoverClick(nil);
    repeat
      wait(1);
      if now>timeout then break;
    until (f_cover.Cover<>covMoving);
    if f_cover.Cover=covClosed then result:=msgOK;
  end;
end;

function Tf_scriptengine.cmd_calibratorstatus: string;
begin
  result:=CalibratorLabel[ord(f_cover.Calibrator)]
end;

function Tf_scriptengine.cmd_calibratorlighton(value:string): string;
var timeout: double;
    i,n: integer;
begin
  result:=msgFailed;
  if f_cover.Connected then begin
    timeout:=now+60/secperday;
    val(value,i,n);
    if n<>0 then exit;
    try
    f_cover.lock:=true;
    f_cover.Brightness.Value:=i;
    f_cover.Light.Checked:=true;
    finally
    f_cover.lock:=false;
    end;
    f_cover.LightClick(nil);
    repeat
      wait(1);
      if now>timeout then break;
    until (f_cover.Calibrator<>calNotReady);
    if f_cover.Calibrator=calReady then result:=msgOK;
  end;
end;

function Tf_scriptengine.cmd_calibratorlightoff: string;
var timeout: double;
begin
  result:=msgFailed;
  if f_cover.Connected then begin
    timeout:=now+60/secperday;
    try
    f_cover.lock:=true;
    f_cover.Light.Checked:=false;
    finally
    f_cover.lock:=false;
    end;
    f_cover.LightClick(nil);
    repeat
      wait(1);
      if now>timeout then break;
    until (f_cover.Calibrator<>calNotReady);
    if f_cover.Calibrator=calOff then result:=msgOK;
  end;
end;

function Tf_scriptengine.cmd_getcalibratorbrightness: integer;
begin
  result:=f_cover.Brightness.Value;
end;

function Tf_scriptengine.cmd_customheader_add(key,value:string): string;
begin
 if CustomHeaderNum<MaxCustomHeaders then begin
   key:=uppercase(copy(trim(key),1,8));
   if (key<>'COMMENT')and(key<>'HISTORY')and(key<>'HIERARCH') then cmd_customheader_del(key); // no duplicate
   inc(CustomHeaderNum);
   CustomHeaders[CustomHeaderNum].key:=key;
   CustomHeaders[CustomHeaderNum].value:=copy(trim(value),1,68);
   result:=msgOK;
 end
 else begin
   result:=msgFailed;
 end;
end;

function Tf_scriptengine.cmd_customheader_del(key:string): string;
var i,j: integer;
    found: boolean;
begin
  key:=uppercase(copy(trim(key),1,8));
  repeat
    found:=false;
    for i:=1 to CustomHeaderNum do begin
      if key=CustomHeaders[i].key then begin
        for j:=i+1 to CustomHeaderNum do begin
          CustomHeaders[j-1].key:=CustomHeaders[j].key;
          CustomHeaders[j-1].value:=CustomHeaders[j].value;
        end;
        dec(CustomHeaderNum);
        found:=true;
        break;
      end;
    end;
  until found=false;
  result:=msgOK; // OK also if not found
end;

function Tf_scriptengine.cmd_customheader_clear: string;
begin
  CustomHeaderNum:=0;
  result:=msgOK;
end;

function Tf_scriptengine.cmd_customheader(key:string): string;
var i: integer;
begin
  result:='';
  key:=uppercase(copy(trim(key),1,8));
  for i:=1 to CustomHeaderNum do begin
    if key=CustomHeaders[i].key then begin
      result:=CustomHeaders[i].value;
      break;
    end;
  end;
end;

///// Python scripts ///////

function Tf_scriptengine.RunPython(pycmd, pyscript, pypath, args: string; debug:boolean=false): boolean;
begin
result:=false;
try
  PythonResult:=-1;
  PythonScr:=TPythonThread.Create;
  PythonScr.FOnShowOutput:=@ShowPythonOutput;
  PythonScr.pycmd:=pycmd;
  PythonScr.pyscript:=pyscript;
  PythonScr.pypath:=pypath;
  PythonScr.args:=args;
  PythonScr.debug:=debug;
  PythonScr.Start;
  if assigned(FonScriptExecute) then FonScriptExecute(self);
  repeat
    wait(1);
  until not PythonRunning;
  result:=(PythonResult=0);
  FreeAndNil(PythonScr);
except
  FreeAndNil(PythonScr);
end;
end;

function Tf_scriptengine.PythonRunning: boolean;
begin
try
  result:=(PythonScr<>nil) and (PythonScr.FRunning);
except
  result:=false;
end;
end;

procedure Tf_scriptengine.StopPython;
begin
  if PythonRunning then PythonScr.PyProcess.Terminate(1);
end;

procedure Tf_scriptengine.ShowPythonOutput(output: TStringList; exitcode: integer);
begin
PythonResult:=exitcode;
PythonOutput.Clear;
if output<>nil then
  PythonOutput.Assign(output);
if assigned(FonScriptAfterExecute) then FonScriptAfterExecute(self);
end;

constructor TPythonThread.Create;
begin
inherited Create(true);
FreeOnTerminate := false;
FRunning:=false;
end;

procedure TPythonThread.Execute;
const READ_BYTES = 2048;
var
  M: TMemoryStream;
  param,scparam: TStringList;
  n: LongInt;
  i,r: integer;
  BytesRead: LongInt;
  {$ifdef mswindows}
  fs : TSearchRec;
  {$endif}
begin
FRunning:=true;
M := TMemoryStream.Create;
PyProcess := TProcess.Create(nil);
param:=TStringList.Create;
scparam:=TStringList.Create;
output:=TStringList.Create;
rc:=1;
try
  if args<>'' then begin
    SplitCmdLineParams(args,scparam,true);
  end;
  BytesRead := 0;
  if debug then begin
     param.Add('-m');
     param.Add('pdb');
     param.Add(pyscript);
     for i:=0 to scparam.Count-1 do param.Add(scparam[i]);
     PyProcess.ShowWindow:=swoShowNormal;
     PyProcess.Options := [poNewConsole];
     PyProcess.StartupOptions:=[suoUseShowWindow];
     {$ifdef mswindows}
     // with embeddable python pdb loss the path to zip module
     if pycmd=defPython then begin
       i:=FindFirst(slash(ScriptsDir)+slash('python')+'python*.zip',0,fs);
       if i=0 then
         pypath:=pypath+';'+slash(ScriptsDir)+slash('python')+fs.name;
       FindClose(fs);
     end;
     {$endif}
  end
  else begin
     param.Add(pyscript);
     for i:=0 to scparam.Count-1 do param.Add(scparam[i]);
     PyProcess.ShowWindow:=swoHIDE;
     if output<>nil then PyProcess.Options := [poUsePipes, poStdErrToOutPut];
  end;
  PyProcess.Executable:=pycmd;
  PyProcess.Parameters:=param;
  PyProcess.Environment.Clear;
  for i:=0 to GetEnvironmentVariableCount-1 do
    PyProcess.Environment.Add(GetEnvironmentString(i));
  PyProcess.Environment.Add('PYTHONPATH='+pypath);
  PyProcess.Environment.Add('CCDCIEL_HOST=localhost');
  PyProcess.Environment.Add('CCDCIEL_PORT='+TCPIPServerPort);
  PyProcess.Execute;
  while PyProcess.Running do begin
    if (output<>nil) and (PyProcess.Output<>nil) then begin
      M.SetSize(BytesRead + READ_BYTES);
      n := PyProcess.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
      if n > 0 then inc(BytesRead, n);
    end;
  end;
  r:=PyProcess.ExitStatus;
  rc:=PyProcess.ExitCode;
  if (output<>nil) and (r<>127)and(PyProcess.Output<>nil) then repeat
    M.SetSize(BytesRead + READ_BYTES);
    n := PyProcess.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
    if n > 0
    then begin
      Inc(BytesRead, n);
    end;
  until (n<=0)or(PyProcess.Output=nil);
  if (output<>nil) then begin
    M.SetSize(BytesRead);
    output.LoadFromStream(M);
  end;
  if Assigned(FOnShowOutput) then Synchronize(@ShowOutput);
  FreeAndNil(PyProcess);
  M.Free;
  param.Free;
  scparam.Free;
  output.Free;
  FRunning:=false;
except
  on E: Exception do begin
    rc:=-1;
    if (output<>nil) then output.add(E.Message);
    if Assigned(FOnShowOutput) then Synchronize(@ShowOutput);
    FreeAndNil(PyProcess);
    M.Free;
    param.Free;
    scparam.Free;
    output.Free;
    FRunning:=false;
  end;
end;
end;

procedure TPythonThread.ShowOutput;
begin
  if Assigned(FOnShowOutput) then FOnShowOutput(output,rc);
end;

end.

