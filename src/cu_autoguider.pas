unit cu_autoguider;

{$mode objfpc}{$H+}

interface

uses u_global,
  Forms, Classes, SysUtils;

type

  T_autoguider = class(TThread)
  protected
    FTargetHost,FTargetPort,FErrorDesc,FRecvData : string;
    FVersion,FMsgVersion,FStatus : String;
    FSettlePix,FSettleTmin,FSettleTmax: string;
    FState: TAutoguiderState;
    FTimeout : integer;
    FonShowMessage: TNotifyMsg;
    FonConnect: TNotifyEvent;
    FonConnectError: TNotifyEvent;
    FonDisconnect: TNotifyEvent;
    FonStatusChange: TNotifyEvent;
    procedure DisplayMessagesyn;
    procedure ProcessDataSyn;
    procedure DisplayMessage(msg:string);
    procedure ProcessData(line:string);
    Procedure ProcessEvent(txt:string); virtual; abstract;
  public
    Constructor Create;
    Procedure Connect(cp1: string; cp2:string=''); virtual; abstract;
    procedure Disconnect; virtual; abstract;
    procedure ConnectGear; virtual; abstract;
    procedure SettleTolerance(pixel:double; mintime,maxtime: integer); virtual; abstract;
    procedure Calibrate; virtual; abstract;
    procedure Guide(onoff:boolean; recalibrate:boolean=false); virtual; abstract;
    procedure Pause(onoff:boolean); virtual; abstract;
    procedure Dither(pixel:double; raonly:boolean); virtual; abstract;
    property Terminated;
    property TargetHost : string read FTargetHost;
    property TargetPort : string read FTargetPort;
    property Timeout : integer read FTimeout write FTimeout;
    property ErrorDesc : string read FErrorDesc;
    property Status : string read FStatus;
    property State : TAutoguiderState read FState;
    property onConnect: TNotifyEvent read FonConnect  write FonConnect;
    property onConnectError: TNotifyEvent read FonConnectError  write FonConnectError;
    property onDisconnect: TNotifyEvent read FonDisconnect  write FonDisconnect;
    property onShowMessage: TNotifyMsg read FonShowMessage write FonShowMessage;
    property onStatusChange: TNotifyEvent read FonStatusChange write FonStatusChange;
  end;

implementation

Constructor T_autoguider.Create ;
begin
// start suspended to let time to the main thread to set the parameters
inherited create(true);
freeonterminate:=true;
FStatus:='Disconnected';
FState:=GUIDER_DISCONNECTED;
FTimeout:=500;
FErrorDesc:='';
FRecvData:='';
FSettlePix:='1.0';
FSettleTmin:='5';
FSettleTmax:='30';
end;

procedure T_autoguider.DisplayMessage(msg:string);
begin
FErrorDesc:=msg;
Synchronize(@DisplayMessageSyn);
end;

procedure T_autoguider.DisplayMessageSyn;
begin
if assigned(FonShowMessage) then FonShowMessage(FErrorDesc);
end;

procedure T_autoguider.ProcessData(line:string);
begin
FRecvData:=line;
Synchronize(@ProcessDataSyn);
end;

procedure T_autoguider.ProcessDataSyn;
begin
 ProcessEvent(FRecvData);
end;

end.

