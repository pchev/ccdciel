unit cu_autoguider;

{
Copyright (C) 2015 Patrick Chevalley

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

{$mode objfpc}{$H+}

interface

uses u_global, LCLIntf,
  Forms, Classes, SysUtils, ExtCtrls;

type

  T_autoguider = class(TThread)
  protected
    FTargetHost,FTargetPort,FErrorDesc,FRecvData,FLastError : string;
    FVersion,FMsgVersion,FStatus : String;
    FSettlePix,FSettleTmin,FSettleTmax: string;
    FRunning, FRecovering,FDithering: boolean;
    FState: TAutoguiderState;
    FAutoguiderType: TAutoguiderType;
    FTimeout : integer;
    FStarLostTimeoutRestart,FStarLostTimeoutCancel: integer;
    FStarLostTime: double;
    FonShowMessage: TNotifyMsg;
    FonConnect: TNotifyEvent;
    FonConnectError: TNotifyEvent;
    FonDisconnect: TNotifyEvent;
    FonStatusChange: TNotifyEvent;
    StarLostTimer: TTimer;
    procedure StarLostTimerTimer(Sender: TObject);
    procedure StatusChange;
    procedure ProcessDisconnectSyn;
    procedure ProcessDisconnect;
    procedure DisplayMessage(msg:string);
    procedure ProcessEvent(txt:string); virtual; abstract;
    procedure StarLostTimerTimer(Sender: TObject); virtual; abstract;
  public
    Constructor Create;
    Destructor Destroy; override;
    Procedure Connect(cp1: string; cp2:string=''); virtual; abstract;
    procedure Disconnect; virtual; abstract;
    procedure Shutdown; virtual; abstract;
    procedure ConnectGear; virtual; abstract;
    procedure SettleTolerance(pixel:double; mintime,maxtime: integer); virtual; abstract;
    procedure Calibrate; virtual; abstract;
    procedure Guide(onoff:boolean; recalibrate:boolean=false); virtual; abstract;
    procedure Pause(onoff:boolean); virtual; abstract;
    procedure Dither(pixel:double; raonly:boolean); virtual; abstract;
    function WaitBusy(maxwait:integer=5):boolean; virtual; abstract;
    function WaitGuiding(maxwait:integer=5):boolean; virtual; abstract;
    property AutoguiderType: TAutoguiderType read FAutoguiderType;
    property Terminated;
    property Running: boolean read FRunning;
    property Recovering: boolean read FRecovering;
    property TargetHost : string read FTargetHost;
    property TargetPort : string read FTargetPort;
    property Timeout : integer read FTimeout write FTimeout;
    property LastError : string read FLastError;
    property ErrorDesc : string read FErrorDesc write FErrorDesc;
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
FRunning:=false;
FRecovering:=false;
FDithering:=false;
FStatus:='Disconnected';
FState:=GUIDER_DISCONNECTED;
FTimeout:=500;
FLastError:='';
FErrorDesc:='';
FRecvData:='';
FSettlePix:='1.0';
FSettleTmin:='5';
FSettleTmax:='60';
StarLostTimer:=TTimer.Create(nil);
StarLostTimer.Enabled:=false;
StarLostTimer.Interval:=10000;
StarLostTimer.OnTimer:=@StarLostTimerTimer;
FStarLostTimeoutRestart:=0;
FStarLostTimeoutCancel:=1800;
FStarLostTime:=0;
end;

Destructor T_autoguider.Destroy;
begin
  StarLostTimer.Free;
  inherited Destroy;
end;

procedure T_autoguider.DisplayMessage(msg:string);
begin
if FErrorDesc='' then
  FErrorDesc:=msg
else
  FErrorDesc:=FErrorDesc+crlf+msg;
PostMessage(MsgHandle, LM_CCDCIEL, M_AutoguiderMessage, 0);
end;

procedure T_autoguider.StatusChange;
begin
 PostMessage(MsgHandle, LM_CCDCIEL, M_AutoguiderStatusChange, 0);
end;

procedure T_autoguider.ProcessDisconnect;
begin
Synchronize(@ProcessDisconnectSyn);
end;

procedure T_autoguider.ProcessDisconnectSyn;
begin
 if assigned(FonDisconnect) then FonDisconnect(self);
end;


end.

