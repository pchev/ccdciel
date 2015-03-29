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

uses u_global,
  Forms, Classes, SysUtils;

type

  T_autoguider = class(TThread)
  protected
    FTargetHost,FTargetPort,FErrorDesc,FRecvData : string;
    FVersion,FMsgVersion,FStatus : String;
    FSettlePix,FSettleTmin,FSettleTmax: string;
    FState: TAutoguiderState;
    FAutoguiderType: TAutoguiderType;
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
    function WaitBusy(maxwait:integer=5):boolean; virtual; abstract;
    property AutoguiderType: TAutoguiderType read FAutoguiderType;
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

