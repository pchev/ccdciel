unit cu_planetarium;

{$mode objfpc}{$H+}

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

interface

uses u_global,
  Forms, Classes, SysUtils;

type

TPlanetarium = class(TThread)
protected
  FTargetHost,FTargetPort,FErrorDesc,FRecvData,FClientId,FClientName : string;
  FTimeout : integer;
  FCmdTimeout : double;
  FStatus: boolean;
  FPlanetariumType: TPlanetariumType;
  FonShowMessage: TNotifyMsg;
  FonReceiveData: TNotifyMsg;
  FonConnect: TNotifyEvent;
  FonDisconnect: TNotifyEvent;
  procedure SetCmdTimeout(value:double);
  function GetCmdTimeout:double;
  procedure DisplayMessagesyn;
  procedure ProcessDataSyn;
  procedure DisplayMessage(msg:string);
  procedure ProcessData(line:string);
public
  Constructor Create;
  Procedure Connect(cp1: string; cp2:string=''); virtual; abstract;
  procedure Disconnect; virtual; abstract;
  function Cmd(const Value: string):string; virtual; abstract;
  function ShowImage(fn: string):boolean; virtual; abstract;
  property Terminated;
  property Connected: boolean read FStatus;
  property Timeout : integer read FTimeout write FTimeout;
  property CmdTimeout: double read GetCmdTimeout write SetCmdTimeout;
  property ErrorDesc : string read FErrorDesc;
  property RecvData : string read FRecvData;
  property ClientId : string read FClientId;
  property ClientName : string read FClientName;
  property PlanetariumType: TPlanetariumType read FPlanetariumType;
  property onConnect: TNotifyEvent read FonConnect  write FonConnect;
  property onDisconnect: TNotifyEvent read FonDisconnect  write FonDisconnect;
  property onShowMessage: TNotifyMsg read FonShowMessage write FonShowMessage;
  property onReceiveData: TNotifyMsg read FonReceiveData write FonReceiveData;
end;

implementation

Constructor TPlanetarium.Create ;
begin
inherited create(true);
freeonterminate:=true;
FStatus:=false;
FTimeout:=500;
FCmdTimeout:=10/86400;
FErrorDesc:='';
FRecvData:='';
end;

procedure TPlanetarium.SetCmdTimeout(value:double);
begin
 FCmdTimeout := value / 86400;  // store timeout value in days
end;

function TPlanetarium.GetCmdTimeout:double;
begin
 result := FCmdTimeout * 86400;
end;

procedure TPlanetarium.DisplayMessage(msg:string);
begin
FErrorDesc:=msg;
Synchronize(@DisplayMessageSyn);
end;

procedure TPlanetarium.DisplayMessageSyn;
begin
if assigned(FonShowMessage) then FonShowMessage(FErrorDesc);
end;

procedure TPlanetarium.ProcessData(line:string);
begin
FRecvData:=line;
Synchronize(@ProcessDataSyn);
end;

procedure TPlanetarium.ProcessDataSyn;
begin
if assigned(FonReceiveData) then FonReceiveData(FRecvData);
end;

end.

