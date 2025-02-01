unit cu_ascomswitch;

{$mode objfpc}{$H+}

{
Copyright (C) 2021 Patrick Chevalley

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

uses cu_switch, u_global,
    {$ifdef mswindows}
    u_translation, indiapi, Variants, comobj, math,
    {$endif}
    Forms, ExtCtrls,Classes, SysUtils;

type
T_ascomswitch = class(T_switch)
 private
   {$ifdef mswindows}
   V: variant;
   {$endif}
   FInterfaceVersion: integer;
   StatusTimer: TTimer;
   procedure StatusTimerTimer(sender: TObject);
   function  Connected: boolean;
   function WaitConnecting(maxtime:integer):boolean;
 protected
   function GetSwitch: TSwitchList; override;
   procedure SetSwitch(value: TSwitchList); override;
   procedure SetTimeout(num:integer); override;
public
   constructor Create(AOwner: TComponent);override;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');  override;
   procedure Disconnect; override;
end;

const statusinterval=2000;
      waitpoll=500;

implementation

constructor T_ascomswitch.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FSwitchInterface:=ASCOM;
 FInterfaceVersion:=1;
 StatusTimer:=TTimer.Create(nil);
 StatusTimer.Enabled:=false;
 StatusTimer.Interval:=statusinterval;
 StatusTimer.OnTimer:=@StatusTimerTimer;
end;

destructor  T_ascomswitch.Destroy;
begin
 StatusTimer.Free;
 inherited Destroy;
end;

procedure T_ascomswitch.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');
var i: Int16;
begin
 {$ifdef mswindows}
  try
  FStatus := devConnecting;
  Fdevice:=cp1;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  V:=Unassigned;
  V:=CreateOleObject(Fdevice);
  try
  FInterfaceVersion:=V.InterfaceVersion;
  except
    FInterfaceVersion:=1;
  end;
  msg('Interface version: '+inttostr(FInterfaceVersion),9);
  if FInterfaceVersion>=3 then begin
    V.Connect;
    WaitConnecting(30000);
  end
  else
    V.Connected:=true;
  if Connected then begin
     try
     msg(V.DriverInfo,9);
     except
     end;
     try
     msg('Driver version: '+V.DriverVersion,9);
     except
       msg('Error: unknown driver version',9);
     end;
     try
     FNumSwitch:=V.MaxSwitch;
     SetLength(FSwitch,FNumSwitch);
     except
       FNumSwitch:=0;
     end;
     for i:=0 to FNumSwitch-1 do begin
       try
       FSwitch[i].Name:=V.GetSwitchName(i);
       except
         FSwitch[i].Name:='Switch-'+IntToStr(i+1);
       end;
       if FInterfaceVersion=1 then begin
         FSwitch[i].CanWrite:=True;
         FSwitch[i].MultiState:=false;
         FSwitch[i].Min:=0;
         FSwitch[i].Max:=0;
         FSwitch[i].Step:=0;
         FSwitch[i].Value:=0;
       end
       else begin
         try
         FSwitch[i].CanWrite:=V.CanWrite(i);
         except
           FSwitch[i].CanWrite:=False;
         end;
         try
         FSwitch[i].Min:=V.MinSwitchValue(i);
         except
           FSwitch[i].Min:=0;
         end;
         try
         FSwitch[i].Max:=V.MaxSwitchValue(i);
         except
           FSwitch[i].Max:=1;
         end;
         try
         FSwitch[i].Step:=V.SwitchStep(i);
         except
           FSwitch[i].Step:=1;
         end;
         if FSwitch[i].Step=0 then FSwitch[i].Step:=FSwitch[i].Max;
         if FSwitch[i].Step=0 then FSwitch[i].Step:=1;
         FSwitch[i].MultiState:=((FSwitch[i].Max-FSwitch[i].Min)/FSwitch[i].Step)>1;
         FSwitch[i].IndiGroup:=-1;
       end;
     end;
     msg(rsConnected3);
     FStatus := devConnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
     StatusTimer.Enabled:=true;
  end
  else
     Disconnect;
  except
    on E: Exception do msg('Connection error: ' + E.Message,0);
  end;
 {$endif}
end;

procedure T_ascomswitch.Disconnect;
begin
 {$ifdef mswindows}
   StatusTimer.Enabled:=false;
   try
   if not VarIsEmpty(V) then begin
     if FInterfaceVersion>=3 then begin
       V.Disconnect;
       WaitConnecting(30000);
     end
     else
       V.Connected:=false;
     V:=Unassigned;
   end;
   except
     on E: Exception do msg('Disconnection error: ' + E.Message,0);
   end;
   FStatus := devDisconnected;
   if Assigned(FonStatusChange) then FonStatusChange(self);
   msg(rsDisconnected3,1);
 {$endif}
end;

function T_ascomswitch.Connected: boolean;
begin
result:=false;
{$ifdef mswindows}
if not VarIsEmpty(V) then begin
  try
  result:=V.connected;
  except
   result:=false;
  end;
end;
{$endif}
end;

function T_ascomswitch.WaitConnecting(maxtime:integer):boolean;
{$ifdef mswindows}
var count,maxcount:integer;
{$endif}
begin
 result:=true;
 {$ifdef mswindows}
 try
   maxcount:=maxtime div waitpoll;
   count:=0;
   while (V.Connecting)and(count<maxcount) do begin
      sleep(waitpoll);
      if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
      inc(count);
   end;
   result:=(count<maxcount);
   if debug_msg then msg('finish to wait for connecting '+BoolToStr(result,true),9);
 except
  on E: Exception do begin
    msg(Format(rsConnectionEr, [E.Message]),0);
    result:=false;
  end;
 end;
 {$endif}
end;

procedure T_ascomswitch.StatusTimerTimer(sender: TObject);
{$ifdef mswindows}
var s: TSwitchList;
    i: integer;
    changed:boolean;
{$endif}
begin
 {$ifdef mswindows}
 try
 StatusTimer.Enabled:=false;
  if not Connected then begin
     FStatus := devDisconnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
  end
  else begin
    try
      s:=GetSwitch;
      changed:=false;
      for i:=0 to FNumSwitch-1 do begin
         changed:=changed or (s[i].Checked<>FSwitch[i].Checked) or (s[i].Value<>FSwitch[i].Value);
         FSwitch[i].Value:=s[i].Value;
         FSwitch[i].Checked:=s[i].Checked;
      end;
      if changed and Assigned(FonSwitchChange) then FonSwitchChange(self);
     except
     on E: Exception do msg('Status error: ' + E.Message,0);
    end;
  end;
  finally
  if FStatus=devConnected then StatusTimer.Enabled:=true;
  end;
 {$endif}
end;

function  T_ascomswitch.GetSwitch: TSwitchList;
var i: Int16;
begin
 SetLength(result,FNumSwitch);
 {$ifdef mswindows}
 if not VarIsEmpty(V) then begin
 try
   for i:=0 to FNumSwitch-1 do begin
     result[i].Name       := FSwitch[i].Name;
     result[i].CanWrite   := FSwitch[i].CanWrite;
     result[i].MultiState := FSwitch[i].MultiState;
     result[i].Min        := FSwitch[i].Min;
     result[i].Max        := FSwitch[i].Max;
     result[i].Step       := FSwitch[i].Step;
     result[i].IndiGroup  := -1;
     if FSwitch[i].MultiState then begin
       result[i].Value    := V.GetSwitchValue(i);
       result[i].Checked  := (result[i].Value = result[i].Max);
     end
     else begin
       result[i].Value    := FSwitch[i].Value;
       result[i].Checked  := V.GetSwitch(i);
     end;
   end;
   except
    on E: Exception do msg('GetSwitch error: ' + E.Message,0);
   end;
 end;
 {$endif}
end;

procedure T_ascomswitch.SetSwitch(value: TSwitchList);
var i: Int16;
begin
 {$ifdef mswindows}
 if (not VarIsEmpty(V)) and (FNumSwitch>0)then begin
 try
   for i:=0 to FNumSwitch-1 do begin
     if value[i].CanWrite and ((value[i].Value<>FSwitch[i].Value)or(value[i].Checked<>FSwitch[i].Checked)) then begin
       if FSwitch[i].MultiState then begin
         V.SetSwitchValue(i,value[i].Value);
       end
       else begin
         V.SetSwitch(i,value[i].Checked);
       end;
     end;
   end;
   except
    on E: Exception do msg('SetSwitch error: ' + E.Message,0);
   end;
 end;
 {$endif}
end;

procedure T_ascomswitch.SetTimeout(num:integer);
begin
 FTimeOut:=num;
end;

initialization
{$ifdef mswindows}
{$if defined(cpui386) or defined(cpux86_64)}
// some Ascom driver raise this exceptions
SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,exOverflow, exUnderflow, exPrecision]);
{$endif}
{$endif}

end.

