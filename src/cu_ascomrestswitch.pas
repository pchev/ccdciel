unit cu_ascomrestswitch;

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

uses cu_switch, cu_ascomrest, fpjson, jsonparser, u_global, u_utils,
    u_translation, indiapi,
    Forms, ExtCtrls,Classes, SysUtils;

type
T_ascomrestswitch = class(T_switch)
 private
   V: TAscomRest;
   FInterfaceVersion: integer;
   StatusTimer: TTimer;
   statusinterval, waitpoll: integer;
   hasDevicestate: boolean;
   procedure StatusTimerTimer(sender: TObject);
   function  GetDevicestate:TAscomDeviceStateList;
   function  GetStateValue(state: TAscomDeviceStateList; n: string):string;
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

implementation

constructor T_ascomrestswitch.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 V:=TAscomRest.Create(self);
 V.ClientId:=3208;
 FSwitchInterface:=ASCOMREST;
 FInterfaceVersion:=1;
 hasDevicestate:=false;
 statusinterval:=2000;
 waitpoll:=500;
 StatusTimer:=TTimer.Create(nil);
 StatusTimer.Enabled:=false;
 StatusTimer.Interval:=statusinterval;
 StatusTimer.OnTimer:=@StatusTimerTimer;
end;

destructor  T_ascomrestswitch.Destroy;
begin
 StatusTimer.Free;
 inherited Destroy;
end;

procedure T_ascomrestswitch.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');
var i: integer;
    J: TAscomResult;
begin
  try
  FStatus := devConnecting;
  V.Host:=cp1;
  V.Port:=cp2;
  V.Protocol:=cp3;
  V.User:=cp5;
  V.Password:=cp6;
  Fdevice:=cp4;
  V.Device:=Fdevice;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  msg('Connecting to Alpaca server '+cp1+':'+cp2,9);
  V.Timeout:=StdTimeout;
  try
  FInterfaceVersion:=V.Get('interfaceversion').AsInt;
  except
    FInterfaceVersion:=1;
  end;
  msg('Interface version: '+inttostr(FInterfaceVersion),9);
  if FInterfaceVersion>=3 then begin
    V.Put('Connect');
    WaitConnecting(30000);
  end
  else
    V.Put('Connected',true);
  if V.Get('connected').AsBool then begin
     try
     msg(V.Get('driverinfo').AsString,9);
     except
     end;
     try
     msg('Driver version: '+V.Get('driverversion').AsString,9);
     except
       msg('Error: unknown driver version',9);
     end;
     try
     FNumSwitch:=V.Get('maxswitch').AsInt;
     SetLength(FSwitch,FNumSwitch);
     except
       FNumSwitch:=0;
     end;
     for i:=0 to FNumSwitch-1 do begin
       try
       FSwitch[i].Name:=V.Get('getswitchname','Id='+IntToStr(i)).AsString;
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
         FSwitch[i].CanWrite:=V.Get('canwrite','Id='+IntToStr(i)).AsBool;
         except
           FSwitch[i].CanWrite:=False;
         end;
         try
         FSwitch[i].Min:=V.Get('minswitchvalue','Id='+IntToStr(i)).AsFloat;
         except
           FSwitch[i].Min:=0;
         end;
         try
         FSwitch[i].Max:=V.Get('maxswitchvalue','Id='+IntToStr(i)).AsFloat;
         except
           FSwitch[i].Max:=1;
         end;
         try
         FSwitch[i].Step:=V.Get('switchstep','Id='+IntToStr(i)).AsFloat;
         except
           FSwitch[i].Step:=1;
         end;
         if FSwitch[i].Step=0 then FSwitch[i].Step:=FSwitch[i].Max;
         if FSwitch[i].Step=0 then FSwitch[i].Step:=1;
         FSwitch[i].MultiState:=((FSwitch[i].Max-FSwitch[i].Min)/FSwitch[i].Step)>1;
         FSwitch[i].IndiGroup:=-1;
       end;
     end;
     if FInterfaceVersion>=3 then begin
       try
         J:=V.Get('devicestate');
         hasDevicestate:=true;
         J.Free;
       except
         hasDevicestate:=false;
       end;
     end;
     if isLocalIP(V.RemoteIP) then begin
       if hasDevicestate then
         statusinterval:=2000
       else
         statusinterval:=5000;
     end
     else begin
       if hasDevicestate then
         statusinterval:=5000
       else
         statusinterval:=10000;
     end;
     msg(rsConnected3);
     FStatus := devConnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
     StatusTimer.Interval:=statusinterval;
     StatusTimer.Enabled:=true;
  end
  else
     Disconnect;
  except
   on E: Exception do begin
      msg(Format(rsConnectionEr, [E.Message]),0);
      FLastError:=E.Message;
      Disconnect;
   end;
  end;
end;

procedure T_ascomrestswitch.Disconnect;
begin
   StatusTimer.Enabled:=false;
   FLastError:='';
   try
   if FInterfaceVersion>=3 then begin
     V.Put('Disconnect');
     WaitConnecting(30000);
   end
   else
     V.Put('Connected',false);
   except
    on E: Exception do msg(Format(rsDisconnectio, [E.Message]),0);
   end;
   FStatus := devDisconnected;
   if Assigned(FonStatusChange) then FonStatusChange(self);
   msg(rsDisconnected3,1);
end;

function T_ascomrestswitch.Connected: boolean;
begin
result:=false;
  try
  result:=V.Get('connected').AsBool;
  except
   on E: Exception do begin
     if debug_msg then msg('Get Connected error: ' + E.Message+', retrying...',0);
     result:=false;
   end;
  end;
end;

function T_ascomrestswitch.WaitConnecting(maxtime:integer):boolean;
var timemax,lastcheck: double;
    ok: boolean;
begin
 result:=true;
 try
   timemax:=now+maxtime/1000/secperday;
   lastcheck:=0;
   repeat
      if now>(lastcheck+waitpoll/1000/secperday) then begin
         ok:=not (V.Get('connecting').AsBool);
         if ok then break;
         lastcheck:=now;
      end;
      sleep(10);
      if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
   until ok or (now>timemax);
   result:=(now<timemax);
 except
   on E: Exception do begin
     msg(Format(rsConnectionEr, [E.Message]),0);
     result:=false;
   end;
 end;
end;

procedure T_ascomrestswitch.StatusTimerTimer(sender: TObject);
var s: TSwitchList;
  i: integer;
  changed:boolean;
begin
 StatusTimer.Enabled:=false;
 try
  if Connected then begin
    try
     s:=GetSwitch;
     if (FStatus<>devConnected)or(FNumSwitch=0) then exit;
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
end;

function  T_ascomrestswitch.GetDevicestate:TAscomDeviceStateList;
var J: TAscomResult;
    d: TJSONArray;
    i,n: integer;
begin
 J:=V.Get('devicestate');
 try
   d:=TJSONArray(J.GetName('Value'));
   n:=d.Count;
   SetLength(Result,n);
   for i:=0 to n-1 do begin
     Result[i].Name:=d.Objects[i].GetPath('Name').AsString;
     Result[i].Value:=d.Objects[i].GetPath('Value').AsString;
   end;
 finally
   J.Free;
 end;
end;

function  T_ascomrestswitch.GetStateValue(state: TAscomDeviceStateList; n: string):string;
var i: integer;
begin
  result:='';
  for i:=0 to length(state)-1 do begin
    if state[i].Name=n then begin
      Result:=state[i].Value;
      Break;
    end;
  end;
end;

function  T_ascomrestswitch.GetSwitch:TSwitchList;
var i: integer;
    statelist:TAscomDeviceStateList;
    buf: string;
begin
  FLastError:='';
  if (FStatus<>devConnected)or(FNumSwitch=0) then exit;
  try
    SetLength(result,FNumSwitch);
    if hasDevicestate then begin
       statelist:=GetDevicestate;
       if length(statelist)<FNumSwitch then hasDevicestate:=false;
    end;
    for i:=0 to FNumSwitch-1 do begin
      result[i].Name       := FSwitch[i].Name;
      result[i].CanWrite   := FSwitch[i].CanWrite;
      result[i].MultiState := FSwitch[i].MultiState;
      result[i].Min        := FSwitch[i].Min;
      result[i].Max        := FSwitch[i].Max;
      result[i].Step       := FSwitch[i].Step;
      result[i].Edited     := false;
      result[i].IndiGroup  := -1;
      if result[i].MultiState then begin
        try
        if hasDevicestate then
          result[i].Value := StrToFloatDef(GetStateValue(statelist,'GetSwitchValue'+IntToStr(i)),FSwitch[i].Min)
        else
          result[i].Value    := V.Get('getswitchvalue','Id='+IntToStr(i)).AsFloat;
        if (FStatus<>devConnected)or(FNumSwitch=0) then exit;
        result[i].Checked  := (result[i].Value = result[i].Max);
        except
         on E: Exception do begin
           FLastError:=E.Message;
           result[i].Value    := FSwitch[i].Min;
           result[i].Checked  := False;
         end;
        end;
      end
      else begin
        try
        if hasDevicestate then begin
          buf := GetStateValue(statelist,'GetSwitch'+IntToStr(i));
          if buf<>'' then
            result[i].Checked := UpperCase(buf)='TRUE'
          else begin
            buf := GetStateValue(statelist,'GetSwitchValue'+IntToStr(i));
            result[i].Checked := buf<>'0';
          end;
        end
        else begin
          result[i].Checked  := V.Get('getswitch','Id='+IntToStr(i)).AsBool;
        end;
        if result[i].Checked then
          result[i].Value := FSwitch[i].Max
        else
          result[i].Value := FSwitch[i].Min;
        if (FStatus<>devConnected)or(FNumSwitch=0) then exit;
        except
          on E: Exception do begin
            FLastError:=E.Message;
            result[i].Value    := FSwitch[i].Min;
            result[i].Checked  := False;
          end;
        end;
      end;
    end;
  except
   on E: Exception do msg('GetSwitch error: ' + E.Message,0);
  end;
end;

procedure T_ascomrestswitch.SetSwitch(value: TSwitchList);
var i: integer;
    p: array[0..3]of string;
begin
 if (FStatus<>devConnected)or(FNumSwitch=0) then exit;
 try
   for i:=0 to FNumSwitch-1 do begin
     if value[i].CanWrite and value[i].Edited then begin
       p[0]:='Id';
       p[1]:=inttostr(i);
       if FSwitch[i].MultiState then begin
         p[2]:='Value';
         p[3]:=FloatToStr(value[i].Value);
         msg('Set switch '+value[i].Name+'='+p[3],3);
         try
         V.Timeout:=LongTimeout;   // TODO: implement async setswich
         V.Put('setswitchvalue',p);
         finally
         V.Timeout:=StdTimeout;
         end;
       end
       else begin
         p[2]:='State';
         p[3]:=BoolToStr(value[i].Checked,true);
         msg('Set switch '+value[i].Name+'='+p[3],3);
         try
         V.Timeout:=LongTimeout;  // TODO: implement async setswich
         V.Put('setswitch',p);
         finally
         V.Timeout:=StdTimeout;
         end;
       end;
     end;
   end;
 except
  on E: Exception do msg('SetSwitch error: ' + E.Message,0);
 end;
end;

procedure T_ascomrestswitch.SetTimeout(num:integer);
begin
 FTimeOut:=num;
end;

end.

