unit cu_autoguider_linguider;

{$mode objfpc}{$H+}

{
Copyright (C) 2017 Patrick Chevalley

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

uses cu_autoguider, u_global,u_utils, Sockets,
  Forms, Classes, SysUtils;

type

  LIN_CMD=(LIN_GET_VER = 1,
    LIN_SET_GUIDER_SQUARE_POS,
    LIN_SAVE_FRAME,
    LIN_DITHER,
    LIN_DITHER_NO_WAIT_XY,
    LIN_GET_DISTANCE,
    LIN_SAVE_FRAME_DECORATED,
    LIN_GUIDER,
    LIN_GET_GUIDER_STATE,
    LIN_SET_GUIDER_OVL_POS,
    LIN_SET_GUIDER_RETICLE_POS,
    LIN_FIND_STAR,
    LIN_SET_DITHERING_RANGE,
    LIN_GET_RA_DEC_DRIFT,
    LIN_CMD_MAX);

  T_autoguider_linguider = class(T_autoguider)
  protected
    FUsock: Integer;
    FUSocket: string;
    function LinGuiderCmd(lincmd:LIN_CMD; param:string=''):string;
    procedure SetState;
    Procedure ProcessEvent(txt:string); override;
    procedure Execute; override;
    procedure StarLostTimerTimer(Sender: TObject); override;
  public
    Constructor Create;
    Destructor Destroy; override;
    Procedure Connect(cp1: string; cp2:string=''); override;
    procedure Disconnect; override;
    procedure Shutdown; override;
    procedure ConnectGear; override;
    procedure SettleTolerance(pixel:double; mintime,maxtime: integer); override;
    procedure Calibrate; override;
    procedure Guide(onoff:boolean; recalibrate:boolean=false); override;
    procedure Pause(onoff:boolean); override;
    procedure Dither(pixel:double; raonly:boolean); override;
    function WaitBusy(maxwait:integer=5):boolean; override;
    function WaitGuiding(maxwait:integer=5):boolean; override;
  end;

implementation


Constructor T_autoguider_linguider.Create ;
begin
  inherited Create;
  FAutoguiderType:=LINGUIDER;
  FUSocket:='/tmp/lg_ss';
  FTimeout:=500;
end;

Destructor T_autoguider_linguider.Destroy;
begin
  inherited Destroy;
end;

function T_autoguider_linguider.LinGuiderCmd(lincmd:LIN_CMD; param:string=''):string;
var buf: string;
    cmd:byte;
    n,i,msgl,to_read: integer;
    srvaddr: sockaddr_un;
    msg: array[0..255] of char;
    buffer: array[0..81] of char;
    readbuf: array[0..1024] of char;
const hdr_sz=8;
begin
    cmd:=ord(lincmd);
    result:=msgFailed;
    try

    // Create socket
    srvaddr.sun_family:=AF_UNIX;
    srvaddr.sun_path:=FUSocket;
    n:=SizeOf(srvaddr.sun_family)+Length(srvaddr.sun_path);
    FUSock:=fpsocket(AF_UNIX,SOCK_STREAM,0);
    if FUsock<0 then exit;

    // Connect
    i:=fpconnect(FUsock,@srvaddr,n);
    if i<0 then exit;

    // signature and command
    for i:=0 to 255 do msg[i]:=chr(0);
    msg[0] := chr(2);	// SIGNATURE
    msg[2] := chr(cmd);	// CMD

    // add parameters
    buf:=param;
    n:=length(buf);
    buffer:=buf;
    msg[4] := chr(n);	// DATA LEN
    Move(buffer,msg[hdr_sz],n);
    msgl:=hdr_sz+n;

    // send command
    fpsend(FUsock,@msg,msgl,0);

    // read response length
    fprecv(FUsock,@readbuf,hdr_sz,0);
    to_read:=ord(readbuf[4]);
    // read response
    fprecv(FUsock,@readbuf,to_read,0);
    buf:=copy(readbuf,1,to_read);

    // Close socket
    CloseSocket(FUSock);

    result:=trim(buf);

    except
      on e:Exception do begin
        result:=msgFailed;
        DisplayMessage('Autoguider: '+e.Message);
      end;
    end;
end;

Procedure T_autoguider_linguider.Connect(cp1: string; cp2:string='');
var buf: string;
begin
  if FRunning then exit;
  FUSocket:=cp1;
  buf:=LinGuiderCmd(LIN_GET_VER);
  if buf=msgFailed then begin
    FStatus:='Disconnected';
    FState:=GUIDER_DISCONNECTED;
    if assigned(FonConnectError) then FonConnectError(self);
  end
  else begin
    FStatus:='Connected';
    Start;
  end;
end;

procedure T_autoguider_linguider.Disconnect;
begin
 Terminate;
end;

procedure T_autoguider_linguider.Execute;
var buf:string;
begin
 try
  FRunning:=true;
  if assigned(FonConnect) then FonConnect(self);
  // main loop
  repeat
    if terminated then break;
    sleep(1000);
    buf:=LinGuiderCmd(LIN_GET_GUIDER_STATE);
    if buf=msgFailed then break;
    ProcessEvent(buf);
  until false;
  finally
  FRunning:=false;
  terminate;
  FStatus:='Disconnected';
  ProcessDisconnect;
  end;
end;

Procedure T_autoguider_linguider.ProcessEvent(txt:string);
begin
 FStatus:=txt;
 SetState;
 StatusChange;
end;

procedure T_autoguider_linguider.SetState;
begin
  if FStatus='GUIDING' then FState:=GUIDER_GUIDING
  else if FStatus='IDLE' then FState:=GUIDER_IDLE
  else if FStatus=msgFailed then FState:=GUIDER_ALERT;
end;

procedure T_autoguider_linguider.ConnectGear;
begin
 //Unsupported by Lin_Guider
end;

procedure T_autoguider_linguider.Shutdown;
begin
if not FRunning then exit;
DisplayMessage('Autoguider: '+'Shutdown command not supported by Lin_Guider');
end;

procedure T_autoguider_linguider.SettleTolerance(pixel:double; mintime,maxtime: integer);
begin
//Unsupported by Lin_Guider
end;

function T_autoguider_linguider.WaitBusy(maxwait:integer=5):boolean;
begin
  wait(1);
  result:=true;
end;

function T_autoguider_linguider.WaitGuiding(maxwait:integer=5):boolean;
var endt: TDateTime;
    n: integer;
begin
  endt:=now+maxwait/secperday;
  n:=0;
  while now<endt do begin
    Sleep(1000);
    Application.ProcessMessages;
    if FState=GUIDER_GUIDING then break;
    inc(n);
    if ((n mod 150)=0) and assigned(FonShowMessage) then
        FonShowMessage('Waiting for autoguider to start...');
  end;
  result:=(FState=GUIDER_GUIDING);
end;

procedure T_autoguider_linguider.Calibrate;
begin
if not FRunning then exit;
DisplayMessage('Autoguider: '+'Calibrate command not supported by Lin_Guider');
end;

procedure T_autoguider_linguider.Guide(onoff:boolean; recalibrate:boolean=false);
var xy,buf:string;
begin
if not FRunning then exit;
  if onoff then begin
     xy:=LinGuiderCmd(LIN_FIND_STAR);
     buf:=LinGuiderCmd(LIN_SET_GUIDER_SQUARE_POS,xy);
     buf:=LinGuiderCmd(LIN_SET_GUIDER_OVL_POS,xy);
     buf:=LinGuiderCmd(LIN_SET_GUIDER_RETICLE_POS,xy);
     buf:=LinGuiderCmd(LIN_GUIDER,'start');
     DisplayMessage('Autoguider: '+'Guide start: '+buf);
  end else begin
     buf:=LinGuiderCmd(LIN_GUIDER,'stop');
     DisplayMessage('Autoguider: '+'Guide stop: '+buf);
  end;
end;

procedure T_autoguider_linguider.Pause(onoff:boolean);
begin
if not FRunning then exit;
DisplayMessage('Autoguider: '+'Pause command not supported by Lin_Guider');
end;

procedure T_autoguider_linguider.Dither(pixel:double; raonly:boolean);
var pix,buf:string;
begin
if not FRunning then exit;
  pix:=FormatFloat(f1,pixel);
  buf:=LinGuiderCmd(LIN_SET_DITHERING_RANGE,pix);
  buf:=LinGuiderCmd(LIN_DITHER);
  DisplayMessage('Autoguider: '+'Dither: '+buf);
end;

procedure T_autoguider_linguider.StarLostTimerTimer(Sender: TObject);
begin
 //Unsupported by Lin_Guider
StarLostTimer.Enabled:=false;
end;

end.

