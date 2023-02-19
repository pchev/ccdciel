unit cu_autoguider_none;

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

uses cu_autoguider, u_global,
  u_translation, Forms, Classes, SysUtils;

type

  T_autoguider_none = class(T_autoguider)
  protected
    Procedure ProcessEvent(txt:string); override;
    procedure Execute; override;
    procedure Terminate;
    procedure StarLostTimerTimer(Sender: TObject); override;
  public
    Constructor Create;
    Destructor Destroy; override;
    Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cb1:boolean=False); override;
    procedure Disconnect; override;
    procedure Shutdown; override;
    procedure ConnectGear; override;
    procedure SettleTolerance(pixel:double; mintime,maxtime: integer); override;
    procedure Calibrate; override;
    procedure Guide(onoff:boolean; recalibrate:boolean=false); override;
    procedure Pause(onoff:boolean; settle:boolean=true); override;
    procedure Dither(pixel:double; raonly:boolean; waittime:double); override;
    function GetLockPosition(out x,y:double):boolean; override;
    procedure SetLockPosition(x,y: double); override;
    function WaitBusy(maxwait:integer=5):boolean; override;
    function WaitGuiding(maxwait:integer=5):boolean; override;
    function WaitDithering(maxwait:integer=5):boolean; override;
  end;

implementation


Constructor T_autoguider_none.Create ;
begin
  inherited Create;
  FAutoguiderType:=agNONE;
  FStatus:=rsNone2;
end;

Destructor T_autoguider_none.Destroy;
begin
  inherited Destroy;
end;

Procedure T_autoguider_none.Connect(cp1: string; cp2:string=''; cp3:string=''; cb1:boolean=False);
begin
  start;
end;

procedure T_autoguider_none.Disconnect;
begin
end;

procedure T_autoguider_none.Execute;
begin
end;

procedure T_autoguider_none.Terminate;
begin
  Free;
end;

Procedure T_autoguider_none.ProcessEvent(txt:string);
begin
end;

procedure T_autoguider_none.ConnectGear;
begin
end;

procedure T_autoguider_none.Shutdown;
begin
end;

procedure T_autoguider_none.SettleTolerance(pixel:double; mintime,maxtime: integer);
begin
end;

function T_autoguider_none.WaitBusy(maxwait:integer=5):boolean;
begin
  result:=true;
end;

function T_autoguider_none.WaitGuiding(maxwait:integer=5):boolean;
begin
  result:=true;
end;

function T_autoguider_none.WaitDithering(maxwait:integer=5):boolean;
begin
  result:=true;
end;

procedure T_autoguider_none.Calibrate;
begin
end;

procedure T_autoguider_none.Guide(onoff:boolean; recalibrate:boolean=false);
begin
end;

function T_autoguider_none.GetLockPosition(out x,y:double):boolean;
begin
  result:=false;
end;

procedure T_autoguider_none.SetLockPosition(x,y:double);
begin
end;

procedure T_autoguider_none.Pause(onoff:boolean; settle:boolean=true);
begin
end;

procedure T_autoguider_none.Dither(pixel:double; raonly:boolean; waittime:double);
begin
end;

procedure T_autoguider_none.StarLostTimerTimer(Sender: TObject);
begin
end;

end.

