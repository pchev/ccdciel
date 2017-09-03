unit pu_pause;
{
Copyright (C) 2016 Patrick Chevalley

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
  non blocking pause message
}

{$mode objfpc}{$H+}

interface

uses  u_global, UScaleDPI, u_utils,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { Tf_pause }

  Tf_pause = class(TForm)
    BtnContinue: TButton;
    BtnCancel: TButton;
    Label1: TLabel;
    PauseLabel: TLabel;
    procedure BtnContinueClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FContinue, Fresult : boolean;
    procedure SetText(value:string);
    function GetText: string;
  public
    { public declarations }
    function Wait(timeout:integer=0): boolean;
    property Text: string read GetText write SetText;
  end;

  function WaitTill(hour:string; showdialog: boolean):boolean;

var
  f_pause,wt_pause: Tf_pause;

implementation

{$R *.lfm}

{ Tf_pause }

procedure Tf_pause.SetText(value:string);
begin
  PauseLabel.Caption:=value;
end;

function Tf_pause.GetText: string;
begin
  result:=PauseLabel.Caption;
end;

procedure Tf_pause.FormShow(Sender: TObject);
begin
  FContinue:=false;
  Fresult:=false;
end;

procedure Tf_pause.BtnContinueClick(Sender: TObject);
begin
  Fresult:=true;
  FContinue:=true;
end;

procedure Tf_pause.BtnCancelClick(Sender: TObject);
begin
  Fresult:=false;
  FContinue:=true;
end;

procedure Tf_pause.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose:=FContinue;
end;

procedure Tf_pause.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self);
end;

function Tf_pause.Wait(timeout:integer=0): boolean;
var endt: TDateTime;
    n,t:integer;
begin
  if timeout>0 then begin
    endt:=now+timeout/secperday;
    PauseLabel.Caption:=PauseLabel.Caption;
    label1.Caption:='Continue automatically in '+inttostr(timeout)+' seconds.';
  end
  else begin
    endt:=MaxInt;
    label1.Caption:='';
  end;
  Show;
  n:=0;
  while (not FContinue) do begin
    if timeout>0 then begin
      inc(n);
      if (n mod 20) = 0 then begin
        t:=round((endt-now)*secperday);
        label1.Caption:='Continue automatically in '+inttostr(t)+' seconds.';
        n:=0;
      end;
    end;
    Application.ProcessMessages;
    Sleep(100);
    if now>endt then begin
       Fresult:=true;
       FContinue:=true;
    end;
  end;
  result:=Fresult;
  Close;
end;

function WaitTill(hour:string; showdialog: boolean):boolean;
var endt: TDateTime;
    daystr:string;
    nextday: boolean;
    wt:integer;
begin
 result:=false;
 if wt_pause<>nil then begin
    globalmsg('Wait dialog already running');
    exit;
 end;
 try
  endt:=StrToTime(hour,':');
  SecondsToWait(endt,wt,nextday);
  if nextday then
     daystr:='tomorrow '
  else
     daystr:='';
  if wt>0 then begin
    WaitTillrunning:=true;
    if showdialog then begin
      wt_pause:=Tf_pause.Create(nil);
      try
      globalmsg('Need to wait until '+daystr+hour);
      wt_pause.Text:='Need to wait until '+daystr+hour;
      result:=wt_pause.Wait(wt)
      finally
      WaitTillrunning:=false;
      FreeAndNil(wt_pause);
      end;
    end
    else begin
      globalmsg('Need to wait until '+daystr+hour);
      while now<endt do begin
        Sleep(100);
        Application.ProcessMessages;
        if cancelWaitTill then begin
          WaitTillrunning:=false;
          cancelWaitTill:=false;
          result:=false;
          exit;
        end;
      end;
     WaitTillrunning:=false;
     result:=true;
    end;
  end else begin
    globalmsg('Time already passed '+daystr+hour);
    result:=true;
  end;
 except
   WaitTillrunning:=false;
   cancelWaitTill:=false;
   if wt_pause<>nil then FreeAndNil(wt_pause);
   result:=false;
 end;
end;

end.

