unit fu_msg;

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

uses UScaleDPI, pu_msgtabs,
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, Buttons;

type

  { Tf_msg }

  Tf_msg = class(TFrame)
    msg: TMemo;
    Panel1: TPanel;
    Timer1: TTimer;
    Title: TLabel;
    procedure FrameResize(Sender: TObject);
    procedure msgMouseEnter(Sender: TObject);
    procedure msgMouseLeave(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    FLogLevel: integer;
    FLogLevelChange: TNotifyEvent;
    procedure TabControlChange(Sender: TObject);
    procedure SetLoglevel(value: integer);
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    property LogLevel: integer read FLogLevel write SetLoglevel;
    property onLogLevelChange: TNotifyEvent read FLogLevelChange write FLogLevelChange;
  end;

implementation

{$R *.lfm}

procedure Tf_msg.FrameResize(Sender: TObject);
var i,w: integer;
begin
  if Parent is TPanel then begin
    w:=TPanel(Parent).Width-Left;
    for i:=0 to TPanel(Parent).ComponentCount-1 do
       if TPanel(Parent).Components[i] is TFrame then
         w:=w-Tframe(TPanel(Parent).Components[i]).Width;
    Width:=w;
  end;
end;

procedure Tf_msg.SetLoglevel(value: integer);
begin
  FLogLevel:=value;
  f_msgtabs.TabControl1.TabIndex:=FLogLevel-1;
end;

procedure Tf_msg.TabControlChange(Sender: TObject);
begin
 FLogLevel:=f_msgtabs.TabControl1.TabIndex+1;
 if Assigned(FLogLevelChange) then FLogLevelChange(Self);
end;

procedure Tf_msg.msgMouseEnter(Sender: TObject);
var p:TPoint;
begin
 timer1.Enabled:=false;
 p.x:=title.Width;
 p.y:=-f_msgtabs.Height;
 p:=ClientToScreen(p);
 f_msgtabs.Left:=p.x;
 f_msgtabs.Top:=p.y;
 f_msgtabs.Width:=msg.Width+2;
 f_msgtabs.Visible:=true;
end;

procedure Tf_msg.msgMouseLeave(Sender: TObject);
begin
  timer1.Enabled:=true;
end;

procedure Tf_msg.Timer1Timer(Sender: TObject);
begin
  timer1.Enabled:=false;
  f_msgtabs.Visible:=false;
end;

constructor Tf_msg.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 ScaleDPI(Self);
 FLogLevel:=3;
 f_msgtabs:=Tf_msgtabs.Create(self);
 f_msgtabs.Visible:=false;
 f_msgtabs.TabControl1.TabIndex:=FLogLevel-1;
 f_msgtabs.TabControl1.OnMouseEnter:=@msgMouseEnter;
 f_msgtabs.TabControl1.OnMouseLeave:=@msgMouseLeave;
 f_msgtabs.TabControl1.OnChange:=@TabControlChange;
end;

destructor  Tf_msg.Destroy;
begin
 inherited Destroy;
end;

end.

