unit fu_internalguider;

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

uses   UScaleDPI, Dialogs, u_translation, u_global, u_utils,
  Classes, SysUtils, FileUtil, Forms, Graphics, Controls, StdCtrls, ExtCtrls, Spin;

type

  { Tf_internalguider }

  Tf_internalguider = class(TFrame)
    ButtonStart: TButton;
    ButtonStop: TButton;
    ButtonCalibrate: TButton;
    Panel1: TPanel;
    Title: TLabel;
    procedure ButtonCalibrateClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
  private
    { private declarations }
    FonStart, FonStop, FonCalibrate: TNotifyEvent;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SetLang;
    property onStart: TNotifyEvent read FonStart write FonStart;
    property onStop: TNotifyEvent read FonStop write FonStop;
    property onCalibrate: TNotifyEvent read FonCalibrate write FonCalibrate;

  end;

implementation

{$R *.lfm}

{ Tf_internalguider }

constructor Tf_internalguider.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 {$ifdef lclcocoa}
 Title.Color:=clWindowFrame;
 {$endif}
 ScaleDPI(Self);
 SetLang;
end;

destructor  Tf_internalguider.Destroy;
begin
 inherited Destroy;
end;


procedure Tf_internalguider.SetLang;
begin
  Title.Caption:='Internal guider';
end;

procedure Tf_internalguider.ButtonStartClick(Sender: TObject);
begin
  if Assigned(FonStart) then FonStart(self);
end;

procedure Tf_internalguider.ButtonStopClick(Sender: TObject);
begin
  if Assigned(FonStop) then FonStop(self);
end;

procedure Tf_internalguider.ButtonCalibrateClick(Sender: TObject);
begin
  if Assigned(FonCalibrate) then FonCalibrate(self);
end;


end.

