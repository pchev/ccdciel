unit fu_video;

{$mode objfpc}{$H+}

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

interface

uses u_global, UScaleDPI, cu_camera,
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls;

type

  { Tf_video }

  Tf_video = class(TFrame)
    Preview: TCheckBox;
    Panel1: TPanel;
    StaticText1: TStaticText;
    procedure PreviewChange(Sender: TObject);
  private
    { private declarations }
    FCamera: T_camera;
    Frunning: boolean;
    FonMsg: TNotifyMsg;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    property camera: T_camera read FCamera write FCamera;
    property Running: boolean read Frunning;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
  end;

implementation

{$R *.lfm}

procedure Tf_video.PreviewChange(Sender: TObject);
begin
  if FCamera<>nil then begin
   if Preview.Checked then begin
      Camera.StartVideoPreview;
      Frunning:=true;
   end
   else begin
      Camera.StopVideoPreview;
      Frunning:=false;
   end;
  end;
end;

constructor Tf_video.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 ScaleDPI(Self);
 Frunning:=false;
end;

destructor  Tf_video.Destroy;
begin
 inherited Destroy;
end;


end.

