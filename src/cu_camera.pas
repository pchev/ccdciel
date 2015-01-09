unit cu_camera;

{$mode objfpc}{$H+}

{
Copyright (C) 2015 Patrick Chevalley

http://www.ap-i.net
pch@ap-i.net

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
}

interface

uses cu_indicamera, cu_ascomcamera, u_global,
  Classes, SysUtils;

type

T_camera = class(TObject)
  private
    indicamera: T_indicamera;
    ascomcamera:T_ascomcamera;
    indiready: Boolean;
    FCameraInterface: TDevInterface;
    FonMsg: TNotifyMsg;
    FonExposureProgress: TNotifyNum;
    FonFilterChange: TNotifyNum;
    FonFrameChange: TNotifyEvent;
    FonTemperatureChange: TNotifyNum;
    FonStatusChange: TNotifyEvent;
    FonFilterNameChange: TNotifyEvent;
    FonNewImage: TNotifyEvent;
    procedure IndiDestroy(Sender: TObject);
    function GetCameraStatus: TDeviceStatus;
    function GetImgStream: TMemoryStream;
    function GetonTemperatureChange: TNotifyNum;
    procedure SetonTemperatureChange(value: TNotifyNum);
    function GetonFilterChange: TNotifyNum;
    procedure SetonFilterChange(value: TNotifyNum);
    function GetonMsg: TNotifyMsg;
    procedure SetonMsg(value: TNotifyMsg);
    function GetonExposureProgress: TNotifyNum;
    procedure SetonExposureProgress(value: TNotifyNum);
    function GetonStatusChange: TNotifyEvent;
    procedure SetonStatusChange(value: TNotifyEvent);
    function GetonFrameChange: TNotifyEvent;
    procedure SetonFrameChange(value: TNotifyEvent);
    function GetonWheelStatusChange: TNotifyEvent;
    procedure SetonWheelStatusChange(value: TNotifyEvent);
    function GetonFilterNameChange: TNotifyEvent;
    procedure SetonFilterNameChange(value: TNotifyEvent);
    function GetonNewImage: TNotifyEvent;
    procedure SetonNewImage(value: TNotifyEvent);
    function GetBinX:integer;
    function GetBinY:integer;
    procedure SetFrametype(f:TFrameType);
    function  GetFrametype:TFrameType;
    function GetBinXrange:TNumRange;
    function GetBinYrange:TNumRange;
    function GetExposureRange:TNumRange;
    function GetTemperatureRange:TNumRange;
    function  GetTemperature: double;
    procedure SetTemperature(value:double);
    procedure SetFilter(num:integer);
    function  GetFilter:integer;
    procedure SetFilterNames(value:TStringList);
    function  GetFilterNames:TStringList;
  public
    constructor Create(devinterface: TDevInterface);
    destructor  Destroy; override;
    Procedure Connect(indiserver, indiserverport, indidevice, indisensor, indideviceport: string);
    Procedure Connect(ascomdevice: string);
    Procedure Disconnect;
    Procedure SetBinning(binX,binY: integer);
    Procedure StartExposure(exptime: double);
    Procedure AbortExposure;
    procedure SetFrame(x,y,width,height: integer);
    procedure GetFrame(out x,y,width,height: integer);
    procedure GetFrameRange(out xr,yr,widthr,heightr: TNumRange);
    procedure ResetFrame;
    Procedure SetActiveDevices(focuser,filters,telescope: string);
    property CameraInterface: TDevInterface read FCameraInterface;
    property Status: TDeviceStatus read GetCameraStatus;
    property ImgStream: TMemoryStream read GetImgStream;
    property Temperature: double read GetTemperature write SetTemperature;
    property BinX: Integer read getBinX;
    property BinY: Integer read getBinY;
    property FrameType: TFrameType read GetFrametype write SetFrametype;
    property BinXrange: TNumRange read GetbinXrange;
    property BinYrange: TNumRange read GetbinYrange;
    property ExposureRange: TNumRange read GetExposureRange;
    property TemperatureRange: TNumRange read GetTemperatureRange;
    property Filter: integer read GetFilter write SetFilter;
    property FilterNames: TStringList read GetFilterNames write SetFilterNames;
    property onMsg: TNotifyMsg read GetonMsg write SetonMsg;
    property onExposureProgress: TNotifyNum read GetonExposureProgress write SetonExposureProgress;
    property onTemperatureChange: TNotifyNum read GetonTemperatureChange write SetonTemperatureChange;
    property onFilterChange: TNotifyNum read GetonFilterChange write SetonFilterChange;
    property onStatusChange: TNotifyEvent read GetonStatusChange write SetonStatusChange;
    property onFrameChange: TNotifyEvent read GetonFrameChange write SetonFrameChange;
    property onFilterNameChange: TNotifyEvent read GetonFilterNameChange write SetonFilterNameChange;
    property onWheelStatusChange: TNotifyEvent read GetonWheelStatusChange write SetonWheelStatusChange;
    property onNewImage: TNotifyEvent read GetonNewImage write SetonNewImage;
end;

implementation

constructor T_camera.Create(devinterface: TDevInterface);
begin
 inherited Create;
 indiready:=False;
 FCameraInterface:=devinterface;
 case FCameraInterface of
    INDI : begin
             indicamera:= T_indicamera.Create;
             indicamera.onDestroy:=@IndiDestroy;
             indiready:=True;
           end;
    ASCOM: begin
             ascomcamera:=T_ascomcamera.Create;
           end;
 end;
end;

destructor  T_camera.Destroy;
begin
 case FCameraInterface of
    INDI : begin
             if indiready then indicamera.Free;
           end;
    ASCOM :begin
             ascomcamera.Free;
          end;
 end;
 inherited Destroy;
end;

procedure T_camera.IndiDestroy(Sender: TObject);
begin
 indiready:=False;
end;

Procedure T_camera.Connect(indiserver, indiserverport, indidevice, indisensor, indideviceport: string);
begin
 if not indiready then begin
   indicamera:= T_indicamera.Create;
   indicamera.onDestroy:=@IndiDestroy;
   indicamera.onMsg:=FonMsg;
   indicamera.onExposureProgress:=FonExposureProgress;
   indicamera.onTemperatureChange:=FonTemperatureChange;
   indicamera.onStatusChange:=FonStatusChange;
   indicamera.onNewImage:=FonNewImage;
   indiready:=True;
 end;
 if indiserver<>'' then indicamera.indiserver:=indiserver;
 if indiserverport<>'' then indicamera.indiserverport:=indiserverport;
 if indidevice<>'' then indicamera.indidevice:=indidevice;
 if indisensor<>'' then indicamera.indisensor:=indisensor;
 indicamera.indideviceport:=indideviceport;
 indicamera.Connect;
end;

Procedure T_camera.Connect(ascomdevice: string);
begin
  ascomcamera.Device:=ascomdevice;
  ascomcamera.Connect;
end;

Procedure T_camera.Disconnect;
begin
 case FCameraInterface of
    INDI : begin
             if indiready then indicamera.Disconnect;
           end;
    ASCOM: begin
             ascomcamera.Disconnect;
           end;
 end;
end;

Procedure T_camera.StartExposure(exptime: double);
begin
 case FCameraInterface of
    INDI : begin
             if indiready then indicamera.StartExposure(exptime);
           end;
    ASCOM: begin
             ascomcamera.StartExposure(exptime);
           end;
 end;
end;

function T_camera.GetExposureRange: TNumRange;
begin
 case FCameraInterface of
    INDI : begin
             if indiready then result:=indicamera.ExposureRange;
           end;
    ASCOM: begin
              result:=ascomcamera.ExposureRange
           end;
 end;
end;

Procedure T_camera.AbortExposure;
begin
 case FCameraInterface of
    INDI : begin
             if indiready then indicamera.AbortExposure;
           end;
    ASCOM: begin
              ascomcamera.AbortExposure;
           end;
 end;
end;

Procedure T_camera.SetActiveDevices(focuser,filters,telescope: string);
begin
 case FCameraInterface of
    INDI : begin
             if indiready then indicamera.SetActiveDevices(focuser,filters,telescope);
           end;
    ASCOM: begin
             ascomcamera.SetActiveDevices(focuser,filters,telescope);
           end;
 end;
end;

function T_camera.GetCameraStatus: TDeviceStatus;
begin
 case FCameraInterface of
    INDI : begin
             if indiready then result:=indicamera.Status
                          else result:=devDisconnected;
           end;
    ASCOM: begin
             result:=ascomcamera.Status;
           end;
 end;
end;

function T_camera.GetImgStream: TMemoryStream;
begin
 case FCameraInterface of
    INDI : begin
             if indiready then result:=indicamera.ImgStream;
           end;
    ASCOM: begin
             result:=ascomcamera.ImgStream;
           end;
 end;
end;

function T_camera.GetonMsg: TNotifyMsg;
begin
 case FCameraInterface of
    INDI : begin
             if indiready then result:=indicamera.onMsg;
           end;
    ASCOM: begin
             result:=ascomcamera.onMsg;
           end;
 end;
end;

procedure T_camera.SetonMsg(value: TNotifyMsg);
begin
 FonMsg:=value;
 case FCameraInterface of
    INDI : begin
             if indiready then indicamera.onMsg:=value;
           end;
    ASCOM: begin
             ascomcamera.onMsg:=value;
           end;
 end;
end;

function T_camera.GetonExposureProgress: TNotifyNum;
begin
 case FCameraInterface of
    INDI : begin
             if indiready then result:=indicamera.onExposureProgress;
           end;
    ASCOM: begin
             result:=ascomcamera.onExposureProgress;
           end;
 end;
end;

procedure T_camera.SetonExposureProgress(value: TNotifyNum);
begin
 FonExposureProgress:=value;
 case FCameraInterface of
    INDI : begin
             if indiready then indicamera.onExposureProgress:=value;
           end;
    ASCOM: begin
             ascomcamera.onExposureProgress:=value;
           end;
 end;
end;

function T_camera.GetonStatusChange: TNotifyEvent;
begin
 case FCameraInterface of
    INDI : begin
             if indiready then result:=indicamera.onStatusChange;
           end;
    ASCOM: begin
             result:=ascomcamera.onStatusChange;
           end;
 end;
end;

procedure T_camera.SetonStatusChange(value: TNotifyEvent);
begin
 FonStatusChange:=value;
 case FCameraInterface of
    INDI : begin
             if indiready then indicamera.onStatusChange:=value;
           end;
    ASCOM: begin
             ascomcamera.onStatusChange:=value;
           end;
 end;
end;

function T_camera.GetonFrameChange: TNotifyEvent;
begin
case FCameraInterface of
   INDI : begin
            if indiready then result:=indicamera.onFrameChange;
          end;
   ASCOM: begin
            result:=ascomcamera.onFrameChange;
          end;
end;
end;

procedure T_camera.SetonFrameChange(value: TNotifyEvent);
begin
 FonFrameChange:=value;
 case FCameraInterface of
    INDI : begin
             if indiready then indicamera.onFrameChange:=value;
           end;
    ASCOM: begin
             ascomcamera.onFrameChange:=value;
           end;
 end;
end;

function T_camera.GetTemperatureRange: TNumRange;
begin
 case FCameraInterface of
    INDI : begin
             if indiready then result:=indicamera.TemperatureRange;
           end;
    ASCOM: begin
             result:=ascomcamera.TemperatureRange;
           end;
 end;
end;

function T_camera.GetonTemperatureChange: TNotifyNum;
begin
 case FCameraInterface of
    INDI : begin
             if indiready then result:=indicamera.onTemperatureChange;
           end;
    ASCOM: begin
             result:=ascomcamera.onTemperatureChange;
           end;
 end;
end;

procedure T_camera.SetonTemperatureChange(value: TNotifyNum);
begin
 FonTemperatureChange:=value;
 case FCameraInterface of
    INDI : begin
             if indiready then indicamera.onTemperatureChange:=value;
           end;
    ASCOM: begin
             ascomcamera.onTemperatureChange:=value;
           end;
 end;
end;

function T_camera.GetonWheelStatusChange: TNotifyEvent;
begin
 case FCameraInterface of
    INDI : begin
             if indiready then result:=indicamera.onWheelStatusChange;
           end;
    ASCOM: begin
             result:=ascomcamera.onWheelStatusChange;
           end;
 end;
end;

procedure T_camera.SetonWheelStatusChange(value: TNotifyEvent);
begin
 FonStatusChange:=value;
 case FCameraInterface of
    INDI : begin
             if indiready then indicamera.onWheelStatusChange:=value;
           end;
    ASCOM: begin
             ascomcamera.onWheelStatusChange:=value;
           end;
 end;
end;

function T_camera.GetonFilterNameChange: TNotifyEvent;
begin
 case FCameraInterface of
    INDI : begin
             if indiready then result:=indicamera.onFilterNameChange;
           end;
    ASCOM: begin
              result:=ascomcamera.onFilterNameChange;
           end;
 end;
end;

procedure T_camera.SetonFilterNameChange(value: TNotifyEvent);
begin
 FonFilterNameChange:=value;
 case FCameraInterface of
    INDI : begin
             if indiready then indicamera.onFilterNameChange:=value;
           end;
    ASCOM: begin
             ascomcamera.onFilterNameChange:=value;
           end;
 end;
end;
function T_camera.GetonNewImage: TNotifyEvent;
begin
 case FCameraInterface of
    INDI : begin
             if indiready then result:=indicamera.onNewImage;
           end;
    ASCOM: begin
             result:=ascomcamera.onNewImage;
           end;
 end;
end;

procedure T_camera.SetonNewImage(value: TNotifyEvent);
begin
 FonNewImage:=value;
 case FCameraInterface of
    INDI : begin
             if indiready then indicamera.onNewImage:=value;
           end;
    ASCOM: begin
             ascomcamera.onNewImage:=value;
           end;
 end;
end;

function T_camera.GetBinX:integer;
begin
 case FCameraInterface of
    INDI : begin
             if indiready then result:=indicamera.BinX;
           end;
    ASCOM: begin
             result:=ascomcamera.BinX;
           end;
 end;
end;

function T_camera.GetBinY:integer;
begin
 case FCameraInterface of
    INDI : begin
             if indiready then result:=indicamera.BinY;
           end;
    ASCOM: begin
             result:=ascomcamera.BinY;
           end;
 end;
end;

function T_camera.GetbinXrange: TNumRange;
begin
 case FCameraInterface of
    INDI : begin
             if indiready then result:=indicamera.BinXrange;
           end;
    ASCOM: begin
             result:=ascomcamera.BinXrange;
           end;
 end;
end;

function T_camera.GetbinYrange: TNumRange;
begin
 case FCameraInterface of
    INDI : begin
             if indiready then result:=indicamera.BinYrange;
           end;
    ASCOM: begin
             result:=ascomcamera.BinYrange;
           end;
 end;
end;

Procedure T_camera.SetBinning(binX,binY: integer);
begin
 case FCameraInterface of
    INDI : begin
             if indiready then indicamera.SetBinning(binX,binY);
           end;
    ASCOM: begin
             ascomcamera.SetBinning(binX,binY);
           end;
 end;
end;

procedure T_camera.SetFrametype(f:TFrameType);
begin
 case FCameraInterface of
    INDI : begin
             if indiready then indicamera.FrameType:=f;
           end;
    ASCOM: begin
             ascomcamera.FrameType:=f;
           end;
 end;
end;

function  T_camera.GetFrametype:TFrameType;
begin
case FCameraInterface of
   INDI : begin
            if indiready then result:=indicamera.FrameType;
          end;
   ASCOM: begin
            result:=ascomcamera.FrameType;
          end;
end;
end;

procedure T_camera.SetFrame(x,y,width,height: integer);
begin
 case FCameraInterface of
    INDI : begin
             if indiready then indicamera.SetFrame(x,y,width,height);
           end;
    ASCOM: begin
             ascomcamera.SetFrame(x,y,width,height);
           end;
 end;
end;

procedure T_camera.GetFrame(out x,y,width,height: integer);
begin
 case FCameraInterface of
    INDI : begin
             if indiready then indicamera.GetFrame(x,y,width,height);
           end;
    ASCOM: begin
             ascomcamera.GetFrame(x,y,width,height);
           end;
 end;
end;

procedure T_camera.GetFrameRange(out xr,yr,widthr,heightr: TNumRange);
begin
 case FCameraInterface of
    INDI : begin
             if indiready then indicamera.GetFrameRange(xr,yr,widthr,heightr);
           end;
    ASCOM: begin
             ascomcamera.GetFrameRange(xr,yr,widthr,heightr);
           end;
 end;
end;

procedure T_camera.ResetFrame;
begin
 case FCameraInterface of
    INDI : begin
             if indiready then indicamera.ResetFrame;
           end;
    ASCOM: begin
             ascomcamera.ResetFrame;
           end;
 end;
end;

function  T_camera.GetTemperature: double;
begin
 case FCameraInterface of
    INDI : begin
             if indiready then result:=indicamera.Temperature;
           end;
    ASCOM: begin
             result:=ascomcamera.Temperature;
           end;
 end;
end;

procedure T_camera.SetTemperature(value:double);
begin
 case FCameraInterface of
    INDI : begin
             if indiready then indicamera.Temperature:=value;
           end;
    ASCOM: begin
             ascomcamera.Temperature:=value;
           end;
 end;
end;

Procedure T_camera.SetFilter(num: integer);
begin
 case FCameraInterface of
    INDI : begin
             if indiready then indicamera.Filter:=num;
           end;
    ASCOM: begin
             ascomcamera.Filter:=num;
           end;
 end;
end;

function  T_camera.GetFilter:integer;
begin
 case FCameraInterface of
    INDI : begin
             if indiready then result:=indicamera.Filter;
           end;
    ASCOM: begin
             result:=ascomcamera.Filter;
           end;
 end;
end;

function T_camera.GetonFilterChange: TNotifyNum;
begin
 case FCameraInterface of
    INDI : begin
             if indiready then result:=indicamera.onFilterChange;
           end;
    ASCOM: begin
             result:=ascomcamera.onFilterChange;
           end;
 end;
end;

procedure T_camera.SetonFilterChange(value: TNotifyNum);
begin
 FonFilterChange:=value;
 case FCameraInterface of
    INDI : begin
             if indiready then indicamera.onFilterChange:=value;
           end;
    ASCOM: begin
             ascomcamera.onFilterChange:=value;
           end;
 end;
end;

procedure T_camera.SetFilterNames(value:TStringList);
begin
 case FCameraInterface of
    INDI : begin
             if indiready then indicamera.FilterNames:=value;
           end;
    ASCOM: begin
             ascomcamera.FilterNames:=value;
           end;
 end;
end;

function  T_camera.GetFilterNames:TStringList;
begin
 case FCameraInterface of
    INDI : begin
             if indiready then result:=indicamera.FilterNames;
           end;
    ASCOM: begin
             result:=ascomcamera.FilterNames;
           end;
 end;
end;

end.

