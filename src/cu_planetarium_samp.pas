unit cu_planetarium_samp;

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
{
  client to connect to SAMP hub and peer.
}

interface

uses u_global, u_utils, cu_planetarium, cu_sampclient, Classes, SysUtils,
    LazFileUtils, ExtCtrls, Forms;

type

  TPlanetarium_samp = class(TPlanetarium)
  private
    SampClient : TSampClient;
    FClientChange: boolean;
    started: boolean;
    procedure DoClientChange;
    procedure ClientChange(Sender: TObject);
    procedure ClientDisconnected(Sender: TObject);
    procedure coordpointAtsky(cra,cdec:double);
    procedure ImageLoadFits(image_name,image_id,url:string);
  protected
    procedure Execute; override;
    procedure ProcessDataSyn; override;
  public
    Constructor Create;
    Destructor Destroy; override;
    procedure Connect(cp1,cp2,cp3,cp4: string; cb1:boolean=False); override;
    procedure Disconnect; override;
    procedure Shutdown; override;
    function ShowImage(fn: string; fovdeg:double=0):boolean; override;
    function DrawFrame(frra,frde,frsizeH,frsizeV,frrot: double):boolean; override;
    function Cmd(const Value: string):string; override;
    function GetEqSys: double; override;
    function Search(sname: string; out sra,sde,v_solar,vpa_solar: double): boolean; override;
    procedure ShowAstrometry(sra,sde: double); override;
    procedure ClearData; override;
  end;


implementation

/////////////////// TPlanetarium_samp ///////////////////////////

Constructor TPlanetarium_samp.Create ;
begin
inherited Create;
FClientChange:=false;
FPlanetariumType:=SAMP;
end;

Destructor TPlanetarium_samp.Destroy;
begin
  inherited Destroy;
end;

function TPlanetarium_samp.GetEqSys: double;
begin
  result:=2000.0;
end;

procedure TPlanetarium_samp.ProcessDataSyn;
begin
 // todo
end;

function TPlanetarium_samp.Cmd(const Value: string):string;
begin
 // todo
  result:=msgFailed;
end;

procedure TPlanetarium_samp.Shutdown;
begin
 // todo ?? maybe not possible ??
end;

procedure TPlanetarium_samp.Execute;
begin
 try
  SampClient:=TSampClient.Create;
  if Terminated then exit;
  SampClient.appname:='ccdciel';
  SampClient.appdesc:='CCDciel image capture software';
  SampClient.appicon:='https://a.fsdn.com/allura/p/ccdciel/icon';
  SampClient.appdoc:='https://sourceforge.net/projects/ccdciel/';
  SampClient.onClientChange:=@ClientChange;
  SampClient.onDisconnect:=@ClientDisconnected;
  SampClient.oncoordpointAtsky:=@coordpointAtsky;
  SampClient.onImageLoadFits:=@ImageLoadFits;
  if SampClient.SampReadProfile then begin
    if not SampClient.SampHubConnect then DisplayMessage('SAMP '+SampClient.LastError);
    if SampClient.Connected then begin
      DisplayMessage('SAMP connected to '+SampClient.HubUrl);
      if not SampClient.SampHubSendMetadata then DisplayMessage('SAMP '+SampClient.LastError);
      if not SampClient.SampSubscribe(true,false,false) then DisplayMessage('SAMP '+SampClient.LastError);
      DisplayMessage('SAMP listen on port '+inttostr(SampClient.ListenPort));
      FStatus:=true;
      FRunning:=true;
      Synchronize(@SyncOnConnect);
      repeat
        if FClientChange then DoClientChange;
        sleep(200);
      until terminated or (not SampClient.Connected);
    end;
  end else begin
      DisplayMessage('SAMP '+SampClient.LastError);
      Terminate;
  end;
  FStatus:=false;
  FRunning:=false;
  SampClient.onDisconnect:=nil;
 finally
   Synchronize(@SyncOnDisconnect);
   if SampClient.Connected then SampClient.SampHubDisconnect;
   SampClient.free;
   Terminate;
 end;
 if FStartedProgram then StopProgram(FProgramName);
end;

procedure TPlanetarium_samp.Connect(cp1,cp2,cp3,cp4: string; cb1:boolean=False);
begin
 if started then exit;
 started:=true;
 if cb1 and (cp3<>'') then begin
   FProgramPath:=ExtractFilePath(cp3);
   FProgramName:=ExtractFileName(cp3);
   if FProgramName<>'' then begin
     FStartedProgram:=StartProgram(FProgramName,FProgramPath,trim(cp4));
     if FStartedProgram then wait(10);
   end;
 end;
 Start;
end;

procedure TPlanetarium_samp.Disconnect;
begin
 Terminate;
end;

procedure TPlanetarium_samp.ClientDisconnected(Sender: TObject);
begin
 FStatus:=false;
 Synchronize(@SyncOnDisconnect);
 Terminate;
end;

procedure TPlanetarium_samp.coordpointAtsky(cra,cdec:double);
begin
   FRecvData:='coordpointAtsky '+formatfloat(f5,cra)+' '+formatfloat(f5,cdec);
   Fra:=cra/15;
   Fde:=cdec;
   if assigned(FonReceiveData) then FonReceiveData(FRecvData);
end;

procedure TPlanetarium_samp.ImageLoadFits(image_name,image_id,url:string);
begin
   // not subscribed
   DisplayMessage('ImageLoadFits '+image_name+chr(13)+image_id+chr(13)+url);
end;

procedure TPlanetarium_samp.ClientChange(Sender: TObject);
begin
  FClientChange:=true;
end;

procedure TPlanetarium_samp.DoClientChange;
var n: integer;
begin
  FClientChange:=false;
  if SampClient.SampHubGetClientList then begin
     n:=SampClient.Clients.Count;
     if n=0 then DisplayMessage('No SAMP clients')
            else DisplayMessage('SAMP clients: '+inttostr(n));
   end
  else DisplayMessage('SAMP error '+inttostr(SampClient.LastErrorcode)+SampClient.LastError);
end;

function TPlanetarium_samp.ShowImage(fn: string; fovdeg:double=0):boolean;
var client,imgname,imgid,url: string;
begin
  client:=''; // broadcast
  imgname:=ExtractFileNameOnly(fn);
  imgid:='ccdciel_'+imgname;
  url:='file://'+fn;
  result:=SampClient.SampSendImageFits(client,imgname,imgid,url);
end;

function TPlanetarium_samp.DrawFrame(frra,frde,frsizeH,frsizeV,frrot: double):boolean;
begin
  DisplayMessage('Function DrawFrame not supported by this planetarium');
  result:=false;
end;

function TPlanetarium_samp.Search(sname: string; out sra,sde,v_solar,vpa_solar: double): boolean;
begin
  DisplayMessage('Function Search not supported by this planetarium');
  result:=false;
end;

procedure TPlanetarium_samp.ShowAstrometry(sra,sde: double);
begin
  DisplayMessage('Function ShowAstrometry not supported by this planetarium');
end;

procedure TPlanetarium_samp.ClearData;
begin
  FRecvData:='';
  Fra:=NullCoord;
  Fde:=NullCoord;
  Fpa:=NullCoord;
  Fmagn:=NullCoord;
  Fobjname:='';
end;

end.
