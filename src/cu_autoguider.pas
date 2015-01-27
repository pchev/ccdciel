unit cu_autoguider;

{$mode objfpc}{$H+}

interface

uses cu_tcpclient, u_global, u_utils, blcksock, fpjson, jsonparser,
  Forms, Classes, SysUtils;

type

  TNotifyMsg = procedure(msg:string) of object;

  TPHDClient = class(TThread)
  private
    FTargetHost,FTargetPort,FErrorDesc,FRecvData : string;
    FPHDVersion,FMsgVersion,FStatus : String;
    FLockshiftenable,FLockshiftaxes,FLockshiftrate0,FLockshiftrate1,FLockshiftunits: string;
    FTimeout : integer;
    FonShowMessage: TNotifyMsg;
    FonConnect: TNotifyEvent;
    FonConnectError: TNotifyEvent;
    FonDisconnect: TNotifyEvent;
    FonStatusChange: TNotifyEvent;
    TcpClient : TTcpclient;
    procedure DisplayMessagesyn;
    procedure ProcessDataSyn;
    procedure DisplayMessage(msg:string);
    procedure ProcessData(line:string);
    Procedure ProcessEvent(txt:string);
    procedure JsonDataToStringlist(var SK,SV: TStringList; prefix:string; D : TJSONData);
    procedure JsonToStringlist(jsontxt:string; var SK,SV: TStringList);
  public
    Constructor Create;
    procedure Execute; override;
    procedure Send(const Value: string);
  published
    property Terminated;
    property TargetHost : string read FTargetHost write FTargetHost;
    property TargetPort : string read FTargetPort write FTargetPort;
    property Timeout : integer read FTimeout write FTimeout;
    property ErrorDesc : string read FErrorDesc;
    property Status : string read FStatus;
    property onConnect: TNotifyEvent read FonConnect  write FonConnect;
    property onConnectError: TNotifyEvent read FonConnectError  write FonConnectError;
    property onDisconnect: TNotifyEvent read FonDisconnect  write FonDisconnect;
    property onShowMessage: TNotifyMsg read FonShowMessage write FonShowMessage;
    property onStatusChange: TNotifyEvent read FonStatusChange write FonStatusChange;
  end;

implementation

Constructor TPHDClient.Create ;
begin
// start suspended to let time to the main thread to set the parameters
inherited create(true);
freeonterminate:=true;
FTargetHost:='localhost';
FTargetPort:='4400';
FTimeout:=500;
FErrorDesc:='';
FRecvData:='';
end;

procedure TPHDClient.Execute;
var buf:string;
    dateto : double;
    i : integer;
    ending : boolean;
begin
ending:=false;
tcpclient:=TTCPClient.Create;
try
 tcpclient.TargetHost:=FTargetHost;
 tcpclient.TargetPort:=FTargetPort;
 tcpclient.Timeout := FTimeout;
 // connect
 if tcpclient.Connect then begin
   if assigned(FonConnect) then FonConnect(self);
   // main loop
   repeat
     if terminated then break;
     buf:=tcpclient.recvstring;
     if ending and (tcpclient.Sock.LastError<>0) then break; // finish to read data before to exit
     if (buf<>'') then ProcessData(buf);
     if tcpclient.sendbuffer<>'' then begin
        // send command
        tcpclient.Sock.SendString(tcpclient.sendbuffer);
        if tcpclient.Sock.LastError<>0 then begin
           terminate;
           break;
        end;
        tcpclient.sendbuffer:='';
     end;
   until false;
 end
 else begin
   DisplayMessage('Cannot connect to PHD2, Is PHD2 running and the server active?');
   if assigned(FonConnectError) then FonConnectError(self);
 end;
DisplayMessage(tcpclient.GetErrorDesc);
finally
terminate;
tcpclient.Disconnect;
if assigned(FonDisconnect) then FonDisconnect(self);
tcpclient.Free;
end;
end;

procedure TPHDClient.DisplayMessage(msg:string);
begin
FErrorDesc:=msg;
Synchronize(@DisplayMessageSyn);
end;

procedure TPHDClient.DisplayMessageSyn;
begin
if assigned(FonShowMessage) then FonShowMessage(FErrorDesc);
end;

procedure TPHDClient.ProcessData(line:string);
begin
FRecvData:=line;
Synchronize(@ProcessDataSyn);
end;

procedure TPHDClient.ProcessDataSyn;
begin
 ProcessEvent(FRecvData);
end;

procedure TPHDClient.Send(const Value: string);
var dateto:double;
begin
 if Value>'' then begin
   tcpclient.sendbuffer:=tcpclient.sendbuffer+Value+crlf;
 end;
end;

procedure TPHDClient.JsonDataToStringlist(var SK,SV: TStringList; prefix:string; D : TJSONData);
var i:integer;
    pr,buf:string;
begin
if Assigned(D) then begin
  case D.JSONType of
    jtArray,jtObject: begin
        for i:=0 to D.Count-1 do begin
           if D.JSONType=jtArray then begin
              if prefix='' then pr:=IntToStr(I) else pr:=prefix+'.'+IntToStr(I);
              JsonDataToStringlist(SK,SV,pr,D.items[i]);
           end else begin
              if prefix='' then pr:=TJSONObject(D).Names[i] else pr:=prefix+'.'+TJSONObject(D).Names[i];
              JsonDataToStringlist(SK,SV,pr,D.items[i]);
           end;
        end;
       end;
    jtNull: begin
       SK.Add(prefix);
       SV.Add('null');
    end;
    jtNumber: begin
       SK.Add(prefix);
       buf:=floattostr(D.AsFloat);
       SV.Add(buf);
    end
    else begin
       SK.Add(prefix);
       SV.Add(D.AsString);
    end;
 end;
end;
end;

procedure TPHDClient.JsonToStringlist(jsontxt:string; var SK,SV: TStringList);
var  J: TJSONData;
begin
J:=GetJSON(jsontxt);
JsonDataToStringlist(SK,SV,'',J);
J.Free;
end;

Procedure TPHDClient.ProcessEvent(txt:string);
var eventname,rpcid,rpcresult,rpcerror,buf,fullmsg: string;
    PHDVersion,MsgVersion: string;
    attrib,value:Tstringlist;
    p,i:integer;
begin
attrib:=Tstringlist.Create;
value:=Tstringlist.Create;
try
fullmsg:=txt;
JsontoStringlist(txt,attrib,value);
p:=attrib.IndexOf('Event');    // PHD events
if p>=0 then begin
   eventname:=value[p];
   if eventname='GuideStep' then FStatus:='Guiding'
   else if eventname='StartGuiding' then FStatus:='Guiding'
   else if eventname='GuidingStopped' then FStatus:='Stopped'
   else if eventname='StarSelected' then FStatus:='Star Selected'
   else if eventname='StarLost' then FStatus:='Star lost'
   else if eventname='Paused' then FStatus:='Paused'
   else if eventname='Resumed' then FStatus:='Resumed'
   else if eventname='LockPositionSet' then FStatus:='Lock Position Set'
   else if eventname='CalibrationComplete' then FStatus:='Calibration Complete'
   else if eventname='StarSelected' then FStatus:='Star Selected'
   else if eventname='StartCalibration' then FStatus:='Start Calibration'
   else if eventname='CalibrationFailed' then FStatus:='Calibration Failed'
   else if eventname='CalibrationDataFlipped' then FStatus:='Calibration Data Flipped'
   else if eventname='LoopingExposures' then FStatus:='Looping Exposures'
   else if eventname='LoopingExposuresStopped' then FStatus:='Looping Exposures Stopped'
   else if eventname='Settling' then FStatus:='Settling'
   else if eventname='SettleDone' then FStatus:='Settle Done'
   else if eventname='GuidingDithered' then FStatus:='Guiding Dithered'
   else if eventname='LockPositionLost' then FStatus:='Lock Position Lost'
   else if eventname='Alert' then FStatus:='Alert'
   else if eventname='AppState' then begin
     i:=attrib.IndexOf('State');
     FStatus:=value[i];
   end
   else if eventname='Version' then begin
     i:=attrib.IndexOf('PHDVersion');
     if i>=0 then begin
        FPHDVersion:=value[i];
     end;
     i:=attrib.IndexOf('MsgVersion');
     if i>=0 then begin
        FMsgVersion:=value[i];
     end;
   end;
end else begin
  p:=attrib.IndexOf('jsonrpc');   // Response to command
  if p>=0 then begin
     p:=attrib.IndexOf('id');
     rpcid:=value[p];
     i:=attrib.IndexOf('result');
     if i>=0 then rpcresult:=value[i]
     else begin
       i:=attrib.IndexOf('error.code');
       if i>=0 then begin
          rpcresult:='error';
          i:=attrib.IndexOf('error.message');
          if i>=0 then rpcerror:=value[i]
                  else rpcerror:='no response';
       end
       else rpcresult:='ok';
     end;
     if rpcid='1000' then begin   // get_app_state
       if rpcresult='error' then begin
          if assigned(FonShowMessage) then FonShowMessage('get_app_state'+' '+rpcresult+' '+rpcerror);
       end else FStatus:=rpcresult;
     end
     else if rpcid='1001' then begin  // set_lock_shift_params
       if rpcresult='error' then begin
          if assigned(FonShowMessage) then FonShowMessage('set_lock_shift_params'+' '+rpcresult+' '+rpcerror);
       end;
     end
     else if rpcid='1002' then begin  // set_lock_shift_enabled
       if rpcresult='error' then begin
          if assigned(FonShowMessage) then FonShowMessage('set_lock_shift_enabled'+' '+rpcresult+' '+rpcerror);
       end;
     end
     else if rpcid='1003' then begin  // get_lock_shift_params
       if rpcresult='error' then begin
          if assigned(FonShowMessage) then FonShowMessage('get_lock_shift_params'+' '+rpcresult+' '+rpcerror);
       end else begin
         i:=attrib.IndexOf('result.enabled');
         if i>=0 then begin
           FLockshiftenable:=value[i];
         end
         else FLockshiftenable:='?';
         i:=attrib.IndexOf('result.axes');
         if i>=0 then FLockshiftaxes:=value[i] else FLockshiftaxes:='?';
         buf:='?';
         i:=attrib.IndexOf('result.rate.0');
         if i>=0 then FLockshiftrate0:=value[i];
         i:=attrib.IndexOf('result.rate.1');
         if i>=0 then FLockshiftrate1:=value[i];
         i:=attrib.IndexOf('result.units');
         if i>=0 then FLockshiftunits:=value[i] else FLockshiftunits:='?';
       end;
     end
     else if rpcid='1004' then begin  // get_lock_shift_enabled
       if rpcresult='error' then begin
          if assigned(FonShowMessage) then FonShowMessage('get_lock_shift_enabled'+' '+rpcresult+' '+rpcerror);
       end else
         FLockshiftenable:=rpcresult;
     end;
  end;
end;
if Assigned(FonStatusChange) then FonStatusChange(self);
finally
 attrib.Free;
 value.Free;
end;
end;

end.

