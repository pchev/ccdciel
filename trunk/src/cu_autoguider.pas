unit cu_autoguider;

{$mode objfpc}{$H+}

interface

uses cu_tcpclient, u_global, blcksock, fpjson, jsonparser,
  Forms, Classes, SysUtils;

type

  TNotifyMsg = procedure(msg:string) of object;

  TPHDClient = class(TThread)
  private
    FTargetHost,FTargetPort,FErrorDesc,FRecvData : string;
    FPHDVersion,FMsgVersion,FStatus : String;
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
    procedure ConnectGear;
    procedure Calibrate;
    procedure Guide(onoff:boolean; recalibrate:boolean=false);
    procedure Pause(onoff:boolean);
    procedure Dither;
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
begin
writeln('>>'+Value);
 if Value>'' then begin
   tcpclient.Sock.SendString(Value+crlf);
   if tcpclient.Sock.LastError<>0 then begin
      Terminate;
   end;
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
var eventname,rpcid,rpcresult,rpcerror: string;
    attrib,value:Tstringlist;
    p,i:integer;
begin
writeln('=='+txt);
attrib:=Tstringlist.Create;
value:=Tstringlist.Create;
try
JsontoStringlist(txt,attrib,value);
p:=attrib.IndexOf('Event');    // PHD events
if p>=0 then begin
   eventname:=value[p];
   if (eventname='GuideStep')and(FStatus<>'Settling') then FStatus:='Guiding'
   else if eventname='StartGuiding' then FStatus:='Start Guiding'
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
     if rpcid='1000' then begin  // set_connected
       if rpcresult='error' then begin
          if assigned(FonShowMessage) then FonShowMessage('set_connected'+' '+rpcresult+' '+rpcerror);
       end;
     end
     else if rpcid='1001' then begin   // get_app_state
       if rpcresult='error' then begin
          if assigned(FonShowMessage) then FonShowMessage('get_app_state'+' '+rpcresult+' '+rpcerror);
       end else FStatus:=rpcresult;
     end
     else if rpcid='2002' then begin  // set_paused
       if rpcresult='error' then begin
          if assigned(FonShowMessage) then FonShowMessage('set_paused'+' '+rpcresult+' '+rpcerror);
       end;
     end
     else if rpcid='2003' then begin  // guide
       if rpcresult='error' then begin
          if assigned(FonShowMessage) then FonShowMessage('guide'+' '+rpcresult+' '+rpcerror);
       end;
     end
     else if rpcid='2004' then begin  // stop_capture
       if rpcresult='error' then begin
          if assigned(FonShowMessage) then FonShowMessage('stop_capture'+' '+rpcresult+' '+rpcerror);
       end;
     end
     else if rpcid='2010' then begin  // dither
       if rpcresult='error' then begin
          if assigned(FonShowMessage) then FonShowMessage('dither'+' '+rpcresult+' '+rpcerror);
       end;
     end;

  end;
end;
if Assigned(FonStatusChange) then FonStatusChange(self);
finally
 attrib.Free;
 value.Free;
end;
end;

procedure TPHDClient.ConnectGear;
var buf:string;
begin
 buf:='{"method": "set_connected", "params": [';
 buf:=buf+'true';
 buf:=buf+'], "id": 1000}';
 Send(buf);
// buf:='{"method": "get_app_state", "id": 1001}';
// Send(buf);
end;

procedure TPHDClient.Calibrate;
begin
  Guide(true,true);
end;

procedure TPHDClient.Guide(onoff:boolean; recalibrate:boolean=false);
var buf:string;
begin
  if onoff then begin
    buf:='{"method": "guide", "params": [';
    buf:=buf+'{"pixels": '+'1.0'+',';      // settle tolerance
    buf:=buf+'"time": 5,';                 // min time
    buf:=buf+'"timeout": '+'30'+'},';     // max time
    if recalibrate then buf:=buf+'true' else buf:=buf+'false';
    buf:=buf+'],';
    buf:=buf+'"id": 2003}';
    Send(buf);
  end else begin
    buf:='{"method": "stop_capture","id":2004}';
    Send(buf);
  end;
end;

procedure TPHDClient.Pause(onoff:boolean);
var buf:string;
begin
  if onoff then begin
    buf:='{"method": "set_paused","params":[true,"full"],"id":2002}';
    Send(buf);
  end else begin
    buf:='{"method": "set_paused","params":[false],"id":2002}';
    Send(buf);
  end;
end;

procedure TPHDClient.Dither;
var buf:string;
begin
  buf:='{"method": "dither", "params": [';
  buf:=buf+'5'+',';                    // pixels
  buf:=buf+'true,';                    // ra only
  buf:=buf+'{"pixels": '+'1.0'+',';      // settle tolerance
  buf:=buf+'"time": 5,';                 // min time
  buf:=buf+'"timeout": '+'30'+'}],';     // max time
  buf:=buf+'"id": 2010}';
  Send(buf);
end;

end.

