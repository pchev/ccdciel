unit cu_cdcclient;

{$MODE Delphi}

{                                        
Copyright (C) 2015 Patrick Chevalley

http://www.ap-i.net
pch@ap-i.net

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
}
{
  TCP/IP client object thread to connect to Cartes du Ciel
}

interface

uses cu_tcpclient, blcksock, Classes, SysUtils,
    Forms;

type

  TNotifyMsg = procedure(msg:string) of object;

  TCdCClient = class(TThread)
  private
    FTargetHost,FTargetPort,FErrorDesc,FRecvData,FClientId,FClientName : string;
    FTimeout : integer;
    FCmdTimeout : double;
    procedure SetCmdTimeout(value:double);
    function GetCmdTimeout:double;
    procedure DisplayMessagesyn;
    procedure ProcessDataSyn;
    procedure DisplayMessage(msg:string);
    procedure ProcessData(line:string);
  public
    FonShowMessage: TNotifyMsg;
    FonReceiveData: TNotifyMsg;
    FonConnect: TNotifyEvent;
    FonConnectError: TNotifyEvent;
    FonDisconnect: TNotifyEvent;
    TcpClient : TTcpclient;
    FTag:Integer;
    Constructor Create;
    procedure Execute; override;
    function Send(const Value: string):string;
  published
    property Terminated;
    property TargetHost : string read FTargetHost write FTargetHost;
    property TargetPort : string read FTargetPort write FTargetPort;
    property Timeout : integer read FTimeout write FTimeout;
    property CmdTimeout: double read GetCmdTimeout write SetCmdTimeout;
    property Tag: integer read FTag write FTag;
    property ErrorDesc : string read FErrorDesc;
    property RecvData : string read FRecvData;
    property ClientId : string read FClientId;
    property ClientName : string read FClientName;
    property onConnect: TNotifyEvent read FonConnect  write FonConnect;
    property onConnectError: TNotifyEvent read FonConnectError  write FonConnectError;
    property onDisconnect: TNotifyEvent read FonDisconnect  write FonDisconnect;
    property onShowMessage: TNotifyMsg read FonShowMessage write FonShowMessage;
    property onReceiveData: TNotifyMsg read FonReceiveData write FonReceiveData;
  end;

const msgTimeout='Timeout!';
      msgOK='OK';
      msgFailed='Failed!';
      msgBye='Bye!';

implementation

/////////////////// TCdCClient ///////////////////////////

Constructor TCdCClient.Create ;
begin
// start suspended to let time to the main thread to set the parameters
inherited create(true);
freeonterminate:=true;
FTag:=0;
FTargetHost:='localhost';
FTargetPort:='3292';
FTimeout:=500;
FCmdTimeout:=10/86400;
FErrorDesc:='';
FRecvData:='';
FClientId:='';
FClientName:='';
end;

procedure TCdCClient.Execute;
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
    // wait connect message
    dateto:=now+Fcmdtimeout;
    repeat
      buf:=tcpclient.recvstring;
      if (buf='')or(buf='.') then continue;  // keepalive
      if copy(buf,1,1)='>' then ProcessData(buf) // mouse click
      else
        if copy(buf,1,2)=msgOK then begin
           // success, parse response
           ProcessData(buf);
           i:=pos('id=',buf);
           Fclientid:=trim(copy(buf,i+3,2));
           i:=pos('chart=',buf);
           buf:=copy(buf,i+6,999)+' ';
           i:=pos(' ',buf);
           FclientName:=trim(copy(buf,1,i-1));
           if assigned(FonConnect) then FonConnect(self);
           break;
        end else begin
           // failure, close thread
           ProcessData(buf);
           terminate;
           break;
        end;
   until now>dateto;
   if tcpclient.resultbuffer='' then tcpclient.resultbuffer:=msgTimeout;
   DisplayMessage(tcpclient.GetErrorDesc);
   // main loop
   repeat
     if terminated then break;
     // handle unattended messages (mouseclick...)
     buf:=tcpclient.recvstring;
     if ending and (tcpclient.Sock.LastError<>0) then break; // finish to read data before to exit
     if (buf<>'')and(buf<>'.') then ProcessData(buf);
     if buf=msgBye then ending:=true;
     // handle synchronous command and response
     if tcpclient.sendbuffer<>'' then begin
        tcpclient.resultbuffer:='';
        // send command
        tcpclient.Sock.SendString(tcpclient.sendbuffer+crlf);
        if tcpclient.Sock.LastError<>0 then begin
           terminate;
           break;
        end;
        tcpclient.sendbuffer:='';
        // wait response
        dateto:=now+Fcmdtimeout;
        repeat
          buf:=tcpclient.recvstring;
          if (buf='')or(buf='.') then continue;  // keepalive
          if copy(buf,1,1)='>' then ProcessData(buf) // mouse click
             else tcpclient.resultbuffer:=buf;   // set result
        until (tcpclient.resultbuffer>'')or(now>dateto);
        if tcpclient.resultbuffer='' then tcpclient.resultbuffer:=msgTimeout;
     end;
   until false;
 end
 else begin
   DisplayMessage('Cannot connect to CdC, Is CdC running and the server active?');
   if assigned(FonConnectError) then FonConnectError(self);
 end;
DisplayMessage(tcpclient.GetErrorDesc);
finally
terminate;
tcpclient.Disconnect;
tcpclient.Free;
end;
end;

procedure TCdCClient.SetCmdTimeout(value:double);
begin
 FCmdTimeout := value / 86400;  // store timeout value in days
end;

function TCdCClient.GetCmdTimeout:double;
begin
 result := FCmdTimeout * 86400;
end;

procedure TCdCClient.DisplayMessage(msg:string);
begin
FErrorDesc:=msg;
Synchronize(DisplayMessageSyn);
end;

procedure TCdCClient.DisplayMessageSyn;
begin
if assigned(FonShowMessage) then FonShowMessage(FErrorDesc);
end;

procedure TCdCClient.ProcessData(line:string);
begin
FRecvData:=line;
Synchronize(ProcessDataSyn);
end;

procedure TCdCClient.ProcessDataSyn;
begin
if assigned(FonReceiveData) then FonReceiveData(FRecvData);
end;

function TCdCClient.Send(const Value: string):string;
// this function is called in the main thread only!
var dateto:double;
begin
 tcpclient.resultbuffer:='';
 if Value>'' then begin
   tcpclient.sendbuffer:=Value;
   // set a double timeout just in case Execute is no more running.
   dateto:=now+2*Fcmdtimeout;
   while (tcpclient.resultbuffer='')and(now<dateto) do application.ProcessMessages;
 end;
 if tcpclient.resultbuffer='' then tcpclient.resultbuffer:=msgTimeout;
 result:=tcpclient.resultbuffer;
end;

end.
