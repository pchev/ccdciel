unit cu_tcpclient;

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

{$mode objfpc}{$H+}

interface

uses blcksock,
  Classes, SysUtils;

type
  TTCPclient = class(TSynaClient)
  private
    FSock: TTCPBlockSocket;
    FSendBuffer,FResultBuffer: string;
  public
    constructor Create;
    destructor Destroy; override;
    function Connect: Boolean;
    procedure Disconnect;
    function RecvString: string;
    function RecvPacket: string;
    function GetErrorDesc: string;
  published
    property Sock: TTCPBlockSocket read FSock;
    property SendBuffer: string read FSendBuffer write FSendBuffer;
    property ResultBuffer: string read FResultBuffer write FResultBuffer;
  end;


implementation

/////////////////// TTCPclient ///////////////////////////

constructor TTCPclient.Create;
begin
  inherited Create;
  FSock := TTCPBlockSocket.Create;
  Fsendbuffer:='';
  Fresultbuffer:='';
end;

destructor TTCPclient.Destroy;
begin
  FSock.Free;
  inherited Destroy;
end;

function TTCPclient.Connect: Boolean;
begin
  FSock.CloseSocket;
  FSock.LineBuffer := '';
  FSock.Bind(FIPInterface, cAnyPort);
  FSock.Connect(FTargetHost, FTargetPort);
  Result := FSock.LastError = 0;
end;

procedure TTCPclient.Disconnect;
begin
  FSock.CloseSocket;
end;

function TTCPclient.RecvString: string;
begin
  Result := FSock.RecvTerminated(FTimeout, crlf);
end;

function TTCPclient.RecvPacket: string;
begin
  Result := FSock.RecvPacket(FTimeout);
end;

function TTCPclient.GetErrorDesc: string;
begin
  Result := FSock.GetErrorDesc(FSock.LastError);
end;

end.

