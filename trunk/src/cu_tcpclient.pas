unit cu_tcpclient;

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

function TTCPclient.GetErrorDesc: string;
begin
  Result := FSock.GetErrorDesc(FSock.LastError);
end;

end.

