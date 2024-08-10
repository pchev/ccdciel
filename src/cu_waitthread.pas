unit cu_waitthread;

{
Copyright (C) 2024 Patrick Chevalley

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

{$mode ObjFPC}{$H+}

interface

uses cthreads,
  Classes, SysUtils, Forms;

type

// A thread that wait the specified time before to
// execute a method in main thread.
TwaitThread=class(TThread)
  private
    Fwaittime : integer;
    Fdata: PtrInt;
    FonTerminated: TDataEvent;
    procedure EndWait;
  protected
    procedure Execute; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    // the procedure to execute
    property onTerminated: TDataEvent read FonTerminated write FonTerminated;
    // the time to wait (ms)
    property waittime : integer read Fwaittime write Fwaittime;
    // eventual data pointer
    property data: PtrInt read Fdata write Fdata;
end;

// similar to queueasynccall() but only execute after a given time
procedure WaitExecute(ms: integer; AMethod: TDataEvent; data:PtrInt);

implementation

constructor TWaitThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  Fwaittime:=0;
  FonTerminated:=nil;
  Fdata:=0;
end;

destructor TWaitThread.Destroy;
begin
  // data is not destroyed here, must be done in the data event.
  inherited;
end;

procedure TWaitThread.Execute;
begin
  sleep(Fwaittime);
  Synchronize(@EndWait);
end;

procedure TWaitThread.endwait;
begin
  if Assigned(FonTerminated) then
    FonTerminated(data);
end;

// similar to queueasynccall() but only execute after a given time
procedure WaitExecute(ms: integer; AMethod: TDataEvent; data:PtrInt);
var wt:TwaitThread;
begin
   wt:=TwaitThread.Create;
   wt.waittime:=ms;
   wt.data:=data;
   wt.onTerminated:=AMethod;
   wt.start;
end;

end.

