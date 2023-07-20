unit CTimer;

{$mode objfpc}{$H+}

interface

uses
  Classes,
{$IFDEF DEBUG}
  {$IFDEF MSWINDOWS}
  messages,
  {$ENDIF}
  {$IFDEF LINUX}
  LMessages,
  {$ENDIF}
{$ENDIF}
  SysUtils,
  {$IFDEF DEBUG}
  uSimplelogger,
  {$ENDIF}
  syncobjs;

{$IFDEF DEBUG}
const
  {$IFDEF MSWINDOWS}
  USER_TRACE = WM_USER + 1;
  {$ENDIF}
  {$IFDEF LINUX}
  USER_TRACE = LM_USER + 1;
  {$ENDIF}
{$ENDIF}

type
  TOnTimerEvent = procedure (const Sender: TObject) of object;

  { TTimerThread }

  TTimerThread = class(TTHread)
  private
    FInterval: Cardinal;
    FEnabled: Boolean;
    FActive: Boolean;
    FEnableEvent: TEvent;
    FID: Integer;
    FOnTimer: TOnTimerEvent;
    {$IFDEF DEBUG}
    FTraceHandle: THandle;
    {$ENDIF}

    procedure SetEnabled(AValue: Boolean);
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    property Active: Boolean read FActive;
    property EnableEvent: TEvent read FEnableEvent;
    property ID: Integer read FID write FID;
    property Interval: Cardinal read FInterval write FInterval;
    property Enabled: Boolean read  FEnabled write SetEnabled;
    property OnTimer: TOnTimerEvent read FOnTimer write FOnTimer;
    {$IFDEF DEBUG}
    property TraceHandle: THandle read FTraceHandle write FTraceHandle;
    {$ENDIF}
  end;

  { TControlTimer }

  { TCustomControlTimer }

  TCustomControlTimer = class(TComponent)
  private
    FTH: TTimerThread;
    // interval du timer en milli-secondes
    FInterval: Cardinal;
    FOnTimer: TOnTimerEvent;
    FID: Integer;
    {$IFDEF DEBUG}
    FTraceHandle: THandle;
    {$ENDIF}

    function GetEnabled: Boolean;
    procedure SetEnabled(AValue: Boolean);
    function GetInterval: Cardinal;
    procedure SetInterval(AValue: Cardinal);
    procedure SetOnTimer(AValue: TOnTimerEvent);
    procedure SetID(AValue: Integer);
    {$IFDEF DEBUG}
    procedure SetTraceHandle(AValue: THandle);
    {$ENDIF}
  protected
    procedure DoTimerEvent(const Sender: TObject);
    {$IFDEF DEBUG}
    procedure PostMsg(const AText: String);
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ID: Integer read FID write SetID;
    property Interval: Cardinal read GetInterval write SetInterval;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property OnTimer: TOnTimerEvent read FOnTimer write SetOnTimer;
    {$IFDEF DEBUG}
    property TraceHandle: THandle read FTraceHandle write SetTraceHandle;
    {$ENDIF}
  published
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  windows,
  {$ENDIF}
  LCLIntf;

constructor TTimerThread.Create;
begin
  Inherited Create(True);
  FreeOnTerminate := False;
  FEnabled := False;
  FInterval := 1000;
  FActive := False;
  FID := 0;
  FEnableEvent := TEvent.Create(nil, True, True, EmptyStr);
  {$IFDEF DEBUG}
  AddLogInfo('New internal Timer created');
  {$ENDIF}
end;

destructor TTimerThread.Destroy;
begin
  FreeAndNil(FEnableEvent);
  {$IFDEF DEBUG}
  AddLogInfo(Format('Timer %d destroyed', [FID]));
  {$ENDIF}
  inherited Destroy;
end;

procedure TTimerThread.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then Exit;
  FEnabled := AValue;
  if AValue then begin
    // unlock the waiting loop event so the timer can restart
    {$IFDEF DEBUG}
    AddLogInfo(Format('Internal Timer %d enabled', [FID]));
    {$ENDIF}
    FEnableEvent.SetEvent
  end else begin
    {$IFDEF DEBUG}
    AddLogInfo(Format('Internal Timer %d disabled', [FID]));
    {$ENDIF}
    // force the timer thread loop to enter a wait state
    FEnableEvent.ResetEvent;
  end
end;

procedure TTimerThread.Execute;
var
  wStart: Int64;
  wElapsed: Int64;
  wSleepTime: Integer = 10;
  wWaitForAwakeTimeOut: Integer = 500;
  EventResult: TWaitResult;
begin
  { run into the loop as long as Interval isn't exhausted
  when there is no more time, fire the event if any and reser de elapsed count

  normally, the first line in the event code should be
    Timer.Enabled := False;

  the loop is an endless loop until the timer is reset and destroyed. In the mean time it never stops the main loop
  the loop has two possible states depending on the Enabled property: either it is True or False
  if the Enabled property is False:
    - the timer still runs with will never fire the timer event
    - will wait until either the Terminated is set or the Enabled property changes to True and unlocks the FEnableEvent
    - in the latter case, the elapsed is reset an the times becomes active again
  If the Enabled property is True:
    - count the elapsed time every wSleepTime (this is the granularity of the timer)
    - when the elapsed time becomes greater than the Interval:
        fire the timer event if any
        reset the elapsed time
    - of course, in all cases, break the loop in case of Terminated is True
  }

  {$IFDEF DEBUG}
  AddLogInfo(Format('Timer %d on the road', [FID]));
  {$ENDIF}
  wStart := GetTickCount64;
  FActive := True;
  while not Terminated do begin
    if not FEnabled then begin
      EventResult := FEnableEvent.WaitFor(wWaitForAwakeTimeOut);
      // waiting for the restart
      while EventResult = wrTimeout do
        EventResult := FEnableEvent.WaitFor(wWaitForAwakeTimeOut);
      if Terminated then
        Break;
      if EventResult = wrSignaled then begin
        // reset elapsed for next start
        {$IFDEF DEBUG}
        AddLogInfo(Format('Internal Timer %d on the road again', [FID]));
        {$ENDIF}
        wStart := GetTickCount64;
        Continue
      end;
      // si on arrive là, c'est qu'on a une grosse bièce: on tue et on sen va
      Break
    end;
    wElapsed := GetTickCount64 - wStart;
    if wElapsed >= FInterval then begin
      // on a atteint le temps imparti et on exécute l'event s'il y en a un
      if Assigned(FOnTimer) then begin
        {$IFDEF DEBUG}
        AddLogInfo(Format('Internal Timer %d fires the event', [FID]));
        {$ENDIF}
        FOnTimer(Self);
      end;
      // là on peut avoir un Terminated ou un Enabled à False
      if Terminated then
        Break;
      // disabling the timer will be processed on next loop
      if not FEnabled then
        Continue;
      // reset for next event to fire
      wStart := GetTickCount64;
    end;
    // if we get there, nothing to do but sleep
    Sleep(wSleepTime);
  end;
  FActive := False;
end;

{ TControlTimer }

constructor TCustomControlTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF DEBUG}
  AddLogInfo('New Control Timer');
  {$ENDIF}
  FTH := TTimerThread.Create;
  FTH.FreeOnTerminate := False;
end;

destructor TCustomControlTimer.Destroy;
var
  Elapsed: Cardinal;
  ThreadTimeout: Cardinal = 100;
begin
  {$IFDEF DEBUG}
  AddLogInfo(Format('Control Timer %d is being destroyed', [FID]));
  {$ENDIF}
  if Assigned(FTH) then begin
    if FTH.Active then begin
      // unlock any waiting sequence
      FTH.EnableEvent.SetEvent;
      FTH.Terminate;
    end;
    Elapsed := 0;
    while FTH.Active do begin
      WaitForThreadTerminate(FTH.Handle, ThreadTimeout);
      if Elapsed > 5000 then
        Break;
      Inc(Elapsed, ThreadTimeout)
    end;
    {$IFDEF DEBUG}
    if FTH.Active then
      AddLogInfo(Format('TH %d still active', [FID]));
    AddLogInfo(Format('Control Timer %d finally destroyed', [FID]));
    {$ENDIF}
    FreeAndNil(FTH);
  end;
  Inherited
end;

function TCustomControlTimer.GetEnabled: Boolean;
begin
  Result := FTH.Enabled;
end;

procedure TCustomControlTimer.SetEnabled(AValue: Boolean);
begin
  if (not Assigned(FTH)) or (AValue = FTH.Enabled) then
    Exit;
  FTH.Enabled := AValue;
  if AValue then begin
    // start timer thread
    {$IFDEF DEBUG}
    AddLogInfo(Format('Control timer %d enabled', [FID]));
    {$ENDIF}
    FTH.Interval := FInterval;
    if not FTH.Active then
      FTH.Start
    else
      FTH.EnableEvent.SetEvent;
    FTH.Enabled := True
  end else begin
    // disable the timer thread
    {$IFDEF DEBUG}
    AddLogInfo(Format('Control timer %d disabled', [FID]));
    {$ENDIF}
    FTH.Enabled := False
  end;
end;

function TCustomControlTimer.GetInterval: Cardinal;
begin
  Result := FInterval;
end;

procedure TCustomControlTimer.SetInterval(AValue: Cardinal);
begin
  if (not Assigned(FTH)) or (AValue = FInterval) then
    Exit;
  FInterval := AValue;
  FTH.Interval := AValue;
end;

procedure TCustomControlTimer.SetOnTimer(AValue: TOnTimerEvent);
begin
  if (not Assigned(FTH)) or (FOnTimer = AValue) then
    Exit;
  FOnTimer := AValue;
  if not Assigned(AValue) then
    FTH.OnTimer := nil
  else
    FTH.OnTimer := AValue;
end;

procedure TCustomControlTimer.DoTimerEvent(const Sender: TObject);
begin
  // this the event callback
  if Assigned(FOnTimer) then
    FOnTimer(Self);
end;

procedure TCustomControlTimer.SetID(AValue: Integer);
begin
  if FID = AValue then Exit;
  FID := AValue;
  if Assigned(FTH) then
    FTH.ID := FID;
end;

{$IFDEF DEBUG}
procedure TCustomControlTimer.PostMsg(const AText: String);
var
  PMsgText: PChar=nil;
begin
  if FTraceHandle = 0 then
    Exit;
  ReAllocMem(PMsgText, Length(AText) + 1);
  Move(PChar(AText)^, PMsgText^, Length(AText));
  (PMsgText + Length(AText))^ := #0;
  PostMessage(FTraceHandle, USER_TRACE, PtrInt(PMsgText), 0)
end;

procedure TCustomControlTimer.SetTraceHandle(AValue: THandle);
begin
  if FTraceHandle = AValue then Exit;
  FTraceHandle := AValue;
  if Assigned(FTH) then
    FTH.TraceHandle := FTraceHandle;
end;
{$ENDIF}

end.
