{**********************************************************************
 Package pl_ExDesign
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit ELDsgUtils;

{$I ELCmps.inc}

interface

uses
 // Windows,
  LCLType, LCLIntf,
  Classes, SysUtils;

  { Quick sort }

type
  TELQSCompareProc = function(AItemIndex1, AItemIndex2: Integer; AData:Pointer): Integer;
  TELQSExchangeProc = procedure(AItemIndex1, AItemIndex2: Integer; AData:Pointer);

procedure ELQuickSort(ALow, AHigh: Integer; ACompareProc: TELQSCompareProc; AExchangeProc: TELQSExchangeProc;AData:Pointer);

  { Hint hooking }

type

TELHintHookProc = procedure(var AMsg: TMsg) of object;

procedure ELHookHint(AHookProc: TELHintHookProc);
procedure ELUnhookHint(AHookProc: TELHintHookProc);

implementation

var
  ELHintHook: HHOOK;
  ELHintHooksProcs: TList;


 
procedure ELQuickSort(ALow, AHigh: Integer; ACompareProc: TELQSCompareProc; AExchangeProc: TELQSExchangeProc;
          AData:Pointer);

  procedure _Sort(AFromIndex, AToIndex: Integer);
  var
    LI, LJ, LCenter: Integer;
  begin
    repeat
      LI := AFromIndex;
      LJ := AToIndex;
      LCenter := (AFromIndex + AToIndex) div 2;
      repeat
        while ACompareProc(LI, LCenter, AData) < 0 do Inc(LI);
        while ACompareProc(LJ, LCenter, AData) > 0 do Dec(LJ);
        if LI <= LJ then
        begin
          AExchangeProc(LI, LJ, AData);
          if LCenter = LI then
            LCenter := LJ
          else if LCenter = LJ then
            LCenter := LI;
          Inc(LI);
          Dec(LJ);
        end;
      until LI > LJ;
      if AFromIndex < LJ then _Sort(AFromIndex, LJ);
      AFromIndex := LI;
    until LI >= AToIndex;
  end;

begin
  if AHigh > ALow then
    _Sort(ALow, AHigh);
end;

function ELHintHookMsgProc(ACode: Integer; AParam: Longint; var AMsg: TMsg): Longint; stdcall;
var
  LI: Integer;
  LList: TList;
begin
  Result := CallNextHookEx(ELHintHook, ACode, AParam, Longint(@AMsg));
  if ELHintHooksProcs <> nil then
  begin
    LList := TList.Create;
    try
      for LI := 0 to ELHintHooksProcs.Count - 1 do
        LList.Add(ELHintHooksProcs[LI]);
        for LI := 0 to LList.Count - 1 do
          TELHintHookProc(LList[LI]^)(AMsg);

    finally
      LList.Free;
    end;
  end;
end;

 
procedure ELClearHintHooks;
var
  LProc: ^TELHintHookProc;
  LI: Integer;
begin
  if ELHintHooksProcs <> nil then
  begin
    for LI := 0 to ELHintHooksProcs.Count - 1 do
    begin
      LProc := ELHintHooksProcs[LI];
      Dispose(LProc);

    end;
    ELHintHooksProcs.Free;
    ELHintHooksProcs := nil;
    if ELHintHook <> 0 then UnhookWindowsHookEx(ELHintHook);
    ELHintHook := 0;
  end;
end;


procedure ELHookHint(AHookProc: TELHintHookProc);
var
  LProc: ^TELHintHookProc;
begin
  if ELHintHooksProcs = nil then
    ELHintHooksProcs := TList.Create;
  New(LProc);
  LProc^ := AHookProc;
  ELHintHooksProcs.Add(LProc);

  if ELHintHook = 0 then
    ELHintHook := SetWindowsHookEx(WH_GETMESSAGE, @ELHintHookMsgProc,
      0, GetCurrentThreadID);

end;

 
procedure ELUnhookHint(AHookProc: TELHintHookProc);
var
  LProc: ^TELHintHookProc;
  LI: Integer;
begin
  if ELHintHooksProcs <> nil then
  begin
    for LI := 0 to ELHintHooksProcs.Count - 1 do
    begin
      LProc := ELHintHooksProcs[LI];
      if (TMethod(LProc^).Code = TMethod(AHookProc).Code) and
        (TMethod(LProc^).Data = TMethod(AHookProc).Data) then
      begin
        Dispose(LProc);
        ELHintHooksProcs.Delete(LI);
        Break;
      end;
     
    end;
    if ELHintHooksProcs.Count = 0 then
    begin
      ELHintHooksProcs.Free;
      ELHintHooksProcs := nil;
      if ELHintHook <> 0 then UnhookWindowsHookEx(ELHintHook);
      ELHintHook := 0;
    end;
  end;
end;

initialization

finalization
  ELClearHintHooks;

end.
