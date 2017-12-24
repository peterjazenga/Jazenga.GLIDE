{$INCLUDE wst_global.inc}  
unit wst_initialization;

interface

uses
  Classes, SysUtils; 

type

  TwstInitializationProc = procedure();
  TwstFinalizationProc = TwstInitializationProc;
  
  procedure wst_initialize();
  procedure wst_add_init_proc(const AProc : TwstInitializationProc);
  procedure wst_add_final_proc(const AProc : TwstFinalizationProc);
  procedure wst_add_procs(const AInitProc : TwstInitializationProc; const AFinalProc : TwstFinalizationProc);
  procedure wst_finalize();
  

implementation

var
  Initialized : Boolean = False;
  InitProcs : TList;
  FinalProcs : TList;
  
procedure wst_prepare();
begin
  if ( InitProcs = nil ) then begin
    Initialized := False;
    InitProcs := TList.Create();
    FinalProcs := TList.Create();
  end;
end;

procedure wst_add_init_proc(const AProc : TwstInitializationProc);
begin
  wst_prepare();
  if Assigned(AProc) and ( InitProcs.IndexOf(@AProc) = -1 ) then
    InitProcs.Add(@AProc);
end;

procedure wst_add_final_proc(const AProc : TwstFinalizationProc);
begin
  wst_prepare();
  if ( @AProc <> nil ) and ( FinalProcs.IndexOf(@AProc) = -1 ) then
    FinalProcs.Add(@AProc);
end;

procedure wst_add_procs(const AInitProc : TwstInitializationProc; const AFinalProc : TwstFinalizationProc);
begin
  wst_add_init_proc(AInitProc);
    wst_add_final_proc(AFinalProc);
end;

procedure wst_initialize();
var
  i, c : Integer;
  p : TwstInitializationProc;
begin
  wst_prepare();
  Initialized := True;
  c := InitProcs.Count;
  for i := 0 to Pred(c) do begin
    p := TwstInitializationProc(InitProcs[i]);
    p();
  end;
end;

procedure wst_finalize();
var
  i : Integer;
  p : TwstInitializationProc;
begin
  if Initialized then begin
    Initialized := False;
    if ( FinalProcs <> nil ) then begin
      i := FinalProcs.Count - 1;
      while (i >= 0) do begin
        p := TwstFinalizationProc(FinalProcs[i]);
        p();
        Dec(i);
      end;
    end;
  end;
  FreeAndNil(FinalProcs);
  FreeAndNil(InitProcs);
end;

initialization
finalization
  wst_finalize();
  
end.
