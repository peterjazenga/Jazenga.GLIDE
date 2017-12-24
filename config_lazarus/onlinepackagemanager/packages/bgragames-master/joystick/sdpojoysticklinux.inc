
procedure TSdpoJoystick.Update;
begin
  // only available in Win32
end;

procedure TSdpoJoystick.Read(var LocalAxis, LocalButtons: array of integer);
var i: integer;
begin
  if Active then begin
    for i:=0 to min(length(LocalAxis)-1,MaxAxis-1) do begin
      LocalAxis[i] := lAxis[i];
    end;
    for i:=0 to min(length(LocalButtons)-1,MaxButtons-1) do begin
      LocalButtons[i] := lButs[i];
    end;
  end;
end;

function TSdpoJoystick.GetAxis(idx: integer): integer;
begin
  if Active then begin
    if idx>MaxAxis-1 then begin
      raise Exception.Create('Invalid axis '+IntToStr(idx));
      exit;
    end;

    result := lAxis[idx];
  end else begin
    raise Exception.Create('Joystick unavailable');
  end;
end;

function TSdpoJoystick.GetButtons(idx: integer): integer;
begin
  if Active then begin
    if idx>MaxButtons-1 then begin
      raise Exception.Create('Invalid button '+IntToStr(idx));
      exit;
    end;

    result := lButs[idx];
  end else begin
    raise Exception.Create('Joystick unavailable');
  end;
end;

procedure TSdpoJoystick.SetActive(state: boolean);
begin
  if state=FActive then exit;

  if state then DeviceOpen
  else DeviceClose;
end;

procedure TSdpoJoystick.DeviceOpen;
var joypath: string;
begin
  joypath:=FLinDevice;
  if OpenJoystick(joypath)<0 then begin
    //Close;
    FActive := false;
    raise Exception.Create('Could not open Joystick '+FLinDevice);
    exit;
  end;
  
  FActive:=true;
  JoystickThread := TSdpoJoystickThread.Create(True); // With the True parameter it doesn't start automatically
  if Assigned(JoystickThread.FatalException) then
    raise JoystickThread.FatalException;

  // Here the code initialises anything required before the threads starts executing
  JoystickThread.Owner := Self;
  JoystickThread.Resume;
end;

procedure TSdpoJoystick.DeviceClose;
begin
  CloseJoystick;

  if JoystickThread<>nil then begin
    JoystickThread.FreeOnTerminate:=false;
    JoystickThread.Abort;
    while FClosing do begin
      Application.ProcessMessages;
    end;
    JoystickThread.Free;
    JoystickThread:=nil;
  end;
end;

procedure TSdpoJoystickThread.SetStatus;
var
  i : integer;
begin
  for i:=0 to MaxAxis-1 do begin
    if (joyEvent.etype=JS_EVENT_AXIS) and (joyEvent.number=i) then begin
      lAxis[i]:=joyEvent.value;
    end;
  end;
  for i:=0 to MaxButtons-1 do begin
    if (joyEvent.etype=JS_EVENT_BUTTON) and (joyEvent.number=i) then begin
      lButs[i]:=joyEvent.value;
    end;
  end;
end;

procedure TSdpoJoystickThread.Execute;
var
  ret: integer;
begin
  while not StopThread do begin

    //here goes the code of the main thread loop
    ret:=fileread(joyfd, joyEvent, sizeof(joyEvent));

    if ret>=0 then begin
      Synchronize(@SetStatus);
    end;
  end;
  Owner.FClosing:=false;
  Terminate;
end;

procedure TSdpoJoystickThread.Abort;
begin
  Owner.FClosing:=true;
  StopThread := True;
end;

