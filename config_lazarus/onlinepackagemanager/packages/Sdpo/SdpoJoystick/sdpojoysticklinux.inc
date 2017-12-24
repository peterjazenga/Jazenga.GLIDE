
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
    //raise Exception.Create('Could not open Joystick '+FLinDevice);
    exit;
  end;
  
  FActive:=true;
  JoystickThread := TSdpoJoystickThread.Create(True); // With the True parameter it doesn't start automatically
  JoystickThread.FreeOnTerminate := false;
  if Assigned(JoystickThread.FatalException) then
    raise JoystickThread.FatalException;

  // Here the code initialises anything required before the threads starts executing
  JoystickThread.Owner := Self;
  JoystickThread.Resume;
end;

procedure TSdpoJoystick.DeviceClose;
begin
  CloseJoystick;

  if assigned(JoystickThread) then begin
    JoystickThread.StopThread := True;
    //CloseJoystick;
    while not JoystickThread.Terminated do begin
      //Application.ProcessMessages;
    end;
    JoystickThread.Free;
    JoystickThread := nil;
  end;

  FActive := false;
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
  ret, rv: integer;
  fd_set: TFDSet;
  //timeout: Timeval;

begin
  while not StopThread do begin
    fpFD_ZERO(fd_set); // clear the set
    fpFD_SET(joyfd, fd_set); // add our file descriptor to the set

    //timeout.tv_sec := 0;
    //timeout.tv_usec := 100000;

    //here goes the code of the main thread loop
    //ret:=fileread(joyfd, joyEvent, sizeof(joyEvent));

    rv := fpselect(joyfd + 1, @fd_set, nil, nil, 100);
    if rv = -1 then begin // an error accured
      break;
    end else if rv = 0 then begin// a timeout occured
      continue;
    end else begin // there was data to read
      ret := fpread(joyfd, joyEvent, sizeof(joyEvent));
    end;

    if (ret > 0) and not StopThread then begin
      Synchronize(@SetStatus);
    end else begin
      break;
    end;
  end;

  Terminate;
end;

procedure TSdpoJoystickThread.Abort;
begin
  StopThread := True;
end;

