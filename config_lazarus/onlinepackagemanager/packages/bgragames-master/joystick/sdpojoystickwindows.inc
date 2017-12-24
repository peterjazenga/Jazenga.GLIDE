
procedure TSdpoJoystick.Update;
begin
  //Joy.dwFlags := Joy.dwFlags or JOY_RETURNBUTTONS;
  Joy.dwSize:=Sizeof(Joy);
  Joy.dwFlags := JOY_RETURNALL;
  ErrorResult := joyGetPosEx(JoystickIDConsts[FWinDevice], @Joy);
  if ErrorResult = JOYERR_NOERROR then
    FLastUpdate := GetTickCount;
end;

procedure TSdpoJoystick.Read(var LocalAxis, LocalButtons: array of integer);
var i: integer;
begin
  if Active then begin
    //Joy.dwFlags := Joy.dwFlags or JOY_RETURNBUTTONS;
    //ErrorResult := joyGetPosEx(JoystickIDConsts[FWinDevice], @Joy);

    if (GetTickCount-FLastUpdate)>FUpdateInterval then
      Update;

    if ErrorResult = JOYERR_NOERROR then begin
      for i:=0 to min(length(LocalAxis)-1,MaxWinAxis-1) do begin
        case i of
          0: LocalAxis[i] := Joy.wXpos;
          1: LocalAxis[i] := Joy.wYpos;
          2: LocalAxis[i] := Joy.wZpos;
          3: LocalAxis[i] := Joy.dwRpos;
          4: LocalAxis[i] := Joy.dwUpos;
          5: LocalAxis[i] := Joy.dwVpos;
          6: LocalAxis[i] := Joy.dwPOV;
        end;
      end;
      for i:=0 to min(length(LocalButtons)-1,MaxButtons-1) do begin
        if (Joy.wbuttons and (1 shl i))>0 then
          LocalButtons[i] := 1
        else
          LocalButtons[i] := 0;
      end;
    end else begin
      //raise exception
      FActive:=false;
      raise Exception.Create('Could not open Joystick'+IntToStr(JoystickIDConsts[FWinDevice]+1));
    end;
  end;
end;

function TSdpoJoystick.GetAxis(idx: integer): integer;
begin
  result := 0;
  if Active then begin
    if idx>MaxWinAxis-1 then begin
      raise Exception.Create('Invalid axis '+IntToStr(idx));
      exit;
    end;

    if (GetTickCount-FLastUpdate)>FUpdateInterval then
      Update;

    case idx of
      0: Result := Joy.wXpos;
      1: Result := Joy.wYpos;
      2: Result := Joy.wZpos;
      3: Result := Joy.dwRpos;
      4: Result := Joy.dwUpos;
      5: Result := Joy.dwVpos;
      6: Result := Joy.dwPOV;
    end;
  end else begin
    raise Exception.Create('Joystick unavailable');
  end;
end;

function TSdpoJoystick.GetButtons(idx: integer): integer;
begin
  result := 0;
  if Active then begin
    if idx>MaxButtons-1 then begin
      raise Exception.Create('Invalid button '+IntToStr(idx));
      exit;
    end;

    if (GetTickCount-FLastUpdate)>FUpdateInterval then
      Update;

    if (Joy.wbuttons and (1 shl idx))>0 then
      result := 1
    else
      result := 0;
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
begin
  if joyGetNumDevs < 0 then begin
    //raise exception
    raise Exception.Create('No Joysticks available');
    FActive := false;
    exit;
  end;

  Update;
  //ErrorResult := joyGetPosEx(JoystickIDConsts[FWinDevice], @Joy);

  if ErrorResult <> JOYERR_NOERROR then begin
    //raise exception
    FActive := false;
    raise Exception.Create('Could not open Joystick'+IntToStr(JoystickIDConsts[FWinDevice]+1));
    exit;
  end;
  FActive:=true;
  FLastUpdate := GetTickCount;
end;

procedure TSdpoJoystick.DeviceClose;
begin
  FActive := false;
end;
