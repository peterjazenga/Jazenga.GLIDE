function wst_GetMem(Size : PtrInt) : Pointer;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF FPC}
  Result := GetMem(Size);
  {$ELSE}
  GetMem(Result,Size);
  {$ENDIF}
end;