
//=====================CPU32=======================================

{$IFDEF CPU32}
procedure SwapBytes(_dw_: PDWORD); overload; assembler;
asm
  mov edi, _dw_
  // mov edi, dw
  mov eax, [edi]
  bswap eax
  mov [edi], eax
end;
//        dw = _byteswap_ulong(dw); //todo: add inline in pure pascal case

procedure SwapBytes(w: PWORD); overload; assembler;
asm
  mov edi, w
  mov ax, [edi]
  xchg ah, al
  mov [edi], ax
end;
//        w = _byteswap_ushort(w); //todo: add inline in pure pascal case
{$ENDIF}

//=====================CPU64=======================================

{$IFDEF CPU64}
procedure SwapBytes(_dw_: PDWORD); overload; assembler;
asm
  mov rdi, _dw_
  // mov edi, dw
  mov rax, [rdi]
  bswap rax
  mov [rdi], rax
end;
//        dw = _byteswap_ulong(dw); //todo: add inline in pure pascal case

procedure SwapBytes(w: PWORD); overload; assembler;
asm
  mov rdi, w
  mov ax, [rdi]
  xchg ah, al
  mov [rdi], ax
end;
{$ENDIF}
               
