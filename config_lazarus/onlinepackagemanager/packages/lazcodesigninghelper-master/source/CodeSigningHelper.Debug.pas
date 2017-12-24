{
   @author(Patrick Michael Kolla-ten Venne [pk] <patrick@kolla-tenvenne.de>)
   @abstract(TODO : please fill in abstract here!)

   @preformatted(
// *****************************************************************************
// Copyright: © 2017 Patrick Michael Kolla-ten Venne. All rights reserved.
// *****************************************************************************
// Changelog (new entries first):
// ---------------------------------------
// 2017-05-24  pk  ---  [CCD] Updated unit header.
// *****************************************************************************
   )
}

unit CodeSigningHelper.Debug;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF FPC}

interface

uses
   Classes,
   SysUtils;

function DumpExceptionCallStack(E: Exception): string;
procedure CodeSigningLogInformation(AObject: TObject; AText: string; AIncludeCallstack: boolean = false);

implementation

uses
   //Windows,
   Dialogs,
   TypInfo,
   IDEIntf,
   LCLProc,
   IDEMsgIntf,
   IDEExternToolIntf;

var
   IMessage: integer = 0;

function DumpExceptionCallStack(E: Exception): string;
var
   I: integer;
   Frames: PPointer;
begin
   Result := '';
   if E <> nil then begin
      Result += 'Exception class: ' + E.ClassName + LineEnding + 'Message: ' + E.Message + LineEnding;
   end;
   Result += BackTraceStrFunc(ExceptAddr);
   Frames := ExceptFrames;
   for I := 0 to ExceptFrameCount - 1 do begin
      Result += LineEnding + BackTraceStrFunc(Frames[I]);
   end;
end;

procedure CodeSigningLogInformation(AObject: TObject; AText: string;
   AIncludeCallstack: boolean);
var
   i: integer;
   sl: TStringList;
begin
   if Assigned(AObject) then begin
      AText := AObject.ClassName + ': ' + AText;
   end;
   //ShowMessage(AText);
   sl := TStringList.Create;
   try
      if AIncludeCallstack then begin
         sl.Text := GetStackTrace(False);
      end;
      if Assigned(IDEMessagesWindow) then begin
         IDEMessagesWindow.AddCustomMessage(mluNone, IntToStr(IMessage) + ': ' + AText, '', 0, 0, 'CodeSigning Debug');
         for i := 0 to Pred(sl.Count) do begin
            IDEMessagesWindow.AddCustomMessage(mluNone, sl[i], '', 0, 0, 'CodeSigning Debug');
         end;
      end else begin
         // ShowMessage(AText + LineEnding + sl.Text);
      end;
   finally
      sl.Free;
   end;
   Inc(IMessage);
end;


end.
