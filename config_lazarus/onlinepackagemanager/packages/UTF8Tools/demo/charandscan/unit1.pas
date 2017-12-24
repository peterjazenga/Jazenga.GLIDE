unit Unit1;

{$MODE objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, utf8scanner, character, unicodeinfo, ComCtrls, LCLProc;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Memo1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{ TForm1 }

//Replace all non Letters-or-Digits except ControlChars with dot. ScanMode example

procedure TForm1.Button1Click(Sender: TObject);
var s: TUTF8Scanner;
  ch: UCS4Char;
begin
  s := TUTF8Scanner.Create(Memo1.text);
  repeat
    ch := s.Next;
    if (not TCharacter.IsLetterOrDigit(ch)) and
      (not TCharacter.IsControl(ch)) then s.Replace('.');
  until s.Done;
  Memo1.Text := s.UTF8String;
  s.free;
end;

//Replace all non Letters-or-Digits except ControlChars with dot. Index example

procedure TForm1.Button2Click(Sender: TObject);
var s: TUTF8Scanner;
  i: integer;
begin
  s := TUTF8Scanner.Create(Memo1.text);
  for i := 1 to s.Length do
    if (not TCharacter.IsLetterOrDigit(s[i])) and
      (not TCharacter.IsControl(s[i])) then s[i] := '.';
  Memo1.Text := s.UTF8String;
  s.free;
end;

//Setting the entire Text to Title Case (first char). See the Digraph ǉ for difference to Upper Case.

procedure TForm1.Button3Click(Sender: TObject);
var s: TUTF8Scanner;
  i: integer;
  LastCharisLetter: Boolean;
begin
  LastCharIsLetter := false;
  s := TUTF8Scanner.Create(Memo1.text);
  for i := 1 to s.Length do
  begin
    if TCharacter.IsLetter(s[i]) then
    begin
      if not LastCharisLetter then s[i] := TCharacter.toTitle(s[i]);
      LastCharIsLetter := true;
    end else LastCharIsLetter := false;
  end;
  Memo1.Text := s.UTF8String;
  s.free;
end;

//Lowercase

procedure TForm1.Button4Click(Sender: TObject);
var s: TUTF8Scanner;
  i: integer;
begin
  s := TUTF8Scanner.Create(Memo1.text);
  for i := 1 to s.Length do
    if TCharacter.IsLetter(s[i]) then s[i] := TCharacter.toLower(s[i]);
  Memo1.Text := s.UTF8String;
  s.free;
end;

//Case Test

procedure TForm1.Button5Click(Sender: TObject);
var s: TUTF8Scanner;
begin
  s := TUTF8Scanner.Create(Memo1.text);
  s.FindChars := 'öäü';
  //Memo1.text:=s.GenerateCaseStatement(false,'s'); //generates case statement
  repeat
    case s.FindIndex(s.Next) of
  {ö} 0: s.Replace('oe');
  {ä} 1: s.Replace('ae');
  {ü} 2: s.Replace('ue');
    end;
  until s.Done;
  Memo1.Text := s.UTF8String;
  s.free;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  Memo1.Text:= TCharacter.Normalize_NFKC(Memo1.text);
end;

procedure TForm1.Memo1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var tmp, dum, i: integer;
  UChar, Pg: string;
  UCP: Longint;
begin
  if Memo1.SelLength > 0 then exit;
  tmp := Memo1.SelStart;
  Memo1.SelLength := 1;
  Memo1.SelStart := tmp;
  UChar := Memo1.SelText;
  UCP := UTF8CharacterToUnicode(Pchar(UChar), dum);
  for i := 0 to MaxUnicodeRanges do if ((UCP >= UnicodeRanges[i].S) and (UCP <= UnicodeRanges[i].E)) then
    begin
      Pg := UnicodeRanges[i].PG;
      break;
    end;
  StatusBar1.SimpleText := UChar + ' $' + IntToHex(UCP, 4) + ' ' + CategoryStrings[TCharacter.GetUnicodeCategory(UCP)] + ' [' + Pg + ']';
  Memo1.SelLength := 0;
end;

initialization
{$I unit1.lrs}

end.
