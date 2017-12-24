{
   @author(Patrick Michael Kolla-ten Venne [pk] <patrick@kolla-tenvenne.de>)
   @abstract(Formf or displaying available code signing certificates.)

   @preformatted(
// *****************************************************************************
// Copyright: © 2017 Patrick Michael Kolla-ten Venne. All rights reserved.
// *****************************************************************************
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// *****************************************************************************
// Changelog (new entries first):
// ---------------------------------------
// 2017-05-12  pk  ---  [CCD] Updated unit header.
// *****************************************************************************
   )
}

unit CodeSigningHelper.Certificates.Form;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF FPC}

interface

uses
   Classes,
   SysUtils,
   FileUtil,
   Forms,
   Controls,
   Graphics,
   Dialogs,
   ButtonPanel,
   {$IFDEF MSWindows}
   JwaWinCrypt,
   JwaWinType,
   JwaWinBase,
   JwaWinSta,
   {$ENDIF MSWindows}
   {$IFDEF Darwin}
   SecBase,
   {$ENDIF Darwin}
   ComCtrls;

type

   { TCertificateListItem }

   TCertificateListItem = class(TListItem)
   private
      FEmailAddress: ansistring;
      FHash: ansistring;
      FIssuer: WideString;
      FSerial: ansistring;
      FSubject: WideString;
      FIssuerProperties: TStringList;
      FSubjectProperties: TStringList;
      FValidFrom: TDateTime;
      FValidTo: TDateTime;
      procedure SetEmailAddress(AValue: ansistring);
      function ValidFromIndex: integer;
      function ValidToIndex: integer;
      function SerialNumberIndex: integer;
      function ThumbprintIndex: integer;
      procedure SetHash(AValue: ansistring);
      procedure SetIssuer(AValue: WideString);
      procedure SetSerial(AValue: ansistring);
      procedure SetSubject(AValue: WideString);
      procedure SetValidFrom(AValue: TDateTime);
      procedure SetValidTo(AValue: TDateTime);
   public
      constructor Create(AOwner: TListItems); override;
      destructor Destroy; override;
      property Hash: ansistring read FHash write SetHash;
      property Serial: ansistring read FSerial write SetSerial;
      property Subject: WideString read FSubject write SetSubject;
      property Issuer: WideString read FIssuer write SetIssuer;
      property EmailAddress: ansistring read FEmailAddress write SetEmailAddress;
      property ValidFrom: TDateTime read FValidFrom write SetValidFrom;
      property ValidTo: TDateTime read FValidTo write SetValidTo;
   end;

   { TFormCertificates }

   TFormCertificates = class(TForm)
      lvCertificates: TListView;
      panelButtons: TButtonPanel;
      procedure lvCertificatesCreateItemClass({%H-}Sender: TCustomListView; var ItemClass: TListItemClass);
      procedure lvCertificatesCustomDrawItem(Sender: TCustomListView; Item: TListItem; {%H-}State: TCustomDrawState; var {%H-}DefaultDraw: boolean);
      procedure lvCertificatesDblClick({%H-}Sender: TObject);
   private
      { private declarations }
      {$IFDEF MSWindows}
      procedure AddCertificate(ACertificate: PCCERT_CONTEXT);
      procedure FillListWithWindowsCertificates;
      {$ENDIF MSWindows}
      procedure FillListWithGnuPGCertificates;
      procedure SelectCertificate(const AHash: ansistring);
   public
      { public declarations }
      function ExecuteWindows(var AHash: ansistring): boolean;
      function ExecuteGnuPG(var AHash: ansistring): boolean;
   end;

function SelectWindowsCertificate(var AHash: ansistring): boolean;
function SelectGnuPGCertificate(var AHash: ansistring): boolean;

implementation

{$R *.lfm}

uses
   RegExpr,
   process;

function SelectWindowsCertificate(var AHash: ansistring): boolean;
var
   form: TFormCertificates;
begin
   form := TFormCertificates.Create(nil);
   try
      Result := form.ExecuteWindows(AHash);
   finally
      form.Free;
   end;
end;

function SelectGnuPGCertificate(var AHash: ansistring): boolean;
var
   form: TFormCertificates;
begin
   form := TFormCertificates.Create(nil);
   try
      Result := form.ExecuteGnuPG(AHash);
   finally
      form.Free;
   end;
end;

{ TCertificateListItem }

procedure TCertificateListItem.SetSubject(AValue: WideString);
begin
   FSubject := AValue;
   if Pos('CN=', AValue) > 0 then begin
      FSubjectProperties.DelimitedText := StringReplace(UTF8Encode(AValue), ', ', ',', [rfReplaceAll]);
      Self.Caption := FSubjectProperties.Values['CN'];
   end else begin
      Self.Caption := UTF8Encode(FSubject);
   end;
end;

procedure TCertificateListItem.SetValidFrom(AValue: TDateTime);
begin
   FValidFrom := AValue;
   while Self.SubItems.Count <= ValidFromIndex do begin
      Self.SubItems.Add('');
   end;
   Self.SubItems[ValidFromIndex] := FormatDateTime('yyyy-mm-dd', AValue);
end;

constructor TCertificateListItem.Create(AOwner: TListItems);
begin
   inherited Create(AOwner);
   FIssuerProperties := TStringList.Create;
   FIssuerProperties.StrictDelimiter := True;
   FIssuerProperties.Delimiter := ',';
   FSubjectProperties := TStringList.Create;
   FSubjectProperties.StrictDelimiter := True;
   FSubjectProperties.Delimiter := ',';
   FValidFrom := 0;
   FValidTo := 0;
end;

destructor TCertificateListItem.Destroy;
begin
   FIssuerProperties.Free;
   FSubjectProperties.Free;
   inherited Destroy;
end;

function TCertificateListItem.ValidFromIndex: integer;
begin
   Result := 1;
end;

procedure TCertificateListItem.SetEmailAddress(AValue: ansistring);
begin
   FEmailAddress := AValue;
   while Self.SubItems.Count <= 2 do begin
      Self.SubItems.Add('');
   end;
   Self.SubItems[0] := FEmailAddress;
   Self.ListView.Column[1].Caption := 'Email';
end;

procedure TCertificateListItem.SetValidTo(AValue: TDateTime);
begin
   FValidTo := AValue;
   while Self.SubItems.Count <= ValidToIndex do begin
      Self.SubItems.Add('');
   end;
   Self.SubItems[ValidToIndex] := FormatDateTime('yyyy-mm-dd', AValue);
end;

function TCertificateListItem.ValidToIndex: integer;
begin
   Result := 2;
end;

function TCertificateListItem.SerialNumberIndex: integer;
begin
   Result := 4;
end;

function TCertificateListItem.ThumbprintIndex: integer;
begin
   Result := 3;
end;

procedure TCertificateListItem.SetHash(AValue: ansistring);
begin
   FHash := AValue;
   while Self.SubItems.Count <= ThumbprintIndex do begin
      Self.SubItems.Add('');
   end;
   Self.SubItems[ThumbprintIndex] := AValue;
end;

procedure TCertificateListItem.SetIssuer(AValue: WideString);
begin
   FIssuer := AValue;
   while Self.SubItems.Count <= 2 do begin
      Self.SubItems.Add('');
   end;
   FIssuerProperties.DelimitedText := StringReplace(UTF8Encode(AValue), ', ', ',', [rfReplaceAll]);
   Self.SubItems[0] := FIssuerProperties.Values['CN'];
end;

procedure TCertificateListItem.SetSerial(AValue: ansistring);
begin
   FSerial := AValue;
   while Self.SubItems.Count <= SerialNumberIndex do begin
      Self.SubItems.Add('');
   end;
   Self.SubItems[SerialNumberIndex] := AValue;
end;

{ TFormCertificates }

procedure TFormCertificates.lvCertificatesCreateItemClass(Sender: TCustomListView; var ItemClass: TListItemClass);
begin
   ItemClass := TCertificateListItem;
end;

procedure TFormCertificates.lvCertificatesCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: boolean);
var
   cli: TCertificateListItem;
begin
   cli := TCertificateListItem(Item);
   if ((Now >= cli.ValidFrom) or (cli.ValidFrom = 0)) and ((Now <= cli.ValidTo) or (cli.ValidTo = 0)) then begin
      Sender.Canvas.Font.Color := clWindowText;
   end else begin
      Sender.Canvas.Font.Color := clGrayText;
   end;
end;

procedure TFormCertificates.lvCertificatesDblClick(Sender: TObject);
begin
   panelButtons.OKButton.Click;
end;

{$IFDEF MSWindows}
procedure TFormCertificates.AddCertificate(ACertificate: PCCERT_CONTEXT);

   function SerialToStr(ASerial: CRYPT_INTEGER_BLOB): string;
   var
      s: ansistring;
      i: integer;
   begin
      SetLength(s, ASerial.cbData);
      Move(ASerial.pbData^, s[1], ASerial.cbData);
      Result := '';
      for i := Length(s) downto 1 do begin
         Result += IntToHex(Ord(s[i]), 2);
      end;
      Result := LowerCase(Result);
   end;

   function GetThumbprint(ACC: PCCERT_CONTEXT): string;
   var
      dw: DWord;
      pc: PAnsiChar;
      s: ansistring;
      i: integer;
   begin
      dw := 22;
      GetMem(pc, dw + 2);
      try
         ZeroMemory(pc, dw + 2);
         if CertGetCertificateContextProperty(ACC, CERT_HASH_PROP_ID, PByte(pc), dw) then begin
            s := ansistring(pc);
            Result := '';
            for i := 1 to Length(s) do begin
               Result += IntToHex(Ord(s[i]), 2);
            end;
            Result := LowerCase(Result);
         end;
      finally
         FreeMem(pc);
      end;
   end;

var
   cli: TCertificateListItem;
   sa: array[0..1023] of widechar;
begin
   cli := TCertificateListItem(lvCertificates.Items.Add);
   // https://msdn.microsoft.com/en-us/library/windows/desktop/aa376556(v=vs.85).aspx
   if CertNameToStrW(X509_ASN_ENCODING or PKCS_7_ASN_ENCODING, @(ACertificate^.pCertInfo^.Subject), CERT_X500_NAME_STR, @sa[0], 1024) > 0 then begin
      cli.Subject := sa;
   end else begin
      cli.Subject := 'n/a';
   end;
   if CertNameToStrW(X509_ASN_ENCODING or PKCS_7_ASN_ENCODING, @(ACertificate^.pCertInfo^.Issuer), CERT_X500_NAME_STR, @sa[0], 1024) > 0 then begin
      cli.Issuer := sa;
   end else begin
      cli.Issuer := 'n/a';
   end;
   cli.ValidFrom := FileTime2DateTime(JwaWinBase._FILETIME(ACertificate^.pCertInfo^.NotBefore));
   cli.ValidTo := FileTime2DateTime(JwaWinBase._FILETIME(ACertificate^.pCertInfo^.NotAfter));
   cli.Serial := SerialToStr(ACertificate^.pCertInfo^.SerialNumber);
   cli.Hash := GetThumbprint(ACertificate);
end;

{$ENDIF MSWindows}

{$IFDEF MSWindows}
procedure TFormCertificates.FillListWithWindowsCertificates;

   procedure FillFromStore(AStore: HCERTSTORE);
   const
      OIDs: array[0..0] of LPSTR = (szOID_PKIX_KP_CODE_SIGNING);
      CertUsage: CERT_ENHKEY_USAGE = (cUsageIdentifier: 1; rgpszUsageIdentifier: @OIDs);
   var
      c: PCCERT_CONTEXT; // https://msdn.microsoft.com/en-us/library/windows/desktop/aa377189(v=vs.85).aspx
   begin
      // https://msdn.microsoft.com/en-us/library/windows/desktop/aa376064(v=vs.85).aspx
      c := nil;
      repeat
         // https://msdn.microsoft.com/en-us/library/windows/desktop/aa376064(v=vs.85).aspx
         // CERT_FIND_HAS_PRIVATE_KEY is only available starting Windows 8
         c := CertFindCertificateInStore(AStore, X509_ASN_ENCODING or PKCS_7_ASN_ENCODING, CERT_FIND_EXT_ONLY_ENHKEY_USAGE_FLAG, CERT_FIND_ENHKEY_USAGE, @CertUsage, c);
         if Assigned(c) then begin
            AddCertificate(c);
         end;
      until not Assigned(c);
   end;

var
   hStore: HCERTSTORE;
begin
   lvCertificates.Items.BeginUpdate;
   try
      lvCertificates.Items.Clear;
      // https://msdn.microsoft.com/en-us/library/windows/desktop/aa376560(v=vs.85).aspx
      hStore := CertOpenSystemStore(0, 'MY');
      if (hStore <> nil) then begin
         try
            FillFromStore(hStore);
         finally
            CertCloseStore(hStore, CERT_CLOSE_STORE_FORCE_FLAG);
         end;
      end;
   finally
      lvCertificates.Items.EndUpdate;
   end;
end;

{$ENDIF MSWindows}

procedure TFormCertificates.SelectCertificate(const AHash: ansistring);
var
   i: integer;
begin
   for i := 0 to Pred(lvCertificates.Items.Count) do begin
      if SameText(TCertificateListItem(lvCertificates.Items[i]).Hash, AHash) then begin
         lvCertificates.Items[i].Selected := True;
         Exit;
      end;
   end;
   lvCertificates.Selected := nil;
end;

procedure TFormCertificates.FillListWithGnuPGCertificates;
(*
sec   rsa2048 2017-05-16 [SC] [verfällt: 2019-05-16]
      880B024649DBBAC8EAE82ADF23404EF5FC66D324
uid        [ ultimativ ] Patrick Kolla-ten Venne <patrick@kolla-tenvenne.de>
ssb   rsa2048 2017-05-16 [E] [verfällt: 2019-05-16]

sec   rsa4096 2017-05-18 [SC]
      4FE1EE9CE228640FBA6B384DB3C4A374C272728C
uid        [ ultimativ ] Patrick Michael Kolla-ten Venne (Test-Key Chains PepiMK.Signing.GnuPG.pas) <patrick.kolla@safer-networking.org>
ssb   rsa4096 2017-05-18 [E]*)
var
   p: TProcess;
   sl: TStringList;
   i: integer;
   cli: TCertificateListItem;
   rSec, rHash, rUID: TRegExpr;
begin
   lvCertificates.Items.BeginUpdate;
   try
      lvCertificates.Items.Clear;
      p := TProcess.Create(nil);
      try
         p.Executable := 'gpg';
         p.Parameters.Add('--list-secret-keys');
         p.Options := [poUsePipes, poNoConsole, poWaitOnExit];
         p.Execute;
         sl := TStringList.Create;
         rSec := TRegExpr.Create('sec   (rsa|dsa)[0-9]* ([0-9]{4})-([0-9]{2})-([0-9]{2}) \[(SC|E)\]( \[[^\:]*: ([0-9]{4})-([0-9]{2})-([0-9]{2})\]|)');
         rHash := TRegExpr.Create('      ([0-9A-F]{40})');
         rUID := TRegExpr.Create('uid        \[[^\\]*] ([^\\<]+)\<([^\@]*@[^\\>]*)\>');
         try
            p.Output.Seek(0, soFromBeginning);
            sl.LoadFromStream(p.Output);
            cli := nil;
            for i := 0 to Pred(sl.Count) do begin
               if rSec.Exec(sl[i]) then begin
                  cli := TCertificateListItem(lvCertificates.Items.Add);
                  cli.ValidFrom := EncodeDate(StrToInt(rSec.Match[2]), StrToInt(rSec.Match[3]), StrToInt(rSec.Match[4]));
                  try
                     cli.ValidTo := EncodeDate(StrToInt(rSec.Match[7]), StrToInt(rSec.Match[8]), StrToInt(rSec.Match[9]));
                  except
                     cli.ValidTo := 0;
                  end;
               end else if rHash.Exec(sl[i]) then begin
                  if Assigned(cli) then begin
                     cli.Hash := rHash.Match[1];
                  end;
               end else if rUID.Exec(sl[i]) then begin
                  cli.Subject := UTF8Decode(rUID.Match[1]);
                  cli.EmailAddress := rUID.Match[2];
               end;
            end;
         finally
            sl.Free;
            rSec.Free;
            rHash.Free;
            rUID.Free;
         end;
      finally
         p.Free;
      end;
   finally
      lvCertificates.Items.EndUpdate;
   end;
end;

function TFormCertificates.ExecuteWindows(var AHash: ansistring): boolean;
begin
   {$IFDEF MSWindows}
   FillListWithWindowsCertificates;
   {$ENDIF MSWindows}
   SelectCertificate(AHash);
   Result := (ShowModal = mrOk);
   if Result and Assigned(lvCertificates.Selected) then begin
      AHash := TCertificateListItem(lvCertificates.Selected).Hash;
   end;
end;

function TFormCertificates.ExecuteGnuPG(var AHash: ansistring): boolean;
begin
   FillListWithGnuPGCertificates;
   SelectCertificate(AHash);
   Result := (ShowModal = mrOk);
   if Result and Assigned(lvCertificates.Selected) then begin
      AHash := TCertificateListItem(lvCertificates.Selected).Hash;
   end;
end;

end.
