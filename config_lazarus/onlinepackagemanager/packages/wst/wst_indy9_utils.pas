{
    This file is part of the Web Service Toolkit
    Copyright (c) 2008 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$INCLUDE wst_global.inc}
unit wst_indy9_utils;

interface
uses SysUtils, IdTCPServer;

type

  TwstIndy9Thread = class(TIdPeerThread)
  protected
    procedure AfterExecute; override;
    procedure BeforeExecute; override;
  end;

implementation
{$IFDEF WST_DELPHI}
uses 
  ActiveX;
{$ENDIF WST_DELPHI}

{ TwstIndy9Thread }

procedure TwstIndy9Thread.AfterExecute;
begin
{$IFDEF WST_DELPHI}
  CoUninitialize();
{$ENDIF WST_DELPHI}
  inherited;
end;

procedure TwstIndy9Thread.BeforeExecute;
begin
  inherited;
{$IFDEF WST_DELPHI}
  CoInitialize(nil);
{$ENDIF WST_DELPHI}
end;

end.
