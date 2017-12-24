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
unit wst_indy10_utils;

interface
uses
   SysUtils, IdThread;

type

  TwstIndy10Thread = class(TIdThreadWithTask)
  protected
    procedure AfterExecute(); override;
    procedure BeforeExecute(); override;
  end;

implementation

{$IFDEF WST_DELPHI}
uses 
  ActiveX;
{$ENDIF WST_DELPHI}

{ TwstIndy10Thread }

procedure TwstIndy10Thread.AfterExecute();
begin
{$IFDEF WST_DELPHI}
  CoUninitialize();
{$ENDIF WST_DELPHI}
  inherited;
end;

procedure TwstIndy10Thread.BeforeExecute();
begin
  inherited;
{$IFDEF WST_DELPHI}
  CoInitialize(nil);
{$ENDIF WST_DELPHI}
end;


end.
