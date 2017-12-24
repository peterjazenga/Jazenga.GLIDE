{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$INCLUDE wst_global.inc}
unit server_listener;

interface
uses
  Classes, SysUtils;

const
  sSEPARATOR = '/';
  sSERVICES_PREFIXE = 'services';
  sWSDL = 'WSDL';
  
type  
  
  TTcpListenerOption = (tloHandleBlockType);
  TTcpListenerOptions = set of TTcpListenerOption; 

  TListnerNotifyMessage = procedure(Sender : TObject; const AMsg : string) of object;
  IBlockHandler = interface
    ['{E0C50F08-A2C3-41D7-ACD5-E7867DD9F981}']
    procedure Execute(
      const ABlockType     : LongInt; 
            ARequestBlock, 
            AResponseBlock : TStream
    );
  end;                      

  { TwstListener }

  TwstListener = class(TObject)
  private
    FOnNotifyMessage: TListnerNotifyMessage;
  protected
    procedure SetOnNotifyMessage(const AValue : TListnerNotifyMessage);virtual;
  public
    class function GetDescription() : string;virtual;
    procedure Start();virtual;abstract;
    procedure Stop();virtual;abstract;
    function IsActive : Boolean; virtual;abstract;
    procedure NotifyMessage(const AMsg : string);
    property OnNotifyMessage : TListnerNotifyMessage read FOnNotifyMessage write SetOnNotifyMessage;
  end;

  { TwstBaseTcpListener }

  TwstBaseTcpListener = class(TwstListener)
  private
    FOptions : TTcpListenerOptions;
    FUnknownBlockHandler : IBlockHandler;  
  protected
    procedure CheckActive(const AActive : Boolean; ACaller : string);
    procedure SetOptions(const AValue : TTcpListenerOptions);
    procedure SetUnknownBlockHandler(const AValue : IBlockHandler); 
  public    
    property Options : TTcpListenerOptions read FOptions write SetOptions;
    property UnknownBlockHandler : IBlockHandler read FUnknownBlockHandler write SetUnknownBlockHandler;  
  end;
  
  function GenerateWSDLHtmlTable(const AServicesModulePath : string=''): string;
  
implementation
uses wst_consts, base_service_intf, metadata_repository,
     metadata_service, metadata_service_binder, metadata_service_imp ;


function GenerateWSDLHtmlTable(const AServicesModulePath : string): string;
var
  r : IModuleMetadataMngr;
  i : Integer;
  locModulePath : string;
begin
  locModulePath := Trim(AServicesModulePath);
  if ( Length(locModulePath) = 0 ) then
    locModulePath := sSEPARATOR+sSERVICES_PREFIXE
  else
    locModulePath := sSEPARATOR + AServicesModulePath + sSERVICES_PREFIXE;
  r := GetModuleMetadataMngr();
  Result := '<html>' +
              '<head>'+
                '<title>'+
                  'The Web Services Toolkit generated Metadata table'+
                '</title>'+
                '<body>' +
                  '<p BGCOLOR="#DDEEFF"><FONT FACE="Arial" COLOR="#0000A0" SIZE="+2">The following repositories has available. Click on the link to view the corresponding WSDL.</FONT></p>'+
                  '<table width="100%">';

  for i := 0 to Pred(r.GetCount()) do begin
    Result := Result +
                '<tr>' +
                      '<td align="left">' +
                          Format('<a href="%s">',[AServicesModulePath+sSEPARATOR+sWSDL+sSEPARATOR+r.GetRepositoryName(i)])+
                          r.GetRepositoryName(i) +
                          '</a>'+
                      '</td>' +
                '</tr>';
  end;
  Result := Result +

                  '</table>'+
                '</body>'+
              '</head>'+
            '</html>';
end;

{ TwstBaseTcpListener }

procedure TwstBaseTcpListener.CheckActive(const AActive : Boolean; ACaller : string); 
begin                      
  if (IsActive() <> AActive) then
    raise Exception.CreateFmt(SERR_ObjectStateDoesNotAllowOperation,[ACaller]);
end;

procedure TwstBaseTcpListener.SetOptions(const AValue : TTcpListenerOptions); 
begin   
  CheckActive(False,'SetOptions');
  if (FOptions=AValue) then 
    exit; 
  FOptions:=AValue;   
end;

procedure TwstBaseTcpListener.SetUnknownBlockHandler(const AValue : IBlockHandler); 
begin 
  CheckActive(False,'SetUnknownBlockHandler');
  if (FUnknownBlockHandler = AValue) then 
    exit; 
  FUnknownBlockHandler := AValue;   
end;

{ TwstListener }

procedure TwstListener.SetOnNotifyMessage(const AValue : TListnerNotifyMessage);
begin
  FOnNotifyMessage := AValue;
end;

class function TwstListener.GetDescription() : string;
begin
  Result := ClassName;
end;

procedure TwstListener.NotifyMessage(const AMsg: string);
begin
  if Assigned(FOnNotifyMessage) then
    FOnNotifyMessage(Self,AMsg);
end;

initialization
  RegisterStdTypes();
  RegisterWSTMetadataServiceImplementationFactory();
  Server_service_RegisterWSTMetadataServiceService();
  
end.
