unit indy_https_server;

interface

uses
   IdSSLOpenSSL,
   indy_http_server;

type

   { TwstIndyHttpsListener }

   TwstIndyHttpsListener = class(TwstIndyHttpListener)
   private
     FCertFile: string;
      FIoHandler: TIdServerIOHandlerSSLOpenSSL;
      FKeyFile: string;
      FRootCertFile: string;
      procedure SetCertFile(AValue: string);
      procedure SetKeyFile(AValue: string);
      procedure SetRootCertFile(AValue: string);
   public
      constructor Create(const AServerIpAddress: string = '127.0.0.1'; const AListningPort: integer = 8000; const ADefaultClientPort: integer = 25000;
         const AServerSoftware: string = 'Web Service Toolkit Application');
      property CertFile: string read FCertFile write SetCertFile;
      property RootCertFile: string read FRootCertFile write SetRootCertFile;
      property KeyFile: string read FKeyFile write SetKeyFile;
   end;

implementation

{ TwstIndyHttpsListener }

procedure TwstIndyHttpsListener.SetCertFile(AValue: string);
begin
  FIoHandler.SSLOptions.CertFile := AValue;
end;

procedure TwstIndyHttpsListener.SetKeyFile(AValue: string);
begin
  FIoHandler.SSLOptions.KeyFile := AValue;
end;

procedure TwstIndyHttpsListener.SetRootCertFile(AValue: string);
begin
   FIoHandler.SSLOptions.RootCertFile := AValue;
end;

constructor TwstIndyHttpsListener.Create(const AServerIpAddress: string; const AListningPort: integer; const ADefaultClientPort: integer; const AServerSoftware: string);
begin
   inherited Create(AServerIpAddress, AListningPort, ADefaultClientPort, AServerSoftware);
   FIoHandler := TIdServerIOHandlerSSLOpenSSL.Create(nil);
   // TIdSSLVersion = (sslvSSLv2, sslvSSLv23, sslvSSLv3, sslvTLSv1,sslvTLSv1_1,sslvTLSv1_2);
   FIoHandler.SSLOptions.Method := sslvSSLv23;
   FIoHandler.SSLOptions.Mode := sslmServer;
   FHTTPServerObject.IOHandler := FIoHandler;
end;

end.
