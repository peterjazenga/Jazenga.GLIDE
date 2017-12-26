unit Unit1;

{$mode objfpc}{$H+}

// Please define at project level (Project Options/Other, defines) either

// -dQBEZEOS: Use Zeos in Visual Query Builder
// Please add a project requirement to Zeos zcomponent
// The example uses a Firebird connection (see code below).
// You can of course change this

// or

// -dQBESQLDB: Use SQLDB in Visual Query Builder
// This example uses a Firebird ibconnection.
// You can of course change this.

// or

// -dQBEIBX: Use IBX objects in Visual Query Builder
// to do: needs to be tested/implemented in this demo form

{ todo:
- rework as lpk package
- doubleclick in sort cell sorts asc/desc/none not only rightclick
- reinstate registering engines such as sqldb, zeos, ibx so package requirements
  can be easily specified
- extract icons rework them as png or whatever so we have source
- test on Linux, OSX
}
interface

uses
  SysUtils, Forms, StdCtrls, QBuilder
  {$IFDEF QBEIBX}
  , {IBDatabase? ,} QBEIBX
  {$ENDIF}
  {$IFDEF QBESQLDB}
  , sqldb, QBESqlDb
  , IBConnection {change this if you want another db}
  {$ENDIF}
  {$IFDEF QBEZEOS}
  , ZConnection, QBEZEOS
  {$ENDIF}
  , Classes;

type

  { TFirebirdQuerybuilder }

  TFirebirdQuerybuilder = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    {$IFDEF QBESQLDB}
    FDbConnection: TIBConnection;
    FDBTrans: TSQLTransaction;
    {$ENDIF}
    {$IFDEF QBEZEOS}
    FDbConnection: TZConnection;
    {$ENDIF}
    //todo: add support for QBEIBX
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FirebirdQuerybuilder: TFirebirdQuerybuilder;

implementation

{ TFirebirdQuerybuilder }

procedure TFirebirdQuerybuilder.Button1Click(Sender: TObject);
var
  meuqb: TOQBuilderDialog;
  {$IFDEF QBEIBX}
  VisualQueryEngine: TOQBEngineIBX;
  {$ENDIF}
  {$IFDEF QBESQLDB}
  VisualQueryEngine: TOQBEngineSQLDB;
  {$ENDIF}
  {$IFDEF QBEZEOS}
  VisualQueryEngine: TOQBEngineZEOS;
  {$ENDIF}
begin
  try
    //todo: add ibx code
    {$IFDEF QBESQLDB}
    FDbConnection.DatabaseName := ExtractFilePath(Application.ExeName)+'EMPLOYEE.FDB';
    {$ENDIF}
    {$IFDEF QBEZEOS}
    DbConnection.Database := ExtractFilePath(Application.ExeName)+'EMPLOYEE.FDB';
    {$ENDIF}
    meuqb := TOQBuilderDialog.Create(nil);
    {$IFDEF QBESQLDB}
    VisualQueryEngine := TOQBEngineSQLDB.Create(nil);
    {$ENDIF}
    {$IFDEF QBEZEOS}
    VisualQueryEngine := TOQBEngineZEOS.Create(nil);
    {$ENDIF}
    VisualQueryEngine.Connection := FDbConnection;
    meuqb.OQBEngine := VisualQueryEngine;
    {$IFDEF QBESQLDB}
    meuqb.OQBEngine.DatabaseName := FDbConnection.DatabaseName;
    {$ENDIF}
    {$IFDEF QBEZEOS}
    meuqb.OQBEngine.DatabaseName := DbConnection.Database;
    // If using OQBEngineZEOS: to specify a  PostgreSQL schema, set
    // VisualQueryEngine.SchemaPostgreSQL := 'my_schema';
    VisualQueryEngine.ShowSystemTables := False;
    {$ENDIF}
    {$IFDEF QBESQLDB}
    FDbConnection.Open;
    {$ENDIF}
    {$IFDEF QBEZEOS}
    DbConnection.Connect;
    {$ENDIF}
    if meuqb.Execute then Memo1.Text := meuqb.SQL.Text;
  finally
    meuqb.Free;
    VisualQueryEngine.Free;
  end;
end;

procedure TFirebirdQuerybuilder.FormCreate(Sender: TObject);
begin
  // Example using Firebird embedded for both QBESQLDB and QBEZEOS
  //todo: add ibx code
  {$IFDEF QBESQLDB}
  FDbConnection := TIBConnection.Create(nil);
  FDbConnection.HostName := '';
  FDbConnection.UserName := 'SYSDBA';
  FDBTrans := TSQLTransaction.Create(nil);
  FDbConnection.Transaction := FDBTrans;
  {$ENDIF}
  {$IFDEF QBEZEOS}
  DbConnection:=ZConnection.Create(nil);
  DbConnection.Protocol := 'firebird-2.5';
  DbConnection.ControlsCodePage := cCP_UTF8;
  DbConnection.User := 'SYSDBA';
  {$ENDIF}
  FDbConnection.Password := ''; //leave empty to avoid lookup in FB security db
end;

procedure TFirebirdQuerybuilder.FormDestroy(Sender: TObject);
begin
  {$IFDEF QBESQLDB}
  FDBTrans.Free;
  {$ENDIF}
  FDbConnection.Free;
end;

{$R *.lfm}

end.

