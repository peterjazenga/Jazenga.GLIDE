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
unit server_binary_formatter;

interface

uses
  Classes, SysUtils, TypInfo,
  base_service_intf, server_service_intf,
  base_binary_formatter;


const
  sBINARY_CONTENT_TYPE = 'binary';
  sPROTOCOL_NAME = sBINARY_CONTENT_TYPE;

type

  { TBinaryFormatter }

  TBinaryFormatter = class(TBaseBinaryFormatter,IFormatterBase,IFormatterResponse)
  Private
    FCallProcedureName : string;
    FCallTarget : string;
  Protected
    procedure BeginCallResponse(Const AProcName,ATarget:string);
    procedure EndCallResponse();
    procedure BeginCallRead(ACallContext : ICallContext);
    function GetCallProcedureName():String;
    function GetCallTarget():String;
    procedure BeginExceptionList(
      const AErrorCode : string;
      const AErrorMsg  : string
    );
    procedure EndExceptionList();
  End;
  
  procedure Server_service_RegisterBinaryFormat();
  
implementation

Type

  { TBinaryFormatterFactory }

  TBinaryFormatterFactory = class(TInterfacedObject,IItemFactory)
  protected
    function CreateInstance():IInterface;
  End;

{ TBinaryFormatterFactory }

function TBinaryFormatterFactory.CreateInstance(): IInterface;
begin
  Result := TBinaryFormatter.Create();
end;


{ TBinaryFormatter }

procedure TBinaryFormatter.BeginCallResponse(const AProcName, ATarget: string);
begin
  Clear();
  BeginObject('Body',Nil);
    BeginObject(ATarget,Nil);
      BeginObject(AProcName + 'Response',Nil);
end;

procedure Print(const AMsg:string);
begin
  WriteLn(AMsg);
End;

procedure TBinaryFormatter.EndCallResponse();
begin
      EndScope();
    EndScope();
  EndScope();
  //PrintObj(GetRootData(),0,@Print);
end;

procedure TBinaryFormatter.BeginCallRead(ACallContext : ICallContext);
Var
  s : string;
begin
  ClearStack();
  PushStack(GetRootData(),stObject);
  s := 'Body';
  BeginObjectRead(s,nil);
    FCallTarget := StackTop().GetByIndex(0)^.Name;
    BeginObjectRead(FCallTarget,nil);
      FCallProcedureName := StackTop().GetByIndex(0)^.Name;
      BeginObjectRead(FCallProcedureName,nil);
end;

function TBinaryFormatter.GetCallProcedureName(): String;
begin
  Result := FCallProcedureName;
end;

function TBinaryFormatter.GetCallTarget(): String;
begin
  Result := FCallTarget;
end;

procedure TBinaryFormatter.BeginExceptionList(
  const AErrorCode: string;
  const AErrorMsg: string
);
begin
  Clear();
  BeginObject('Body',Nil);
    BeginObject('Fault',Nil);
      Put('faultcode',TypeInfo(string),AErrorCode);
      Put('faultstring',TypeInfo(string),AErrorMsg);
end;

procedure TBinaryFormatter.EndExceptionList();
begin
    EndScope();
  EndScope();
end;

procedure Server_service_RegisterBinaryFormat();
begin
  GetFormatterRegistry().Register(sPROTOCOL_NAME,sBINARY_CONTENT_TYPE,TBinaryFormatterFactory.Create());
end;

Initialization

end.
