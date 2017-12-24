unit rx_ext_test_case_1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, rxmemds, Dialogs;

type

  { TTCRxMemDataLifecycle }

  TTCRxMemDataLifecycle= class(TTestCase)
  protected
    procedure SetUp; override; 
    procedure TearDown; override; 
  published
    procedure TestHookUp;
    procedure TestSave;
  end; 

implementation

uses
    db, ex_rx_datapacket;

procedure TTCRxMemDataLifecycle.TestHookUp;
var
  ads : TRxMemoryData;
  aField : TField;
  i : integer;
begin
  ads := TRxMemoryData.Create(nil);

  aField := TIntegerField.Create(nil);
  aField.FieldName:= 'IntegerField1';
  aField.Name := 'adsIntegerField1';
  aField.DataSet := ads;

  aField := TStringField.Create(nil);
  aField.FieldName:= 'StringField1';
  aField.Name := 'adsStringField1';
  aField.Size := 20;
  aField.DataSet := ads;

  aField := TFloatField.Create(nil);
  aField.FieldName:= 'FloatField1';
  aField.Name := 'adsFloatField1';
  aField.DataSet := ads;

  ads.Open;

  for i := 0 to 500 do
  begin
    ads.AppendRecord([i,'Name '+ IntToStr(i), 200 + (i*0.1)]);
  end;

  ads.First;

  i:= 0;
  while not ads.EOF do
  begin
    AssertTrue('Integer is incorrect ' + IntToStr(i), ads.FieldByName('IntegerField1').AsInteger = i);
    AssertTrue('Float is incorrect ' + IntToStr(i), ads.FieldByName('FloatField1').AsFloat - (200 + (i*0.1)) < 0.01);
    AssertTrue('String is incorrect ' + IntToStr(i), ads.FieldByName('StringField1').AsString = 'Name '+ IntToStr(i));
    inc(i);
    ads.Next;
  end;


end;

procedure TTCRxMemDataLifecycle.TestSave;
var
  ads1,ads2 : TRxMemoryData;
  aField : TField;
  i : integer;
  sMemoTest : String;
begin
  ads1 := TRxMemoryData.Create(nil);
  ads2 := TRxMemoryData.Create(nil);

  aField := TIntegerField.Create(nil);
  aField.FieldName:= 'IntegerField1';
  aField.Name := 'adsIntegerField1';
  aField.DataSet := ads1;

  aField := TStringField.Create(nil);
  aField.FieldName:= 'StringField1';
  aField.Name := 'adsStringField1';
  aField.Size := 20;
  aField.DataSet := ads1;

  aField := TFloatField.Create(nil);
  aField.FieldName:= 'FloatField1';
  aField.Name := 'adsFloatField1';
  aField.DataSet := ads1;

  // TBooleanField

  aField := TBooleanField.Create(nil);
  aField.FieldName:= 'BooleanField1';
  aField.Name := 'adsBooleanField1';
  aField.DataSet := ads1;

  // TDateTimeField

    aField := TDateTimeField.Create(nil);
  aField.FieldName:= 'DateTimeField1';
  aField.Name := 'adsDateTimeField1';
  aField.DataSet := ads1;


  // TMemoField

  aField := TMemoField.Create(nil);
  aField.FieldName:= 'MemoField1';
  aField.Name := 'adsMemoField1';
  aField.Size:= 600;
  aField.DataSet := ads1;

  // TCurrencyField

  aField := TCurrencyField.Create(nil);
  aField.FieldName:= 'CurrencyField1';
  aField.Name := 'adsCurrencyField1';
  aField.DataSet := ads1;

  ads1.Open;
  ads2.Open;

  sMemoTest := 'memo1';
  for i := 0 to 500 do
  begin
    ads1.AppendRecord([i,'Name '+ IntToStr(i), 200 + (i*0.1),(i div 2) = 1, EncodeDate(2011,10,03) + i, sMemoTest, 1000 + (i*0.1)]);
    sMemoTest := sMemoTest + 'a';
  end;

  ads1.SaveToFile('/tmp/testfile.xml',dfXML);
  ads2.LoadFromFile('/tmp/testfile.xml',dfXML);

  ads2.First;

  sMemoTest := 'memo1';
  i:= 0;
  while not ads2.EOF do
  begin
    AssertTrue('Integer is incorrect ' + IntToStr(i), ads2.FieldByName('IntegerField1').AsInteger = i);
    AssertTrue('Float is incorrect ' + IntToStr(i), ads2.FieldByName('FloatField1').AsFloat - (200 + (i*0.1)) < 0.01);
    AssertTrue('String is incorrect ' + IntToStr(i), ads2.FieldByName('StringField1').AsString = 'Name '+ IntToStr(i));

    AssertTrue('Currency is incorrect ' + IntToStr(i), ads2.FieldByName('CurrencyField1').AsFloat - (1000 + (i*0.1)) < 0.01);
    AssertTrue('DateTime is incorrect ' + IntToStr(i), ads2.FieldByName('DateTimeField1').AsDateTime = EncodeDate(2011,10,03) + i);
    AssertTrue('Memo is incorrect ' + IntToStr(i) + ads2.FieldByName('MemoField1').AsString, StrComp(Pchar(ads2.FieldByName('MemoField1').asString),Pchar(sMemoTest)) = 0);

    sMemoTest := sMemoTest + 'a';
    inc(i);
    ads2.Next;
  end;

  ads1.Free;
  ads2.Free;


end;

procedure TTCRxMemDataLifecycle.SetUp; 
begin

end; 

procedure TTCRxMemDataLifecycle.TearDown; 
begin

end; 

initialization

  RegisterTest(TTCRxMemDataLifecycle); 
end.

