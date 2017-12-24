{*********************************************************************}
{                                                                     }
{                                                                     }
{             Matthieu Giroux                                         }
{             TExtOperation  :                                       }
{             Operating 2 numbers              }
{             TExtDBOperation :                                       }
{             Operating 2 numbers in database }
{             22 Avril 2006                                           }
{                                                                     }
{                                                                     }
{*********************************************************************}

unit u_extoperate;

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

interface

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}

uses
  SysUtils, Classes, Graphics, Controls,
  Messages,
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
{$IFDEF FPC}
  lmessages,
{$ENDIF}
  U_ExtNumEdits,
  StdCtrls,
  DB, DBCtrls ;

const
{$IFDEF VERSIONS}
    gVer_TExtOperate : T_Version = ( Component : 'Composant TExtOperate' ;
                                     FileUnit : 'U_ExtOperate' ;
                                     Owner : 'Matthieu Giroux' ;
                                     Comment : 'Caluls sur les Opérateurs de nombres.' ;
                                     BugsStory : '0.9.9.9 : Tested.' + #13#10
                                               + '0.9.9.0 : Not fully tested.';
                                     UnitType : 3 ;
                                     Major : 0 ; Minor : 9 ; Release : 9 ; Build : 9 );
{$ENDIF}
 CST_EXTOPERATION = 'TExtOperation';
 CST_EXTMOPERATION = 'TExtMOperation';
 CST_EXTCOPERATION = 'TExtCOperation';

type

  TExtOperate   = class;

  { TExtOperateColumn }

  TExtOperateColumn = class(TCollectionItem)
  private
   FOperate : TExtOperate;
  protected
   procedure p_SetOperate ( AOperate    : TExtOperate ); virtual;
  public
    constructor Create(ACollection: TCollection); override;
  published
    property Operate    : TExtOperate read FOperate    write p_SetOperate;
  end;

  { TExtOperateColumns }

  TExtOperateColumns = class(TOwnedCollection)
  private
    function GetColumn ( Index: Integer): TExtOperateColumn;
    procedure SetColumn( Index: Integer; const Value: TExtOperateColumn);
  public
    function Add: TExtOperateColumn;
    property Items[Index: Integer]: TExtOperateColumn read GetColumn write SetColumn; default;
  end;

  { TExtNumOperate }

  TExtNumOperate   = class(TExtNumEdit)
   private
     FOperate   : TExtOperate;
     FOnCalculate  ,
     FOnCalculated : TNotifyEvent;
    procedure p_setOperate   (const AOperate  :TExtOperate);
   protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
   public
     constructor Create(AOwner: TComponent); override;
     procedure Calculate ( const ab_all : Boolean = False ); virtual; abstract;
     procedure CalculateEvent ( const AOperated : TCustomEdit ); virtual;
   published
     property OnCalculate : TNotifyEvent read FOnCalculate write FOnCalculate;
     property OnCalculated : TNotifyEvent read FOnCalculated write FOnCalculated;
     property Operate   : TExtOperate  read FOperate   write p_setOperate;
  End;

  { TExtOperateForm }

  TExtOperateForm   = class(TComponent)
  private
      FOperate   : TExtOperate;
      FShowForm : TNotifyEvent;
      FOnCalculate  ,
      FOnCalculated : TNotifyEvent;
      procedure p_setOperate   (const AOperate  :TExtOperate);
    protected
      procedure Notification(AComponent: TComponent;
        Operation: TOperation); override;
      procedure p_FormShow(AObject: TObject); virtual;
      procedure UnLinkForm; virtual;
      procedure SetLinkForm; virtual;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Calculate; virtual;
      procedure Loaded; override;
    published
      property OnCalculate : TNotifyEvent read FOnCalculate write FOnCalculate;
      property OnCalculated : TNotifyEvent read FOnCalculated write FOnCalculated;
      property ShowForm  : TNotifyEvent  read FShowForm  write FShowForm;
      property Operate   : TExtOperate  read FOperate   write p_setOperate;
    end;

  TExtOperate   = class(TComponent)
    private
      gaO_Operations : TList;
      FMain : Boolean;
      FOnCalculate  ,
      FOnCalculated : TNotifyEvent;
    protected
      FColumns : TExtOperateColumns;
      procedure AddOperation    ( AExtOperation : TExtNumOperate );virtual;
      procedure RemoveOperation ( AExtOperation : TExtNumOperate );virtual;
      procedure SetColumns(const AValue: TExtOperateColumns);virtual;
      procedure Notification(AComponent: TComponent;
        Operation: TOperation); override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Calculate; virtual;
      procedure CalculateAll; virtual;
      procedure CreateColumns; virtual;
      function  FindOperation ( const AOperated : TExtNumOperate ):TExtNumOperate; virtual;
    published
      property OnCalculate : TNotifyEvent read FOnCalculate write FOnCalculate;
      property OnCalculated : TNotifyEvent read FOnCalculated write FOnCalculated;
      property More   : TExtOperateColumns  read FColumns   write SetColumns;
      property IsMain : Boolean             read FMain    write FMain default True;
    end;

implementation

uses
    fonctions_erreurs,
    {$IFDEF FPC}
    LCLType,
    {$ENDIF}
    Forms,
    fonctions_numedit ;

{ TExtOperateForm }

procedure TExtOperateForm.p_setOperate(const AOperate: TExtOperate);
begin
  FOperate := AOperate;
end;

procedure TExtOperateForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove Then
   if AComponent = FOperate Then
     Operate := nil;
end;

procedure TExtOperateForm.p_FormShow(AObject: TObject);
begin
  if assigned ( FShowForm )
   Then
     FShowForm ( Self );
  Calculate;
end;

procedure TExtOperateForm.UnLinkForm;
begin
  if  not (csDesigning in ComponentState )
  and (Owner is TForm)
   Then ( Owner as TForm).OnShow := FShowForm ;
end;

procedure TExtOperateForm.SetLinkForm;
begin
  if  not (csDesigning in ComponentState )
  and (Owner is TForm)
   then
     Begin
      FShowForm := ( Owner as TForm).OnShow;
      (Owner as TForm).OnShow := p_FormShow;
     end;
end;

constructor TExtOperateForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOperate := nil;
  FOnCalculate  := nil;
  FOnCalculated := nil;
end;

destructor TExtOperateForm.Destroy;
begin
  UnLinkForm;
  inherited Destroy;
end;

procedure TExtOperateForm.Calculate;
begin
  if assigned ( Foperate ) Then
   Begin
     if assigned (FOnCalculate)
      Then FOnCalculate ( Self );
     FOperate.CalculateAll;
     if assigned (FOnCalculated)
      Then FOnCalculated ( Self );
   end;
end;

procedure TExtOperateForm.Loaded;
begin
  SetLinkForm;
  inherited Loaded;
end;

{ TExtOperateColumn }
procedure TExtOperateColumn.p_SetOperate(AOperate: TExtOperate);
begin
  if  ( AOperate <> FOperate )
  and ( AOperate <> Collection.Owner ) Then
    FOperate := AOperate;
end;

constructor TExtOperateColumn.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FOperate := nil;
end;

{ TExtOperateColumns }

// function TExtOperateColumns.GetColumn
// Getting the Column property from index
function TExtOperateColumns.GetColumn( Index: Integer): TExtOperateColumn;
begin
  if  ( Index >= 0 )
  Then Result := TExtOperateColumn( inherited Items[Index] )
  Else Result := nil;
end;

// procedure TExtOperateColumns.SetColum
// Setting the Column property with index
procedure TExtOperateColumns.SetColumn( Index: Integer; const Value: TExtOperateColumn);
begin
  if  assigned ( Value )
  and ( Index >= 0 )
   Then
    Items[Index].Assign( Value );
end;

// function TExtOperateColumns.Add
// Adding a Column
function TExtOperateColumns.Add: TExtOperateColumn;
begin
  result := TExtOperateColumn( Inherited Add );
end;


{ TExtNumOperate }

constructor TExtNumOperate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOperate   := nil;
  FOnCalculate  := nil;
  FOnCalculated := nil;
end;

procedure TExtNumOperate.CalculateEvent(const AOperated: TCustomEdit);
begin
  if  not ( AOperated.ClassNameIs(CST_EXTOPERATION ))
  and not ( AOperated.ClassNameIs(CST_EXTCOPERATION))
  and not ( AOperated.ClassNameIs(CST_EXTMOPERATION))
   Then
    Begin
      if assigned (FOnCalculate)
       Then FOnCalculate ( Self );
      Calculate;
      if assigned (FOnCalculated)
       Then FOnCalculated ( Self );

    end;
end;

procedure TExtNumOperate.p_setOperate(const AOperate: TExtOperate);
begin
  if FOperate <> AOperate
   Then
    Begin
      if assigned ( FOperate ) Then
        FOperate.RemoveOperation ( Self );
      FOperate := AOperate;
      if assigned ( FOperate ) Then
        FOperate.AddOperation    ( Self );
    end;
end;

procedure TExtNumOperate.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation=opRemove Then
   Begin
     if acomponent = FOperate Then
      Operate := nil;
   end;
end;


{ TExtOperate }

procedure TExtOperate.CreateColumns;
begin
  FColumns := TExtOperateColumns.Create(Self,TExtOperateColumn);
end;

procedure TExtOperate.AddOperation(AExtOperation: TExtNumOperate);
var i : Integer;
begin
  if csDesigning in ComponentState Then
    Exit;
  for i := 0 to gaO_Operations.count - 1 do
   Begin
     if AExtOperation = gaO_Operations [ i ] Then
       Exit;
   end;
  gaO_Operations.Add ( AExtOperation );
end;

procedure TExtOperate.RemoveOperation(AExtOperation: TExtNumOperate);
var i : Integer;
begin
  if csDesigning in ComponentState Then
    Exit;
  for i := 0 to gaO_Operations.count - 1 do
   Begin
     if AExtOperation = gaO_Operations [ i ] Then
      Begin
        gaO_Operations.Delete ( i );
        Exit;
      end;
   end;

end;

constructor TExtOperate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  gao_Operations := TList.Create;
  FMain := True;
  CreateColumns;
  FOnCalculate  := nil;
  FOnCalculated := nil;
end;

destructor TExtOperate.Destroy;
begin
  inherited Destroy;
  gao_Operations.Destroy;
end;

procedure TExtOperate.Calculate;
begin
  if FMain Then
   CalculateAll;
end;

// procedure TExtOperate.SetColumns
// This Component uses TExtOperateColumns
procedure TExtOperate.SetColumns(const AValue: TExtOperateColumns);
begin
  FColumns.Assign ( AValue );
end;

procedure TExtOperate.Notification(AComponent: TComponent; Operation: TOperation
  );
var i : integer ;
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove Then
   if AComponent is TExtOperate Then
    for i := 0 to FColumns.Count-1 do
     if AComponent=FColumns[i].Operate then
      Begin
        FColumns.Delete(i);
        Exit;
      end;
end;

procedure TExtOperate.CalculateAll;
var i : Integer;
Begin
  if assigned (FOnCalculate)
   Then FOnCalculate ( Self );
  for i := 0 to gaO_Operations.count -1 do
   if assigned (gaO_Operations [ i ]) Then
    (TExtNumOperate(gaO_Operations [ i ])).Calculate ( True );
  for i := 0 to FColumns.count -1 do
   if assigned (FColumns [ i ].FOperate ) Then
    FColumns [ i ].FOperate.CalculateAll;
  if assigned (FOnCalculated)
   Then FOnCalculated ( Self );
end;

function TExtOperate.FindOperation ( const AOperated : TExtNumOperate ):TExtNumOperate;
var i : Integer;
Begin
  Result := nil;
  for i := 0 to gaO_Operations.count -1 do
   if gaO_Operations [ i ] = AOPerated Then
    Begin
     Result := gaO_Operations [ i ];
     Break;
    End;
End;



{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_TExtOperate );
{$ENDIF}
end.
