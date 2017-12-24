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

unit U_ExtOperation;

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

interface

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}

uses
  SysUtils, Classes, Graphics, Controls,
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
  fonctions_numedit,
  StdCtrls,
  U_ExtNumEdits, U_ExtOperate;

{$IFDEF VERSIONS}
const
    gVer_TExtOperation : T_Version = ( Component : 'Composant TExtOperation' ;
                                               FileUnit : 'U_ExtOperation' ;
                                               Owner : 'Matthieu Giroux' ;
                                               Comment : 'Opération de nombres.' ;
                                               BugsStory : '1.0.0.0 : TCustomEdit recognize but blank at beginning.' +#13#10
                                                         + '0.9.9.9 : Tested but blank at beginning.' +#13#10
                                                         + '0.9.9.0 : Not fully tested.' ;
                                               UnitType : 3 ;
                                               Major : 1 ; Minor : 0 ; Release : 0 ; Build : 0 );
{$ENDIF}

type

  TAnOperation = ( aoNone, aoMinus, aoPlus, aoDivide, aoMultiple, aoExponent, aoLogn );
const
  TAnOperationChars = ' -+/*el';

function fe_Calculate ( const AValue1, AValue2 : TAFloat ; const AnOperation : TAnOperation ) : TAFloat;

type

  { TExtOperation }

  TExtOperation   = class(TExtNumOperate)
  private
    FOperated1,
    FOperated2 : TCustomEdit;
    FOperated1Exit,
    FOperated2Exit : TNotifyEvent;
    FOperation : TAnOperation;
    procedure p_setOperated1 (const AOperated :TCustomEdit);
    procedure p_setOperated2 (const AOperated :TCustomEdit);
    procedure p_setOperation (const AOperation:TAnOperation);
  protected
    procedure UnLinkOperated1; virtual;
    procedure SetLinkOperated1; virtual;
    procedure UnLinkOperated2; virtual;
    procedure SetLinkOperated2; virtual;
    procedure p_Operated1Exit(AObject: TObject); virtual;
    procedure p_Operated2Exit(AObject: TObject); virtual;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    property ReadOnly default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Calculate ( const ab_all : Boolean = False ); override;
    procedure Loaded; override;
  published
    property ExitEdit1 : TNotifyEvent  read FOperated1Exit write FOperated1Exit;
    property ExitEdit2 : TNotifyEvent  read FOperated2Exit write FOperated2Exit;
    property Operated1 : TCustomEdit  read FOperated1 write p_setOperated1;
    property Operated2 : TCustomEdit  read FOperated2 write p_setOperated2;
    property Operation : TAnOperation read FOperation write p_setOperation default aoPlus;
  end;

implementation

uses
    fonctions_erreurs,
    {$IFDEF FPC}
    LCLType,
    {$ELSE}
    Windows,
    {$ENDIF}
    Math,
    fonctions_proprietes ;

{ TExtOperation }

procedure TExtOperation.p_setOperated1(const AOperated: TCustomEdit);
begin
  if  (AOperated <> Self)
  and (AOperated <> FOperated1) Then
    Begin
      UnLinkOperated1;
      FOperated1 := AOperated;
      SetLinkOperated1;
      if ([csDesigning,csLoading] * ComponentState = [])
      and not (csCreating in ControlState) Then
        Calculate;
    end;
end;

procedure TExtOperation.p_setOperated2(const AOperated: TCustomEdit);
begin
  if  (AOperated <> Self)
  and (AOperated <> FOperated2) Then
   Begin
     UnLinkOperated2;
     FOperated2 := AOperated;
     SetLinkOperated2;
     if ([csDesigning,csLoading] * ComponentState = [])
     and not (csCreating in ControlState) Then
       Calculate;
   end;
end;

procedure TExtOperation.p_setOperation(const AOperation: TAnOperation);
begin
  if FOperation <> AOperation
   Then
    Begin
      FOperation := AOperation;
      if ([csDesigning,csLoading] * ComponentState = [])
      and not (csCreating in ControlState) Then
         Calculate;
    end;
end;

procedure TExtOperation.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation=opRemove Then
   Begin
     if acomponent = FOperated1 Then
      Operated1 := nil;
     if acomponent = FOperated2 Then
      Operated2 := nil;
   end;
end;

constructor TExtOperation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.ReadOnly := True;
  FOperated1 := nil;
  FOperated2 := nil;
  FOperated1Exit := nil;
  FOperated2Exit := nil;
  FOperation := aoPlus;
end;

destructor TExtOperation.Destroy;
begin
  UnLinkOperated1;
  UnLinkOperated2;
  inherited Destroy;
end;

 procedure TExtOperation.SetLinkOperated1;
var lmet_MethodeDistribuee: TMethod;
Begin
  if not ( csDesigning in ComponentState )
  and assigned ( FOperated1 )
  and not ( FOperated1.ClassNameIs ( CST_EXTOPERATION ))
  and not ( FOperated1.ClassNameIs ( CST_EXTCOPERATION ))
  and not ( FOperated1.ClassNameIs ( CST_EXTMOPERATION ))
   Then
    Begin
      lmet_MethodeDistribuee.Data := Self;
      lmet_MethodeDistribuee.Code := MethodAddress('p_Operated1Exit');
      FOperated1Exit := TNotifyEvent(fmet_getComponentMethodProperty(FOperated1,'OnExit'));
      p_SetComponentMethodProperty (FOperated1,'OnExit', lmet_MethodeDistribuee);
    end;

end;


procedure TExtOperation.SetLinkOperated2;
var lmet_MethodeDistribuee: TMethod;
Begin
  if not ( csDesigning in ComponentState )
  and assigned ( FOperated2 )
  and not ( FOperated2.ClassNameIs ( CST_EXTOPERATION ))
  and not ( FOperated2.ClassNameIs ( CST_EXTCOPERATION ))
  and not ( FOperated2.ClassNameIs ( CST_EXTMOPERATION ))
   Then
    Begin
      lmet_MethodeDistribuee.Data := Self;
      lmet_MethodeDistribuee.Code := MethodAddress('p_Operated2Exit');
      FOperated2Exit := TNotifyEvent(fmet_getComponentMethodProperty(FOperated2,'OnExit'));
      p_SetComponentMethodProperty (FOperated2,'OnExit', lmet_MethodeDistribuee);
    end;

end;

procedure TExtOperation.p_Operated1Exit ( AObject : TObject );
Begin
  if assigned ( FOperated1Exit ) Then
   FOperated1Exit ( Self );
  CalculateEvent ( FOperated1 );
end;

procedure TExtOperation.p_Operated2Exit ( AObject : TObject );
Begin
  if assigned ( FOperated2Exit ) Then
   FOperated2Exit ( Self );
  CalculateEvent ( FOperated2 );
end;

procedure TExtOperation.UnLinkOperated1;
Begin
  if not ( csDesigning in ComponentState )
  and assigned ( FOperated1 )
  and not ( FOperated1.ClassNameIs ( CST_EXTOPERATION ))
  and not ( FOperated1.ClassNameIs ( CST_EXTCOPERATION ))
  and not ( FOperated1.ClassNameIs ( CST_EXTMOPERATION ))
    Then
       p_SetComponentMethodProperty (FOperated1,'OnExit', TMethod(FOperated1Exit));
end;

procedure TExtOperation.UnLinkOperated2;
Begin
  if not ( csDesigning in ComponentState )
  and assigned ( FOperated2 )
  and not ( FOperated2.ClassNameIs ( CST_EXTOPERATION ))
  and not ( FOperated2.ClassNameIs ( CST_EXTCOPERATION ))
  and not ( FOperated2.ClassNameIs ( CST_EXTMOPERATION ))
   Then
    p_SetComponentMethodProperty (FOperated2,'OnExit', TMethod(FOperated2Exit));
end;

procedure TExtOperation.Loaded;
Begin
  inherited;
  Calculate;
end;

procedure TExtOperation.Calculate(const ab_all: Boolean);
begin
  if ( FOperated1 = nil )
  or ( FOperated2 = nil )
  or ( csloading   in ComponentState )
  or ( csCreating  in   ControlState )
  or ( csdesigning in ComponentState )
   Then Exit;
  Value := fe_Calculate ( fvar_getComponentProperty ( FOperated1, CST_PROPERTY_VALUE),  fvar_getComponentProperty ( FOperated2, CST_PROPERTY_VALUE), FOperation );
  if not ab_all
  and assigned ( operate ) Then
    Operate.Calculate;
end;

function fe_Calculate ( const AValue1, AValue2 : TAFloat ; const AnOperation : TAnOperation ) : TAFloat;
var i : integer;
Begin
  case AnOperation of
  aoPlus    : Result := AValue1 + AValue2;
  aoMinus   : Result := AValue1 - AValue2;
  aoDivide  : if AValue1 = 0
               Then Result := 0
              else if AValue2 = 0
              Then Result := TMaxFloat
              Else Result := AValue1 / AValue2;
  aoMultiple: Result := AValue1 * AValue2;
  aoExponent: Result := Power( AValue1, AValue1);
  aoLogn    : Result := logn (AValue1 , AValue1 );
  End;
End;

{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_TExtOperation );
{$ENDIF}
end.
