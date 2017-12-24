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

unit u_extmultioperation;

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
  U_ExtNumEdits,
  U_ExtOperation;

{$IFDEF VERSIONS}
const
     gVer_TExtMOperation : T_Version = ( Component : 'Composant TExtMOperation' ;
                                         FileUnit : 'U_ExtOperation' ;
                                         Owner : 'Matthieu Giroux' ;
                                         Comment : 'Opération de 3 nombres.' ;
                                         BugsStory : '1.0.0.0 : TCustomEdit recognize but blank at beginning.' +#13#10
                                                   + '0.9.9.0 : Tested.'+#13#10
                                                   + '0.9.0.0 : Not tested.';
                                         UnitType : 3 ;
                                         Major : 1 ; Minor : 0 ; Release : 0 ; Build : 0 );
{$ENDIF}

type
  { TExtMOperation }

  TExtMOperation   = class(TExtOperation)
  private
    FOperated3 : TExtOperation;
    FOperated3Exit : TNotifyEvent;
    FOperation2 : TAnOperation;
    p_setOperation: TAnOperation;
    procedure p_setOperated3 (const AOperated :TExtOperation);
    procedure p_setOperation2 (const AOperation:TAnOperation);
  protected
    procedure p_Operated3Exit(AObject: TObject); virtual;
    procedure UnLinkOperated3; virtual;
    procedure SetLinkOperated3; virtual;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
    destructor Destroy; override;
    procedure Calculate ( const ab_all : Boolean = False ); override;
  published
    property ExitOperated3 : TNotifyEvent  read FOperated3Exit write FOperated3Exit;
    property Operated3  : TExtOperation read FOperated3 write p_setOperated3;
    property Operation2 : TAnOperation  read FOperation2 write p_setOperation default aoPlus;
  end;

implementation

uses
    fonctions_erreurs,
    {$IFDEF FPC}
    LCLType,
    {$ENDIF}
{$IFNDEF FPC}
  Windows,
{$ENDIF}
    u_extcomponent,
    fonctions_string,
    fonctions_numedit,
    fonctions_proprietes ;

{ TExtMOperation }

procedure TExtMOperation.p_setOperated3(const AOperated: TExtOperation);
begin
  if  (AOperated <> Self)
  and (AOperated <> FOperated3) Then
   Begin
    UnLinkOperated3;
    FOperated3 := AOperated;
    SetLinkOperated3;
    if ([csDesigning,csLoading] * ComponentState = [])
    and not (csCreating in ControlState) Then
      Calculate;
   end;
end;

procedure TExtMOperation.p_setOperation2(const AOperation: TAnOperation);
begin
  if FOperation2 <> AOperation
   Then
    Begin
     FOperation2 := AOperation;
     if ([csDesigning,csLoading] * ComponentState = [])
     and not (csCreating in ControlState) Then
        Calculate;
    end;
end;

procedure TExtMOperation.p_Operated3Exit(AObject: TObject);
begin
  if assigned ( FOperated3Exit ) Then
   FOperated3Exit ( Self );
  CalculateEvent(FOperated3);
end;

procedure TExtMOperation.UnLinkOperated3;
begin
  if not ( csDesigning in ComponentState )
  and assigned ( FOperated3 )
  and not ( FOperated3.ClassNameIs ( CST_EXTOPERATION ))
  and not ( FOperated3.ClassNameIs ( CST_EXTCOPERATION ))
  and not ( FOperated3.ClassNameIs ( CST_EXTMOPERATION ))
   Then
    Begin
      if Assigned ( FOperated3Exit )
        Then FOperated3.OnExit := FOperated3Exit
        Else FOperated3.OnExit := nil;
    end;
end;

procedure TExtMOperation.SetLinkOperated3;
begin
  if not ( csDesigning in ComponentState )
  and assigned ( FOperated3 )
  and not ( FOperated3.ClassNameIs ( CST_EXTOPERATION ))
  and not ( FOperated3.ClassNameIs ( CST_EXTCOPERATION ))
  and not ( FOperated3.ClassNameIs ( CST_EXTMOPERATION ))
   Then
    Begin
      FOperated3Exit := FOperated3.OnExit;
      FOperated3.OnExit := p_Operated3Exit;
    end;
end;

procedure TExtMOperation.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation=opRemove Then
   Begin
     if acomponent = FOperated3 Then
      Operated3 := nil;
   end;
end;

constructor TExtMOperation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOperated3 := nil;
  FOperation2 := aoPlus;
  FOperated3Exit := nil;
end;

destructor TExtMOperation.Destroy;
begin
  UnLinkOperated3;
  inherited Destroy;
end;

procedure TExtMOperation.Loaded;
Begin
  inherited;
  Calculate;
end;

procedure TExtMOperation.Calculate(const ab_all: Boolean);
begin
  if ( Operated1  = nil )
  or ( Operated2  = nil )
  or ( FOperated3 = nil )
  or ( csloading in ComponentState )
  or ( csdesigning in ComponentState )
   Then Exit;
  Value := fe_Calculate (
           fe_Calculate ( fvar_getComponentProperty ( Operated1, CST_PROPERTY_VALUE),  fvar_getComponentProperty ( Operated2, CST_PROPERTY_VALUE),
                          Operation )
                        , fvar_getComponentProperty ( FOperated3, CST_PROPERTY_VALUE), FOperation2 );
  if not ab_all
  and assigned ( operate ) Then
    Operate.Calculate;
end;

{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_TExtMOperation );
{$ENDIF}
end.
