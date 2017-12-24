{*********************************************************************}
{                                                                     }
{                                                                     }
{             Matthieu Giroux                                         }
{             TExtCOperation  :                                       }
{             Operating 2 numbers              }
{             TExtCDBOperation :                                       }
{             Operating 2 numbers in database }
{             22 Avril 2006                                           }
{                                                                     }
{                                                                     }
{*********************************************************************}

unit u_extconstoperation;

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
  U_ExtOperation,
  U_ExtNumEdits,U_ExtOperate ;

{$IFDEF VERSIONS}
const
     gVer_TExtCOperation : T_Version = ( Component : 'Composant TExtCOperation' ;
                                         FileUnit : 'U_ExtOperation' ;
                                         Owner : 'Matthieu Giroux' ;
                                         Comment : 'Opération de nombres.' ;
                                         BugsStory : '1.0.0.0 : TCustomEdit recognize but blank at beginning.' +#13#10
                                                   + '0.9.9.0 : Tested.'+#13#10
                                                   + '0.9.0.0 : Not tested.';
                                         UnitType : 3 ;
                                         Major : 1 ; Minor : 0 ; Release : 0 ; Build : 0 );
{$ENDIF}

type

  { TExtCOperation }

  TExtCOperation   = class(TExtNumOperate)
  private
    FOperated1 : TExtNumEdit;
    FOperated2 : Extended;
    FOperation : TAnOperation;
    FOperated1Exit : TNotifyEvent;
    FLevel     : Byte;
//    FIsAll     : Boolean;
    procedure p_setOperated1 (const AOperated :TExtNumEdit);
    procedure p_setOperated2 (const AOperated :Extended);
    procedure p_setOperation (const AOperation:TAnOperation);
    procedure p_setLevel     (const ALevel    : Byte );
//    procedure p_setIsAll     (const AIsAll    : Boolean );
  protected
    procedure UnLinkOperated1; virtual;
    procedure SetLinkOperated1; virtual;
    procedure p_Operated1Exit(AObject: TObject); virtual;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    property ReadOnly default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Calculate ( const ab_all : Boolean = False ); override;
  published
    property Operated1 : TExtNumEdit  read FOperated1 write p_setOperated1;
    property Operated2 : Extended     read FOperated2 write p_setOperated2;
    property Exit1Operated : TNotifyEvent  read FOperated1Exit write FOperated1Exit;
    property Operation : TAnOperation read FOperation write p_setOperation;
    property Level     : Byte         read FLevel     write p_setLevel;
//    property IsAll     : Boolean     read FIsAll     write p_setIsAll;
  end;

implementation

uses
    fonctions_erreurs,
    {$IFDEF FPC}
    LCLType,
    {$ELSE}
    Windows,
    {$ENDIF}
    u_extcomponent,
    fonctions_string,
    fonctions_numedit,
    fonctions_proprietes ;

{ TExtCOperation }

procedure TExtCOperation.p_setOperated1(const AOperated: TExtNumEdit);
begin
  if  (AOperated <> Self)
  and (AOperated <> FOperated1)
   Then
    Begin
      UnLinkOperated1;
      FOperated1 := AOperated;
      SetLinkOperated1;
      if ([csDesigning,csLoading] * ComponentState = [])
      and not (csCreating in ControlState) Then
        Calculate;
    end;
end;


procedure TExtCOperation.p_setOperated2(const AOperated: Extended);
begin
  FOperated2 := AOperated;
  Calculate;
end;

procedure TExtCOperation.p_setOperation(const AOperation: TAnOperation);
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

procedure TExtCOperation.p_setLevel(const ALevel: Byte);
var ab_calculate : Boolean;
begin
  if alevel <> FLevel Then
   Begin
     ab_calculate := Alevel < FLevel ;
     Flevel := Alevel;
     if ab_calculate
      Then
       Calculate ( True );
   end;
end;

procedure TExtCOperation.UnLinkOperated1;
begin
  if not ( csDesigning in ComponentState )
  and assigned ( FOperated1 )
  and not ( FOperated1.ClassNameIs ( CST_EXTOPERATION ))
  and not ( FOperated1.ClassNameIs ( CST_EXTCOPERATION ))
  and not ( FOperated1.ClassNameIs ( CST_EXTMOPERATION ))
   Then
    Begin
      if Assigned ( FOperated1Exit )
       Then FOperated1.OnExit := FOperated1Exit
       Else FOperated1.OnExit := nil;
    end;

end;

procedure TExtCOperation.SetLinkOperated1;
begin
  if not ( csDesigning in ComponentState )
  and assigned ( FOperated1 )
  and not ( FOperated1.ClassNameIs ( CST_EXTOPERATION ))
  and not ( FOperated1.ClassNameIs ( CST_EXTCOPERATION ))
  and not ( FOperated1.ClassNameIs ( CST_EXTMOPERATION ))
   Then
    Begin
      FOperated1Exit := FOperated1.OnExit;
      FOperated1.OnExit := p_Operated1Exit;
    end;
end;

procedure TExtCOperation.p_Operated1Exit ( AObject : TObject );
Begin
  if assigned ( FOperated1Exit ) Then
   FOperated1Exit ( Self );
  CalculateEvent(FOperated1);
end;

procedure TExtCOperation.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation=opRemove Then
   Begin
     if acomponent = FOperated1 Then
      Operated1 := nil;
   end;
end;

constructor TExtCOperation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.ReadOnly := True;
  FOperated1 := nil;
  FOperated1Exit := nil;
end;

destructor TExtCOperation.Destroy;
begin
  inherited Destroy;
end;

procedure TExtCOperation.Loaded;
Begin
  inherited;
  Calculate;
end;

procedure TExtCOperation.Calculate(const ab_all: Boolean);
begin
  if ( FOperated1 = nil )
  or ( csdesigning in ComponentState )
  or ( csloading in ComponentState )
   Then Exit;
  Value := fe_Calculate ( FOperated1.Value,  FOperated2, FOperation );
  if not ab_all
  and assigned ( operate ) Then
    Operate.Calculate;
end;

{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_TExtCOperation );
{$ENDIF}
end.
