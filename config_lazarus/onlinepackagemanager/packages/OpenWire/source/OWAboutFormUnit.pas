unit OWAboutFormUnit;

{$MODE DELPHI}{$H+}

interface

uses

  LCLIntf, LMessages, LResources,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TOWAboutForm = class(TForm)
    Panel2: TPanel;
    Panel3: TPanel;
    Image1: TImage;
    BitBtn1: TBitBtn;
    Label1: TLabel;
    OWLabel: TLabel;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure OWLabelClick(Sender: TObject);
    procedure OWLabelMouseEnter(Sender: TObject);
    procedure OWLabelMouseLeave(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

implementation

{$R *.lfm}  //=== ct9999 ========

procedure TOWAboutForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if( Key = #27 ) then
    begin
    Close();
    Key := '0';
    end;

end;

procedure TOWAboutForm.OWLabelClick(Sender: TObject);
begin
  OpenURL( 'http://www.openwire.org' );
end;

procedure TOWAboutForm.OWLabelMouseEnter(Sender: TObject);
begin
  OWLabel.Font.Color := clRed;
end;

procedure TOWAboutForm.OWLabelMouseLeave(Sender: TObject);
begin
  OWLabel.Font.Color := clBlue;
end;

procedure TOWAboutForm.FormCreate(Sender: TObject);
begin
  OWLabel.OnMouseEnter := OWLabelMouseEnter;
  OWLabel.OnMouseLeave := OWLabelMouseLeave;
end;

end.
