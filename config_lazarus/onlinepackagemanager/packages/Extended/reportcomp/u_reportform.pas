unit u_reportform;

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}


interface

uses
{$IFDEF FPC}
{$ELSE}
{$ENDIF}
  ExtCtrls,
  u_formadapt,
  RLRichFilter, RLPDFFilter,
  RLHTMLFilter, u_reports_rlcomponents,
  RLDraftFilter, Classes, Controls, RLReport;

type
  { TReportForm }

  TReportForm = class(TF_FormAdapt)
    RLDraftFilter: TRLDraftFilter;
    RLHTMLFilter: TRLHTMLFilter;
    RLPDFFilter: TRLPDFFilter;
    RLReport: TExtReport;
    Panel1: TPanel;
    RLRichFilter: TRLRichFilter;
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation


{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}



end.

