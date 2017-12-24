unit fonctions_ibx;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils,IBCustomDataSet,db;

procedure p_DatasetCancelUpdatesIfEdited ( const AIBDataset : TIBCustomDataSet );

implementation

procedure p_DatasetCancelUpdatesIfEdited ( const AIBDataset : TIBCustomDataSet );
Begin
  with AIBDataset do
   if State in [dsEdit,dsInsert] then
    CancelUpdates;
end;

end.

