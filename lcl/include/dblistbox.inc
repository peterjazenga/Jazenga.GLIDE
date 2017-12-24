{%MainUnit ../dbctrls.pp}

{******************************************************************************
                                     TDBListBox
                    data aware ListBox, base found in dbctrls.pp
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

// included by dbctrls.pp

{ TDBListBox }

{ Protected Methods }

procedure TDBListBox.DataChange(Sender: TObject);
var
  DataLinkField: TField;
begin
  DataLinkField := FDataLink.Field;
  if Assigned(DataLinkField) then
  begin
  // changed by Peter Dunne (Jazenga.GLIDE) to link by index to numeric data fields
    if DataLinkField is TNumericField then
    ItemIndex := DataLinkField.AsInteger else
    ItemIndex := Items.IndexOf(DataLinkField.Text)
   end
  else
    ItemIndex := -1;
end;

procedure TDBListBox.DoSelectionChange(User: Boolean);
begin
  if User then
  begin
    if FDataLink.CanModify then
    begin
      //protect against undesired call to DataChange
      FDataLink.OnDataChange := nil;
      if FDataLink.Edit then
        FDataLink.Modified;
      FDataLink.OnDataChange := @DataChange;
    end
    else
      DataChange(Self);
  end;
  inherited DoSelectionChange(User);
end;

procedure TDBListBox.UpdateData(Sender: TObject);
begin
  if ItemIndex >= 0 then
  begin
    // changed by Peter Dunne (Jazenga.GLIDE) to link by index to numeric data fields
  if FDataLink.Field is TNumericField then
  FDataLink.Field.AsInteger:=ItemIndex
  else
    FDataLink.Field.Text := Items[ItemIndex];
  end;
end;

procedure TDBListBox.EditingDone;
begin
  FDataLink.UpdateRecord;
  inherited EditingDone;
end;

