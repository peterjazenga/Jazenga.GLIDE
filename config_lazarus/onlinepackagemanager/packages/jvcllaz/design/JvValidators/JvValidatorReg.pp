unit JvValidatorReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, SysUtils, ComponentEditors, PropEdits, ImageListEditor,
  GraphPropEdits;

procedure Register;

implementation

{$R ..\..\resource\JvValidatorsReg.res}

uses
  JvDsgnConsts, JvValidators, JvErrorIndicator, JvValidatorsEditorForm;

procedure Register;
begin
  RegisterComponents(RsPaletteValidators, [TJvValidators, TJvValidationSummary, TJvErrorIndicator]);

  RegisterNoIcon([TJvRequiredFieldValidator, TJvCompareValidator,
    TJvRangeValidator, TJvRegularExpressionValidator, TJvCustomValidator, TJvControlsCompareValidator]);

  RegisterComponentEditor(TJvValidators, TJvValidatorEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TJvErrorIndicator, 'ImageIndex', TImageIndexPropertyEditor);
//  RegisterPropertyEditor(TypeInfo(string), TJvCustomFormatEdit, 'Characters', TJvCharStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvBaseValidator, 'PropertyToValidate', TJvPropertyValidateProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvBaseValidator, 'CompareToProperty', TJvPropertyToCompareProperty); end;


end.
