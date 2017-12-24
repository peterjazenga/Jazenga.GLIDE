{ string const unit fo DB-aware modules

  Copyright (C) 2005-2017 Lagunov Aleksey alexs75@yandex.ru and Lazarus team
  original conception from rx library for Delphi (c)

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit rxdconst;

interface


resourcestring

{ RxDBCtrl }

  SLocalDatabase         = 'Unable complete this operation on local dataset';
  SRetryLogin            = 'Retry to connect with database?';
  SExprNotBoolean        = 'Field ''%s'' is not boolean';
  SExprBadNullTest       = 'NULL-values enabled in ''='' and ''<>''';
  SExprBadField          = 'Field ''%s'' not used in filter expression';
  SCaptureFilter         = 'Control locked by filter';
  SNotCaptureFilter      = 'Control needs to be locked by filter';
  SInactiveData          = 'inactive';
  SBrowseData            = 'browse';
  SEditData              = 'editing';
  SInsertData            = 'append';
  SSetKeyData            = 'find';
  SCalcFieldsData        = 'calc';
  SRegistration          = 'Register';
  SAppTitleLabel         = 'Application "%s"';
  SHintLabel             = 'Enter your user name and password';
  SUserNameLabel         = '&User name:';
  SPasswordLabel         = '&Password:';
  SMore1                 = '&More >>';
  SMore2                 = '&Less <<';
  SInvalidUserName       = 'User name or password is not valid';
  SChangePassword        = 'Change password';
  SOldPasswordLabel      = '&Old password:';
  SNewPasswordLabel      = '&New password:';
  SConfirmPasswordLabel  = '&Confirm:';
  SPasswordChanged       = 'Password changed';
  SPasswordNotChanged    = 'Password not changed';
  SPasswordsMismatch     = 'New password and confirmation not equal';
  SDBExceptCaption       = 'Error in DB engine';
  SServerErrorLabel        = 'Server error';
  SErrorMsgLabel           = 'Error message';
  SNextButton              = '&Next';
  SPrevButton              = '&Prior';
  SExprIncorrect           = 'Error in filter expression';
  SExprTermination         = 'Error in filter end';
  SExprNameError           = 'Error in filed name';
  SExprStringError         = 'Error in string const';
  SExprInvalidChar         = 'Error symbol in expression: ''%s''';
  SExprNoRParen            = 'Error '')'', error: %s';
  SExprExpected            = 'Error %s';
  SExprBadCompare          = 'Compare opertion needs field and const';
  SConfirmSave             = 'Data changed. Save?';
  SDatabaseName            = 'Database locked: %s';
  SUnlockCaption           = 'Unlock';
  SUnlockHint              = 'Enter your password';
  SDeleteMultipleRecords   = 'Delete all selected records?';

  SDBComboBoxFieldNotAssigned        = '%s:TDBComboBox - DataField not assigned';

  //SPropDefByLookup         = 'PropDefByLookup';
  //SDataSourceFixed         = 'SDataSourceFixed';
  SCircularDataLink        = 'Circular data link';
  sRxAscendign             = 'Ascendign';
  sRxDescending            = 'Descending';
  sRxAllFields             = 'All fields';
  sRxFieldsLookupDisplay   = 'Fields as LookupDisplay';
  sRxFillFieldsLookupDisp  = 'Fill fields in LookupDisplay property';
  sRxSortFieldsDisplay     = 'Fields as SortField';
  sRxFillSortFieldsDisp    = 'Fill fields in SortField property';


  SDeleteRecordQuestion    = 'Delete record?';
  SFieldTypeMismatch       = 'Type mismatch for field ''%s'', expecting: %s actual: %s';
  SInvalidDate             = 'Invalid Date';
  SFieldRequired           = 'Field ''%s'' must have a value';
  SNotEditing              = 'Dataset not in edit or insert mode';
  SUnknownFieldType        = 'SUnknownFieldType %s';
  SFieldReadOnly           = 'SFieldReadOnly %s';

  //RXDBgrid
  sRxDBGridFind               = 'Find data';
  sRxDBGridFilter             = 'Filter data';
  sRxDBGridFilterSimple       = 'Filter in table';
  sRxDBGridFilterClear        = 'Clear filter';
  sRxDBGridSortByColumns      = 'Sort data for columns';
  sRxDBGridSelectColumns      = 'Select visible columns';
  sRxDBGridEmptiFilter        = '(Empty)';
  sRxDBGridAllFilter          = '(All values)';
  sRxDBGridSelectAllRows      = 'Select all rows';
  sRxDBGridCopyCellValue      = 'Copy cell value';
  sRxDBGridOptimizeColWidth   = 'Optimize column width';

  //RxDBGrid filter form
  sRxFilterFormSelectExp   = 'Enter filter expression for data in table:';
  sRxFilterFormOnField     = 'On field :';
  sRxFilterFormOperaion    = 'Operation :';
  sRxFilterFormCondition   = 'Condition :';
  sRxFilterFormOperand     = 'Operand :';
  sRxFilterFormEnd         = 'end.';
  sRxFilterFormClear       = 'Clear filter';
  sRxFilterFormCancel      = 'Cancel';
  sRxFilterFormApply       = 'Apply';
  sRxFilterFormCaption     = 'Filter conditions';

  //TrxSortByForm
  sRxSortByFormCaption     = 'Sort on field';
  sRxSortByFormAllFields   = '&Fields in dataset:';
  sRxSortByFormSortFields  = '&Selected fields:';
  sRxSortByFormSortOrder   = 'Select f&ield for sort data:';
  sRxSortByFormAddField    = '&Add field';
  sRxSortByFormRemoveField = '&Remove';
  sRxSortByFormMoveUpField = '&Up';
  sRxSortByFormMoveDnField = '&Down';
  sRxSortByFormCaseInsens  = '&Case insensitive sort';

  //TRxMemoryData
  SMemNoRecords            = 'No data found';
  SInvalidFields           = 'No fields defined';

  //TrxDBGridFindForm
  sRxDbGridFindCaption     = 'Find data';
  sRxDbGridFindText        = 'Text to find';
  sRxDbGridFindOnField     = 'Find on field';
  sRxDbGridFindCaseSens    = 'Case sensetive';
  sRxDbGridFindPartial     = 'Partial key';
  sRxDbGridFindDirecion    = 'Direction';
  sRxDbGridFindRangeAll    = 'All';
  sRxDbGridFindRangeForw   = 'Forward';
  sRxDbGridFindRangeBack   = 'Backward';
  sRxFindMore              = 'Find more';

  //TrxDBGridColumsForm
  sRxDbGridSelColCaption   = 'Grid columns';
  sRxDbGridSelColHint1     = 'Move selected column up';
  sRxDbGridSelColHint2     = 'Move selected column down';
  sRxDbGridSelApplyCaption = 'Apply';
  sRxDbGridSelApplyHint    = 'Apply current column settings';

  //seldsfrm
  sRxBorrowStructure       = 'Borrow structure...';
  sRxSelectDatasetStruct   = 'Select dataset to copy to';
  sRxCopyOnlyMetadata      = 'Copy only metadata';
  sRxSourceDataset         = 'Source dataset';

  sUnknownXMLDatasetFormat = 'Unknown XML Dataset format';

  sExportParams              = 'Export params';
  sToolsExportSpeadSheet     = 'Export to speadsheet';
  sToolsExportPDF            = 'Export to PDF file';
  sExportFileName            = 'Export file name';
  sOpenAfterExport           = 'Open after export';
  sPageName                  = 'Page name';
  sExportColumnHeader        = 'Export column header';
  sExportColumnFooter        = 'Export column footer';
  sExportCellColors          = 'Export cell colors';
  sExportFormula             = 'Export footer formula';
  sExportImages              = 'Export images';
  sExportSelectedRows        = 'Export only selected rows';
  sOverwriteExisting         = 'Overwrite existing';
  sShowColumnHeaderOnAllPage = 'Show column header on all pages';
  sPageMargins               = 'Page margins';
  sLeftCaption               = 'Left';
  sTopCaption                = 'Top';
  sRightCaption              = 'Right';
  sBottomCaption             = 'Bottom';
  sReportTitle               = 'Report title';
  sOrientation               = 'Orientation';
  sPortrait                  = 'Portrait';
  sLandscape                 = 'Landscape';
  sPrintOptions              = 'Print options';
  sPaperType                 = 'Paper type';
  sTitleColor                = 'Title color';
  sGlobal                    = 'Global';
  sPDFOptions                = 'PDF options';
  sOutLine                   = 'Out line';
  sCompressText              = 'Compress text';
  sCompressFonts             = 'Compress fonts';
  sCompressImages            = 'Compress images';
  sUseRawJPEG                = 'Use raw JPEG';

  sShowTitle                 = 'Show column title';
  sShowFooter                = 'Show footer';
  sShowFooterColor           = 'Show footer color';
  sShowGridColor             = 'Show grid color';
  sShowReportTitle           = 'Show report title';
  sPrintGrid                 = 'Print grid';
  sHideZeroValues            = 'Hide zero values';

  sRxDBGridToolsCaption      = 'Totals row';
  sfvtNon                    = 'None';
  sfvtSum                    = 'Sum';
  sfvtAvg                    = 'AVG';
  sfvtCount                  = 'Count';
  sfvtFieldValue             = 'Field value';
  sfvtStaticText             = 'Static text';
  sfvtMax                    = 'Max value';
  sfvtMin                    = 'Min value';
  sfvtRecNo                  = 'Record no';
  sSetupTotalRow             = 'Setup total row';
  sCollumnName               = 'Column name';
  sFunction                  = 'Function';
  sBlobText                  = '(blob)';
  sOtherOptions              = 'Other options';
  sFooterRowColor            = 'Footer row color';



const
  { The following strings should not be localized }
  sAction       = '.Action';
  sCount        = '.Count';
  sVisible      = '.Visible';
  sItem         = '.Item';
  sWidth        = '.Width';
  sTop          = '.Top';
  sVersion      = '.Version';
  sLeft         = '.Left';
  sShowHint     = '.ShowHint';
  sShowCaption  = '.ShowCaption';
  sToolBarStyle = '.ToolBarStyle';
  sButtonAllign = '.ButtonAllign';
  sOptions      = '.Options';
  sCaption      = '.Caption';
  sIndex        = '.Index';
  sSortMarker   = '.SortMarker';
  sSortField    = '.SortField';
  sShortCut     = '.ShortCut';


implementation


end.
