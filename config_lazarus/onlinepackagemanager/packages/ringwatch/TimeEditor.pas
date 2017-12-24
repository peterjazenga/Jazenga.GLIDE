{
 /***************************************************************************
                               TimeEditor.pas
                               --------
                 MeetTime Editor for TAnalogWatch.MeetTime

 ***************************************************************************/

 *****************************************************************************
 * These files are distributed under the Library GNU General Public License  *
 * (see the file COPYING.LPGL) with the following modification:              *
 *                                                                           *
 * As a special exception, the copyright holders of this library give you    *
 * permission to link this library with independent modules to produce an    *
 * executable, regardless of the license terms of these independent modules, *
 * and to copy and distribute the resulting executable under terms of your   *
 * choice, provided that you also meet, for each linked independent module,  *
 * the terms and conditions of the license of that module. An independent    *
 * module is a module which is not derived from or based on this library. If *
 * you modify this library, you may extend this exception to your version of *
 * the library, but you are not obligated to do so. If you do not wish to do *
 * so, delete this exception statement from your version.                    *
 *                                                                           *
 * If you didn't receive a copy of the file COPYING.LGPL, contact:           *
 *     Free Software Foundation, Inc.,                                       *
 *     675 Mass Ave                                                          *
 *     Cambridge, MA  02139                                                  *
 *     USA                                                                   *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Salvatore Coppola
}
unit TimeEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  Buttons, Spin, StdCtrls ,PropEdits;
type

  { TTimeEditorForm }

  TTimeEditorForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    procedure FormActivate(Sender: TObject);
  private
    { private declarations }
    FMeetTime: string;
    function GetMeetTime: string;
    procedure SetMeetTime(Value: string);
  public
    { public declarations }
    property MeetTime:string read GetMeetTime write SetMeetTime;
  end; 

  { TEditTimeProperty }

  TMeetTimeProperty = class(TStringPropertyEditor)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

implementation

function TMeetTimeProperty.GetAttributes: TPropertyAttributes;
begin
  Result:= [paDialog];
end;

procedure TMeetTimeProperty.Edit;
var
  TimeEditorForm: TTimeEditorForm;
  str1:string;
begin
  TimeEditorForm:=TTimeEditorForm.Create(Application);
  try
    str1:=GetValue;
    TimeEditorForm.MeetTime:=str1;
    if TimeEditorForm.ShowModal = mrOk then
      SetValue(TimeEditorForm.MeetTime);
  finally
    TimeEditorForm.Free;
  end;
end;

procedure TTimeEditorForm.FormActivate(Sender: TObject);
begin
  SpinEdit3.Width:=49;
  SpinEdit2.Width:=49;
  SpinEdit1.Width:=49;
end;

function TTimeEditorForm.GetMeetTime: string;
begin
  Result:=FormatFloat('00',SpinEdit1.Value)+TimeSeparator+FormatFloat('00',SpinEdit2.Value)+TimeSeparator+FormatFloat('00',SpinEdit3.Value);
end;

procedure TTimeEditorForm.SetMeetTime(Value: string);
begin
  FMeetTime:=Value;
  if Value='' then begin
    SpinEdit1.Value:=0;
    SpinEdit2.Value:=0;
    SpinEdit3.Value:=0;
  end else begin
    SpinEdit1.Value:=StrToInt(Copy(FMeetTime,1,2));
    SpinEdit2.Value:=StrToInt(Copy(FMeetTime,4,2));
    SpinEdit3.Value:=StrToInt(Copy(FMeetTime,7,2))
  end;
end;


initialization
  {$I timeeditor.lrs}

end.

