{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit wst_design;

{$warn 5023 off : no warning about unused units}
interface

uses
  wstimportdlg, wst_register, uwsttypelibraryedit, uabout, udm, ubindingedit, 
  ueditoptions, ufarrayedit, ufclassedit, ufEnumedit, ufpropedit, 
  ufrecordedit, ufrmsaveoption, uftypealiasedit, uinterfaceedit, umoduleedit, 
  uprocedit, view_helper, uargedit, command_line_parser, generator, 
  generatorbase, generatorj, locators, logger_intf, metadata_generator, 
  parserutils, pascal_parser_intf, source_utils, ws_parser_imp, 
  wsdl_generator, wsdl_parser, wst_resources_utils, xsd_consts, xsd_generator, 
  xsd_parser, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('wst_register', @wst_register.Register);
end;

initialization
  RegisterPackage('wst_design', @Register);
end.
