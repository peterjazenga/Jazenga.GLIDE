<?PHP
// 
// Converted From Pascal By Pas2Php v0.8
// Converts Object Pascal to PHP
// http://www.wascal.net/?page=pas2php
// Pas2Php v0.8 is Copyright (C) 2014-2016 Derek John Evans
// 
include_once('system.php');
include_once('fileutil.php');
include_once('p2pcgi.php');
include_once('phplib.php');
include_once('strutils.php');
include_once('sysutils.php');
// function FilenameIsAbsolute(const AFileName: string) : Boolean;
function FilenameIsAbsolute($AFileName) {
  // LPos : Integer;
  $LPos = 0;
  // Result : Boolean;
  $Result = false;
  $LPos = PosSet(AllowDirectorySeparators(), $AFileName);
  $Result = $LPos == 1 || $LPos > 1 && $AFileName[($LPos - 1) - 1] == DriveSeparator;
  return $Result;
}
// function FileIsReadable(const AFileName: string) : Boolean;
function FileIsReadable($AFileName) {
  // Result : Boolean;
  $Result = false;
  $Result = is_readable($AFileName);
  return $Result;
}
// function FileIsWritable(const AFileName: string) : Boolean;
function FileIsWritable($AFileName) {
  // Result : Boolean;
  $Result = false;
  $Result = is_writable($AFileName);
  return $Result;
}
// function ProgramDirectory : string;
function ProgramDirectory() {
  // Result : string;
  $Result = "";
  $Result = ExtractFilePath(CgiGetEnv(CGI_SCRIPT_FILENAME));
  return $Result;
}
// function ReadFileToString(const AFileName: string) : string;
function ReadFileToString($AFileName) {
  // Result : string;
  $Result = "";
  $Result = file_get_contents($AFileName);
  return $Result;
}
?>