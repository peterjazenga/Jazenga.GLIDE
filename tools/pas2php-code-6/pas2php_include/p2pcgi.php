<?PHP
// 
// Converted From Pascal By Pas2Php v0.8
// Converts Object Pascal to PHP
// http://www.wascal.net/?page=pas2php
// Pas2Php v0.8 is Copyright (C) 2014-2016 Derek John Evans
// 
include_once('system.php');
include_once('httpdefs.php');
include_once('strutils.php');
include_once('sysutils.php');
// CGI_QUERY_STRING_PHPSESSID = 'PHPSESSID';
define("CGI_QUERY_STRING_PHPSESSID", "PHPSESSID");
// CGI_AUTH_TYPE = 'AUTH_TYPE';
define("CGI_AUTH_TYPE", "AUTH_TYPE");
// CGI_CONTENT_LENGTH = 'CONTENT_LENGTH';
define("CGI_CONTENT_LENGTH", "CONTENT_LENGTH");
// CGI_CONTENT_TYPE = 'CONTENT_TYPE';
define("CGI_CONTENT_TYPE", "CONTENT_TYPE");
// CGI_GATEWAY_INTERFACE = 'GATEWAY_INTERFACE';
define("CGI_GATEWAY_INTERFACE", "GATEWAY_INTERFACE");
// CGI_HTTP_ACCEPT = 'HTTP_ACCEPT';
define("CGI_HTTP_ACCEPT", "HTTP_ACCEPT");
// CGI_HTTP_ACCEPT_LANGUAGE = 'HTTP_ACCEPT_LANGUAGE';
define("CGI_HTTP_ACCEPT_LANGUAGE", "HTTP_ACCEPT_LANGUAGE");
// CGI_HTTP_COOKIE = 'HTTP_COOKIE';
define("CGI_HTTP_COOKIE", "HTTP_COOKIE");
// CGI_HTTP_USER_AGENT = 'HTTP_USER_AGENT';
define("CGI_HTTP_USER_AGENT", "HTTP_USER_AGENT");
// CGI_PATH_INFO = 'PATH_INFO';
define("CGI_PATH_INFO", "PATH_INFO");
// CGI_PATH_TRANSLATED = 'PATH_TRANSLATED';
define("CGI_PATH_TRANSLATED", "PATH_TRANSLATED");
// CGI_QUERY_STRING = 'QUERY_STRING';
define("CGI_QUERY_STRING", "QUERY_STRING");
// CGI_REMOTE_ADDR = 'REMOTE_ADDR';
define("CGI_REMOTE_ADDR", "REMOTE_ADDR");
// CGI_REMOTE_HOST = 'REMOTE_HOST';
define("CGI_REMOTE_HOST", "REMOTE_HOST");
// CGI_REMOTE_IDENT = 'REMOTE_IDENT';
define("CGI_REMOTE_IDENT", "REMOTE_IDENT");
// CGI_REMOTE_USER = 'REMOTE_USER';
define("CGI_REMOTE_USER", "REMOTE_USER");
// CGI_REQUEST_METHOD = 'REQUEST_METHOD';
define("CGI_REQUEST_METHOD", "REQUEST_METHOD");
// CGI_SCRIPT_NAME = 'SCRIPT_NAME';
define("CGI_SCRIPT_NAME", "SCRIPT_NAME");
// CGI_SCRIPT_FILENAME = 'SCRIPT_FILENAME';
define("CGI_SCRIPT_FILENAME", "SCRIPT_FILENAME");
// CGI_SERVER_NAME = 'SERVER_NAME';
define("CGI_SERVER_NAME", "SERVER_NAME");
// CGI_SERVER_PORT = 'SERVER_PORT';
define("CGI_SERVER_PORT", "SERVER_PORT");
// CGI_SERVER_PROTOCOL = 'SERVER_PROTOCOL';
define("CGI_SERVER_PROTOCOL", "SERVER_PROTOCOL");
// CGI_SERVER_SOFTWARE = 'SERVER_SOFTWARE';
define("CGI_SERVER_SOFTWARE", "SERVER_SOFTWARE");
// GQueryStrings : Array of string;
$GQueryStrings = array();
// procedure CgiProcessQueryString;
function CgiProcessQueryString() {
  global $GQueryStrings;
  // LPos : Integer;
  $LPos = 0;
  // LEnd : Integer;
  $LEnd = 0;
  // LEqu : Integer;
  $LEqu = 0;
  // LString : string;
  $LString = "";
  // LLine : string;
  $LLine = "";
  $LString = CgiGetEnv(CGI_QUERY_STRING);
  $LPos = 1;
  do {
    $LEnd = PosEx("&", $LString, $LPos);
    if ($LEnd == 0) {
      $LEnd = Length($LString) + 1;
    }
    $LLine = MidStr($LString, $LPos, $LEnd - $LPos);
    $LEqu = PosEx("=", $LLine);
    if ($LEqu > 0) {
      SetLength($GQueryStrings, Length($GQueryStrings) + 2);
      $GQueryStrings[Length($GQueryStrings) - 2] = HttpDecode(AnsiLeftStr($LLine, $LEqu - 1));
      $GQueryStrings[Length($GQueryStrings) - 1] = HttpDecode(MidStr($LLine, $LEqu + 1, MaxInt));
    }
    $LPos = $LEnd + 1;
  }
  while(!($LPos > Length($LString)));
}
// function CgiGetEnv(const AName: string) : string;
function CgiGetEnv($AName) {
  global $GQueryStrings;
  // Result : string;
  $Result = "";
  $Result = GetEnvironmentVariable($AName);
  return $Result;
}
// function CgiGetParam(const AName: string) : string;
function CgiGetParam($AName) {
  global $GQueryStrings;
  // LIndex : Integer;
  $LIndex = 0;
  // Result : string;
  $Result = "";
  $LIndex = 0;
  while ($LIndex < High($GQueryStrings)) {
    if (SameText($GQueryStrings[$LIndex], $AName)) {
      return($GQueryStrings[$LIndex + 1]);
    }
    $LIndex = $LIndex + 2;
  }
  throw new EArgumentException("Unable to locate parameter.");
  return $Result;
}
// function CgiHasParam(const AName: string) : Boolean;
function CgiHasParam($AName) {
  global $GQueryStrings;
  // Result : Boolean;
  $Result = false;
  try {
    CgiGetParam($AName);
    $Result = true;
  }
  catch (Exception $exception) {
    $Result = false;
  }
  return $Result;
}
/*
function CgiGetParamDef(const AName: string;const ADefault: string)
                        : string;
*/
function CgiGetParamDef($AName, $ADefault = "") {
  global $GQueryStrings;
  // Result : string;
  $Result = "";
  try {
    $Result = CgiGetParam($AName);
  }
  catch (Exception $exception) {
    $Result = $ADefault;
  }
  return $Result;
}
CgiProcessQueryString();
?>