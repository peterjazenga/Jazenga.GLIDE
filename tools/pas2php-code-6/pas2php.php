<?PHP
  error_reporting(E_ALL ^ E_NOTICE);
  ini_set('display_errors', 'On');
  ini_set('memory_limit', '64M');
  session_start();
  set_include_path(get_include_path() . PATH_SEPARATOR . dirname(__FILE__) . "/pas2php_include/");
  include_once('system.php');
?>
