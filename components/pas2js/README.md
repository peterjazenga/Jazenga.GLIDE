# README #

Source code organisation

3 main directories:

* src 
* docs
* bin

### Sources

* compiler: the actual compiler sources. 
  This will only compile with a trunk version of FPC. 
  There are 3 programs:
  - pas2js : the command-line compiler
  - libpas2js : the library version of the compiler
  - makestub: a program to make stubs for the Delphi IDE.
  - compileserver: a webserver that compiles files and serves the resulting
     java script. It has an interface for recompiling. See the hotreload demo.

* proxy: the proxy component for the library version of the compiler.
  A Delphi and a lazarus version of the DLL can be compiled. Any recent
  version of both will work. 
  There is also a proxy for the makestub functionality - present in the DLL.

* rtl: the RTL for use in the Javascript environment.
  * system unit
  * sysutils unit (no filesystem functions)
  * dateutils unit
  * strutils unit
  * typinfo unit
  * classes unit
  * contnrs unit
  * js unit: provides the basic language features of Javascript
  * web unit: provides the basic browser objects (document, window etc.)
  * libjquery unit: this is an import class for the jquery javascript library.

### Bin

* The bin directory is intended to check in a version of the compiler and
  the DLL.

### Docs

* Here an initial version of the documentation of the compiler can be found.
