FOR LAZARUS

Install Indy 10 from https://indy.fulgan.com/ZIP/
It is in downloads too.

Just Register lazregisterextcomp package after registering ExpertSuperForm from Ancestromana HG.
Register your ibx, zeos, dbnet data connexion pakage.

Need to compile lazextcore or typhonextcore one time before linking lazarus, or write in u_traducefile on extcopy package.
Use pl_lnet from Code Typhon, changing pl_IDEIntf package to IDEIntf from Lazarus.

Versioning is in each unit and global versioning is un lazextcomponents.lpk

Now Extended uses rx, JvXPBar and other lazarus components from lazarus-ccr SVN repositories. Last Lazarus CCR Update : 1 November 2016.

You need te find each version compiling looking at each component log.

For Extended extensions : 

extends.inc and dlcompilers.inc must be copied to inherited packages with root extendsinc.sh or extendsinc.bat. Verify directories in these files.

ExtCopy may use imagemagick and magickwand development libraries.

Use Fortes Report version 3.24 from official web site

FOR DELPHI

Just register ExtComponents package with no register, no docs and no demos directories.
ExtNumEdits, ExtOperations and ExtImages have not been tested on Delphi.
