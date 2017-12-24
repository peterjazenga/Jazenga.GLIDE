History Files
===============================================================================
Version:  1.3.0 for Delphi 1,2,3,4,5,6,7,8,D2005,D2009 Kylix and Lazarus
          (also all standard and personal editions)

Revised:  February 13, 2013

Tested on: Delphi 2,3,4,6,7,D2005,D2009 all personal or standard editions;
           Kylix 3 Open Edition
           Lazarus on Windows and Linux

Note:      The Delphi Demo project works from D4 to D2009
===============================================================================
July      9, 2014   - Tested on Lazarus 1.2.4 (Windows and Linux)

February 13, 2013   - New version 1.3.
                      Added LastItemIndex, GetItemValue, DeleteItem, ItemBitmap,
                      ItemSelectedBitmap, CheckLastItem, ClearLastItem.
                      New demos for Delphi and Lazarus.
                      Added unicode support for Lazarus.
                      Tested on Lazarus 1.0.6

December 5, 2012    - Tested on Lazarus 1.0.4

May     21, 2009    - Tested on Delphi 2009.
                      Changed Lazarus Demo: TBitmap compatibility formats
                        changed on new Lazarus versions.
                        (Tested on Lazarus 0.9.26.2)
                      Added bitmaps transparency into the demos; some little
                      changes into the demos (Delphi and Lazarus).
                      Added a demo for Kylix3.
                      Version 1.2.1 (without changes into the component). 

October 12, 2006    - Some changes for Lazarus; added Count property;
                      added default values for the properties;
                      added Item parameter on FOnHistoryItemClick for
                      personalize the TMenuItem when is clicked;
                      added demo projects for Delphi and for Lazarus.

September  14, 2005 - Added Sorted and ShowNumber properties;
                      Change type from Byte to Integer;
                      Added Packages (also for D2005);
                      Added code and package for Lazarus.

January 08, 2005 -    Initial version 1.00
===============================================================================

Legal issues: 
-------------
Copyright (C) 2005-2014 by Andrea Russo

info@andrearusso.it
 
http://www.andrearusso.it

              This software is provided 'as-it-is', without any express or
              implied warranty. In no event will the author be held liable
              for any damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. If do you alter this component please send to me the source
                 to following email address: info@andrearusso.it

Support:
--------
If do you find a bug, please make a short program which reproduce the problem 
attach it to a message addressed to me. If I can reproduce the problem, I 
can find a fix ! Do not send exe file but just source code and instructions.
Always use the latest version (beta if any) before reporting any bug.

The component:
--------------
This component stores the recent files list into an .ini file and shows the result
into a menu.
It's possible to insert the list into any point of your menu.
A method helps you to have access to all the properties and methods of the Menu Items so
for example do you can specify the image of each item.

Properties:
-----------

FileMustExist   	If true the files that don't exists will be deleted from the history list
                	(default False)

IniFile			This is the name of the IniFile with full path
                	(default History.ini)

IniKey          	This is the name of the key into the ini file

LocalPath       	If specified all the files that have the path equal to
                   	LocalPath are shows into the list without the path.
                   	If ShowFullPath is false this value it's indifferent.

MaxItems        	This is the max lenght of the recent list showed
                   	and stored into the ini file (it's a lifo list)
                  	(default 5)

ParentMenu      	This is the TMenuItem parent of the history list

Position        	This is the insert position of the list into the parent menu
                	The possible position is between 0 and the value of ParentMenu.Count

Separator       	To divide the list by the other menu items if do you want      
               		Possible values: sepNone, sepTop, sepBottom, sepBoth 
                	(default sepNone)

ShowFullPath    	If true the file names are showed with full path otherwise not.
                	(see also the LocalPath property)
                	(default True)

ShowNumber		If true	an item number is shown before the file name
                	(default True)

Sorted			If true the list is sorted
                	(default False)

Count			Indicates the number of items of the history list.
                	(read only)

CheckLastItem		If true the last item used it's automatically checked
			(with a normal check or with an image if the ItemSelectedBitmap it's assigned)

ItemBitmap		If it is assigned is the image used for the menù items

ItemSelectedBitmap	If it is assigned is the image used for the menù item selected
			(if CheckLastItem=true)

LastItemIndex		It's the relative index (0..Count-1) of the last item used (selected or added)


Methods:
--------

UpdateList(thefile: string)

    Insert the file into the list and rebuild the history menu.

UpdateParentMenu

    Rebuild the menu (called also by UpdateList).

GetItemValue(const Index : integer): string

    The index is the relative index into the history list (0..Count-1).
    Return the value (the full filename) assigned at the item.

DeleteItem(const Index : integer)

    The index is the relative index into the history list (0..Count-1).
    Delete the item from the menù.

ClearLastItem

    Clear the value of LastItemIndex

Events:
-------

OnClickHistoryItem 
  
  procedure (Sender: TObject; Item: TMenuItem; const Filename: string)  

    When one history item is clicked this event returns the related file name;

    By this event do you personalize the item menu clicked (properties like image or checked;
      or assign events like OnDrawItem,etc.);

OnCreateItem
  
  procedure (Sender: TObject; Item: TMenuItem; const Filename: string)  
  

    When an history item is inserted into the parent menu this event returns this
      item and the filename;
    By this event do you personalize each item menu (properties like image or checked;
      or assign events like OnDrawItem,etc.);
 
 Warning: Don't modify the tag property of the TMeuItem created by this component, this
  property is used into the source code.
  
 Warning: Don't re-assign the OnClick events that it's used by the component to return
          the filename (OnClickHistoryItem);

Install on Delphi 2005 and UP (win32)
-------------------------------------------
Start Delphi and choose the menù File-->Open Project and open the file HistoryD6.dpk,
then upgrade the project to Delphi for Win32.
Into the "Project Manager" window click on History.bpl with the right button of your mouse
and select "Install".
Then select into the menù Tools-->Options, and add into the Library Path the folder where
do you have saved the component (Enviromnent Options->Delphi Options->Library - Win 32).
For update the component replace the file HistoryFiles.pas open the file HistoryD6.dpk
and recompile it.
Otherwise do you can open the .dpk project related to your Delphi version if exists
(HistoryD2005.dpk, HistoryD2009.dpk, ...).

Install on Delphi 3,4,5,6,7,Kylix 
----------------------------------
Open the file History*.dpk compatible with your Delphi version then compile and install.
(HistoryD6UP.dpk is for all versions >= Delphi 6).
Then select the menù Tools-->Enviromnent Options, choise the tab Library and add the folder
where do you have saved the component into the Library Path.

(Otherwise do you can start Delphi close all projects and choise the menù Component-->Install Component.
Select the tab "Into new package": fill the form (Unit file name (browse and open the
file HistoryFiles.pas); for "Package file name" write History.dpk), compile and install.)

For update the component replace the file HistoryFiles.pas, open the file History*.dpk and recompile.

Install on Lazarus 
------------------
Open the package HistoryLazarus.lpk (menù File-->Open), compile and install it.

Install on Delphi 2
---------------------------
Start Delphi and choose the menù Component-->Install.
Press the Add button and browse the file HistoryFiles.pas.  
