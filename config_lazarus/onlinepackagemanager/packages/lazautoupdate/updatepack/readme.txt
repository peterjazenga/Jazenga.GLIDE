Update Pack for LazAutoUpdate system Version 0.0.0.0

Getting Started

* If you haven't installed the LazAutoUpdate component to your IDE yet, click the [Install Component] and the source code will be downloaded to a subfolder 'lazautoupdatesource/packagesource' from this one. Use the 'lazupdate.lpk' to install LazAutoUpdate into the 'System' tab of the Lazarus IDE

* From time to time the component will be updated.  Use the [Update Component] button to check and/or download the latest version.

* This app uses 'profiles' so that you can manage the updating of many SourceForge projects at once.  To make a new profile, type its name into the selection box and click the [New] button.

* To change the 'App to Update', 'Output Directory' and 'WhatsNew TextFile' you need to use the 'directory' mini-buttons.  The text cannot be edited directly.

* The 'versions.ini' and 'Online Project Name' can be edited directly.

* When the [Create Update Pack] is clicked, the Zip file is created, the versions ini file is created, and both are copied to the specified 'Output directory' - ready for you to upload.  Sample Lazarus code is shown for you to copy and paste into your app

* [Advanced Tab]

* With CopyTree, you can update not only the application, but many directories and files at once.  The directory you select will be cloned to the user's application folder.

    * You can also manually add files and folders to the generated zipfile before uploading it of course.

* You can drag any extra files (not folders) into a list that should be updated into the user app's directory.

* Other LazAutoUpdate Tweaks

    * By default, LazAutoUpdate does it's updating silently and unobtrusively, but you can set properties that give the user a more interactive experience.

    * The 'tweaks' setting are stored in each profile, and displayed in the Code window (shown after each 'Create Update')  It is up to you to implement them in your application code of course.

Click the [Configure] tab to continue...