set JAVA_HOME=C:\Program Files (x86)\Java\jdk1.7.0_21
path %JAVA_HOME%\bin;%path%
cd C:\android-neon\eclipse\workspace\AppListViewDemo3
jarsigner -verify -verbose -certs C:\android-neon\eclipse\workspace\AppListViewDemo3\bin\AppListViewDemo3-release.apk
