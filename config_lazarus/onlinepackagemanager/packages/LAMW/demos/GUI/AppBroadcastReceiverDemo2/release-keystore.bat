set JAVA_HOME=C:\Program Files (x86)\Java\jdk1.7.0_21
set PATH=%JAVA_HOME%\bin;%PATH%
set JAVA_TOOL_OPTIONS=-Duser.language=en
cd C:\android-neon\eclipse\workspace\AppBroadcastReceiverDemo2
keytool -genkey -v -keystore AppBroadcastReceiverDemo2-release.keystore -alias appbroadcastreceiverdemo2aliaskey -keyalg RSA -keysize 2048 -validity 10000 < C:\android-neon\eclipse\workspace\AppBroadcastReceiverDemo2\keytool_input.txt
