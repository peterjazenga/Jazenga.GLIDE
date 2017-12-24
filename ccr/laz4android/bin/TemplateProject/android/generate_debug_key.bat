REM Adjust these paths to yours
SET PATH=
SET APP_NAME=
SET ANDROID_HOME=
SET APK_SDK_PLATFORM=
SET APK_PROJECT_PATH=
SET SDK_BUILDTOOLS=
SET PASSWORD=

mkdir %APK_PROJECT_PATH%\bin

keytool -genkey -v -keystore %APK_PROJECT_PATH%\bin\LCLDebugKey.keystore -alias LCLDebugKey -keyalg RSA -validity 10000

