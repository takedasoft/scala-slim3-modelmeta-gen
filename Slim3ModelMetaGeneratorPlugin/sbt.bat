set SCRIPT_DIR=%~dp0
java -XX:+CMSClassUnloadingEnabled -Xmx256M -Xss2M -XX:MaxPermSize=128m -cp target\classes -jar "%SCRIPT_DIR%sbt-launch-0.10.1.jar" %*
