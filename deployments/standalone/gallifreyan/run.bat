@echo off
IF "%JAVA_HOME%" == "" (
  ECHO Circular Gallifreyan Transliterator needs Java: http://java.com/download
  ECHO Please install Java and try again
  PAUSE
  EXIT
)
CHDIR %~dp0
IF EXIST .extract RD /s/q .extract
IF EXIST log.txt DEL log.txt
START "" "http://localhost:8080/"
ECHO Circular Gallifreyan Transliterator ist starting up. Please wait...
ECHO.
ECHO A new window should open http://localhost:8080/ in your default browser.
ECHO.
ECHO All output is logged to log.txt
JAVA -jar Gallifreyan-1.0-SNAPSHOT-war-exec.jar 2> log.txt
PAUSE
