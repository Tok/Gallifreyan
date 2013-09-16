#!/bin/bash
if [ "$JAVA_HOME" == "" ]; then
  echo Circular Gallifreyan Transliterator needs Java: http://java.com/download
  echo Please install Java and try again
  exit
fi
cd ${0%/*}
rm -rf .extract 1> /dev/null
rm -f log.txt 1> /dev/null
open "http://localhost:8080/" 2> /dev/null
echo Circular Gallifreyan Transliterator ist starting up. Please wait...
echo
echo A new window should open http://localhost:8080/ in your default browser.
echo
echo All output is logged to log.txt
java -jar Gallifreyan-1.0-SNAPSHOT-war-exec.jar 2> log.txt
