# Circular Gallifreyan Transliterator

#### WARNING: Work in progress.
#### This application isn't finished and may therefore give false results.

<pre>
            _ _
       .- '     ' -.
     .'             '.
      .           . ' '
      '          .
     .'       ..'.       .
 ..-'     ..''    '. _ .'.
 .    ..'' .'''.         .
  ..''   .'   o '.      .
   .     '     o '     .
    '.    '. _ .'    .'
       '- . _ _ . -'
</pre>

This application is running on OpenShift at: http://gallifreyan-9000.rhcloud.com

---------------------------------------

#### TODO

##### High Priority

* Calculate and draw direct line connections
* Improve code quality and reduce redundancies

##### Medium Priority

* Allow manual line connections
* Consider to draw syllables near the center for words with more than six syllables
* Allow manual separation of vowels from syllables
* Improve SVG output by using groups and better orders
* Add SVG metadata

##### Low Priority

* Allow different output sizes
* Add option for quadruple dot C
* Optimize for mobile devices
* Fix SVG download problems for IE if possible

---------------------------------------

#### Frameworks and Technologies

* Scala
* Vaadin 7 (user interface)
* Apache Batik (SVG support)
* Maven (build and reporting)
* Tomcat 7 (for [standalone version](/deployments/standalone/gallifreyan) only)

---------------------------------------

#### Important Classes

* [Init-Class](/src/main/scala/gallifreyan/GallifreyanInit.scala)
* [Drawing-Util](/src/main/scala/gallifreyan/util/DrawUtil.scala)
* [Text-Util](/src/main/scala/gallifreyan/util/TextUtil.scala)
* [Calculation-Util](/src/main/scala/gallifreyan/util/CalcUtil.scala)

---------------------------------------

Doctor Who and the concept of Gallifreyan are © BBC (http://www.bbc.co.uk/)

The Circular Gallifreyan Alphabet is © Loren Sherman (http://www.shermansplanet.com/gallifreyan)
