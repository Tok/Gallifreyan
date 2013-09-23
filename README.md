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

* Improve code quality and reduce redundancies

##### Medium Priority

* Allow manual line connections (or randomize them)
* Allow more words per sentence and more syllables per word
* Consider to draw syllables near the center for words with more than six syllables
* Allow manual separation of vowels from syllables?
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
* Apache Batik (SVG generation)
* Vaadin 7 (user interface)
* Maven (build and reporting)
* Tomcat 7 (for [standalone version](/deployments/standalone/gallifreyan) only)

---------------------------------------

#### Important Classes

* [Init-Class](/src/main/scala/gallifreyan/GallifreyanInit.scala)
* [Generation-Util](/src/main/scala/gallifreyan/util/GenerationUtil.scala)
* [Drawing-Util](/src/main/scala/gallifreyan/util/DrawUtil.scala)
* [Calculation-Util](/src/main/scala/gallifreyan/util/CalcUtil.scala)
* [Text-Util](/src/main/scala/gallifreyan/util/TextUtil.scala)

---------------------------------------

Doctor Who and the concept of Gallifreyan are © BBC (http://www.bbc.co.uk/)

The Circular Gallifreyan Alphabet is © Loren Sherman (http://www.shermansplanet.com/gallifreyan)
