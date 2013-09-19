package gallifreyan.util

import java.awt.Color
import java.awt.Dimension
import java.awt.Graphics2D
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.InputStream
import java.io.OutputStreamWriter

import org.apache.batik.dom.GenericDOMImplementation
import org.apache.batik.dom.svg.SVGDOMImplementation
import org.apache.batik.svggen.SVGGeneratorContext
import org.apache.batik.svggen.SVGGraphics2D
import org.apache.batik.transcoder.TranscoderInput
import org.apache.batik.transcoder.TranscoderOutput
import org.apache.batik.transcoder.image.PNGTranscoder

import com.vaadin.server.StreamResource.StreamSource

import gallifreyan.Size
import gallifreyan.engine.data.Sentence

object ImageUtil {
  def makeSvg(sentence: Sentence, fg: Color, bg: Color, addText: Boolean, stubs: Boolean): Array[Byte] = {
    val domImpl = GenericDOMImplementation.getDOMImplementation
    val document = domImpl.createDocument(SVGDOMImplementation.SVG_NAMESPACE_URI, "svg", None.orNull)
    val context = SVGGeneratorContext.createDefault(document)
    context.setComment(" Generated with Apache Batik by Toks Circular Gallifreyan Transliterator. ")
    val g2d = new SVGGraphics2D(context, false)
    g2d.setSVGCanvasSize(new Dimension(Size.width, Size.height))
    g2d.setClip(0, 0, Size.width, Size.height)
    g2d.setPaint(fg)
    g2d.setColor(fg)
    g2d.setBackground(bg)
    g2d.setStroke(DrawUtil.STROKE)
    g2d.setFont(DrawUtil.FONT)
    val gen = new Generate(sentence, fg, bg, addText, stubs)
    gen.paint(g2d)
    val buffer = new ByteArrayOutputStream
    val out = new OutputStreamWriter(buffer, "UTF-8")
    g2d.stream(out)
    buffer.toByteArray
  }

  class Generate(val sentence: Sentence, val fg: Color, val bg: Color, val addText: Boolean, val stubs: Boolean) {
    def paint(g2d: Graphics2D): Unit = DrawUtil.drawSentence(g2d, sentence, fg, bg, addText, stubs)
  }

  def makePngFromSvg(svgBytes: Array[Byte]): StreamSource = {
    val bais = new ByteArrayInputStream(svgBytes)
    val inputSvgImage = new TranscoderInput(bais)
    val baos = new ByteArrayOutputStream
    val outputPngImage = new TranscoderOutput(baos)
    val pngTranscoder = new PNGTranscoder
    pngTranscoder.transcode(inputSvgImage, outputPngImage)
    val pngBytes: Array[Byte] = baos.toByteArray
    makeStreamSource(pngBytes)
  }

  def makeStreamSource(bytes: Array[Byte]): StreamSource = {
    new StreamSource() {
      override def getStream(): InputStream = new ByteArrayInputStream(bytes)
    }
  }

  def makeAwtFromVaadinColor(col: com.vaadin.shared.ui.colorpicker.Color): java.awt.Color = new java.awt.Color(col.getRGB)
}
