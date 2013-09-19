package gallifreyan

import java.text.SimpleDateFormat
import java.util.Date
import java.util.Locale
import com.vaadin.annotations.PreserveOnRefresh
import com.vaadin.annotations.Theme
import com.vaadin.annotations.Title
import com.vaadin.data.Property.ValueChangeEvent
import com.vaadin.data.Property.ValueChangeListener
import com.vaadin.event.EventRouter
import com.vaadin.server.Page
import com.vaadin.server.StreamResource
import com.vaadin.server.VaadinRequest
import com.vaadin.shared.ui.colorpicker.Color
import com.vaadin.ui.Alignment
import com.vaadin.ui.Button
import com.vaadin.ui.CheckBox
import com.vaadin.ui.ColorPicker
import com.vaadin.ui.HorizontalLayout
import com.vaadin.ui.Image
import com.vaadin.ui.Notification
import com.vaadin.ui.OptionGroup
import com.vaadin.ui.TextField
import com.vaadin.ui.UI
import com.vaadin.ui.VerticalLayout
import com.vaadin.ui.components.colorpicker.ColorChangeEvent
import com.vaadin.ui.components.colorpicker.ColorChangeListener
import gallifreyan.engine.ImageFormat
import gallifreyan.util.ImageUtil
import gallifreyan.util.TextUtil
import com.vaadin.server.UserError

@Title("Circular Gallifreyan Transliterator")
@Theme("gallifreyantheme")
class GallifreyanInit extends UI {
  val df = new SimpleDateFormat("yyyyMMddHHmmssSSS")
  val PX = "px"
  val eventRouter = new EventRouter
  val input = new TextField
  val button = new Button("Transliterate")
  val formatOption = new OptionGroup
  val fgPicker = new ColorPicker("Color", Color.WHITE)
  val bgPicker = new ColorPicker("Background", new Color(0, 59, 111)) //BBC-approved Tardis Blue (Pantone 2955C)
  val addText = new CheckBox("Add Text")
  val stubs = new CheckBox("Stubs")
  val image = new Image
  val inputLayout = new HorizontalLayout
  val layout = new VerticalLayout
  val footer = new Footer

  override def init(request: VaadinRequest): Unit = {
    val frag = Page.getCurrent.getUriFragment match {
      case s: String => s
      case _ => ""
    }

    layout.setMargin(true)
    layout.setSpacing(true)

    inputLayout.setSpacing(true)
    input.setValue(frag)
    input.setWidth(620 + PX)
    input.focus
    input.setImmediate(true)
    input.addValueChangeListener(makeValueChangeListener)
    inputLayout.addComponent(input)
    button.setWidth(150 + PX)
    button.setDescription("Generate Image")
    inputLayout.addComponent(button)
    ImageFormat.values.foreach(formatOption.addItem(_))
    formatOption.select(ImageFormat.SVG)
    formatOption.setNullSelectionAllowed(false)
    formatOption.setImmediate(true)
    formatOption.addValueChangeListener(new ValueChangeListener {
      override def valueChange(event: ValueChangeEvent): Unit = {
        if (formatOption.isSelected(ImageFormat.SVG)) { footer.addValid } else { footer.removeValid  }
        drawWords
      }
    })

    inputLayout.addComponent(formatOption)
    fgPicker.setImmediate(true)
    fgPicker.setDescription("Line Color")
    fgPicker.addColorChangeListener(makeColorChangeListener)
    inputLayout.addComponent(fgPicker)
    bgPicker.setImmediate(true)
    bgPicker.setDescription("Background Color")
    bgPicker.addColorChangeListener(makeColorChangeListener)
    inputLayout.addComponent(bgPicker)
    addText.setWidth(80 + PX)
    addText.setValue(false)
    addText.setImmediate(true)
    addText.setDescription("Add resulting text to image.")
    addText.addValueChangeListener(makeValueChangeListener)
    inputLayout.addComponent(addText)
    stubs.setWidth(80 + PX)
    stubs.setValue(true)
    stubs.setImmediate(true)
    stubs.setDescription("Leave all lines as stubs for easy manupulation of the resulting image.")
    stubs.addValueChangeListener(makeValueChangeListener)
    inputLayout.addComponent(stubs)

    layout.addComponent(inputLayout)
    layout.setComponentAlignment(inputLayout, Alignment.TOP_CENTER)

    image.setWidth(Size.width + PX)
    image.setHeight(Size.height + PX)
    image.setImmediate(true)
    layout.addComponent(image)
    layout.setComponentAlignment(image, Alignment.MIDDLE_CENTER)

    footer.setWidth(Size.width + PX)
    layout.addComponent(footer)
    layout.setComponentAlignment(footer, Alignment.BOTTOM_CENTER)

    drawWords

    setContent(layout)
  }

  private def drawWords(): Unit = {
    val in = input.getValue
    try {
      val sentence = TextUtil.makeSentence(input.getValue)
      input.setComponentError(None.orNull)
      val sentenceString = sentence.mkString
      Page.getCurrent.setUriFragment(sentenceString)
      val fg = ImageUtil.makeAwtFromVaadinColor(fgPicker.getColor)
      val bg = ImageUtil.makeAwtFromVaadinColor(bgPicker.getColor)
      val svgBytes: Array[Byte] = ImageUtil.makeSvg(sentence, fg, bg, addText.getValue, stubs.getValue)
      val imageName = sentenceString + "-" + df.format(new Date())
      if (in.toUpperCase(Locale.getDefault).contains("C")) {
        notify("C has been replaced with K.", Notification.Type.HUMANIZED_MESSAGE)
      }
      image.setSource(makeStreamResource(svgBytes, imageName))
      image.markAsDirty
    } catch {
      case iae: IllegalArgumentException =>
        notify(iae.getMessage, Notification.Type.ERROR_MESSAGE)
        input.setComponentError(new UserError(iae.getMessage))
        input.focus
        input.selectAll
    }
  }

  private def makeStreamResource(svgBytes: Array[Byte], imageName: String): StreamResource = {
    if (formatOption.isSelected(ImageFormat.SVG)) {
      val svgStreamSource = ImageUtil.makeStreamSource(svgBytes)
      new StreamResource(svgStreamSource, imageName + ImageFormat.SVG.extension)
    } else {
      val pngStreamSource = ImageUtil.makePngFromSvg(svgBytes)
      new StreamResource(pngStreamSource, imageName + ImageFormat.PNG.extension)
    }
  }

  private def notify(message: String, notType: Notification.Type): Unit = {
    val notification = new Notification(message, notType)
    notification.setDelayMsec(500)
    notification.show(Page.getCurrent)
  }

  private def makeValueChangeListener(): ValueChangeListener = {
    new ValueChangeListener {
      override def valueChange(event: ValueChangeEvent): Unit = drawWords
    }
  }

  private def makeColorChangeListener(): ColorChangeListener = {
    new ColorChangeListener {
      override def colorChanged(event: ColorChangeEvent): Unit = drawWords
    }
  }
}
