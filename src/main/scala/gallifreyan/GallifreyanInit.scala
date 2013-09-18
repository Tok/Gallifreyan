package gallifreyan

import java.text.SimpleDateFormat
import java.util.Date
import com.vaadin.annotations.Theme
import com.vaadin.data.Property.ValueChangeEvent
import com.vaadin.data.Property.ValueChangeListener
import com.vaadin.event.EventRouter
import com.vaadin.server.Page
import com.vaadin.server.StreamResource
import com.vaadin.server.VaadinRequest
import com.vaadin.shared.ui.colorpicker.Color
import com.vaadin.ui.Alignment
import com.vaadin.ui.Button
import com.vaadin.ui.Button.ClickEvent
import com.vaadin.ui.Button.ClickListener
import com.vaadin.ui.CheckBox
import com.vaadin.ui.ColorPicker
import com.vaadin.ui.HorizontalLayout
import com.vaadin.ui.Image
import com.vaadin.ui.OptionGroup
import com.vaadin.ui.TextField
import com.vaadin.ui.UI
import com.vaadin.ui.VerticalLayout
import gallifreyan.engine.ImageFormat
import gallifreyan.engine.Size
import gallifreyan.util.ImageUtil
import gallifreyan.util.TextUtil
import com.vaadin.ui.components.colorpicker.ColorChangeListener
import com.vaadin.ui.components.colorpicker.ColorChangeListener
import com.vaadin.ui.components.colorpicker.ColorChangeEvent
import com.vaadin.ui.Label
import com.vaadin.ui.Link
import com.vaadin.server.ExternalResource
import com.vaadin.annotations.Title
import com.vaadin.annotations.PreserveOnRefresh

@PreserveOnRefresh
@Title("Circular Gallifreyan Transliterator")
@Theme("gallifreyantheme")
class GallifreyanInit extends UI {
  val df = new SimpleDateFormat("yyyyMMddHHmmssSSS")
  val PX = "px"
  val eventRouter = new EventRouter
  val input = new TextField
  val button = new Button("Transliterate")
  val formatOption = new OptionGroup
  val fgPicker = new ColorPicker("Color", Color.CYAN)
  val bgPicker = new ColorPicker("Background", Color.BLUE)
  val addText = new CheckBox("Add Text")
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
    input.setWidth(685 + PX)
    input.focus
    input.setImmediate(true)
    input.addValueChangeListener(getValueChangeListener)
    inputLayout.addComponent(input)
    button.setWidth(150 + PX)
    inputLayout.addComponent(button)
    ImageFormat.values.foreach(formatOption.addItem(_))
    formatOption.select(ImageFormat.PNG)
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
    fgPicker.addColorChangeListener(getColorChangeListener)
    inputLayout.addComponent(fgPicker)
    bgPicker.setImmediate(true)
    bgPicker.addColorChangeListener(getColorChangeListener)
    inputLayout.addComponent(bgPicker)
    addText.setWidth(100 + PX)
    addText.setValue(false)
    addText.setImmediate(true)
    addText.addValueChangeListener(getValueChangeListener)
    inputLayout.addComponent(addText)

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
    val sentence = TextUtil.getSyllables(input.getValue)
    val sentenceString = sentence.map(_.mkString).mkString
    Page.getCurrent.setUriFragment(sentenceString)
    val fg = ImageUtil.getAwtFromVaadinColor(fgPicker.getColor)
    val bg = ImageUtil.getAwtFromVaadinColor(bgPicker.getColor)
    val svgBytes: Array[Byte] = ImageUtil.makeSvg(sentence, fg, bg, addText.getValue)
    val imageName = sentenceString + "-" + df.format(new Date())
    val resource = if (formatOption.isSelected(ImageFormat.SVG)) {
      val svgStreamSource = ImageUtil.makeStreamSource(svgBytes)
      new StreamResource(svgStreamSource, imageName + ImageFormat.SVG.extension)
    } else {
      val pngStreamSource = ImageUtil.makePngFromSvg(svgBytes)
      new StreamResource(pngStreamSource, imageName + ImageFormat.PNG.extension)
    }
    image.setSource(resource)
    image.markAsDirty
  }

  private def getValueChangeListener(): ValueChangeListener = {
    new ValueChangeListener {
      override def valueChange(event: ValueChangeEvent): Unit = drawWords
    }
  }

  private def getColorChangeListener(): ColorChangeListener = {
    new ColorChangeListener {
      override def colorChanged(event: ColorChangeEvent): Unit = drawWords
    }
  }
}
