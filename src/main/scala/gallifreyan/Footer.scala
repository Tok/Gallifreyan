package gallifreyan

import com.vaadin.ui.VerticalLayout
import com.vaadin.ui.Label
import com.vaadin.ui.Link
import com.vaadin.server.ExternalResource
import com.vaadin.ui.HorizontalLayout
import com.vaadin.ui.Alignment
import com.vaadin.ui.Component
import com.vaadin.ui.Image
import com.vaadin.server.ThemeResource
import com.vaadin.ui.GridLayout

class Footer extends VerticalLayout {
  val valid = new GridLayout(1, 1)
  val validatorLink = new Link(None.orNull, new ExternalResource("http://validator.w3.org/#validate_by_upload"))
  validatorLink.setIcon(new ThemeResource("valid-svg10.gif"))
  validatorLink.setTargetName("_blank")

  val bbc = new Label("Doctor Who and the concept of Gallifreyan are ©")
  val bbcLink = new Link("BBC", new ExternalResource("http://www.bbc.co.uk/"))
  val sherman = new Label("- The Circular Gallifreyan Alphabet is ©")
  val shermanLink = new Link("Loren Sherman", new ExternalResource("http://www.shermansplanet.com/gallifreyan"))
  val tok = new Label("- The source code for this transliterator can be found on ")
  val tokLink = new Link("GitHub", new ExternalResource("https://github.com/Tok/Gallifreyan"))

  val horLayout = new HorizontalLayout
  horLayout.setSizeFull

  val textLayout = new HorizontalLayout
  textLayout.setSpacing(true)
  addToLayout(textLayout, bbc)
  addToLayout(textLayout, bbcLink)
  addToLayout(textLayout, sherman)
  addToLayout(textLayout, shermanLink)
  addToLayout(textLayout, tok)
  addToLayout(textLayout, tokLink)

  horLayout.addComponent(textLayout)
  horLayout.setComponentAlignment(textLayout, Alignment.TOP_LEFT)
  horLayout.addComponent(valid)
  horLayout.setComponentAlignment(valid, Alignment.TOP_RIGHT)

  addComponent(horLayout)

  private def addToLayout(layout: HorizontalLayout, component: Component): Unit = {
    layout.addComponent(component)
    layout.setComponentAlignment(component, Alignment.TOP_LEFT)
  }

  def removeValid: Unit = valid.removeAllComponents
  def addValid: Unit = {
    removeValid
    valid.addComponent(validatorLink, 0, 0)
  }
}
