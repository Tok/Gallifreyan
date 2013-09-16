package gallifreyan

import com.vaadin.ui.VerticalLayout
import com.vaadin.ui.Label
import com.vaadin.ui.Link
import com.vaadin.server.ExternalResource
import com.vaadin.ui.HorizontalLayout
import com.vaadin.ui.Alignment
import com.vaadin.ui.Component

class Footer extends VerticalLayout {
  val bbc = new Label("Doctor Who and the concept of Gallifreyan are ©")
  val bbcLink = new Link("BBC", new ExternalResource("http://www.bbc.co.uk/"))
  val sherman = new Label("- The Circular Gallifreyan Alphabet is ©")
  val shermanLink = new Link("Loren Sherman", new ExternalResource("http://www.shermansplanet.com/gallifreyan"))
  val tok = new Label("- The source code for this transliterator can be found on ")
  val tokLink = new Link("GitHub", new ExternalResource("https://github.com/Tok/Gallifreyan"))

  val horLayout = new HorizontalLayout
  horLayout.setSpacing(true)
  addToLayout(horLayout, bbc)
  addToLayout(horLayout, bbcLink)
  addToLayout(horLayout, sherman)
  addToLayout(horLayout, shermanLink)
  addToLayout(horLayout, tok)
  addToLayout(horLayout, tokLink)

  addComponent(horLayout)

  private def addToLayout(layout: HorizontalLayout, component: Component): Unit = {
    layout.addComponent(component)
    layout.setComponentAlignment(component, Alignment.BOTTOM_LEFT)
  }
}
