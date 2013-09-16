package gallifreyan

import gallifreyan.engine.traits.Character
package object engine {
  object Size {
    val width = 1280
    val height = 854
  }
  type Syllable = List[Character]
  type Sentence = List[Syllable]
}
