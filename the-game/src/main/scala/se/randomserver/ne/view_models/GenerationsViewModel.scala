package se.randomserver.ne.view_models

import scalafx.collections.ObservableBuffer
import scalafx.collections.ObservableHashMap
import scalafx.collections.ObservableMap
import scalafx.beans.property.LongProperty

class GenerationsViewModel(session: SessionViewModel) {
  val generationIds = ObservableBuffer[Long]()
  val selectedGenerationId = LongProperty(-1)

  selectedGenerationId <==> session.currentGenerationId
  
  session.generations.onChange { (_, change) =>
    change match
      case ObservableMap.Add(key, _) => generationIds += key
      case ObservableMap.Remove(key, _) => generationIds -= key
    generationIds.sortInPlace() 
  }
}
