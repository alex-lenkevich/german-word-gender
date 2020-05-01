package link.german.gender.trainer

object enums {

  object AnswerType extends Enumeration {
    type AnswerType = Value
    val Correct,Incorrect,Typo = Value
  }
  object MemoryState extends Enumeration {
    type MemoryState = Value
    val New,Learning,Hard,InMemory,Persistent = Value
  }


}
