package link.german.gender.trainer2

object enums {

  object QuestionType extends Enumeration {
    type QuestionType = Value
    val TR,P3,PP,PR = Value
  }
  object AnswerType extends Enumeration {
    type AnswerType = Value
    val Correct,Incorrect,Typo = Value
  }

  object MemoryState extends Enumeration {
    type MemoryState = Value
    val New,Learning,Hard,Temp,InMemory,Persistent = Value
  }

}
