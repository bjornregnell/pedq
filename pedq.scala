//> using scala 3.4
//  https://scastie.scala-lang.org/dmHBRfkHQ0y4lJ13TuOcwA

object PedagogicalQualityModel:

  enum Quality:
    case VeryLow, Low, Medium, High, VeryHigh

  type Number = Double // a decimal number

  case class LearningGoal(description: String)

  case class TeachingResource(description: String)

  case class Course(
    name: String,
    credits: Number,
    pedagogicalAmbitions: Map[LearningGoal, Quality],
    teachingQuality:      Map[TeachingResource, Quality],
    teachingCosts:        Map[TeachingResource, Number],
    learningOutcome:      Map[Student, Map[LearningGoal, Quality]],
    isPassed:             Map[Student, Boolean],
  )

  case class Student(
    id: String,
    motivation:               Map[Course, Quality],
    preKnowledge:             Map[Course, Quality],
    participationInTeaching:  Map[Course, Number],
    selfStudyEffort:          Map[Course, Number],
    selfStudyEfficiency:      Map[Course, Quality],
    socialStudyContext:       Map[Course, Quality],
    inherentTalentForSubject: Map[Course, Quality],
  )

  object Metrics:

    extension (q: Quality) def toNumber = q.ordinal * 0.25

    extension (course: Course)
      def students: List[Student] = course.isPassed.keySet.toList.sortBy(_.id)

      def learningGoals: Set[LearningGoal] = course.pedagogicalAmbitions.keySet

      def totalLearningOutcome: Number =
        val result = for
          student <- course.students
          (goal, quality) <- course.learningOutcome(student)
        yield quality.toNumber
        result.sum / students.size

      def throughput: Number =
        val nbrPassed = course.isPassed.values.count(_ == true)
        nbrPassed.toDouble / students.size

      def totalCostOfTeaching: Number = 
        course.teachingCosts.values.sum

  def Hypotheses = Map(
    "A" -> "totalLearningOutcome is correlated with pedagogicalAmbitions, teachingQuality",
    "B" -> """pedagogicalAmbitions, teachingQuality are correlated with
      motivation, participationInTeaching, selfStudyEffort, selfStudyEfficiency, socialStudyContext""",
    "C" -> "teachingQuality is correlated with totalCostOfTeaching",
    "D" -> "throughput is correlated with totalLearningOutcome",
  )

  @main def showHypotheses =
    println:
      "Some hypotheses on correlations among pedagogical quality aspects:"
    for (id, str) <- Hypotheses do println(s"$id -> $str")