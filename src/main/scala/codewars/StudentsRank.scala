package codewars

// https://www.codewars.com/kata/528d36d7cc451cd7e4000339
object StudentsRank {

  case class Student(name: String, fives: Int, tens: Int, twenties: Int)

  // NOTE: the student case class is preloaded
  def mostMoney(students: List[Student]): String = {
    def amount(student: Student): Int = student.fives * 5 + student.tens * 10 + student.twenties * 20

    students match {
      case Nil => "all"
      case x :: Nil => x.name
      case _ => {
        val sorted = students.sortWith((a: Student, b: Student) => amount(a) < amount(b))
        if (amount(sorted.head) == amount(sorted.last)) "all"
        else sorted.last.name
      }
    }
  }
}
