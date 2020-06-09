package cp1

object App {

  def readCSP_test1() = {
    cspFactory.fromFile("CspFiles/toy.csp")
  }

  def createCSP_test1() = {
    val x1 = Variable("x1")
    val x2 = Variable("x2")
    val x3 = Variable("x3")

    val vars = Seq(x1, x2, x3)

    val doms =
      Seq(
        x1 -> Domain(Seq(1, 2, 3)),
        x2 -> Domain(Seq(1, 2, 3)),
        x3 -> Domain(Seq(1, 2, 3))).toMap

    val cons =
      Seq(
        Ne(x1, x2),
        Ne(x1, x3),
        Ne(x2, x3))

    CSP(vars, doms, cons)
  }


  def assign_test1() = {
    ???
  }

  def gt01 = {

    ???

  }

  def gt02 = {
    ???
  }

  def gt03 = {
    ???
  }

  def gt04 = {
    ???
  }

  def test_dAC = {
    ???
  }

}

object plspSolver {
  def main(args: Array[String]): Unit = {

    val id: String = "123t456s" // 学籍番号を書く

    val fileName = args(0)

    println(s"ID: $id")
    println(s"CSP: $fileName")

    val csp = cspFactory.fromFile(fileName)

    println("c Parse Done.")

    val solver: CspSolver = new BT // BT ではなく new "自分ソルバークラス" を書く
    val solution = solver.solve(csp)
    if (solution.nonEmpty) {
      println("s SAT")
      printAssignment(solution.get)
    } else {
      println("s UNSAT")
    }

  }

  def printAssignment(a: Assignment) = {
    a.amap.map { case (x, v) => s"v ${x.name} = $v" }.foreach(println)
  }
}
