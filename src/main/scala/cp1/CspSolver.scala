package cp1

/*
 * CSP ソルバーに関するクラスを定義するためのファイル
 */
abstract class CspSolver {
  def solve(csp: CSP): Option[Assignment]
}

class GT extends CspSolver {
  def solve(csp: CSP): Option[Assignment] = {
    def gt(xs: Seq[Variable], partialAssign: Assignment): Option[Assignment] = {
      ???
    }

    gt(csp.vars, Assignment())
  }
}

class BT extends CspSolver {
  def solve(csp: CSP): Option[Assignment] = {

    def selectVariable(xs: Seq[Variable]): (Variable, Seq[Variable]) = (xs.head, xs.tail)

    def valueOrder(dom: Domain): Seq[Int] = dom.values

    def bt(xs: Seq[Variable], partialAssign: Assignment): Option[Assignment] = {
      ???
    }

    bt(csp.vars, Assignment())
  }
}

class BTwithAC(ACfunc: AC) extends CspSolver {
  def solve(csp: CSP): Option[Assignment] = {
    def selectVariable(xs: Seq[Variable]): (Variable, Seq[Variable]) = (xs.head, xs.tail)

    def valueOrder(dom: Domain): Seq[Int] = dom.values

    def bt(xs: Seq[Variable], partialAssign: Assignment, csp0: CSP): Option[Assignment] = {
      ???
    }

    bt(csp.vars, Assignment(), csp)
  }
}

object bdAC extends Function2[Constraint, Map[Variable, Domain], Map[Variable, Domain]] {
  def apply(c: Constraint, doms: Map[Variable, Domain]): Map[Variable, Domain] =
    (for (x <- c.vars) yield dAC(c, x, doms)).filter(xd => doms(xd._1) != xd._2).toMap
}

object dAC extends Function3[Constraint, Variable, Map[Variable, Domain], (Variable, Domain)] {
  def apply(c: Constraint, x: Variable, doms: Map[Variable, Domain]): (Variable, Domain) = {
    require(c.vars.contains(x))

    ???

  }
}

trait AC extends Function1[CSP, CSP] {
  def apply(csp: CSP): CSP
}

object Logger {
  var noloops = 0
}

object AC3 extends AC {
  def apply(csp: CSP): CSP = {
    ???
  }
}
