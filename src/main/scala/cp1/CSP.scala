package cp1

/*
 * 制約充足問題 (Constraint Satisfaction Problem; CSP) に関するクラスを定義するためのファイル
 */

abstract class Expression

abstract class Term extends Expression {
  def vars: Set[Variable]
  def valuedWith(a: Assignment): Int
}

case class Variable(name: String) extends Term {
  def vars = Set(this)
  def valuedWith(a: Assignment) = a(this)
}

case class Add(ts: Seq[Term]) extends Term {
  def vars = ts.foldLeft(Seq.empty[Variable])((xs, t) => xs ++ t.vars).toSet
  def valuedWith(a: Assignment): Int = ts.map(t => t.valuedWith(a)).sum
}

abstract class Constraint extends Expression {
  def vars: Set[Variable]

  def isSatisfiedWith(a: Assignment): Option[Boolean]
}

case class Ne(x1: Term, x2: Term) extends Constraint {
  def vars = x1.vars ++ x2.vars
  def isSatisfiedWith(a: Assignment) = {
    if (vars.forall(x => a.contains(x)))
      Some(x1.valuedWith(a) != x2.valuedWith(a))
    else
      None
  }
}

case class Eq(t1: Term, t2: Term) extends Constraint {
  def vars = t1.vars ++ t2.vars
  def isSatisfiedWith(a: Assignment): Option[Boolean] = {
    if (vars.forall(x => a.contains(x)))
      Some(t1.valuedWith(a) == t2.valuedWith(a))
    else
      None
  }
}


case class Domain(values: Seq[Int]) extends Expression {
  def lb = values.min
  def ub = values.max
  def size = values.size
  def contains(v: Int) = values.contains(v)
}


case class Assignment(amap: Map[Variable, Int]) {
  def apply(x: Variable) = amap(x)

  def contains(x: Variable) = amap.contains(x)

  def +(x: Variable, v: Int) = Assignment(amap + (x -> v))

  def +(xv: Tuple2[Variable, Int]) = Assignment(amap + (xv._1 -> xv._2))

  def toDoms: Map[Variable, Domain] = amap.map { xv => xv._1 -> Domain(Seq(xv._2)) }

  override def toString = {
    amap.map{ case (x, v) => s"v ${x.name} = $v"}.mkString("\n")
  }
}

object Assignment {
  def apply(): Assignment = Assignment(Map.empty)
}


case class CSP(
                var vars: Seq[Variable],
                var doms: Map[Variable, Domain],
                var cons: Seq[Constraint]
              ) {
  def hasNoEmptyDomain = doms.forall(_._2.size > 0)

  def isSatisfiedWith(a: Assignment) =
    doms.forall{ case (x,dom) => dom.contains(a(x))} && cons.forall(_.isSatisfiedWith(a).getOrElse(false))

  lazy val var2cons = (for (x <- vars) yield x -> cons.filter(_.vars.contains(x))).toMap

  def toSugar(t: Expression): String = t match {
    case x: Variable => s"(int ${x.name} ${toSugar(doms(x))})"
    case d: Domain => if (d.values.size == d.ub - d.lb + 1) s"${d.lb} ${d.ub}" else d.values.mkString(" ")
    case Ne(x1: Variable, x2: Variable) => s"(ne ${x1.name} ${x2.name})"
    case Ne(x1: Term, x2: Term) => s"(ne $x1 $x2)"
  }

  def toSugar: Seq[String] = {
    vars.map(toSugar(_)) ++ cons.map(toSugar(_))
  }
}

object CSP {
  def apply() = new CSP(Seq.empty, Map.empty, Seq.empty)
}


object cspFactory {
  private[this] def varFactory(x: SIntVar): Variable = Variable(x.name)

  private[this] def domFactory(d: SDomain) = {
    val ds = d.dom.foldLeft(Seq.empty[Int])((seq, lu) => seq ++ (lu._1 to lu._2))
    Domain(ds)
  }

  private[this] def termFactory(t: SugarCspTerm): Term = {
    t match {
      case x: SIntVar => varFactory(x)
      case t: SAdd => Add(t.ts.map(termFactory))
    }
  }

  private[this] def constraintFactory(c: SugarCspConstraint): Constraint = {
    c match {
      case SNe(t1: SugarCspTerm, t2: SugarCspTerm) => Ne(termFactory(t1), termFactory(t2))
      case SEq(t1: SugarCspTerm, t2: SugarCspTerm) => Eq(termFactory(t1), termFactory(t2))
      case SGe(t1: SugarCspTerm, t2: SugarCspTerm) => ???
      case SAlldifferent(ts) => ???
    }
  }

  def fromFile(fileName: String): CSP = {
    val csp = CSP()
    val sp = new SugarCspLangParser
    sp.parse(new java.io.File(fileName))
    sp.domMap.keys.foreach { x0 =>
      val x = varFactory(x0)
      csp.vars = x +: csp.vars
      csp.doms += x -> domFactory(sp.domMap(x0))
    }
    csp.cons = sp.scons.map(constraintFactory)
    csp
  }
}

/*
 * (ne x1 x2)
 * (ne x1 1)
 * (eq x1 x2)
 * (eq x2 3)
 * (ge x1 3)
 * (ge (add x1 x2) 3)
 * Alldifferent
 *
 */