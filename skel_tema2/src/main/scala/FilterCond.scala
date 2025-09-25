import scala.language.implicitConversions

trait FilterCond {
  def eval(r: Row): Option[Boolean]

  // TODO 2.2
  def ===(other: FilterCond): FilterCond = {
    Compound(_ == _, List(this, other))
  }
  def &&(other: FilterCond): FilterCond = {
    Compound(_ && _, List(this, other))
  }
  def ||(other: FilterCond): FilterCond = {
    Compound(_ || _, List(this, other))
  }
  def unary_! : FilterCond = {
    Not(this)
  }
}

case class Field(colName: String, predicate: String => Boolean) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = {
    if (r.contains(colName)) {
      if (predicate(r(colName))) Some(true)
      else Some(false)
    } else None
  }
}

case class Compound(op: (Boolean, Boolean) => Boolean, conditions: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = {
    def aux(el: FilterCond, acc: Option[Boolean]): Option[Boolean] = {
      val res = el.eval(r)
      (acc, res) match {
        case (Some(x), Some(y)) => Some(op(x, y))
        // deoarece acc este initial None, si eu vreau primul element
        // doar la inceput va intra in acest case
        case (None, Some(y)) => Some(y)
        case (_, _) => None
      }
    }
    conditions.foldRight(None)(aux)
  }
}

case class Not(f: FilterCond) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = {
    val res = f.eval(r)
    res match {
      case Some(x) => Some(!x)
      case _ => None
    }
  }
}

def And(f1: FilterCond, f2: FilterCond): FilterCond = {
  Compound(_ && _, List(f1, f2))
}
def Or(f1: FilterCond, f2: FilterCond): FilterCond = {
  Compound(_ || _, List(f1, f2))
}
def Equal(f1: FilterCond, f2: FilterCond): FilterCond = {
  Compound(_ == _, List(f1, f2))
}

case class Any(fs: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = {
    Compound(_ || _, fs).eval(r)
  }
}

case class All(fs: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = {
    Compound(_ && _, fs).eval(r)
  }
}