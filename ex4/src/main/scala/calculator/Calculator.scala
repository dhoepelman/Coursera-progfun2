package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
        val _eval = bindeval(namedExpressions)
        namedExpressions.mapValues(e => Signal(_eval(e())))
      }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    bindeval(references)(expr)
  }

  private def bindeval(references: Map[String, Signal[Expr]]) : Expr => Double = {
    def eval(path: Set[String])(expr: Expr) : Double = {
      def _eval = eval(path) _
      def binop(op : (Double,Double) => Double)(a: Expr, b: Expr) = op(_eval(a),_eval(b))

      expr match {
        case Literal(v) => v
        // Detect cyclical reference, if the path already contains the variable we've detected a loop
        case Ref(name) if path.contains(name) => Double.NaN
        case Ref(name) => eval(path + name)(getReferenceExpr(name, references))
        case Plus(a,b) => binop(_ + _)(a,b)
        case Minus(a,b) => binop(_ - _)(a,b)
        case Times(a,b) => binop(_ * _)(a,b)
        case Divide(a,b) => binop(_ / _)(a,b)
      }
    }
    eval(Set.empty)
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
