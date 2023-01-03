import net.bulbyvr.common.!!!

object Day21 {
  /* 
  extension (sc: StringContext) {
    def algebriacNumber(args: Any*): AlgebraicNumber = {
      require(args.isEmpty)
      AlgebraicNumber.parse(sc.parts(0))
    }
    def x(args: Any*): AlgebraicNumber = algebriacNumber(args : _*)
  }
  */
  val testInput = """|root: pppw + sjmn
                     |dbpl: 5
                     |cczh: sllz + lgvd
                     |zczc: 2
                     |ptdq: humn - dvpt
                     |dvpt: 3
                     |lfqf: 4
                     |humn: 5
                     |ljgn: 2
                     |sjmn: drzm * dbpl
                     |sllz: 4
                     |pppw: cczh / lfqf
                     |lgvd: ljgn * ptdq
                     |drzm: hmdt - zczc
                     |hmdt: 32""".stripMargin
  enum Operation {
    case Multiply, Divide, Add, Subtract
    def eval(l: Long, r: Long): Long = 
      this match
        case Operation.Multiply => l * r 
        case Operation.Divide => l / r 
        case Operation.Add => l + r 
        case Operation.Subtract => l - r 
    def invL(t: Long, r: Long): Long = 
      this match
        case Operation.Multiply => t / r 
        case Operation.Divide => t * r 
        case Operation.Add => t - r 
        case Operation.Subtract => t + r 
    def invR(t: Long, l: Long): Long = 
      this match
        case Operation.Multiply => t / l 
        case Operation.Divide => l / t
        case Operation.Add => t - l 
        case Operation.Subtract =>  l - t 
      
      
  }
  enum Expression {
    case Number(n: Long) extends Expression
    case Op(op: Operation, l: String, r: String) extends Expression
    case Unknown

  }

  def buildExpr(ctx: Context): BuiltExpr = {
    def buildExprHelper(expr: Expression): BuiltExpr = {
      expr match
        case Expression.Number(n) => 
          BuiltExpr.Number(n)
        case Expression.Op(op, l, r) => 
          val e = BuiltExpr.Op(buildExprHelper(ctx.monkes(l)), op, buildExprHelper(ctx.monkes(r)))
          if (e.canEval)
            BuiltExpr.Number(e.eval)
          else 
            e
        case Expression.Unknown =>
          BuiltExpr.Human 
    }
    buildExprHelper(ctx.monkes("root"))
  }

  enum BuiltExpr {
    case Number(n: Long) extends BuiltExpr 
    case Op(l: BuiltExpr, op: Operation, r: BuiltExpr) extends BuiltExpr 
    case Human

    def canEval: Boolean = 
      this match
        case BuiltExpr.Number(_) => true 
        case BuiltExpr.Op(l, op, r) => l.canEval && r.canEval 
        case BuiltExpr.Human => false
    def eval: Long = 
      this match
        case BuiltExpr.Number(n) => n 
        case BuiltExpr.Op(l, op, r) => op.eval(l.eval, r.eval)
        case BuiltExpr.Human => !!! 
    def solve(res: Long): Long = {
      this match
        case BuiltExpr.Op(Number(l), op, r) => r.solve(op.invR(res, l))
        case BuiltExpr.Op(l, op, Number(r)) => l.solve(op.invL(res, r))
        case BuiltExpr.Human => res 
        case _ => throw new Exception(s"unhandled $this")
      
    }
      
      
  }
  /*
  sealed trait AlgebraicNumber {
    def *(that: AlgebraicNumber): AlgebraicNumber = 
      (this, that) match
        case (RealNumber(l), RealNumber(r)) =>  RealNumber(l * r)
        case (l, r) if l.validUnknown && r.validUnknown => {  
          val ll = l.asUnknown.get 
          val rr = r.asUnknown.get
          // Multiplication with an x. 
          // Real parts multiply together. 
          val realPart = ll.realPart * rr.realPart 
          // Unknown powers add together
          val xPower = ll.xPower + rr.xPower
          UnknownNumber(realPart, xPower)
        }
        case (l, r) => 
          val ll = l.asPolynomial
          val rr = r.asPolynomial
          // Polynomials are multiplied like a table. 
          // A for comprehension SHOULD handle it
          Polynomial { 
            (for {
              ln <- ll.ns
              rn <- rr.ns 
            } yield (ln * rn)).map(_.asUnknown.get)
          }.simplify 
      
    def +(that: AlgebraicNumber): AlgebraicNumber = {
      (this, that) match {
        case (RealNumber(l), RealNumber(r)) => RealNumber(l + r)
        case (l, r) => 
          val ll = l.asPolynomial
          val rr = r.asPolynomial 
          // Polynomial addition is simply joining the two polynomials together 
          Polynomial(ll.ns ++ rr.ns).superSimplify
      }
    }
    def unary_- : AlgebraicNumber = {
      this match {
        case RealNumber(l) => RealNumber(-l)
        case UnknownNumber(l, x) => UnknownNumber(-l, x)
        case Polynomial(ns) => Polynomial(ns.map(it => (-it).asUnknown.get))
      }
    }
    def -(that: AlgebraicNumber): AlgebraicNumber = {
      this + (-that) 
    }

    def /(that: AlgebraicNumber): AlgebraicNumber = {
      // this is the hard one : ( 
      (this, that) match {
        case (RealNumber(l), RealNumber(r)) => RealNumber(l / r)
        case (l, r) if r.validUnknown => 
          val rr = r.asUnknown.get
          Polynomial(l.asPolynomial.ns.map(it => it.copy(realPart = it.realPart / rr.realPart, xPower = it.xPower - rr.xPower))).superSimplify
        case (l, right) => 
          val n = l.asPolynomial
          val d = right.asPolynomial
          var q: AlgebraicNumber = RealNumber(0)
          var r = n
          while (!r.ns.forall(_.realPart == 0) && r.degree >= d.degree) {
            val t = r.lead / d.lead 
            q = q + t 
            r = (r - (t * d)).asPolynomial
          }
          q

      }
    }
    def validUnknown: Boolean = this match
      case RealNumber(real) => true 
      case UnknownNumber(realPart, xPower) => true 
      case Polynomial(ns) => false
    
    def asUnknown: Option[UnknownNumber] = 
      this match
        case RealNumber(real) => Some(UnknownNumber(real, 0))
        case self @ UnknownNumber(_, _) => Some(self)
        case _ => None
    def asPolynomial: Polynomial = 
      this match {
        case self @ Polynomial(_) => self.simplify
        case other => Polynomial(List(other.asUnknown.get)).simplify
      }
    def givenX(x: Long): Long = {
      this match
        case RealNumber(real) => real 
        case UnknownNumber(realPart, xPower) => realPart * (x ^ xPower)
        case Polynomial(ns) => ns.map(_.givenX(x)).sum
      
    }
    def canBeSolved: Boolean = 
      this.asPolynomial.degree <= 3
    def solveForX(equals: Long): Option[Long] = {
      this.asPolynomial.superSimplify match {
        case RealNumber(real) => None 
        case UnknownNumber(realPart, 0) => None
        case UnknownNumber(realPart, xPower) => 
          Some(math.pow((equals.toDouble / realPart), 1D / xPower).round)
        case poly @ Polynomial(ns) => 
          poly.degree match {
            // Should have been auto corrected to RealNumber 
            case 0 => ??? 
            case 1 => 
              val newns = ns.sortBy(_.xPower)(Ordering[Int].reverse).groupMapReduce(_.xPower)(_.realPart)(_ + _)
              val x1 = newns(1)
              val x0 = newns(0)
              val newEquals = equals - x0  
              Some((newEquals.toDouble / x1).round)
            // i will do it later
            case _ => ??? 
          }
      }
    }
    override def equals(x: Any): Boolean = 
      x match {
        case that: AlgebraicNumber => 
          (this, that) match {
            case (l, r) if l.validUnknown && r.validUnknown => 
              val ll = l.asUnknown.get 
              val rr = r.asUnknown.get 
              ll.realPart == rr.realPart && ll.xPower == rr.xPower
            case (l, r) => 
              l.asPolynomial.ns.toSet == r.asPolynomial.ns.toSet
          } 
        case _ => false 
      }
  }
  case class RealNumber(real: Long) extends AlgebraicNumber
  /// A number representing Nx^p where realPart is N and xPower is p. If xPower is 0 then it is the same as RealNumber. 
  case class UnknownNumber(realPart: Long, xPower: Int) extends AlgebraicNumber 

  object AlgebraicNumber {
    def parse(str: String): AlgebraicNumber = {
      str.trim match {
        case s"${n}x${p}" if !str.contains("+") =>
          val daN = 
            if (n.isEmpty)
              1L 
            else 
              n.toLong 
          val daP = 
            if (p.isEmpty)
              1
            else 
              p.toInt
          UnknownNumber(daN, daP)
        case s"$n" if !str.contains("+") => RealNumber(n.toLong)
        case poly => Polynomial(poly.split('+').map(it => parse(it.trim).asUnknown.get).toList)
      }
    }
  }
  /// A polynomial representing a string of Nx^p where N is a constant and p is a constant power
  case class Polynomial(ns: List[UnknownNumber]) extends AlgebraicNumber {
    def simplify: Polynomial = {
      Polynomial(ns.groupMapReduce(_.xPower)(identity)((l, r) => UnknownNumber(l.realPart + r.realPart, l.xPower)).values.filterNot(_.realPart == 0).toList)
    }
    def superSimplify: AlgebraicNumber = {
      this.simplify.ns match {
        case Nil => RealNumber(0)
        case UnknownNumber(real, 0) :: Nil => RealNumber(real)
        case UnknownNumber(real, x) :: Nil => UnknownNumber(real, x)
        case ns => Polynomial(ns)

      }
    }
    def degree: Int = {
      lead.xPower
    }
    def lead: UnknownNumber = simplify.ns.maxBy(_.xPower)
  }
  */
  case class Context(monkes: Map[String, Expression])
  def evaluate(ctx: Context, expr: Expression): Long = {
    expr match {
      case Expression.Number(n) => n
      case Expression.Op(op, l, r) => {
        val ll = evaluate(ctx, ctx.monkes(l))
        val rr = evaluate(ctx, ctx.monkes(r))
        op match
          case Operation.Multiply => ll * rr 
          case Operation.Divide => ll / rr
          case Operation.Add => ll + rr 
          case Operation.Subtract => ll - rr
       }
     case Expression.Unknown => !!! 
    }
  }
  

  def parseMonke(input: String, p2: Boolean): (String, Expression) = {
    input match {
      case s"humn: $_" if p2 => 
        "humn" -> Expression.Unknown

      case s"$monke: $l $op $r" => 
        val daOp = op match {
          case "+" => Operation.Add 
          case "-" => Operation.Subtract 
          case "*" => Operation.Multiply
          case "/" => Operation.Divide
          case _ => ??? 
        }
        monke -> Expression.Op(daOp, l, r)
      case s"$monke: $n" => 
        monke -> Expression.Number(n.toLong)
      case _ => ???
    }
  }
  def parse(input: String, p2: Boolean): Context = {
    Context(input.linesIterator.map(it => parseMonke(it, p2)).toMap)
  }
  def part1(input: String): Long = {
    val ctx = parse(input, false)
    val root = ctx.monkes("root")
    evaluate(ctx, root) 
  }
  def part2(input: String): Long = {
    import BuiltExpr.* 
    val ctx = parse(input, true)

    val Op(l, _, r) = buildExpr(ctx): @unchecked 
    if (!(l.canEval ^ r.canEval)) {
      throw new Exception("one side must be unevaluatable") 
    }
    val (expr, value) = if (l.canEval) (r, l.eval) else (l, r.eval)
    expr.solve(value)
    
  }
}
