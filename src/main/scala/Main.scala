trait Node {
  var left: Node = _
  var right: Node = _

  def left_= (n: Int): Unit = left = new ValueNode(n)
  def right_= (n: Int): Unit = right = new ValueNode(n)
}

trait OperatorNode extends Node {
  override def toString: String = s"(${display(left, right)})"
  val operand: String

  def compute(a: Int, b: Int): Int
  def compute(a: ValueNode, b: ValueNode): Int = compute(a.value, b.value)
  def display(left: Node, right: Node): String = s"$left $operand $right"

  def solve(): Int = {
    println(s"Step> $left $operand $right")

    (left, right) match {
      case (l: ValueNode, r: ValueNode) => compute(l.value, r.value)
      case (l: ValueNode, r: OperatorNode) => compute(l.value, r.solve())
      case (l: OperatorNode, r: ValueNode) => compute(l.solve(), r.value)
      case (l: OperatorNode, r: OperatorNode) => compute(l.solve(), r.solve())
      case _ => 0
    }
  }
}

class ValueNode(var value: Int = 0) extends Node {
  override def toString: String = value.toString
}

class Adder extends OperatorNode {
  override val operand: String = "+"
  override def compute(a: Int, b: Int): Int = a + b
}

class Subtractor extends OperatorNode {
  override val operand: String = "-"
  override def compute(a: Int, b: Int): Int = a - b
}

class Multiplier extends OperatorNode {
  override val operand: String = "*"
  override def compute(a: Int, b: Int): Int = a * b
}

class Divider extends OperatorNode {
  override val operand: String = "/"
  override def compute(a: Int, b: Int): Int = a / b
}

object Main extends App {
  val n = new Adder()
  n.left = 500
  n.right = new Subtractor()
  n.right.left = 20
  n.right.right = new Multiplier()
  n.right.right.left = 40
  n.right.right.right = new Divider()
  n.right.right.right.left = 500
  n.right.right.right.right = new Adder()
  n.right.right.right.right.left = 40
  n.right.right.right.right.right = 20

  val value = n.solve()

  println(n)
  println(value)
}
