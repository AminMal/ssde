package tree

enum BTree[+T] {
  case Node(value: T)
  case Leaf(lhs: BTree[T], rhs: BTree[T])
  
  def map[B](f: T => B): BTree[B] = this match {
    case Node(value) => Node(f(value))
    case Leaf(lhs, rhs) => Leaf(lhs.map(f), rhs.map(f))
  }
  
  def flatMap[B](f: T => BTree[B]): BTree[B] = this match {
    case Node(value) => f(value)
    case Leaf(lhs, rhs) => Leaf(lhs.flatMap(f), rhs.flatMap(f))
  }
}