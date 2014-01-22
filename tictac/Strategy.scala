package tictac

object Strategy {
  /** Game rules for state space S and players in P */
  trait GameRules[S, P] {
    /**
     * Score a position relative to a player. That is, a better
     * position should have a bigger score than a worse one.
     */
    def children(position: S): List[S]
    def turn(position: S): P // Whose turn is it on some position?
  }

  /**
   * Scores positions for a Strategy. R should have a total ordering (see the
   * signature of pickMove below) and should be positive if the given player has
   * an advantage.
   */
  trait GameOracle[S, R, P] {
    def score(player: P, position: S): R
  }

  sealed trait NonEmptyList[A] {
    def fold[R](singleton: A => R, cons: (A, R) => R): R
    def map[B](f: A => B): NonEmptyList[B] = fold(head => Singleton[B](f(head)),
      (head: A, tail: NonEmptyList[B]) => Cons[B](f(head), tail))
    def maxBy[B](f: A => B)(implicit ord: Ordering[B]): A =
      map[(A, B)](x => (x, f(x))).fold[(A, B)](x => x, {
        case ((a1, b1), (a2, b2)) => if (ord.compare(b1, b2) > 0) (a1, b1) else (a2, b2)
      })._1
  }

  object NonEmptyListUtils {
    def fromList[A](l: List[A]): Option[NonEmptyList[A]] =
      l.foldRight[Option[NonEmptyList[A]]](None)({
        case (head, None) => Some(Singleton(head))
        case (head, Some(tail)) => Some(Cons(head, tail))
      })

    def toList[A](l: NonEmptyList[A]): List[A] =
      l.fold(head => List(head),
        (head, tail: List[A]) => head +: tail)
  }

  case class Singleton[A](head: A) extends NonEmptyList[A] {
    def fold[R](singleton: A => R, cons: (A, R) => R): R = singleton(head)
  }
  case class Cons[A](head: A, tail: NonEmptyList[A]) extends NonEmptyList[A] {
    def fold[R](singleton: A => R, cons: (A, R) => R): R =
      cons(head, tail.fold(singleton, cons))
  }

  /** Strategy over a state space S with scores in R and players in P */
  trait Strategy[S, R, P] {
    /**
     * Universally quantified in M, so that the only possible return
     * values are elements of the candidates list
     */
    def pickMove[M](rules: GameRules[S, P], oracle: GameOracle[S, R, P],
      ord: Ordering[R], position: S, update: M => S,
      candidates: NonEmptyList[M]): M
  }
  /** Int Ordering **/
  object IntOrdering extends Ordering[Int] { def compare(x: Int, y: Int): Int = x - y }
}