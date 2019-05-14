package codewars

object Isomorphism {
  /**
  * The type [[Nothing]] has no value.
  * So it is impossible to construct an instance of it.
  * In this solution, wherever a situation arises where
  * for types to check, you need a function that takes a [[Nothing]],
  * you can use [[absurd]].
  */
  // def absurd[R](n: Nothing): R = match n {
  //   case _ => ???
  // }

  def absurd[R]: Nothing => R = { case _ => ??? }

  // so, when are two type, `A` and `B`, considered equal?
  // a definition might be, it is possible to go from `A` to `B`,
  // and from `B` to `A`.
  // Going a roundway trip should leave you the same value.
  // Unfortunately it is virtually impossible to test this in Scala.
  // This is called Isomorphism.

  type ISO[A, B] = (A => B, B => A)

  // given ISO a b, we can go from a to b
  def substL[A, B]: ISO[A, B] => (A => B) = _._1

  // and vice versa
  def substR[A, B]: ISO[A, B] => (B => A) = _._2

  // There can be more than one ISO a b
  def isoBool: ISO[Boolean, Boolean] = (identity, identity)
  def isoBoolNot: ISO[Boolean, Boolean] = (! _, ! _)

  // isomorphism is reflexive
  def refl[A]: ISO[A, A] = (identity, identity)

  // isomorphism is symmetric
  def symm[A, B]: ISO[A, B] => ISO[B, A] = {
    case (ab, ba) => (ba, ab)
  }

  // isomorphism is transitive
  def trans[A, B, C]: (ISO[A, B], ISO[B, C]) => ISO[A, C] = {
    case ((ab, ba), (bc, cb)) => (bc compose ab, ba compose cb)
  }

  // We can combine isomorphism:
  def isoTuple[A, B, C, D]: (ISO[A, B], ISO[C, D]) => ISO[(A, C), (B, D)] = {
    case ((ab, ba), (cd, dc)) => {
      ({ case (a, c) => (ab(a), cd(c)) }, { case (b, d) => (ba(b), dc(d)) })
    }
  }

  def isoList[A, B]: ISO[A, B] => ISO[List[A], List[B]] = {
    case (ab, ba) => {
      (listA => listA.map(ab), listB => listB.map(ba))
    }
  }

  def isoOption[A, B]: ISO[A, B] => ISO[Option[A], Option[B]] = {
    case (ab, ba) => {
      (OpA => OpA.map(ab), OpB => OpB.map(ba))
    }
  }

  def isoEither[A, B, C, D]: (ISO[A, B], ISO[C, D]) => ISO[Either[A, C], Either[B, D]] = {
    case ((ab, ba), (cd, dc)) => {
      (
        {
          case Left(a) => Left(ab(a))
          case Right(c) => Right(cd(c))
        },
        {
          case Left(b) => Left(ba(b))
          case Right(d) => Right(dc(d))
        } 
      )
    }
  }

  def isoFunc[A, B, C, D]: (ISO[A, B], ISO[C, D]) => ISO[(A => C), (B => D)] = {
    case ((ab, ba), (cd, dc)) => {
      (ac => cd compose ac compose ba, bd => dc compose bd compose ab)
    }
  }

  // Going another way is hard (and is generally impossible)
  def isoUnOption[A, B]: ISO[Option[A], Option[B]] => ISO[A, B] = {
    case (opAopB, opBopA) => {
      (
        a => opAopB(Some(a)) match {
          case Some(b) => b
          case None => (opAopB(None)).get
        },
        b => opBopA(Some(b)) match {
          case Some(a) => a
          case None => (opBopA(None)).get
        }
      )
    }
  }
  // Remember, for all valid ISO, converting and converting back
  // Is the same as the original value.
  // You need this to prove some case are impossible.

  // We cannot have
  // isoUnEither[A, B, C, D]: (ISO[Either[A, B], Either[C, D]], ISO[A, C]) => ISO[B, D]
  // Note that we have
  def isoEU: ISO[Either[List[Unit], Unit], Either[List[Unit], Nothing]] = {
    (
      {
        case Left(xs) => Left(Unit :: xs)
        case _ => Left(Nil)
      },
      {
        case Left(Nil) => Right(Unit)
        case Left(_ :: xs) => Left(xs)
        case Right(void) => absurd(void)
      }
    )
  }
  // where Unit, has 1 value, (the value is also called Unit), and Void has 0 values.
  // If we have isoUnEither,
  // We have ISO[Unit, Nothing] by calling isoUnEither isoEU
  // That is impossible, since we can get a Nothing by substL on ISO[Unit, Nothing]
  // So it is impossible to have isoUnEither

  // And we have isomorphism on isomorphism!
  def isoSymm[A, B]: ISO[ISO[A, B], ISO[B, A]] = (symm, symm)
}

