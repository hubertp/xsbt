/* sbt -- Simple Build Tool
 * Copyright 2010 Mark Harrah
 */
package sbt

// used instead of Either[Incomplete, T] for type inference

/** Result of completely evaluating a task.*/
sealed trait Result[+T]
{
	def toEither: Either[IncompleteStub, T]
}
/** Indicates the task did not complete normally and so it does not have a value.*/
final case class Inc(cause: IncompleteStub) extends Result[Nothing]
{
	def toEither: Either[IncompleteStub, Nothing] = Left(cause)
}
/** Indicates the task completed normally and produced the given `value`.*/
final case class Value[+T](value: T) extends Result[T]
{
	def toEither: Either[IncompleteStub, T] = Right(value)
}

object Result
{
	type Id[X] = X
	val tryValue = new (Result ~> Id) {
		def apply[T](r: Result[T]): T =
			r match {
				case Value(v) => v
				case Inc(i) => throw i
			}
	}
	def tryValues[S](r: Seq[Result[Unit]], v: Result[S]): S =
	{
		r foreach tryValue[Unit]
		tryValue[S](v)
	}
	implicit def fromEither[T](e: Either[IncompleteStub, T]): Result[T] = e match {
		case Left(i) => Inc(i)
		case Right(v) => Value(v)
	}
}