/* sbt -- Simple Build Tool
 * Copyright 2010 Mark Harrah
 */
package sbt

import Incomplete.{Error, Value => IValue}
final case class IncompleteStub(node: Option[AnyRef], tpe: IValue = Error, message: Option[String] = None, causes: Seq[IncompleteStub] = Nil, directCause: Option[Throwable] = None)
	extends Exception(message.orNull, directCause.orNull) {
		override def toString = "Incomplete(node=" + node + ", tpe=" + tpe + ", msg=" + message + ", causes=" + causes + ", directCause=" + directCause +")"
}

object Incomplete extends Enumeration {
	val Skipped, Error = Value
	
	def transformTD(i: IncompleteStub)(f: IncompleteStub => IncompleteStub): IncompleteStub = transform(i, true)(f)
	def transformBU(i: IncompleteStub)(f: IncompleteStub => IncompleteStub): IncompleteStub = transform(i, false)(f)
	def transform(i: IncompleteStub, topDown: Boolean)(f: IncompleteStub => IncompleteStub): IncompleteStub =
	{
			import collection.JavaConversions._
		val visited: collection.mutable.Map[IncompleteStub,IncompleteStub] = new java.util.IdentityHashMap[IncompleteStub, IncompleteStub]
		def visit(inc: IncompleteStub): IncompleteStub =
			visited.getOrElseUpdate(inc, if(topDown) visitCauses(f(inc)) else f(visitCauses(inc)))
		def visitCauses(inc: IncompleteStub): IncompleteStub =
			inc.copy(causes = inc.causes.map(visit) )

		visit(i)
	}
	def visitAll(i: IncompleteStub)(f: IncompleteStub => Unit)
	{
		val visited = IDSet.create[IncompleteStub]
		def visit(inc: IncompleteStub): Unit =
			visited.process(inc)( () ) {
				f(inc)
				inc.causes.foreach(visit)
			}
		visit(i)
	}
	def linearize(i: IncompleteStub): Seq[IncompleteStub] =
	{
		var ordered = List[IncompleteStub]()
		visitAll(i) { ordered ::= _ }
		ordered
	}
	def allExceptions(is: Seq[IncompleteStub]): Iterable[Throwable] =
		allExceptions(new IncompleteStub(None, causes = is))
	def allExceptions(i: IncompleteStub): Iterable[Throwable] =
	{
		val exceptions = IDSet.create[Throwable]
		visitAll(i) { exceptions ++= _.directCause.toList }
		exceptions.all
	}
	def show(tpe: Value) = tpe match { case Skipped=> "skipped"; case Error => "error" }
}