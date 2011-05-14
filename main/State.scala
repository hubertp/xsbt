/* sbt -- Simple Build Tool
 * Copyright 2008, 2009, 2010  Mark Harrah
 */
package sbt

	import java.io.File
	import CommandSupport.FailureWall

final case class State(
	configuration: xsbti.AppConfiguration,
	definedCommands: Seq[Command],
	exitHooks: Set[ExitHook],
	onFailure: Option[String],
	remainingCommands: Seq[String],
	attributes: AttributeMap,
	next: Next.Value
) extends Identity {
	lazy val combinedParser = Command.combine(definedCommands)(this)
}

trait Identity {
	override final def hashCode = super.hashCode
	override final def equals(a: Any) = super.equals(a)
	override final def toString = super.toString
}

object Next extends Enumeration {
	val Reload, Fail, Done, Continue = Value
}

trait StateOps {
	def process(f: (String, State) => State): State
	def ::: (commands: Seq[String]): State
	def :: (command: String): State
	def continue: State
	def reboot(full: Boolean): State
	def reload: State
	def exit(ok: Boolean): State
	def fail: State
	def ++ (newCommands: Seq[Command]): State
	def + (newCommand: Command): State
	def get[T](key: AttributeKey[T]): Option[T]
	def put[T](key: AttributeKey[T], value: T): State
	def remove(key: AttributeKey[_]): State
	def baseDir: File
	def runExitHooks(): State
	def addExitHook(f: => Unit): State
}
object State
{
	implicit def stateOps(s: State): StateOps = new StateOps {
		def process(f: (String, State) => State): State =
			s.remainingCommands match {
				case Seq(x, xs @ _*) => f(x, s.copy(remainingCommands = xs))
				case Seq() => exit(true)
			}
			s.copy(remainingCommands = s.remainingCommands.drop(1))
		def ::: (newCommands: Seq[String]): State = s.copy(remainingCommands = newCommands ++ s.remainingCommands)
		def :: (command: String): State = (command :: Nil) ::: this
		def ++ (newCommands: Seq[Command]): State = s.copy(definedCommands = (s.definedCommands ++ newCommands).distinct)
		def + (newCommand: Command): State = this ++ (newCommand :: Nil)
		def baseDir: File = s.configuration.baseDirectory
		def setNext(n: Next.Value) = s.copy(next = n)
		def continue = setNext(Next.Continue)
		def reboot(full: Boolean) = throw new xsbti.FullReload(s.remainingCommands.toArray, full)
		def reload = setNext(Next.Reload)
		def exit(ok: Boolean) = setNext(if(ok) Next.Done else Next.Fail)
		def get[T](key: AttributeKey[T]) = s.attributes get key
		def put[T](key: AttributeKey[T], value: T) = s.copy(attributes = s.attributes.put(key, value))
		def remove(key: AttributeKey[_]) = s.copy(attributes = s.attributes remove key)
		def fail =
		{
			val remaining = s.remainingCommands.dropWhile(_ != FailureWall)
			if(remaining.isEmpty)
				applyOnFailure(s, Nil, exit(ok = false))
			else
				applyOnFailure(s, remaining, s.copy(remainingCommands = remaining))
		}
		private[this] def applyOnFailure(s: State, remaining: Seq[String], noHandler: => State): State =
			s.onFailure match
			{
				case Some(c) => s.copy(remainingCommands = c +: remaining, onFailure = None)
				case None => noHandler
			}

		def addExitHook(act: => Unit): State =
			s.copy(exitHooks = s.exitHooks + ExitHook(act))
		def runExitHooks(): State = {
			ExitHooks.runExitHooks(s.exitHooks.toSeq)
			s.copy(exitHooks = Set.empty)
		}
	}
}