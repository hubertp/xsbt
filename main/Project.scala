/* sbt -- Simple Build Tool
 * Copyright 2011 Mark Harrah
 */
package sbt

	import java.io.File
	import java.net.URI
	import Project._
	import Keys.{appConfiguration, stateBuildStructure, commands, configuration, historyPath, logged, projectCommand, sessionSettings, shellPrompt, streams, thisProject, thisProjectRef, watch}
	import Scope.{GlobalScope,ThisScope}
	import Load.BuildStructure
	import CommandSupport.logger
	import Types.idFun

sealed trait ProjectDefinition[PR <: ProjectReference]
{
	def id: String
	def base: File
	def configurations: Seq[Configuration]
	def settings: Seq[Project.Setting[_]]
	def aggregate: Seq[PR]
	def delegates: Seq[PR]
	def dependencies: Seq[ClasspathDep[PR]]
	def uses: Seq[PR] = aggregate ++ dependencies.map(_.project)
	def referenced: Seq[PR] = delegates ++ uses

	override final def hashCode = id.hashCode
	override final def equals(o: Any) = o match {
		case p: ProjectDefinition[_] => p.getClass == this.getClass && p.id == id
		case _ => false
	}
}
sealed trait Project extends ProjectDefinition[ProjectReference]
{
	def copy(id: String = id, base: File = base, aggregate: => Seq[ProjectReference] = aggregate, dependencies: => Seq[ClasspathDep[ProjectReference]] = dependencies, delegates: => Seq[ProjectReference] = delegates,
		settings: Seq[Project.Setting[_]] = settings, configurations: Seq[Configuration] = configurations): Project =
			Project(id, base, aggregate = aggregate, dependencies = dependencies, delegates = delegates, settings, configurations)

	def resolve(resolveRef: ProjectReference => ProjectRef): ResolvedProject =
	{
		def resolveRefs(prs: Seq[ProjectReference]) = prs map resolveRef
		def resolveDeps(ds: Seq[ClasspathDep[ProjectReference]]) = ds map resolveDep
		def resolveDep(d: ClasspathDep[ProjectReference]) = ResolvedClasspathDependency(resolveRef(d.project), d.configuration)
		resolved(id, base, aggregate = resolveRefs(aggregate), dependencies = resolveDeps(dependencies), delegates = resolveRefs(delegates), settings, configurations)
	}
	def resolveBuild(resolveRef: ProjectReference => ProjectReference): Project =
	{
		def resolveRefs(prs: Seq[ProjectReference]) = prs map resolveRef
		def resolveDeps(ds: Seq[ClasspathDep[ProjectReference]]) = ds map resolveDep
		def resolveDep(d: ClasspathDep[ProjectReference]) = ClasspathDependency(resolveRef(d.project), d.configuration)
		apply(id, base, aggregate = resolveRefs(aggregate), dependencies = resolveDeps(dependencies), delegates = resolveRefs(delegates), settings, configurations)
	}

	def dependsOn(deps: ClasspathDep[ProjectReference]*): Project = copy(dependencies = dependencies ++ deps)
	def delegateTo(from: ProjectReference*): Project = copy(delegates = delegates ++ from)
	def aggregate(refs: ProjectReference*): Project = copy(aggregate = (aggregate: Seq[ProjectReference]) ++ refs)
	def configs(cs: Configuration*): Project = copy(configurations = configurations ++ cs)
	def settings(ss: Project.Setting[_]*): Project = copy(settings = (settings: Seq[Project.Setting[_]]) ++ ss)
}
sealed trait ResolvedProject extends ProjectDefinition[ProjectRef]

final case class Extracted(structure: BuildStructure, session: SessionSettings, currentRef: ProjectRef, rootProject: URI => String)
{
	lazy val currentUnit = structure units currentRef.build
	lazy val currentProject = currentUnit defined currentRef.project
	def get[T](key: ScopedTask[T]): Task[T] = get(key.task)
	def get[T](key: ScopedSetting[T]): T =
	{
		val scope = if(key.scope.project == This) key.scope.copy(project = Select(currentRef)) else key.scope
		getOrError(scope, key.key)
	}
	def runTask[T](key: ScopedTask[T], state: State): T =
	{
			import EvaluateTask._
		val value: Option[Result[T]] = evaluateTask(structure, key.task.scoped, state, currentRef)
		val result = getOrError(key.scope, key.key, value)
		processResult(result, ConsoleLogger())
	}
	private def getOrError[T](scope: Scope, key: AttributeKey[_], value: Option[T]): T =
		value getOrElse error(Project.display(ScopedKey(scope, key)) + " is undefined.")
	private def getOrError[T](scope: Scope, key: AttributeKey[T]): T =
		structure.data.get(scope, key) getOrElse error(Project.display(ScopedKey(scope, key)) + " is undefined.")
}

sealed trait ClasspathDep[PR <: ProjectReference] { def project: PR; def configuration: Option[String] }
final case class ResolvedClasspathDependency(project: ProjectRef, configuration: Option[String]) extends ClasspathDep[ProjectRef]
final case class ClasspathDependency(project: ProjectReference, configuration: Option[String]) extends ClasspathDep[ProjectReference]

object Project extends Init[Scope] with ProjectExtra
{
	private abstract class ProjectDef[PR <: ProjectReference](val id: String, val base: File, aggregate0: => Seq[PR], dependencies0: => Seq[ClasspathDep[PR]], delegates0: => Seq[PR],
		val settings: Seq[Setting[_]], val configurations: Seq[Configuration]) extends ProjectDefinition[PR]
	{
		lazy val aggregate = aggregate0
		lazy val dependencies = dependencies0
		lazy val delegates = delegates0
	
		Dag.topologicalSort(configurations)(_.extendsConfigs) // checks for cyclic references here instead of having to do it in Scope.delegates
	}

	def apply(id: String, base: File, aggregate: => Seq[ProjectReference] = Nil, dependencies: => Seq[ClasspathDep[ProjectReference]] = Nil, delegates: => Seq[ProjectReference] = Nil,
		settings: Seq[Setting[_]] = defaultSettings, configurations: Seq[Configuration] = Configurations.default): Project =
			new ProjectDef[ProjectReference](id, base, aggregate, dependencies, delegates, settings, configurations) with Project

	def resolved(id: String, base: File, aggregate: => Seq[ProjectRef], dependencies: => Seq[ResolvedClasspathDependency], delegates: => Seq[ProjectRef],
		settings: Seq[Setting[_]], configurations: Seq[Configuration]): ResolvedProject =
			new ProjectDef[ProjectRef](id, base, aggregate, dependencies, delegates, settings, configurations) with ResolvedProject

	def defaultSettings: Seq[Setting[_]] = Defaults.defaultSettings

	final class Constructor(p: ProjectReference) {
		def %(conf: String): ClasspathDependency = new ClasspathDependency(p, Some(conf))
	}

	def getOrError[T](state: State, key: AttributeKey[T], msg: String): T = state get key getOrElse error(msg)
	def structure(state: State): BuildStructure = getOrError(state, stateBuildStructure, "No build loaded.")
	def session(state: State): SessionSettings = getOrError(state, sessionSettings, "Session not initialized.")

	def extract(state: State): Extracted  =  extract( session(state), structure(state) )
	def extract(se: SessionSettings, st: BuildStructure): Extracted  =  Extracted(st, se, se.current, Load.getRootProject(st.units))

	def getProjectForReference(ref: Reference, structure: BuildStructure): Option[ResolvedProject] =
		ref match { case pr: ProjectRef => getProject(pr, structure); case _ => None }
	def getProject(ref: ProjectRef, structure: BuildStructure): Option[ResolvedProject] =
		(structure.units get ref.build).flatMap(_.defined get ref.project)

	def setProject(session: SessionSettings, structure: BuildStructure, s: State): State =
	{
		val previousOnUnload = orIdentity(s get Keys.onUnload.key)
		val unloaded = previousOnUnload(s.runExitHooks())
		val (onLoad, onUnload) = getHooks(structure.data)
		val newAttrs = unloaded.attributes.put(stateBuildStructure, structure).put(sessionSettings, session).put(Keys.onUnload.key, onUnload)
		val newState = unloaded.copy(attributes = newAttrs)
		onLoad(updateCurrent( newState ))
	}
	def orIdentity[T](opt: Option[T => T]): T => T = opt getOrElse idFun
	def getHook[T](key: ScopedSetting[T => T], data: Settings[Scope]): T => T  =  orIdentity(key in GlobalScope get data)
	def getHooks(data: Settings[Scope]): (State => State, State => State)  =  (getHook(Keys.onLoad, data), getHook(Keys.onUnload, data))

	def current(state: State): ProjectRef = session(state).current
	def updateCurrent(s0: State): State =
	{
		val structure = Project.structure(s0)
		val s = installGlobalLogger(s0, structure)
		val ref = Project.current(s)
		val project = Load.getProject(structure.units, ref.build, ref.project)
		logger(s).info("Set current project to " + ref.project + " (in build " + ref.build +")")
		def get[T](k: SettingKey[T]): Option[T] = k in ref get structure.data
		def commandsIn(axis: ResolvedReference) = commands in axis get structure.data toList ;

		val allCommands = commandsIn(ref) ++ commandsIn(BuildRef(ref.build)) ++ (commands in Global get structure.data toList )
		val history = get(historyPath) flatMap idFun
		val prompt = get(shellPrompt)
		val watched = get(watch)
		val commandDefs = allCommands.distinct.flatten[Command].map(_ tag (projectCommand, true))
		val newDefinedCommands = commandDefs ++ BuiltinCommands.removeTagged(s.definedCommands, projectCommand)
		val newAttrs = setCond(Watched.Configuration, watched, s.attributes).put(historyPath.key, history)
		s.copy(attributes = setCond(shellPrompt.key, prompt, newAttrs), definedCommands = newDefinedCommands)
	}
	def setCond[T](key: AttributeKey[T], vopt: Option[T], attributes: AttributeMap): AttributeMap =
		vopt match { case Some(v) => attributes.put(key, v); case None => attributes.remove(key) }
	def makeSettings(settings: Seq[Setting[_]], delegates: Scope => Seq[Scope], scopeLocal: ScopedKey[_] => Seq[Setting[_]]) =
		translateCyclic( make(settings)(delegates, scopeLocal) )

	def display(scoped: ScopedKey[_]): String = Scope.display(scoped.scope, scoped.key.label)
	def display(ref: Reference): String =
		ref match
		{
			case pr: ProjectReference => display(pr)
			case br: BuildReference => display(br)
		}
	def display(ref: BuildReference) =
		ref match
		{
			case ThisBuild => "<this>"
			case BuildRef(uri) => "{" + uri + "}"
		}
	def display(ref: ProjectReference) =
		ref match
		{
			case ThisProject => "(<this>)<this>"
			case LocalProject(id) => "(<this>)" + id
			case RootProject(uri) => "{" + uri + " }<root>"
			case ProjectRef(uri, id) => "{" + uri + "}" + id
		}

	def fillTaskAxis(scoped: ScopedKey[_]): ScopedKey[_] =
		ScopedKey(Scope.fillTaskAxis(scoped.scope, scoped.key), scoped.key)

	def mapScope(f: Scope => Scope) = new  (ScopedKey ~> ScopedKey) { def apply[T](key: ScopedKey[T]) =
		ScopedKey( f(key.scope), key.key)
	}

	def transform(g: Scope => Scope, ss: Seq[Setting[_]]): Seq[Setting[_]] = {
		val f = mapScope(g)
		ss.map(_ mapKey f mapReferenced f)
	}
	def transformRef(g: Scope => Scope, ss: Seq[Setting[_]]): Seq[Setting[_]] = {
		val f = mapScope(g)
		ss.map(_ mapReferenced f)
	}
	def translateCyclic[T](f: => T): T =
		try { f } catch { case c: Dag.Cyclic => throw new MessageOnlyException(c.getMessage) }

	def delegates(structure: BuildStructure, scope: Scope, key: AttributeKey[_]): Seq[ScopedKey[_]] =
		structure.delegates(scope).map(d => ScopedKey(d, key))

	def details(structure: BuildStructure, actual: Boolean, scope: Scope, key: AttributeKey[_]): String =
	{
		val scoped = ScopedKey(scope,key)
		val value = 
			structure.data.get(scope, key) match {
				case None => "No entry for key."
				case Some(v: Task[_]) => "Task"
				case Some(v: InputTask[_]) => "Input task"
				case Some(v) => "Value:\n\t" + v.toString
			}
		val description = key.description match { case Some(desc) => "Description:\n\t" + desc + "\n"; case None => "" }
		val definedIn = structure.data.definingScope(scope, key) match {
			case Some(sc) => "Provided by:\n\t" + Scope.display(sc, key.label) + "\n"
			case None => ""
		}
		val cMap = compiled(structure.settings, actual)(structure.delegates, structure.scopeLocal)
		val related = cMap.keys.filter(k => k.key == key && k.scope != scope)
		val depends = cMap.get(scoped) match { case Some(c) => c.dependencies.toSet; case None => Set.empty }
		val reverse = reverseDependencies(cMap, scoped)
		def printScopes(label: String, scopes: Iterable[ScopedKey[_]]) =
			if(scopes.isEmpty) "" else scopes.map(display).mkString(label + ":\n\t", "\n\t", "\n")

		value + "\n" +
			description +
			definedIn +
			printScopes("Dependencies", depends) +
			printScopes("Reverse dependencies", reverse) +
			printScopes("Delegates", delegates(structure, scope, key)) +
			printScopes("Related", related)
	}
	def graphSettings(structure: BuildStructure, basedir: File)
	{
		def graph(actual: Boolean, name: String) = graphSettings(structure, actual, name, new File(basedir, name + ".dot"))
		graph(true, "actual_dependencies")
		graph(false, "declared_dependencies")
	}
	def graphSettings(structure: BuildStructure, actual: Boolean, graphName: String, file: File)
	{
		type Rel = Relation[ScopedKey[_], ScopedKey[_]]
		val cMap = compiled(structure.settings, actual)(structure.delegates, structure.scopeLocal)
		val relation =
			((Relation.empty: Rel) /: cMap) { case (r, (key, value)) =>
				r + (key, value.dependencies)
			}
		val keyToString = (key: ScopedKey[_]) => Project display key
		DotGraph.generateGraph(file, graphName, relation, keyToString, keyToString)
	}
	def reverseDependencies(cMap: CompiledMap, scoped: ScopedKey[_]): Iterable[ScopedKey[_]] =
		for( (key,compiled) <- cMap; dep <- compiled.dependencies if dep == scoped)  yield  key

	object LoadAction extends Enumeration {
		val Return, Current, Plugins = Value
	}
	import LoadAction._
	import complete.DefaultParsers._

	val loadActionParser = token(Space ~> ("plugins" ^^^ Plugins | "return" ^^^ Return)) ?? Current
	
	val ProjectReturn = AttributeKey[List[File]]("project-return", "Maintains a stack of builds visited using reload.")
	def projectReturn(s: State): List[File] = s.attributes get ProjectReturn getOrElse Nil
	def setProjectReturn(s: State, pr: List[File]): State = s.copy(attributes = s.attributes.put( ProjectReturn, pr) )
	def loadAction(s: State, action: LoadAction.Value) = action match {
		case Return =>
			projectReturn(s) match
			{
				case current :: returnTo :: rest => (setProjectReturn(s, returnTo :: rest), returnTo)
				case _ => error("Not currently in a plugin definition")
			}
		case Current =>
			val base = s.configuration.baseDirectory
			projectReturn(s) match { case Nil => (setProjectReturn(s, base :: Nil), base); case x :: xs => (s, x) }
		case Plugins =>
			val extracted = Project.extract(s)
			val newBase = extracted.currentUnit.unit.plugins.base
			val newS = setProjectReturn(s, newBase :: projectReturn(s))
			(newS, newBase)
	}
	
	def evaluateTask[T](taskKey: ScopedKey[Task[T]], state: State, checkCycles: Boolean = false, maxWorkers: Int = EvaluateTask.SystemProcessors): Option[Result[T]] =
	{
		val extracted = Project.extract(state)
		EvaluateTask.evaluateTask(extracted.structure, taskKey, state, extracted.currentRef, checkCycles, maxWorkers)
	}
	def globalLoggerKey = fillTaskAxis(ScopedKey(GlobalScope, streams.key))
	def installGlobalLogger(s: State, structure: BuildStructure): State =
	{
		val str = structure.streams(globalLoggerKey)
		str.open()
		s.put(logged, str.log).addExitHook { str.close() }
	}
	// this is here instead of Scoped so that it is considered without need for import (because of Project.Initialize)
	implicit def richInitializeTask[T](init: Initialize[Task[T]]): Scoped.RichInitializeTask[T] = new Scoped.RichInitializeTask(init)
}

trait ProjectExtra
{
	implicit def configDependencyConstructor[T <% ProjectReference](p: T): Constructor = new Constructor(p)
	implicit def classpathDependency[T <% ProjectReference](p: T): ClasspathDependency = new ClasspathDependency(p, None)

	def inConfig(conf: Configuration)(ss: Seq[Setting[_]]): Seq[Setting[_]] =
		inScope(ThisScope.copy(config = Select(conf)) )( (configuration :== conf) +: ss)
	def inTask(t: Scoped)(ss: Seq[Setting[_]]): Seq[Setting[_]] =
		inScope(ThisScope.copy(task = Select(t.key)) )( ss )
	def inScope(scope: Scope)(ss: Seq[Setting[_]]): Seq[Setting[_]] =
		Project.transform(Scope.replaceThis(scope), ss)
}