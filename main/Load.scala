/* sbt -- Simple Build Tool
 * Copyright 2011 Mark Harrah
 */
package sbt

	import java.io.File
	import java.net.URI
	import compiler.{Discovered,Discovery,Eval,EvalImports}
	import classpath.ClasspathUtilities
	import scala.annotation.tailrec
	import collection.mutable
	import Compiler.{Compilers,Inputs}
	import inc.{FileValueCache, Locate}
	import Project.{inScope, ScopedKey, ScopeLocal, Setting}
	import Keys.{appConfiguration, baseDirectory, configuration, streams, Streams, thisProject, thisProjectRef}
	import Keys.{isDummy, parseResult, resolvedScoped, taskDefinitionKey}
	import tools.nsc.reporters.ConsoleReporter
	import Build.{analyzed, data}
	import Scope.{GlobalScope, ThisScope}

object Load
{
	import BuildPaths._
	import BuildStreams._
	import Locate.DefinesClass

	// note that there is State passed in but not pulled out
	def defaultLoad(state: State, baseDirectory: File, log: Logger): (() => Eval, BuildStructure) =
	{
		val provider = state.configuration.provider
		val scalaProvider = provider.scalaProvider
		val stagingDirectory = defaultStaging.getCanonicalFile
		val base = baseDirectory.getCanonicalFile
		val loader = getClass.getClassLoader
		val classpath = provider.mainClasspath ++ scalaProvider.jars
		val compilers = Compiler.compilers(ClasspathOptions.boot)(state.configuration, log)
		val evalPluginDef = EvaluateTask.evalPluginDef(log) _
		val delegates = defaultDelegates
		val inject: Seq[Project.Setting[_]] = ((appConfiguration in GlobalScope) :== state.configuration) +: EvaluateTask.injectSettings
		val definesClass = FileValueCache(Locate.definesClass _)
		val rawConfig = new LoadBuildConfiguration(stagingDirectory, Nil, classpath, loader, compilers, evalPluginDef, definesClass.get, delegates, EvaluateTask.injectStreams, inject, log)
		val commonPlugins = if(baseDirectory == defaultGlobalPlugins) Nil else buildGlobalPlugins(defaultGlobalPlugins, state, rawConfig)
		val config = rawConfig.copy(commonPluginClasspath = commonPlugins)
		val result = apply(base, state, config)
		definesClass.clear()
		result
	}
	def buildGlobalPlugins(baseDirectory: File, state: State, config: LoadBuildConfiguration): Seq[Attributed[File]] =
		if(baseDirectory.isDirectory) buildPluginDefinition(baseDirectory, state, config) else Nil
	def defaultDelegates: LoadedBuild => Scope => Seq[Scope] = (lb: LoadedBuild) => {
		val rootProject = getRootProject(lb.units)
		def resolveRef(project: Reference): ResolvedReference = Scope.resolveReference(lb.root, rootProject, project)
		Scope.delegates(
			lb.allProjectRefs,
			(_: ResolvedProject).configurations.map(c => ConfigKey(c.name)),
			resolveRef,
			rootProject,
			project => projectInherit(lb, project),
			(project, config) => configInherit(lb, project, config, rootProject),
			task => task.extend,
			(project, extra) => Nil
		)
	}
	def configInherit(lb: LoadedBuild, ref: ResolvedReference, config: ConfigKey, rootProject: URI => String): Seq[ConfigKey] =
		ref match
		{
			case pr: ProjectRef => configInheritRef(lb, pr, config)
			case BuildRef(uri) => configInheritRef(lb, ProjectRef(uri, rootProject(uri)), config)
		}
	def configInheritRef(lb: LoadedBuild, ref: ProjectRef, config: ConfigKey): Seq[ConfigKey] =
		configurationOpt(lb.units, ref.build, ref.project, config).toList.flatMap(_.extendsConfigs).map(c => ConfigKey(c.name))

	def projectInherit(lb: LoadedBuild, ref: ProjectRef): Seq[ProjectRef] =
		getProject(lb.units, ref.build, ref.project).delegates

		// build, load, and evaluate all units.
		//  1) Compile all plugin definitions
		//  2) Evaluate plugin definitions to obtain and compile plugins and get the resulting classpath for the build definition
		//  3) Instantiate Plugins on that classpath
		//  4) Compile all build definitions using plugin classpath
		//  5) Load build definitions.
		//  6) Load all configurations using build definitions and plugins (their classpaths and loaded instances).
		//  7) Combine settings from projects, plugins, and configurations
		//  8) Evaluate settings
	def apply(rootBase: File, s: State, config: LoadBuildConfiguration): (() => Eval, BuildStructure) =
	{
		// load, which includes some resolution, but can't fill in project IDs yet, so follow with full resolution
		val loaded = resolveProjects(load(rootBase, s, config))
		val projects = loaded.units
		lazy val rootEval = lazyEval(loaded.units(loaded.root).unit)
		val settings = finalTransforms(config.injectSettings ++ buildConfigurations(loaded, getRootProject(projects), rootEval))
		val delegates = config.delegates(loaded)
		val data = Project.makeSettings(settings, delegates, config.scopeLocal)
		val index = structureIndex(data)
		val streams = mkStreams(projects, loaded.root, data)
		(rootEval, new BuildStructure(projects, loaded.root, settings, data, index, streams, delegates, config.scopeLocal))
	}

	// map dependencies on the special tasks so that the scope is the same as the defining key
	// additionally, set the task axis to the defining key if it is not set
	def finalTransforms(ss: Seq[Setting[_]]): Seq[Setting[_]] =
	{
		import Keys.{parseResult, resolvedScoped}
		def isSpecial(key: AttributeKey[_]) = key == streams.key || key == resolvedScoped.key || key == parseResult.key
		def mapSpecial(to: ScopedKey[_]) = new (ScopedKey ~> ScopedKey){ def apply[T](key: ScopedKey[T]) =
			if(isSpecial(key.key))
			{
				val replaced = Scope.replaceThis(to.scope)(key.scope)
				val scope = if(key.key == resolvedScoped.key) replaced else Scope.fillTaskAxis(replaced, to.key)
				ScopedKey(scope, key.key)
			}
			else key
		}
		def setDefining[T] = (key: ScopedKey[T], value: T) => value match {
			case tk: Task[t] => setDefinitionKey(tk, key).asInstanceOf[T]
			case ik: InputTask[t] => ik.mapTask( tk => setDefinitionKey(tk, key) ).asInstanceOf[T]
			case _ => value
		}
		ss.map(s => s mapReferenced mapSpecial(s.key) mapInit setDefining )
	}
	def setDefinitionKey[T](tk: Task[T], key: ScopedKey[_]): Task[T] =
		if(isDummy(tk)) tk else Task(tk.info.set(Keys.taskDefinitionKey, key), tk.work)

	def structureIndex(settings: Settings[Scope]): StructureIndex =
		new StructureIndex(Index.stringToKeyMap(settings), Index.taskToKeyMap(settings), Index.triggers(settings), KeyIndex(settings.allKeys( (s,k) => ScopedKey(s,k))))

		// Reevaluates settings after modifying them.  Does not recompile or reload any build components.
	def reapply(newSettings: Seq[Setting[_]], structure: BuildStructure): BuildStructure =
	{
		val newData = Project.makeSettings(newSettings, structure.delegates, structure.scopeLocal)
		val newIndex = structureIndex(newData)
		val newStreams = mkStreams(structure.units, structure.root, newData)
		new BuildStructure(units = structure.units, root = structure.root, settings = newSettings, data = newData, index = newIndex, streams = newStreams, delegates = structure.delegates, scopeLocal = structure.scopeLocal)
	}

	def isProjectThis(s: Setting[_]) = s.key.scope.project match { case This | Select(ThisProject) => true; case _ => false }
	def buildConfigurations(loaded: LoadedBuild, rootProject: URI => String, rootEval: () => Eval): Seq[Setting[_]] =
		loaded.units.toSeq flatMap { case (uri, build) =>
			val eval = if(uri == loaded.root) rootEval else lazyEval(build.unit)
			val pluginSettings = build.unit.plugins.plugins
			val (pluginThisProject, pluginNotThis) = pluginSettings partition isProjectThis
			val projectSettings = build.defined flatMap { case (id, project) =>
				val srcs = configurationSources(project.base)
				val ref = ProjectRef(uri, id)
				val defineConfig = for(c <- project.configurations) yield ( (configuration in (ref, ConfigKey(c.name))) :== c)
				val settings =
					(thisProject :== project) +:
					(thisProjectRef :== ref) +:
					(defineConfig ++ project.settings ++ pluginThisProject ++ configurations(srcs, eval, build.imports))
				 
				// map This to thisScope, Select(p) to mapRef(uri, rootProject, p)
				transformSettings(projectScope(ref), uri, rootProject, settings)
			}
			val buildScope = Scope(Select(BuildRef(uri)), Global, Global, Global)
			val buildBase = baseDirectory :== build.localBase
			val buildSettings = transformSettings(buildScope, uri, rootProject, pluginNotThis ++ (buildBase +: build.buildSettings))
			buildSettings ++ projectSettings
		}
	def transformSettings(thisScope: Scope, uri: URI, rootProject: URI => String, settings: Seq[Setting[_]]): Seq[Setting[_]] =
		Project.transform(Scope.resolveScope(thisScope, uri, rootProject), settings)
	def projectScope(project: Reference): Scope  =  Scope(Select(project), Global, Global, Global)
	
	def lazyEval(unit: BuildUnit): () => Eval =
	{
		lazy val eval = mkEval(unit)
		() => eval
	}
	def mkEval(unit: BuildUnit): Eval = mkEval(unit.definitions, unit.plugins, Nil)
	def mkEval(defs: LoadedDefinitions, plugs: LoadedPlugins, options: Seq[String]): Eval =
		new Eval(options, defs.target +: plugs.classpath, s => new ConsoleReporter(s), defs.loader, Some(evalOutputDirectory(defs.base)))

	def configurations(srcs: Seq[File], eval: () => Eval, imports: Seq[String]): Seq[Setting[_]] =
		if(srcs.isEmpty) Nil else EvaluateConfigurations(eval(), srcs, imports)

	def load(file: File, s: State, config: LoadBuildConfiguration): PartBuild =
	{
		val loader = (uri: URI, local: File) => loadUnit(uri, local, s, config)
		val fail = (uri: URI) => error("Invalid build URI: " + uri)
		val builtinLoader = BuildLoader(loader, info => RetrieveUnit(info.staging, info.build), fail, config)
		load(file, builtinLoader)
	}
	def load(file: File, loaders: BuildLoader): PartBuild = loadURI(IO.directoryURI(file), loaders)
	def loadURI(uri: URI, loaders: BuildLoader): PartBuild =
	{
		IO.assertAbsolute(uri)
		val (referenced, map) = loadAll(uri :: Nil, Map.empty, loaders, Map.empty)
		checkAll(referenced, map)
		new PartBuild(uri, map)
	}
	def addResolvers(unit: BuildUnit, isRoot: Boolean, loaders: BuildLoader): BuildLoader =
		unit.definitions.builds.flatMap(_.buildResolvers) match
		{
			case Nil => loaders
			case x :: xs =>
				import Alternatives._
				val resolver = (x /: xs){ _ | _ }
				if(isRoot) loaders.setRoot(resolver) else loaders.addNonRoot(unit.uri, resolver)
		}

	def loaded(unit: BuildUnit): (PartBuildUnit, List[ProjectReference]) =
	{
		val defined = projects(unit)
		if(defined.isEmpty) error("No projects defined in build unit " + unit)

		// since base directories are resolved at this point (after 'projects'),
		//   we can compare Files instead of converting to URIs
		def isRoot(p: Project) = p.base == unit.localBase

		val externals = referenced(defined).toList
		val projectsInRoot = defined.filter(isRoot).map(_.id)
		val rootProjects = if(projectsInRoot.isEmpty) defined.head.id :: Nil else projectsInRoot
		(new PartBuildUnit(unit, defined.map(d => (d.id, d)).toMap, rootProjects, buildSettings(unit)), externals)
	}
	def buildSettings(unit: BuildUnit): Seq[Setting[_]] =
	{
		val buildScope = GlobalScope.copy(project = Select(BuildRef(unit.uri)))
		val resolve = Scope.resolveBuildScope(buildScope, unit.uri)
		Project.transform(resolve, unit.definitions.builds.flatMap(_.settings))
	}

	@tailrec def loadAll(bases: List[URI], references: Map[URI, List[ProjectReference]], loaders: BuildLoader, builds: Map[URI, PartBuildUnit]): (Map[URI, List[ProjectReference]], Map[URI, PartBuildUnit]) =
		bases match
		{
			case b :: bs =>
				if(builds contains b)
					loadAll(bs, references, loaders, builds)
				else
				{
					val (loadedBuild, refs) = loaded(loaders(b))
					checkBuildBase(loadedBuild.unit.localBase)
					val newLoader = addResolvers(loadedBuild.unit, builds.isEmpty, loaders)
					loadAll(refs.flatMap(Reference.uri) reverse_::: bs, references.updated(b, refs), newLoader, builds.updated(b, loadedBuild))
				}
			case Nil => (references, builds)
		}
	def checkProjectBase(buildBase: File, projectBase: File)
	{
		checkDirectory(projectBase)
		assert(buildBase == projectBase || IO.relativize(buildBase, projectBase).isDefined, "Directory " + projectBase + " is not contained in build root " + buildBase)
	}
	def checkBuildBase(base: File) = checkDirectory(base)
	def checkDirectory(base: File)
	{
		assert(base.isAbsolute, "Not absolute: " + base)
		if(base.isFile)
			error("Not a directory: " + base)
		else if(!base.exists)
			IO createDirectory base
	}
	def resolveAll(builds: Map[URI, PartBuildUnit]): Map[URI, LoadedBuildUnit] =
	{
		val rootProject = getRootProject(builds)
		builds map { case (uri,unit) =>
			(uri, unit.resolveRefs( ref => Scope.resolveProjectRef(uri, rootProject, ref) ))
		} toMap;
	}
	def checkAll(referenced: Map[URI, List[ProjectReference]], builds: Map[URI, PartBuildUnit])
	{
		val rootProject = getRootProject(builds)
		for( (uri, refs) <- referenced; ref <- refs)
		{
			val ProjectRef(refURI, refID) = Scope.resolveProjectRef(uri, rootProject, ref)
			val loadedUnit = builds(refURI)
			if(! (loadedUnit.defined contains refID) )
				error("No project '" + refID + "' in '" + refURI + "'")
		}
	}

	def resolveBase(against: File): Project => Project =
	{
		def resolve(f: File) =
		{
			val fResolved = new File(IO.directoryURI(IO.resolve(against, f)))
			checkProjectBase(against, fResolved)
			fResolved
		}
		p => p.copy(base = resolve(p.base))
	}
	def resolveProjects(loaded: PartBuild): LoadedBuild =
	{
		val rootProject = getRootProject(loaded.units)
		new LoadedBuild(loaded.root, loaded.units map { case (uri, unit) =>
			IO.assertAbsolute(uri)
			(uri, resolveProjects(uri, unit, rootProject))
		})
	}
	def resolveProjects(uri: URI, unit: PartBuildUnit, rootProject: URI => String): LoadedBuildUnit =
	{
		IO.assertAbsolute(uri)
		val resolve = (_: Project).resolve(ref => Scope.resolveProjectRef(uri, rootProject, ref))
		new LoadedBuildUnit(unit.unit, unit.defined mapValues resolve toMap, unit.rootProjects, unit.buildSettings)
	}
	def projects(unit: BuildUnit): Seq[Project] =
	{
		// we don't have the complete build graph loaded, so we don't have the rootProject function yet.
		//  Therefore, we use resolveProjectBuild instead of resolveProjectRef.  After all builds are loaded, we can fully resolve ProjectReferences.
		val resolveBuild = (_: Project).resolveBuild(ref => Scope.resolveProjectBuild(unit.uri, ref))
		val resolve = resolveBuild compose resolveBase(unit.localBase)
		unit.definitions.builds.flatMap(_.projectDefinitions(unit.localBase) map resolve)
	}
	def getRootProject(map: Map[URI, BuildUnitBase]): URI => String =
		uri => getBuild(map, uri).rootProjects.headOption getOrElse emptyBuild(uri)
	def getConfiguration(map: Map[URI, LoadedBuildUnit], uri: URI, id: String, conf: ConfigKey): Configuration =
		configurationOpt(map, uri, id, conf) getOrElse noConfiguration(uri, id, conf.name)
	def configurationOpt(map: Map[URI, LoadedBuildUnit], uri: URI, id: String, conf: ConfigKey): Option[Configuration] =
		getProject(map, uri, id).configurations.find(_.name == conf.name)

	def getProject(map: Map[URI, LoadedBuildUnit], uri: URI, id: String): ResolvedProject =
		getBuild(map, uri).defined.getOrElse(id, noProject(uri, id))
	def getBuild[T](map: Map[URI, T], uri: URI): T =
		map.getOrElse(uri, noBuild(uri))

	def emptyBuild(uri: URI) = error("No root project defined for build unit '" + uri + "'")
	def noBuild(uri: URI) = error("Build unit '" + uri + "' not defined.")
	def noProject(uri: URI, id: String) = error("No project '" + id + "' defined in '" + uri + "'.")
	def noConfiguration(uri: URI, id: String, conf: String) = error("No configuration '" + conf + "' defined in project '" + id + "' in '" + uri +"'")

	def loadUnit(uri: URI, localBase: File, s: State, config: LoadBuildConfiguration): BuildUnit =
	{
		val normBase = localBase.getCanonicalFile
		val defDir = selectProjectDir(normBase)
		val pluginDir = pluginDirectory(defDir)
		val plugs = plugins(pluginDir, s, config)

		val defs = definitionSources(defDir)
		val target = buildOutputDirectory(defDir, config.compilers)
		IO.createDirectory(target)
		val loadedDefs =
			if(defs.isEmpty)
				new LoadedDefinitions(defDir, target, plugs.loader, Build.default :: Nil, Nil)
			else
				definitions(defDir, target, defs, plugs, config.definesClass, config.compilers, config.log, normBase)

		new BuildUnit(uri, normBase, loadedDefs, plugs)
	}

	def plugins(dir: File, s: State, config: LoadBuildConfiguration): LoadedPlugins = if(dir.exists) buildPlugins(dir, s, config) else noPlugins(dir, config)
	def noPlugins(dir: File, config: LoadBuildConfiguration): LoadedPlugins = loadPluginDefinition(dir, config, config.commonPluginClasspath)
	def buildPlugins(dir: File, s: State, config: LoadBuildConfiguration): LoadedPlugins =
		loadPluginDefinition(dir, config, buildPluginDefinition(dir, s, config))

	def loadPluginDefinition(dir: File, config: LoadBuildConfiguration, pluginClasspath: Seq[Attributed[File]]): LoadedPlugins =
	{
		val definitionClasspath = if(pluginClasspath.isEmpty) config.classpath else (data(pluginClasspath) ++ config.classpath).distinct
		val pluginLoader = if(pluginClasspath.isEmpty) config.loader else ClasspathUtilities.toLoader(definitionClasspath, config.loader)
		loadPlugins(dir, definitionClasspath, pluginLoader, analyzed(pluginClasspath))
	}
	def buildPluginDefinition(dir: File, s: State, config: LoadBuildConfiguration): Seq[Attributed[File]] =
	{
		val (eval,pluginDef) = apply(dir, s, config)
		val pluginState = Project.setProject(Load.initialSession(pluginDef, eval), pluginDef, s)
		val thisPluginClasspath = config.evalPluginDef(pluginDef, pluginState)
		(thisPluginClasspath ++ config.commonPluginClasspath).distinct
	}

	def definitions(base: File, targetBase: File, srcs: Seq[File], plugins: LoadedPlugins, definesClass: DefinesClass, compilers: Compilers, log: Logger, buildBase: File): LoadedDefinitions =
	{
		val (inputs, defAnalysis) = build(plugins.classpath, srcs, targetBase, compilers, definesClass, log)
		val target = inputs.config.classesDirectory
		val definitionLoader = ClasspathUtilities.toLoader(target :: Nil, plugins.loader)
		val defNames = findDefinitions(defAnalysis)
		val defs = if(defNames.isEmpty) Build.default :: Nil else loadDefinitions(definitionLoader, defNames)
		new LoadedDefinitions(base, target, definitionLoader, defs, defNames)
	}

	def loadDefinitions(loader: ClassLoader, defs: Seq[String]): Seq[Build] =
		defs map { definition => loadDefinition(loader, definition) }
	def loadDefinition(loader: ClassLoader, definition: String): Build =
		ModuleUtilities.getObject(definition, loader).asInstanceOf[Build]

	def build(classpath: Seq[File], sources: Seq[File], target: File, compilers: Compilers, definesClass: DefinesClass, log: Logger): (Inputs, inc.Analysis) =
	{
		val inputs = Compiler.inputs(classpath, sources, target, Nil, Nil, definesClass, Compiler.DefaultMaxErrors, CompileOrder.Mixed)(compilers, log)
		val analysis =
			try { Compiler(inputs, log) }
			catch { case _: xsbti.CompileFailed => throw new NoMessageException } // compiler already logged errors
		(inputs, analysis)
	}

	def loadPlugins(dir: File, classpath: Seq[File], loader: ClassLoader, analysis: Seq[inc.Analysis]): LoadedPlugins =
	{
		val (pluginNames, plugins) = if(classpath.isEmpty) (Nil, Nil) else {
			val names = ( binaryPlugins(loader) ++ (analysis flatMap findPlugins) ).distinct
			(names, loadPlugins(loader, names) )
		}
		new LoadedPlugins(dir, classpath, loader, plugins, pluginNames)
	}
	def binaryPlugins(loader: ClassLoader): Seq[String] =
	{
		import collection.JavaConversions._
		loader.getResources("sbt/sbt.plugins").toSeq flatMap { u => IO.readLinesURL(u) map { _.trim } filter { !_.isEmpty } };
	}

	def loadPlugins(loader: ClassLoader, pluginNames: Seq[String]): Seq[Setting[_]] =
		pluginNames.flatMap(pluginName => loadPlugin(pluginName, loader))

	def loadPlugin(pluginName: String, loader: ClassLoader): Seq[Setting[_]] =
		ModuleUtilities.getObject(pluginName, loader).asInstanceOf[Plugin].settings

	def importAll(values: Seq[String]) = if(values.isEmpty) Nil else values.map( _ + "._" ).mkString("import ", ", ", "") :: Nil
		
	def findPlugins(analysis: inc.Analysis): Seq[String]  =  discover(analysis, "sbt.Plugin")
	def findDefinitions(analysis: inc.Analysis): Seq[String]  =  discover(analysis, "sbt.Build")
	def discover(analysis: inc.Analysis, subclasses: String*): Seq[String] =
	{
		val subclassSet = subclasses.toSet
		val ds = Discovery(subclassSet, Set.empty)(Tests.allDefs(analysis))
		ds.flatMap {
			case (definition, Discovered(subs,_,_,true)) =>
				if((subs & subclassSet).isEmpty) Nil else definition.name :: Nil
			case _ => Nil
		}
	}

	def initialSession(structure: BuildStructure, rootEval: () => Eval): SessionSettings =
		new SessionSettings(structure.root, rootProjectMap(structure.units), structure.settings, Map.empty, rootEval)
		
	def rootProjectMap(units: Map[URI, LoadedBuildUnit]): Map[URI, String] =
	{
		val getRoot = getRootProject(units)
		units.keys.map(uri => (uri, getRoot(uri))).toMap
	}

	def baseImports = "import sbt._, Process._, Keys._" :: Nil

	final class EvaluatedConfigurations(val eval: Eval, val settings: Seq[Setting[_]])
	final class LoadedDefinitions(val base: File, val target: File, val loader: ClassLoader, val builds: Seq[Build], val buildNames: Seq[String])
	final class LoadedPlugins(val base: File, val classpath: Seq[File], val loader: ClassLoader, val plugins: Seq[Setting[_]], val pluginNames: Seq[String])
	final class BuildUnit(val uri: URI, val localBase: File, val definitions: LoadedDefinitions, val plugins: LoadedPlugins)
	{
		override def toString = if(uri.getScheme == "file") localBase.toString else (uri + " (locally: " + localBase +")")
	}
	
	final class LoadedBuild(val root: URI, val units: Map[URI, LoadedBuildUnit])
	{
		checkCycles(units)
		def allProjectRefs: Seq[(ProjectRef, ResolvedProject)] = for( (uri, unit) <- units.toSeq; (id, proj) <- unit.defined ) yield ProjectRef(uri, id) -> proj
	}
	def checkCycles(units: Map[URI, LoadedBuildUnit])
	{
		def getRef(pref: ProjectRef) = units(pref.build).defined(pref.project)
		def deps(proj: ResolvedProject)(base: ResolvedProject => Seq[ProjectRef]): Seq[ResolvedProject]  =  Dag.topologicalSort(proj)(p => base(p) map getRef)
		 // check for cycles
		for( (_, lbu) <- units; proj <- lbu.defined.values) {
			deps(proj)(_.dependencies.map(_.project))
			deps(proj)(_.delegates)
			deps(proj)(_.aggregate)
		}
	}
	final class PartBuild(val root: URI, val units: Map[URI, PartBuildUnit])
	sealed trait BuildUnitBase { def rootProjects: Seq[String]; def buildSettings: Seq[Setting[_]] }
	final class PartBuildUnit(val unit: BuildUnit, val defined: Map[String, Project], val rootProjects: Seq[String], val buildSettings: Seq[Setting[_]]) extends BuildUnitBase
	{
		def resolve(f: Project => ResolvedProject): LoadedBuildUnit = new LoadedBuildUnit(unit, defined mapValues f toMap, rootProjects, buildSettings)
		def resolveRefs(f: ProjectReference => ProjectRef): LoadedBuildUnit = resolve(_ resolve f)
	}
	final class LoadedBuildUnit(val unit: BuildUnit, val defined: Map[String, ResolvedProject], val rootProjects: Seq[String], val buildSettings: Seq[Setting[_]]) extends BuildUnitBase
	{
		assert(!rootProjects.isEmpty, "No root projects defined for build unit " + unit)
		def localBase = unit.localBase
		def classpath = unit.definitions.target +: unit.plugins.classpath
		def loader = unit.definitions.loader
		def imports = getImports(unit)
		override def toString = unit.toString
	}
	def getImports(unit: BuildUnit) = baseImports ++ importAll(unit.plugins.pluginNames ++ unit.definitions.buildNames)

	def referenced[PR <: ProjectReference](definitions: Seq[ProjectDefinition[PR]]): Seq[PR] = definitions flatMap { _.referenced }
	
	final class BuildStructure(val units: Map[URI, LoadedBuildUnit], val root: URI, val settings: Seq[Setting[_]], val data: Settings[Scope], val index: StructureIndex, val streams: Streams, val delegates: Scope => Seq[Scope], val scopeLocal: ScopeLocal)
	{
		def allProjects: Seq[ResolvedProject] = units.values.flatMap(_.defined.values).toSeq
		def allProjects(build: URI): Seq[ResolvedProject] = units(build).defined.values.toSeq
		def allProjectRefs: Seq[ProjectRef] = units.toSeq flatMap { case (build, unit) => refs(build, unit.defined.values.toSeq) }
		def allProjectRefs(build: URI): Seq[ProjectRef] = refs(build, allProjects(build))
		private[this] def refs(build: URI, projects: Seq[ResolvedProject]): Seq[ProjectRef] = projects.map { p => ProjectRef(build, p.id) }
	}
	final case class LoadBuildConfiguration(stagingDirectory: File, commonPluginClasspath: Seq[Attributed[File]], classpath: Seq[File], loader: ClassLoader, compilers: Compilers, evalPluginDef: (BuildStructure, State) => Seq[Attributed[File]], definesClass: DefinesClass, delegates: LoadedBuild => Scope => Seq[Scope], scopeLocal: ScopeLocal, injectSettings: Seq[Setting[_]], log: Logger)
	// information that is not original, but can be reconstructed from the rest of BuildStructure
	final class StructureIndex(val keyMap: Map[String, AttributeKey[_]], val taskToKey: Map[Task[_], ScopedKey[Task[_]]], val triggers: Triggers[Task], val keyIndex: KeyIndex)
}
