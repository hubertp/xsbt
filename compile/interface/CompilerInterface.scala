/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import xsbti.{AnalysisCallback,Logger,Problem,Reporter,Controller}
import scala.tools.nsc.{Phase, SubComponent, Settings}
import Log.debug

class CompilerInterface
{
  
 	def run(args: Array[String], callback: AnalysisCallback, log: Logger,
	        delegate: Reporter, controller: Controller)
	{
      // Just provide basic settings with appropriate error function
      run(args, callback, log, delegate, controller, new Settings(Log.settingsError(log)))
	}
  
	def run(args: Array[String], callback: AnalysisCallback, log: Logger,
	        delegate: Reporter, controller: Controller, settings0: Settings)
	{
		import scala.tools.nsc.Global

		debug(log, "Interfacing (CompilerInterface) with Scala compiler " + scala.tools.nsc.Properties.versionString)

		val settings = settings0//new Settings(Log.settingsError(log))
		val command = Command(args.toList, settings)
		val reporter = DelegatingReporter(settings, delegate)
		def noErrors = !reporter.hasErrors && command.ok

		val phasesSet = new scala.collection.mutable.HashSet[Any] // 2.7 compatibility
		object compiler extends Global(command.settings, reporter)
		{
			object dummy // temporary fix for #4426
			object sbtAnalyzer extends
			{
				val global: compiler.type = compiler
				val phaseName = Analyzer.name
				val runsAfter = List("jvm")
				override val runsBefore = List("terminal")
				val runsRightAfter = None
			}
			with SubComponent with Compat27
			{
				val analyzer = new Analyzer(global, callback)
				def newPhase(prev: Phase) = analyzer.newPhase(prev)
				def name = phaseName
			}
			object apiExtractor extends
			{
				val global: compiler.type = compiler
				val phaseName = API.name
				val runsAfter = List("typer")
				override val runsBefore = List("erasure")
				val runsRightAfter = Some("typer")
			}
			with SubComponent with Compat27
			{
				val api = new API(global, callback)
				def newPhase(prev: Phase) = api.newPhase(prev)
				def name = phaseName
			}
			
			override lazy val phaseDescriptors = // done this way for compatibility between 2.7 and 2.8
			{
				phasesSet += sbtAnalyzer
				phasesSet += apiExtractor
				val superd = superComputePhaseDescriptors
				if(superd.contains(sbtAnalyzer))
					superd
				else
				{
					val typerIndex = superd.indexOf(analyzer.typerFactory)
					assert(typerIndex >= 0)
					superd.take(typerIndex+1) ::: apiExtractor :: superd.drop(typerIndex+1) ::: List(sbtAnalyzer)
				}
			}
			private def superComputePhaseDescriptors() = // required because 2.8 makes computePhaseDescriptors private
			{
				val meth = classOf[Global].getDeclaredMethod("computePhaseDescriptors")
				meth.setAccessible(true)
				meth.invoke(this).asInstanceOf[List[SubComponent]]
			}
			trait Compat27 { val runsBefore: List[String] = Nil }
		}
		if(command.shouldStopWithInfo)
		{
			reporter.info(null, command.getInfoMessage(compiler), true)
			throw new InterfaceCompileFailed(args, Array(), "Compiler option supplied that disabled actual compilation.")
		}
		if(noErrors)
		{
      trait Compat28 {
        def informUnitStarting(phase: Phase, unit: compiler.CompilationUnit) {}
      }
			val run = new compiler.Run with Compat28 {
        override def informUnitStarting(phase: Phase, unit: compiler.CompilationUnit) {
          controller.runInformUnitStarting(phase.name, unit.source.path)
        }
        override def progress(current: Int, total: Int) {
          if (!controller.runProgress(current, total))
            cancel
        }
      }
      debug(log, args.mkString("Calling Scala compiler with arguments  (CompilerInterface):\n\t", "\n\t", ""))
      run compile command.files
		}
		reporter.printSummary()
		if(!noErrors)
		{
			debug(log, "Compilation failed (CompilerInterface)")
			throw new InterfaceCompileFailed(args, reporter.problems, "Compilation failed")
		}
	}
}
class InterfaceCompileFailed(val arguments: Array[String], val problems: Array[Problem], override val toString: String) extends xsbti.CompileFailed