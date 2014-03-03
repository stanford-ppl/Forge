package LOWERCASE_DSL_NAME.library

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

trait ProfilingWrapper extends HUMAN_DSL_NAMEBase {
  val componentTimes = scala.collection.mutable.HashMap[String,Long]()

  def forge_profile_start(component: Rep[String], deps: Seq[Rep[Any]])(implicit ctx: SourceContext): Rep[Unit] = {
    componentTimes(component) = System.currentTimeMillis()
  }
  def forge_profile_stop(component: Rep[String], deps: Seq[Rep[Any]])(implicit ctx: SourceContext): Rep[Unit] = {
    if (componentTimes.contains(component)) {
      val elapsed = (System.currentTimeMillis() - componentTimes(component))/1000.0
      println("Latest time for component " + component + ": " + elapsed + "s")
    }
    else {
      println("No tic found for component " + component)
    }
  }
  def forge_profile_time(deps: Seq[Rep[Any]])(implicit ctx: SourceContext): Rep[Long] = {
    System.currentTimeMillis()
  }
}





