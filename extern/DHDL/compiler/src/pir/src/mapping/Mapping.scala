package dhdl.graph.mapping
import dhdl.graph.{ComputeUnit => CU, MemoryController => MC, _}
import dhdl.Design
import dhdl.Config
import dhdl.plasticine.config._
import dhdl.plasticine.graph.{ComputeUnit => PCU, MemoryController => PMC}

import scala.collection.immutable.Set
import scala.collection.immutable.Map

abstract class Mapping[N,R,V](implicit design:Design) {

  var mapping:Map[N, V]
  def reset:Unit = { mapping = null }
  def map:(Boolean, List[Hint])
  def printMap:Unit

  def pln(s:String) = println(s"${design.tab*design.level}${s}")
  def pBS(s:String) = { 
    println(s"${design.tab*design.level}${s}{")
    design.level += 1 
  }
  def pBE = { design.level -= 1; println(s"${design.tab*design.level}}") }

  def simAneal(allRes:List[R], allNodes:List[N], initMap:Map[N,V], 
    constrains:List[(N, R, Map[N, V]) => (Boolean, List[Hint], Map[N, V])]):
  (Boolean, List[Hint], Map[N, V]) = {

    def recMap(usedRes:Set[R], remainNodes:List[N])
    (hints:List[Hint], map:Map[N, V]):
    (Boolean, List[Hint], Map[N, V]) = {
      if (remainNodes.size==0) {
        return (true, hints, map)
      }
      val n::restNodes = remainNodes 

      // Try map n on all unused resource
      allRes.foldLeft((false, hints, map)) { case ((preSuc, preHints, preMap), res) =>
        if (preSuc) // Already find a mapping for cu
          return (true, preHints, preMap)
        if (usedRes.contains(res))
          return (preSuc, preHints, preMap)

        // Checking whether mapping n on res satisfies all constrains 
        val (csuc, chints, cmap) = 
          constrains.foldLeft(true, preHints, preMap) { case ((ps, ph, pm), cons) =>
          if (!ps) //Previous constrain/constrains failed
            return (false, ph, pm)
          // Check current constrain, if success, update mapping
          val (s, h, m) = cons(n, res, pm) 
          (s, ph ++ h, if(s) m else pm)
        }

        if (csuc) // If all constrains pass, recursively map next node 
          recMap(usedRes + res, restNodes) (chints, cmap)
        else // If failed, try next res
          (false, chints, preMap)
      }
    }

    recMap(Set[R](), allNodes) (Nil, initMap)
  }
}

