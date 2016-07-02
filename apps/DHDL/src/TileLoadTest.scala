import dhdl.compiler._
import dhdl.library._
import dhdl.shared._

object TileLoadCompiler extends DHDLApplicationCompiler with TileLoadTest
trait TileLoadTest extends DHDLApplication {

  lazy val T = param("T", 2)
  lazy val MaxNumEdges = 8.as[Index]

  def main() {
    val NE = args(0).to[SInt]

    val edgeList = OffChipMem[Index](NE*2) // srcs of edges

    Accel {
      val vB = BRAM[Index](T, 2)
      Pipe (T by 1) {iv =>
        val vpt = Reg[Index] // ptr to v's edgelist
        val eB = BRAM[Index](MaxNumEdges) // edge list of v
        Pipe {
          vpt := vB(iv,0)
        }
        Parallel {
          // This is a problem for 2 reasons:
          // 1. Register reads are effectful, hence not CSE'd
          // 2. Register reads are primitive, but there is no control node for them here
          //    And even if one was added, it would appear to be in parallel with the tile load
          eB := edgeList(vpt.value::vpt.value+T)
        }
      }
    }

  }
}
