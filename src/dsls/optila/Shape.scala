package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

/**
 * Shape is a type class that represents a structured 2d index space. It can be
 * used to traverse the elements of a matrix in a particular order (e.g. upper triangular).
 * 
 * e.g., to map just the upper triangular elements of a square matrix:
 *   val tri = utriangle(m.numRows)
 *   (0::m.numRows, 0::m.numCols) { (i,j) => if (tri.contains(i,j)) m(i,j) else 0.0 }
 * 
 * to extract all of the upper triangular elements of an N x N matrix into a vector:
 *   val tri = utriangle(m.numRows)
 *   (0::tri.size) { i => val (r,c) = unpack(tri(i)); m(r,c) }
 */
trait ShapeOps {
  this: OptiLADSL =>

  object TShape extends TypeClassSignature {
    def name = "Shape"
    def prefix = "_sh"
    def wrapper = Some("shtype")
  }

  def importShapeOps() {
    importShapeCls()
    importTriangleOps()
  }

  def importShapeCls() {
    val S = tpePar("S")
    
    val Shape = tpeClass("Shape", TShape, S)
    val Tup2 = lookupTpe("Tup2")

    // Shape type class interface

    // test whether a 2d index is contained in this shape
    infix (Shape) ("contains", S, (S, MInt, MInt) :: MBoolean)

    // return a particular 2d index in the shape
    infix (Shape) ("apply", S, (S, MInt) :: Tup2(MInt,MInt))

    // the total number of indices contained in the shape
    infix (Shape) ("size", S, S :: MInt)        
  }    

  def importTriangleOps() {
    val Tup2 = lookupTpe("Tup2")
    val UTri = tpe("UTriangle")
    data(UTri, ("N", MInt), ("_includeDiagonal", MBoolean))

    // create a new upper triangular shape for an N x N square matrix
    direct (UTri) ("utriangle", Nil, MethodSignature(List(("N", MInt), ("includeDiagonal", MBoolean, "unit(true)")), UTri)) implements allocates(UTri, ${$0}, ${$1})

    compiler (UTri) ("tri_size", Nil, ("n", MInt) :: MInt) implements composite ${ n*(n+1)/2 } // arithmetic sequence sum

    val UTriOps = withTpe(UTri)
    UTriOps {
      infix ("N") (Nil :: MInt) implements getter(0, "N")
      infix ("includeDiagonal") (Nil :: MBoolean) implements getter(0, "_includeDiagonal")      

      infix ("size") (Nil :: MInt) implements composite ${
        val total = tri_size($self.N)
        if ($self.includeDiagonal) total else total - 1        
      }

      infix ("contains") ((("i",MInt), ("j",MInt)) :: MBoolean) implements composite ${ 
        if ($self.includeDiagonal) {
          i < $self.N && i <= j
        }
        else {
          i < $self.N && i < j
        } 
      }

      // http://stackoverflow.com/a/244550  
      // O(1), but with non-trivial overhead. We should compare against the explicitly stored version.
      infix ("apply") (("n", MInt) :: Tup2(MInt, MInt)) implements composite ${        
        val m = if ($self.includeDiagonal) $self.N else $self.N-1
        val off = if ($self.includeDiagonal) 0 else 1
        val t = tri_size(m)-1-n
        val k = floor((sqrt(8*t+1)-1)/2)
        val row = m-1-k
        pack((row, (n+tri_size(row))%m+off))
      }      
    }

    // add to Shape class
    val S = tpePar("S")
    val Shape = lookupGrp("Shape").asInstanceOf[Rep[DSLTypeClass]]    
    val UTriShape = tpeClassInst("ShapeUTri", Nil, Shape(UTri))
    infix (UTriShape) ("contains", Nil, (UTri, MInt, MInt) :: MBoolean) implements composite ${ $0.contains($1,$2) }
    infix (UTriShape) ("apply", Nil, (UTri, MInt) :: Tup2(MInt,MInt)) implements composite ${ $0.apply($1) }
    infix (UTriShape) ("size", Nil, UTri :: Tup2(MInt,MInt)) implements composite ${ $0.size }    
  }
}
