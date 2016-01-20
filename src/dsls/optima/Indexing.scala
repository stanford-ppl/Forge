package ppl.dsl.forge
package dsls
package optima

import core.{ForgeApplication,ForgeApplicationRunner}

// Index math
// TODO: Probably want to delete/move to primitives later?
trait IndexingOps { this: OptiMADSL =>

  def importIndexingOps() {
    val Prim = grp("Primitive")
    val T = tpePar("T", stage=compile)

    val not = infix (Prim) ("unary_!", Nil, MBoolean :: MBoolean)
    val int_plus = direct (Prim) ("forge_int_plus", Nil, (MInt,MInt) :: MInt)
    val int_minus = direct (Prim) ("forge_int_minus", Nil, (MInt,MInt) :: MInt)
    val int_times = direct (Prim) ("forge_int_times", Nil, (MInt,MInt) :: MInt)
    val int_divide = direct (Prim) ("forge_int_divide", Nil, (MInt,MInt) :: MInt)
    for (g <- List($cala, cuda, cpp)) {
      impl (not) (codegen(g, "!" + quotedArg(0) ))
      impl (int_plus) (codegen(g, ${$0 + $1}))
      impl (int_minus) (codegen(g, ${$0 - $1}))
      impl (int_times) (codegen(g, ${$0 * $1}))
      impl (int_divide) (codegen(g, ${$0 / $1}))
    }

    infix (Prim) ("unary_-", Nil, MInt :: MInt) implements redirect ${ unit(-1)*$0 }

    // Workaround for java class cast exception inside numeric while unboxing java.lang.Integer
    // TBD: Is this still needed in the case where we have Exp[Int] rather than Exp[T]?
    val Num = tpe("java.lang.Number", stage=compile)
    compiler (Prim) ("unbox", T, Num :: T) implements composite ${
      val mD = manifest[Double]
      val mF = manifest[Float]
      val mI = manifest[Int]
      val mL = manifest[Long]
      manifest[T] match {
        case `mD` => $0.doubleValue().asInstanceOf[T]
        case `mF` => $0.floatValue().asInstanceOf[T]
        case `mI` => $0.intValue().asInstanceOf[T]
        case `mL` => $0.longValue().asInstanceOf[T]
      }
    }
    // This method is somewhat dangerous - use only if you know what you're doing!
    compiler (Prim) ("extract", T, IAny :: T) implements composite ${
      if ($0.isInstanceOf[java.lang.Number]) unbox[T]($0.asInstanceOf[java.lang.Number])
      else $0.asInstanceOf[T]
    }

    // TODO: The syntax for these rules is still a bit ugly...
    rewrite (int_plus) using pattern((${Const(x)}, ${Const(y)}) -> ${ unit(extract[Int](x) + extract[Int](y)) })
    rewrite (int_plus) using commutative((${Const(0)}, ${x}) -> ${x})

    rewrite (int_minus) using pattern((${Const(x)}, ${Const(y)}) -> ${ unit(extract[Int](x) - extract[Int](y)) })
    rewrite (int_minus) using pattern((${x}, ${Const(0)}) -> ${x})   // x - 0 => x

    rewrite (int_times) using pattern((${Const(x)}, ${Const(y)}) -> ${ unit(extract[Int](x) * extract[Int](y)) })
    rewrite (int_times) using commutative((${Const(1)}, ${x}) -> ${x})
    rewrite (int_times) using commutative((${Const(0)}, ${x}) -> ${unit(0)})

    infix (Prim) ("+", Nil, (MInt,MInt) :: MInt) implements redirect ${ forge_int_plus($0, $1) }
    infix (Prim) ("-", Nil, (MInt,MInt) :: MInt) implements redirect ${ forge_int_minus($0, $1) }
    infix (Prim) ("*", Nil, (MInt,MInt) :: MInt) implements redirect ${ forge_int_times($0, $1) }
    infix (Prim) ("/", Nil, (MInt,MInt) :: MInt) implements redirect ${ forge_int_divide($0, $1) }

    lift (Prim) (MBoolean)
    lift (Prim) (MInt)
  }
}