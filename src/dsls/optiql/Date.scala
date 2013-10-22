package ppl.dsl.forge
package dsls
package optiql

import core.{ForgeApplication,ForgeApplicationRunner}

trait DateOps {
  this: OptiQLDSL =>

  def importDateOps() {

    val Date = tpe("Date")
    data(Date, ("value", MInt))

    static (Date) ("apply", Nil, MInt :: Date) implements allocates (Date, ${$0})

    static (Date) ("apply", Nil, MString :: Date) implements single ${
      val tokens = $0.fsplit("-")
      val year = tokens(0).toInt
      val month = tokens(1).toInt
      val day = tokens(2).toInt
      Date((year << 9) + (month << 5) + day)
    }

    val DateOps = withTpe (Date)
    DateOps {

      compiler ("date_value") (Nil :: MInt) implements getter(0, "value")
      // compiler ("date_year") (Nil :: MInt) implements single ${ date_value($self) >>> 9 }
      // compiler ("date_month") (Nil :: MInt) implements single ${ (date_value($self) >>> 5) & 0xf }
      // compiler ("date_day") (Nil :: MInt) implements single ${ date_value($self) & 0x1f }

      infix ("<") (Date :: MBoolean) implements single ${ date_value($self) < date_value($1) }
      infix ("<=") (Date :: MBoolean) implements single ${ date_value($self) <= date_value($1) }
      infix (">") (Date :: MBoolean) implements single ${ date_value($self) > date_value($1) }
      infix (">=") (Date :: MBoolean) implements single ${ date_value($self) >= date_value($1) }

      // infix ("toString") (Nil :: MString) implements single ${
      //   date_year($self).ToString + "-" + date_month($self).ToString + "-" + date_day($self).ToString
      // }

    }
  }
}
