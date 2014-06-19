package ppl.dsl.forge
package dsls
package optiwrangler

import core.{ForgeApplication,ForgeApplicationRunner}

trait WranglerTableOps {
  this: OptiWranglerDSL =>

  def importWranglerTableOps() {
  	val Table = tpe("Table")
    val Range = lookupTpe("Range")
    val Condition = lookupTpe("Condition")
    val Tuple2 = lookupTpe("Tup2")

    val String = grp("String")
    val IO = grp("IO")

    data(Table, "numCols" -> MInt, "numRows" -> MInt, "data" -> MArray(Tuple2(MString, MArray(MString))))
    static (Table) ("apply", Nil, Nil :: Table, effect = mutable) implements allocates (Table, "unit(0)", "unit(0)", ${array_empty[Tup2[String, ForgeArray[String]]](unit(0))})
    static (Table) ("apply", Nil, (MInt, MInt, MArray(Tuple2(MString, MArray(MString)))) :: Table, effect = mutable) implements allocates (Table, ${$0}, ${$1}, ${$2})
    static (Table) ("fromFile", Nil, (MString, MString ==> MBoolean) :: Table) implements single ${
      val lines = array_filter(ForgeFileReader.readLines($0)(l => l), ((l:Rep[String]) => $1(l)))
      val cols = array_fromfunction(1, (i => pack((unit("data"), lines))))
      Table.apply(1, array_length(lines), cols)
    }

    direct (String) ("splitString", Nil, ("str" -> MString, "delimiter" -> MString, "numOfSplits" -> MInt) :: MArray(MString)) implements single ${
      val arr = array_string_split($str, $delimiter) // TODO: Should actually be val arr = array_string_split($str, $delimiter, $numOfSplits)
      array_fromfunction($numOfSplits, (i => if (i < array_length(arr)) arr(i) else ""))
    }

    direct (IO) ("writeToFile", Nil, (("table", Table),("path",MString)) :: MUnit, effect = simple) implements composite ${
      val data = table_data($table)
      val headers = array_reduce(array_map(data, ((tpl:Rep[Tup2[String, ForgeArray[String]]]) => tpl._1)), ((a:Rep[String],b:Rep[String]) => a + "," + b), "")
      val cols = array_map(data, ((tpl:Rep[Tup2[String, ForgeArray[String]]]) => tpl._2))
      val rows = array_fromfunction(table_num_rows($table), (r => {
        val arr = array_fromfunction(table_num_cols($table), (c => {
          val col = cols(c)
          col(r)
          }))
        array_reduce(arr, ((a:Rep[String],b:Rep[String]) => a + "," + b), "")
      }))

      writeToFileHelper($path, headers, rows, table_num_rows($table)) 
    }

    compiler (IO) ("writeToFileHelper", Nil, (("path",MString), ("headers", MString), ("rows", MArray(MString)), ("numRows", MInt)) :: MUnit, effect = simple) implements codegen($cala, ${
      val xfs = new java.io.BufferedWriter(new java.io.FileWriter($path))
      xfs.write($headers + "\\n" + "\\n")
      for (i <- 0 until $numRows) {
        xfs.write($rows(i) + "\\n")
      }

      xfs.close()
    })
    
  	val WranglerTableOps = withTpe (Table)
  	WranglerTableOps {
      infix ("Split") (MethodSignature(List(("col", MString),("delimiter", MString, "unit(\",\")"),("numOfSplits", MInt, "unit(1)")), Table)) implements composite ${
        val origCol = get_column_by_name($self, $col)
        val splitElems = array_map(origCol, ((s:Rep[String]) => splitString(s, $delimiter, $numOfSplits)))

        val data = table_data($self)
        val numCurrCols = table_num_cols($self)
        val numColsInNewTable = numCurrCols + $numOfSplits
        val numOfPastSplits = get_num_past_ops($self, "split")
        val newData = array_fromfunction(numColsInNewTable, c => {
          if (c < numCurrCols) data(c)
          else {
            val i = c - numCurrCols
            val tmp = numOfPastSplits + i
            val colName = "split" + tmp
            tuple($self, colName, array_map(splitElems, ((r:Rep[ForgeArray[String]]) => r(i))))
          }
        })

        Table(numColsInNewTable, table_num_rows($self), newData)
      }

      infix ("Translate") (MethodSignature(List(("direction", MString, "unit(\"left\")")), Table)) implements composite ${
        val numCurrCols = table_num_cols($self)
        val numCurrRows = table_num_rows($self)
        val newData = array_fromfunction(numCurrCols, {c => 
          val newCol = array_fromfunction(numCurrRows, ((r:Rep[Int]) => get_translate_value($self, c, r, $direction)))
          tuple($self, get_col_name($self, c), newCol)
        })

        Table(numCurrCols, numCurrRows, newData)
      }

      infix ("Fill") (MethodSignature(List(("direction", MString, "unit(\"down\")")), Table)) implements composite ${
        val numCurrCols = table_num_cols($self)
        val numCurrRows = table_num_rows($self)
        val newData = array_fromfunction(numCurrCols, {c => 
          val newCol = array_fromfunction(numCurrRows, ((r:Rep[Int]) => get_fill_value($self, c, r, $direction)))
          tuple($self, get_col_name($self, c), newCol)
        })

        Table(numCurrCols, numCurrRows, newData)
      }

      infix ("Drop") (("colName" -> MString) :: Table) implements composite ${
        val colIndex = get_col_index($self, $colName)
        val data = table_data($self)
        val numCurrCols = table_num_cols($self)
        val newData = array_fromfunction(numCurrCols - 1, {c =>
          if (c < colIndex) tuple($self, data(c)._1, data(c)._2)
          else tuple($self, data(c + 1)._1, data(c + 1)._2)
        })

        Table(numCurrCols - 1, table_num_rows($self), newData)
      }

      infix ("SetName") (("currColName" -> MString, "newColName" -> MString) :: Table) implements composite ${
        val colIndex = get_col_index($self, $currColName)
        val data = table_data($self)
        val newData = array_fromfunction(table_num_cols($self), {c => 
          if (c == colIndex) tuple($self, $newColName, data(c)._2)
          else data(c)
        })

        Table(table_num_cols($self), table_num_rows($self), newData)
      }

      infix ("Filter") (("cond" -> Condition) :: Table) implements composite ${
        val startsWithColIndex = get_col_index($self, get_col_startswith($cond))
        val containsColIndex = get_col_index($self, get_col_contains($cond))
        val startsWithValue = get_value_startswith($cond)
        val containsValue = get_value_contains($cond)
        val rowIndices = get_row_indices($cond)
        
        val b = (array_length(rowIndices) == 0)
        val rowWiseTestResults = array_fromfunction(table_num_rows($self), { r => 
          (b || array_contains_int($self, rowIndices, r)) && test_row($self, r, startsWithColIndex, startsWithValue, containsColIndex, containsValue)
        })

        val arr1 = array_fromfunction(table_num_rows($self), (r => r)) // create an arr of rowIndices from [0... (numCurrRows - 1)]
        val indicesOfFilteredRows = array_filter(arr1, ((r:Rep[Int]) => !rowWiseTestResults(r))) // filter out (ie, remove) the indices of those rows that failed the filter test
        val currData = table_data($self)
        val newData = array_fromfunction(table_num_cols($self), {c =>
          val tpl = currData(c)
          val currCol = tpl._2
          tuple($self, tpl._1, array_map(indicesOfFilteredRows, ((r:Rep[Int]) => currCol(r))))
        })

        val numRowsInNewTable = array_length(indicesOfFilteredRows)
        Table(table_num_cols($self), numRowsInNewTable, newData)
      }

      infix ("Transpose") (MethodSignature(List(("incCurrHeadersAsCol", MBoolean, "unit(true)")), Table)) implements composite ${
        val numOfColsInNewTable = table_num_rows($self)
        val newData = array_fromfunction(numOfColsInNewTable, {r => 
          val colName = if (r == 0) "transpose" else "transpose" + r
          tuple($self, colName, transpose_row($self, r))
        })

        Table(numOfColsInNewTable, table_num_rows($self), newData)
      }

      infix ("Merge") (MethodSignature(List(("colNames", MArray(MString)), ("delimiter", MString, "unit(\" \")")), Table)) implements composite ${
        val numCurrCols = table_num_cols($self)
        val colIndices = array_map($colNames, ((n:Rep[String]) => get_col_index($self, n)))
        val data = table_data($self)
        val numOfPastMerges = get_num_past_ops($self, "merge")
        val newData = array_fromfunction(numCurrCols + 1, {c => 
          if (c < numCurrCols) data(c)
          else {
            val colName = if (numOfPastMerges == 0) "merge" else ("merge" + numOfPastMerges)
            tuple($self, colName, get_merged_col($self, colIndices, $delimiter))
          }
        })

        Table(numCurrCols + 1, table_num_rows($self), newData)
      }

      // ASSUMPTIONS:
      //  i. Key-rows are adjacent and are the first rows in the table. This should be changed. Any set of rows could be the key rows
      //  ii. If ($numOfKeyRows == 0) the set of column headers is used as the key row. Else, the headers are ignored.
      infix ("Fold") (MethodSignature(List(("selectedColNames", MArray(MString)), ("numOfKeyRows", MInt, "unit(0)")), Table)) implements composite ${
        val numSelCols = array_length($selectedColNames)
        val numSelRows = if ($numOfKeyRows > 0) $numOfKeyRows else 1
        val numCurrCols = table_num_cols($self)
        val numCurrRows = table_num_rows($self)
        val numColsInNewTable = numSelRows + 1 + (numCurrCols - numSelCols)
        val numRowsInNewTable = (numCurrRows - $numOfKeyRows) * numSelCols

        // Generating the portion of each 'selected col' that would keep repeating
        val keyPortionsOfSelCols = array_map($selectedColNames, (colName:Rep[String]) => {
          val col = get_column_by_name($self, colName)
          array_take(col, $numOfKeyRows)
        })

        // Generating the "fold*" columns
        val numOfPastFolds = get_num_past_ops($self, "fold")
        val foldCols = if ($numOfKeyRows > 0) {
          array_fromfunction(numSelRows, { r => 
            val pattern = array_fromfunction(numSelCols, (c => array_apply(keyPortionsOfSelCols(c), r)))
            val col = generate_col_by_array_duplication($self, pattern, (numRowsInNewTable / numSelCols))
            val name = "fold" + (numOfPastFolds + r)
            tuple($self, name, col)
          })
        } else {
          array_fromfunction(1, {r => 
            val col = generate_col_by_array_duplication($self, $selectedColNames, (numRowsInNewTable / numSelCols))
            val name = "fold" + (numOfPastFolds + r)
            tuple($self, name, col)
          })
        }
        
        // Generating the "value" column
        val selColArrs = array_map($selectedColNames, ((n:Rep[String]) => get_column_by_name($self, n)))
        val valueCol = array_fromfunction(numRowsInNewTable, {r => array_apply(selColArrs(r % numSelCols), (r / numSelCols) + $numOfKeyRows)})

        // Generating the expanded versions of the non-selected columns
        val nonSelectedColIndices = array_filter(array_fromfunction(numCurrCols, (i => i)), {(c:Rep[Int]) => 
          val n = get_col_name($self, c)
          !array_contains_str($self, $selectedColNames, n)
        })
        val data = table_data($self)
        val expandedNonSelCols = array_map(nonSelectedColIndices, {(c:Rep[Int]) => 
          val arr = expand_col_by_elem_duplication($self, data(c)._2, $numOfKeyRows, numSelCols)
          tuple($self, get_col_name($self, c), arr)
        })

        val newData = array_fromfunction(numColsInNewTable, {c =>
          if (c < numSelRows) foldCols(c)
          else if (c == numSelRows) tuple($self, "value", valueCol)
          else expandedNonSelCols(c - numSelRows - 1)
        })

        Table(numColsInNewTable, numRowsInNewTable, newData)
      }

      // DEPRECATED: This implementation uses a hash map which is not thread safe. Need to rewrite the function using groupBy.
      // ASSUMPTIONS:
      //   i. None of the values in the header col are equal to the name of any col in the current table
      /*
      infix ("Unfold") (("headerColName" -> MString, "measureColName" -> MString) :: Table) implements composite ${
        // Find the total number of distinct values in the header column
        val headerValSet = FHashMap[String, Int]()
        val headerCol = get_column_by_name($self, $headerColName)
        Range(0, table_num_rows($self)) foreach (r => headerValSet update (headerCol(r), 0))
        val headerVals = headerValSet keys
        val numHeaderVals = array_length(headerVals)
        val numKeyCols = table_num_cols($self) - 2
        Range(0, numHeaderVals) foreach (i => headerValSet update (headerVals(i), numKeyCols + i))

        // Get the indices of the key columns
        val headerColIndex = get_col_index($self, $headerColName)
        val measureColIndex = get_col_index($self, $measureColName)
        val currColIndices = array_fromfunction(table_num_cols($self), (i => i))
        val keyColsIndices = array_filter(currColIndices, ((i:Rep[Int]) => (i != headerColIndex) && (i != measureColIndex)))

        // Get the set of distinct keys
        val keyToRowIndexInCurrTable = FHashMap[String, Int]()
        Range(0, table_num_rows($self)) foreach (r => {
          //val key = concatColValues(keyColsIndices, r, ",") 
          val key = concatenate_row_elems($self, r, keyColsIndices, ",")
          keyToRowIndexInCurrTable update (key, r) // TODO: Could result in race conditions. Is FHashMap thread safe?
        })
        val reductionKeys = keyToRowIndexInCurrTable keys
        val numKeys = array_length(reductionKeys)

        var keyIndex = 0
        val keyToRowIndexInNewTable = FHashMap[String, Int]()
        Range(0, numKeys) foreach (i => {
          keyToRowIndexInNewTable update (reductionKeys(i), keyIndex)
          keyIndex = keyIndex + 1
        })

        // Create the new table
        val data = table_data($self)
        val numColsInNewTable = numHeaderVals + numKeyCols
        val numRowsInNewTable = numKeys
        val keyRowIndices = array_map(reductionKeys, ((k:Rep[String]) => keyToRowIndexInCurrTable(k)))
        val newData = array_fromfunction(numColsInNewTable, {c => 
          val isKeyCol = c < numKeyCols
          val colName = if (isKeyCol) tup2__1(data(keyColsIndices(c))) else headerVals(c - numKeyCols)
          val col = if (isKeyCol) get_elems($self, tup2__2(data(keyColsIndices(c))), keyRowIndices) else array_empty[String](numRowsInNewTable)
          tuple($self, colName, col)
        })

        val measureCol = get_column($self, measureColIndex)
        Range(0, table_num_rows($self)) foreach (r => {
          val key = concatenate_row_elems($self, r, keyColsIndices, ",")
          val i = keyToRowIndexInNewTable(key) // max row-index of the key in the current table

          // Enter the value of the measure col (in curr table) in the appropriate cell in the new table
          val valOfHeaderCol = headerCol(r)
          val colIndexInNewTable = headerValSet(valOfHeaderCol)
          val colInNewTable = get_column_from_arr($self, newData, colIndexInNewTable)
          array_update(colInNewTable, i, measureCol(r))
        })

        Table(numColsInNewTable, numRowsInNewTable, newData)
      }
      */

      infix ("printTable") (Nil :: MString) implements single ${
        val data = table_data($self)
        val numOfRows = array_length(data(0)._2)
        val rowRange = Range(0, numOfRows)
        val colRange = Range(0, table_num_cols($self))

        var outString = ""
        rowRange foreach (r => {
          colRange foreach (c => outString = outString + "," + get_cell($self, c, r))
          outString = outString + "\\n"
        })

        outString
      }

      compiler ("get_num_past_ops") (("op" -> MString) :: MInt) implements composite ${
        val a = array_filter(get_col_headers($self), ((s:Rep[String]) => s.startsWith($op)))
        array_length(a)
      }

      compiler ("get_cell") ((MInt, MInt) :: MString) implements composite ${
        val data = table_data($self)
        val s = Option(array_apply(tup2__2(data($1)), $2))
        if (s.isEmpty) ""
        else s.get
      }

      compiler ("set_cell") ((MInt, MInt, MString) :: MUnit, effect = write(0)) implements single ${
        val data = table_data($self)
        val col = tup2__2(array_apply(data, $1))
        array_update(col, $2, $3)
      } 

    	//getter and setter functions
    	compiler ("table_data") (Nil :: MArray(Tuple2(MString, MArray(MString)))) implements getter(0, "data")
    	compiler ("table_num_cols") (Nil :: MInt) implements getter(0, "numCols")
      compiler ("table_num_rows") (Nil :: MInt) implements getter(0, "numRows")

  		compiler ("get_column_by_name") (MString :: MArray(MString)) implements composite ${
        val data = table_data($self)
        val filtered_arr = array_filter(data, ((tpl:Rep[Tup2[String, ForgeArray[String]]]) => tup2__1(tpl) == $1))
        filtered_arr(0)._2
  		}

      compiler ("empty_data") (MInt :: MArray(Tuple2(MString, MArray(MString)))) implements composite ${
        array_empty[Tup2[String, ForgeArray[String]]]($1)
      }

      compiler ("get_column") (MInt :: MArray(MString)) implements composite ${
        val data = table_data($self)
        tup2__2(data($1))
      }

      compiler ("get_column_from_arr") ((MArray(Tuple2(MString, MArray(MString))), MInt) :: MArray(MString)) implements composite ${
        tup2__2($1($2))
      }

      compiler ("get_translate_value") (("col" -> MInt, "row" -> MInt, "direction" -> MString) :: MString) implements composite ${
        val data = table_data($self)
        val shift = if ($direction == "up" || $direction == "left") 1 else { -1 }
        if ($direction == "up" || $direction == "down") {
          val r = $row + shift
          if ((r < 0) || (r >= table_num_rows($self))) "" else get_cell($self, $col, r)
        } else {
          val c = $col + shift
          if ((c < 0) || (c >= table_num_cols($self))) "" else get_cell($self, c, $row)
        }
      }

      compiler ("get_fill_value") (("col" -> MInt, "row" -> MInt, "direction" -> MString) :: MString) implements composite ${
        val data = table_data($self)
        val currValue = get_cell($self, col, row)
        if (currValue == "") {
          var fillValue = ""
          val shift = if ($direction == "up" || $direction == "left") { -1 } else 1
          if ($direction == "up" || $direction == "down") {
            var r = $row + shift
            while ((r >= 0) && (r < table_num_rows($self)) && (fillValue == "")) {
              fillValue= get_cell($self, $col, r)
              r = $row + shift
            }
          } else {
            var c = $col + shift
            while ((c >= 0) && (c < table_num_cols($self)) && (fillValue == "")) {
              fillValue= get_cell($self, c, $row)
              c = $col + shift
            }
          }

          val ret = fillValue
          ret
        } else currValue
      }

      compiler ("transpose_row") (MInt :: MArray(MString)) implements single ${
        array_fromfunction(table_num_cols($self), (c => get_cell($self, c, $1)))
      }

      compiler ("get_col_headers") (Nil :: MArray(MString)) implements composite ${
        val data = table_data($self)
        array_fromfunction(table_num_cols($self), (c => tup2__1(data(c))))
      }

      compiler ("get_merged_col") (("colIndices" -> MArray(MInt), "delimiter" -> MString) :: MArray(MString)) implements composite ${
        array_fromfunction(table_num_rows($self), (r => concatenate_row_elems($self, r, $colIndices, $delimiter)))
      }

      compiler ("concatenate_row_elems") (("rowIndex" -> MInt, "colIndices" -> MArray(MInt), "delimiter" -> MString) :: MString) implements composite ${
        val cellVals = array_map($colIndices, ((c:Rep[Int]) => get_cell($self, c, $rowIndex)))
        array_reduce(cellVals, ((a:Rep[String], b:Rep[String]) => a + $delimiter + b), "")
      }

      // If this function returns 'true' for a given row, that row is deleted from the table
      compiler ("test_row") (("rowIndex" -> MInt, "startsWithColIndex" -> MInt, "startsWithValue" -> MString, "containsColIndex" -> MInt, "containsValue" -> MString) :: MBoolean) implements composite ${
        //val b1 = (get_cell($self, $startsWithColIndex, $rowIndex)).startsWith($startsWithValue)
        //val b2 = (get_cell($self, $containsColIndex, $rowIndex)).contains($containsValue)
        //b1 && b2 && (is_row_empty($self, $rowIndex))

        is_row_empty($self, $rowIndex)
      }

      compiler ("is_row_empty") (("rowIndex" -> MInt) :: MBoolean) implements composite ${
        /*
        val data = table_data($self)
        val numCols = table_num_cols($self)
        val arr = array_empty[Int](numCols)
        Range(0, numCols) foreach (i => array_update(arr, i, i))
        val isCellEmpty = array_map(arr, (i => get_cell($self, i, $rowIndex) == ""))
        */
        val isCellEmptyFlags = array_fromfunction(table_num_cols($self), (i => (get_cell($self, i, $rowIndex) == "")))
        array_reduce(isCellEmptyFlags, ((b1:Rep[Boolean], b2:Rep[Boolean]) => b1 && b2), true)
      }

      compiler ("expand_col_by_elem_duplication") (("col" -> MArray(MString), "numOfRowsToExcludeFromStart" -> MInt, "duplicationFactor" -> MInt) :: MArray(MString)) implements composite ${
        val lengthOfExpandedCol = (array_length($col) - $numOfRowsToExcludeFromStart) * $duplicationFactor
        array_fromfunction(lengthOfExpandedCol, (i => $col((i / $duplicationFactor) + $numOfRowsToExcludeFromStart)))
      }

      compiler ("generate_col_by_array_duplication") (("pattern" -> MArray(MString), "duplicationFactor" -> MInt) :: MArray(MString)) implements composite ${
        val patternLength = array_length($pattern)
        val lengthOfNewCol = patternLength * $duplicationFactor
        array_fromfunction(lengthOfNewCol, (i => $pattern(i % patternLength)))
      }

      // Given an array of strings, creates a sub-array of elements at the given indices
      compiler ("get_elems") ((MArray(MString), MArray(MInt)) :: MArray(MString)) implements composite ${
        val numElemsInRes = array_length($2) 
        array_fromfunction(numElemsInRes, (i => $1($2(i))))
      }

      compiler ("tuple") ((MString, MArray(MString)) :: Tuple2(MString, MArray(MString))) implements composite ${
        pack(($1, $2))
      }

      compiler ("get_col_index") (MString :: MInt) implements composite ${
        var index = -1
        val data = table_data($self)
        Range(0, table_num_cols($self)) foreach (c => if (data(c)._1 == $1) index = c)

        index
      }

      compiler ("get_col_name") (MInt :: MString) implements composite ${
        val data = table_data($self)
        data($1)._1
      }

      // TODO: Combine these two functions into one generic function.
      compiler ("array_contains_int") ((MArray(MInt), MInt) :: MBoolean) implements composite ${
        val a = array_filter($1, ((e:Rep[Int]) => e == $2))
        array_length(a) > 0
      }

      compiler ("array_contains_str") ((MArray(MString), MString) :: MBoolean) implements composite ${
        val a = array_filter($1, ((e:Rep[String]) => e == $2))
        array_length(a) > 0
      }
  	}
  }
}
