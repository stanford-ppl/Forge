OptiWrangler
============

A DSL for data wrangling. Can be used to convert unstructured data to a structured format through a series of transforms.

Supported Transforms
====================

Drop
------------

- Delete a column
- Method signature: Drop($colName)
		
Fill	
------------

- Fill each empty cell in the table with the value of the nearest non-empty neighbor cell in the direction given as input
- Method signature: Fill($direction)
- Default values for input parameters:
	- $direction = "down"

Filter
------------

- Filter rows based on the specified condition
  NOTE: Currently, the transform only supports the filtering of empty rows. 
        It would be updated to support filtering based on regex patterns
- Method signature: Filter(Condition.apply)

Fold
------------

- Method signature: Fold($selectedColNames, $numOfKeyRows)
- Default values for input parameters:
	- $numOfKeyRows = 0

Merge
------------

- Merge multiple columns to form a new column
- Method signature: Merge($colNames, $delimiter)
- Default values for input parameters:
	- $delimiter = " "

SetName 
------------

- Rename a column
- Method signature: SetName($currColName, $newColName)

Split
------------

- Split a column into a specified number of columns based on a delimiter.
- Method signature: Split($col, $delimiter, $numOfSplits)
- Default values for input parameters:
	- $delimiter = ","
	- $numOfSplits = 1

Translate 
------------

- Shift values of cells in the direction specified as input
- Method signature: Translate($direction)
- Default values for input parameters:
	- $direction = "left"

Transpose 
------------

- Transpose the input table. Rows would be turned to columns and vice versa
- Method signature: Transpose()

Sample code
=============

Example apps that make use of optiwrangler can be found in the apps/optiwrangler/src folder.
