
.. role:: black
.. role:: gray
.. role:: silver
.. role:: white
.. role:: maroon
.. role:: red
.. role:: fuchsia
.. role:: pink
.. role:: orange
.. role:: yellow
.. role:: lime
.. role:: green
.. role:: olive
.. role:: teal
.. role:: cyan
.. role:: aqua
.. role:: blue
.. role:: navy
.. role:: purple

.. _BRAM:

BRAM
====


BRAMs are on-chip scratchpads with fixed size. BRAMs can be specified as multi-dimensional, but the underlying addressing
in hardware is always flat. The contents of BRAMs are currently persistent across loop iterations, even when they are declared in an inner scope.
BRAMs can have an arbitrary number of readers but only one writer. This writer may be an element-based store or a load from an OffChipMem.


Static methods
--------------

.. parsed-literal::

  :maroon:`def` apply(name: String, dims: :doc:`Index <fixpt>`\*)(:maroon:`implicit` ev0: Num[T]): :doc:`bram`\[T\]

Creates a BRAM with given name and dimensions. Dimensions must be statically known signed integers (constants or parameters). 


*********

.. parsed-literal::

  :maroon:`def` apply(dims: :doc:`Index <fixpt>`\*)(:maroon:`implicit` ev0: Num[T]): :doc:`bram`\[T\]

Creates an unnamed BRAM with given dimensions. Dimensions must be statically known signed integers (constants or parameters). 


Infix methods
-------------

.. parsed-literal::

  :maroon:`def` :=(tile: :doc:`tile`\[T\]): Unit

Creates a tile store from a Tile of an OffChipMem to this BRAM. 


*********

.. parsed-literal::

  :maroon:`def` apply(ii: :doc:`Index <fixpt>`\*): T

Creates a read from this BRAM at the given multi-dimensional address. Number of indices given can either be 1 or the same as the number of dimensions that the BRAM was declared with. 

	* **ii** \- multi-dimensional address

*********

.. parsed-literal::

  :maroon:`def` update(i: :doc:`Index <fixpt>`, x: T): Unit

Creates a write to this BRAM at the given 1D address. 

	* **i** \- 1D address
	* **x** \- element to be stored to BRAM

*********

.. parsed-literal::

  :maroon:`def` update(i: :doc:`Index <fixpt>`, j: :doc:`Index <fixpt>`, x: T): Unit

Creates a write to this BRAM at the given 2D address. The BRAM must have initially been declared as 2D. 

	* **i** \- row index
	* **j** \- column index
	* **x** \- element to be stored to BRAM

*********

.. parsed-literal::

  :maroon:`def` update(i: :doc:`Index <fixpt>`, j: :doc:`Index <fixpt>`, k: :doc:`Index <fixpt>`, x: T): Unit

Creates a write to this BRAM at the given 3D address. The BRAM must have initially been declared as 3D. 

	* **i** \- row index
	* **j** \- column index
	* **k** \- page index
	* **x** \- element to be stored to BRAM

*********

.. parsed-literal::

  :maroon:`def` update(y: Seq\[:doc:`Index <fixpt>`\], z: T): Unit




