
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

.. _OffChipMem:

OffChipMem
==========


OffChipMems are pointers to locations in the accelerators main memory to dense multi-dimensional arrays. They are the primary form of communication
of data between the host and the accelerator. Data may be loaded to and from the accelerator in contiguous chunks (Tiles). Other access patterns
will be supported soon!


Static methods
--------------

.. parsed-literal::

  :maroon:`def` apply(name: String, dims: :doc:`Index <fixpt>`\*)(:maroon:`implicit` ev0: Num[T]): :doc:`offchipmem`\[T\]

Creates a reference to a multi-dimensional array in main memory with given name and dimensions 


*********

.. parsed-literal::

  :maroon:`def` apply(dims: :doc:`Index <fixpt>`\*)(:maroon:`implicit` ev0: Num[T]): :doc:`offchipmem`\[T\]

Creates a reference to an unnamed multi-dimensional array in main memory with given dimensions 


Infix methods
-------------

.. parsed-literal::

  :maroon:`def` apply(cols: Range): :doc:`tile`\[T\]

Creates a reference to a 1D Tile of this 1D OffChipMem which can be loaded into on-chip BRAM. 


*********

.. parsed-literal::

  :maroon:`def` apply(rows: Range, cols: Range): :doc:`tile`\[T\]

Creates a reference to a 2D Tile of this 2D OffChipMem which can be loaded into on-chip BRAM. 


*********

.. parsed-literal::

  :maroon:`def` apply(rows: Range, cols: Range, pages: Range): :doc:`tile`\[T\]

Creates a reference to a 3D Tile of this 3D OffChipMem which can be loaded into on-chip BRAM. 


*********

.. parsed-literal::

  :maroon:`def` apply(row: :doc:`Index <fixpt>`, cols: Range): :doc:`tile`\[T\]

Creates a reference to a 1D row Tile of this 2D OffChipMem 


*********

.. parsed-literal::

  :maroon:`def` apply(rows: Range, col: :doc:`Index <fixpt>`): :doc:`tile`\[T\]

Creates a reference to a 1D column Tile of this 2D OffChipMem 


*********

.. parsed-literal::

  :maroon:`def` apply(row: :doc:`Index <fixpt>`, cols: Range, pages: Range): :doc:`tile`\[T\]

Creates a reference to a 2D column/page Tile of this 3D OffChipMem 


*********

.. parsed-literal::

  :maroon:`def` apply(rows: Range, col: :doc:`Index <fixpt>`, pages: Range): :doc:`tile`\[T\]

Creates a reference to a 2D row/page Tile of this 3D OffChipMem 


*********

.. parsed-literal::

  :maroon:`def` apply(rows: Range, cols: Range, page: :doc:`Index <fixpt>`): :doc:`tile`\[T\]

Creates a reference to a 2D row/column Tile of this 3D OffChipMem 


*********

.. parsed-literal::

  :maroon:`def` apply(row: :doc:`Index <fixpt>`, col: :doc:`Index <fixpt>`, pages: Range): :doc:`tile`\[T\]

Creates a reference to a 1D page Tile of this 3D OffChipMem 


*********

.. parsed-literal::

  :maroon:`def` apply(row: :doc:`Index <fixpt>`, cols: Range, page: :doc:`Index <fixpt>`): :doc:`tile`\[T\]

Creates a reference to a 1D column Tile of this 3D OffChipMem 


*********

.. parsed-literal::

  :maroon:`def` apply(rows: Range, col: :doc:`Index <fixpt>`, page: :doc:`Index <fixpt>`): :doc:`tile`\[T\]

Creates a reference to a 1D row Tile of this 3D OffChipMem 


