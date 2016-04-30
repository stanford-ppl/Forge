
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

.. _FixPt:

FixPt
=====

FixPt[S,I,F] represents an arbitrary precision fixed point representation.
FixPt values may be signed or unsigned. Negative values, if applicable, are represented
in twos complement.

The type parameters for FixPt are:

+---+-----------------------------------+-----------------+
| S | Signed or unsigned representation | (Signed/Unsign) |
+---+-----------------------------------+-----------------+
| I | Number of integer bits            | (B0 - B64)      |
+---+-----------------------------------+-----------------+
| F | Number of fractional bits         | (B0 - B64)      |
+---+-----------------------------------+-----------------+

Note that numbers of bits use the B- prefix as integers cannot be used as type parameters in Scala


Type Aliases
------------

+----------+-------+-------------------------------+----------------------------------+
| **type** | SInt  | :doc:`fixpt`\[Signed,B32,B0\] | Signed 32 bit integer            |
+----------+-------+-------------------------------+----------------------------------+
| **type** | Index | :doc:`fixpt`\[Signed,B32,B0\] | Signed 32 bit integer (indexing) |
+----------+-------+-------------------------------+----------------------------------+
| **type** | UInt  | :doc:`fixpt`\[Unsign,B32,B0\] | Unsigned 32 bit integer          |
+----------+-------+-------------------------------+----------------------------------+


Infix methods
-------------

.. parsed-literal::

  :maroon:`def` !=(y: :doc:`fixpt`\[S,I,F\]): :doc:`bit`




*********

.. parsed-literal::

  :maroon:`def` !=(y: Int): :doc:`bit`




*********

.. parsed-literal::

  :maroon:`def` !=(y: Long): :doc:`bit`




*********

.. parsed-literal::

  :maroon:`def` !=(y: Float): :doc:`bit`




*********

.. parsed-literal::

  :maroon:`def` !=(y: Double): :doc:`bit`




*********

.. parsed-literal::

  :maroon:`def` %(y: :doc:`fixpt`\[S,I,B0\]): :doc:`fixpt`\[S,I,B0\]




*********

.. parsed-literal::

  :maroon:`def` %(y: Int): :doc:`fixpt`\[S,I,B0\]




*********

.. parsed-literal::

  :maroon:`def` %(y: Long): :doc:`fixpt`\[S,I,B0\]




*********

.. parsed-literal::

  :maroon:`def` %(y: Float): :doc:`fixpt`\[S,I,B0\]




*********

.. parsed-literal::

  :maroon:`def` %(y: Double): :doc:`fixpt`\[S,I,B0\]




*********

.. parsed-literal::

  :maroon:`def` &(y: :doc:`fixpt`\[S,I,F\]): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` &(y: Int): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` &(y: Long): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` &(y: Float): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` &(y: Double): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` \*(y: :doc:`fixpt`\[S,I,F\]): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` \*(y: Int): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` \*(y: Long): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` \*(y: Float): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` \*(y: Double): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` \*\*(y: Int): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` +(y: :doc:`fixpt`\[S,I,F\]): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` +(y: Int): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` +(y: Long): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` +(y: Float): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` +(y: Double): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` -(y: :doc:`fixpt`\[S,I,F\]): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` -(y: Int): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` -(y: Long): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` -(y: Float): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` -(y: Double): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` \/(y: :doc:`fixpt`\[S,I,F\]): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` \/(y: Int): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` \/(y: Long): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` \/(y: Float): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` \/(y: Double): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` ::(y: :doc:`Index <fixpt>`): Range




*********

.. parsed-literal::

  :maroon:`def` <(y: :doc:`fixpt`\[S,I,F\]): :doc:`bit`




*********

.. parsed-literal::

  :maroon:`def` <(y: Int): :doc:`bit`




*********

.. parsed-literal::

  :maroon:`def` <(y: Long): :doc:`bit`




*********

.. parsed-literal::

  :maroon:`def` <(y: Float): :doc:`bit`




*********

.. parsed-literal::

  :maroon:`def` <(y: Double): :doc:`bit`




*********

.. parsed-literal::

  :maroon:`def` <<(y: :doc:`fixpt`\[S,I,B0\]): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` <<(y: Int): :doc:`fixpt`\[S,I,B0\]




*********

.. parsed-literal::

  :maroon:`def` <=(y: :doc:`fixpt`\[S,I,F\]): :doc:`bit`




*********

.. parsed-literal::

  :maroon:`def` <=(y: Int): :doc:`bit`




*********

.. parsed-literal::

  :maroon:`def` <=(y: Long): :doc:`bit`




*********

.. parsed-literal::

  :maroon:`def` <=(y: Float): :doc:`bit`




*********

.. parsed-literal::

  :maroon:`def` <=(y: Double): :doc:`bit`




*********

.. parsed-literal::

  :maroon:`def` >(y: :doc:`fixpt`\[S,I,F\]): :doc:`bit`




*********

.. parsed-literal::

  :maroon:`def` >(y: Int): :doc:`bit`




*********

.. parsed-literal::

  :maroon:`def` >(y: Long): :doc:`bit`




*********

.. parsed-literal::

  :maroon:`def` >(y: Float): :doc:`bit`




*********

.. parsed-literal::

  :maroon:`def` >(y: Double): :doc:`bit`




*********

.. parsed-literal::

  :maroon:`def` >=(y: :doc:`fixpt`\[S,I,F\]): :doc:`bit`




*********

.. parsed-literal::

  :maroon:`def` >=(y: Int): :doc:`bit`




*********

.. parsed-literal::

  :maroon:`def` >=(y: Long): :doc:`bit`




*********

.. parsed-literal::

  :maroon:`def` >=(y: Float): :doc:`bit`




*********

.. parsed-literal::

  :maroon:`def` >=(y: Double): :doc:`bit`




*********

.. parsed-literal::

  :maroon:`def` >>(y: :doc:`fixpt`\[S,I,B0\]): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` >>(y: Int): :doc:`fixpt`\[S,I,B0\]




*********

.. parsed-literal::

  :maroon:`def` by(y: :doc:`Index <fixpt>`): :doc:`looprange`




*********

.. parsed-literal::

  :maroon:`def` mkString(): :doc:`string`




*********

.. parsed-literal::

  :maroon:`def` to(): R




*********

.. parsed-literal::

  :maroon:`def` toString(): :doc:`string`




*********

.. parsed-literal::

  :maroon:`def` unary\_-(): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` until(y: :doc:`Index <fixpt>`): :doc:`looprange`




*********

.. parsed-literal::

  :maroon:`def` \|(y: :doc:`fixpt`\[S,I,F\]): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` \|(y: Int): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` \|(y: Long): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` \|(y: Float): :doc:`fixpt`\[S,I,F\]




*********

.. parsed-literal::

  :maroon:`def` \|(y: Double): :doc:`fixpt`\[S,I,F\]




