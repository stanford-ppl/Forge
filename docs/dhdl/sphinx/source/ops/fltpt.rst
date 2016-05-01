
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

.. _FltPt:

FltPt
=====

FltPt[G,E] represents an arbitrary precision, IEEE-754-like representation.
FltPt values are always assumed to be signed.

The type parameters for FltPt are:

+---+------------------------------------------------+---------------+
| G | Number of significand bits, including sign bit | (B2 - B64)    |
+---+------------------------------------------------+---------------+
| E | Number of exponent bits                        | (B1 - B64)    |
+---+------------------------------------------------+---------------+

Note that numbers of bits use the B- prefix as integers cannot be used as type parameters in Scala


Type Aliases
------------

+----------+------+-------------------------+---------------------------+
| **type** | Half | :doc:`fltpt`\[B11,B5\]  | IEEE-754 half precision   |
+----------+------+-------------------------+---------------------------+
| **type** | Flt  | :doc:`fltpt`\[B24,B8\]  | IEEE-754 single precision |
+----------+------+-------------------------+---------------------------+
| **type** | Dbl  | :doc:`fltpt`\[B53,B11\] | IEEE-754 double precision |
+----------+------+-------------------------+---------------------------+


Infix methods
-------------

.. parsed-literal::

  :maroon:`def` !=(y: :doc:`fltpt`\[G,E\]): :doc:`bit`




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

  :maroon:`def` \*(y: :doc:`fltpt`\[G,E\]): :doc:`fltpt`\[G,E\]




*********

.. parsed-literal::

  :maroon:`def` \*(y: Int): :doc:`fltpt`\[G,E\]




*********

.. parsed-literal::

  :maroon:`def` \*(y: Long): :doc:`fltpt`\[G,E\]




*********

.. parsed-literal::

  :maroon:`def` \*(y: Float): :doc:`fltpt`\[G,E\]




*********

.. parsed-literal::

  :maroon:`def` \*(y: Double): :doc:`fltpt`\[G,E\]




*********

.. parsed-literal::

  :maroon:`def` \*\*(y: Int): :doc:`fltpt`\[G,E\]




*********

.. parsed-literal::

  :maroon:`def` +(y: :doc:`fltpt`\[G,E\]): :doc:`fltpt`\[G,E\]




*********

.. parsed-literal::

  :maroon:`def` +(y: Int): :doc:`fltpt`\[G,E\]




*********

.. parsed-literal::

  :maroon:`def` +(y: Long): :doc:`fltpt`\[G,E\]




*********

.. parsed-literal::

  :maroon:`def` +(y: Float): :doc:`fltpt`\[G,E\]




*********

.. parsed-literal::

  :maroon:`def` +(y: Double): :doc:`fltpt`\[G,E\]




*********

.. parsed-literal::

  :maroon:`def` -(y: :doc:`fltpt`\[G,E\]): :doc:`fltpt`\[G,E\]




*********

.. parsed-literal::

  :maroon:`def` -(y: Int): :doc:`fltpt`\[G,E\]




*********

.. parsed-literal::

  :maroon:`def` -(y: Long): :doc:`fltpt`\[G,E\]




*********

.. parsed-literal::

  :maroon:`def` -(y: Float): :doc:`fltpt`\[G,E\]




*********

.. parsed-literal::

  :maroon:`def` -(y: Double): :doc:`fltpt`\[G,E\]




*********

.. parsed-literal::

  :maroon:`def` \/(y: :doc:`fltpt`\[G,E\]): :doc:`fltpt`\[G,E\]




*********

.. parsed-literal::

  :maroon:`def` \/(y: Int): :doc:`fltpt`\[G,E\]




*********

.. parsed-literal::

  :maroon:`def` \/(y: Long): :doc:`fltpt`\[G,E\]




*********

.. parsed-literal::

  :maroon:`def` \/(y: Float): :doc:`fltpt`\[G,E\]




*********

.. parsed-literal::

  :maroon:`def` \/(y: Double): :doc:`fltpt`\[G,E\]




*********

.. parsed-literal::

  :maroon:`def` <(y: :doc:`fltpt`\[G,E\]): :doc:`bit`




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

  :maroon:`def` <=(y: :doc:`fltpt`\[G,E\]): :doc:`bit`




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

  :maroon:`def` >(y: :doc:`fltpt`\[G,E\]): :doc:`bit`




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

  :maroon:`def` >=(y: :doc:`fltpt`\[G,E\]): :doc:`bit`




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

  :maroon:`def` mkString(): :doc:`string`




*********

.. parsed-literal::

  :maroon:`def` to(): R




*********

.. parsed-literal::

  :maroon:`def` toString(): :doc:`string`




*********

.. parsed-literal::

  :maroon:`def` unary\_-(): :doc:`fltpt`\[G,E\]




