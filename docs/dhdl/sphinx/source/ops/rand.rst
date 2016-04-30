
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

.. _Rand:

Rand
====

Unsynthesizable group of operations for generating random data for testing

Related methods
---------------

.. parsed-literal::

  :maroon:`def` random(x: A): A

Returns a uniformly distributed random value of type A with the given maximum value


*********

.. parsed-literal::

  :maroon:`def` random(x: Int): A




*********

.. parsed-literal::

  :maroon:`def` random(x: Long): A




*********

.. parsed-literal::

  :maroon:`def` random(x: Float): A




*********

.. parsed-literal::

  :maroon:`def` random(x: Double): A




*********

.. parsed-literal::

  :maroon:`def` random(): A

Returns a uniformly distributed random value of type A. Fixed point types are unbounded, while floating point types are between 0 and 1


