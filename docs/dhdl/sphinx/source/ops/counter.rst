
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

.. _Counter:

Counter
=======

Counter is a single hardware counter with an associated minimum, maximum, step size, and parallelization factor.
By default, the parallelization factor is assumed to be a design parameter. Counters can be chained together using
CounterChain, but this is typically done implicitly when creating controllers.


Static methods
--------------

.. parsed-literal::

  :maroon:`def` apply(max: :doc:`Index <fixpt>`): :doc:`counter`

Creates an unnamed Counter with min of 0, given max, and step size of 1


*********

.. parsed-literal::

  :maroon:`def` apply(min: :doc:`Index <fixpt>`, max: :doc:`Index <fixpt>`): :doc:`counter`

Creates an unnamed Counter with given min and max, and step size of 1


*********

.. parsed-literal::

  :maroon:`def` apply(min: :doc:`Index <fixpt>`, max: :doc:`Index <fixpt>`, step: :doc:`Index <fixpt>`): :doc:`counter`

Creates an unnamed Counter with given min, max and step size


*********

.. parsed-literal::

  :maroon:`def` apply(min: :doc:`Index <fixpt>`, max: :doc:`Index <fixpt>`, step: :doc:`Index <fixpt>`, par: Int): :doc:`counter`

Creates an unnamed Counter with given min, max, step size, and parallelization factor


*********

.. parsed-literal::

  :maroon:`def` apply(name: String, max: :doc:`Index <fixpt>`): :doc:`counter`

Creates a named Counter with min of 0, given max, and step size of 1


*********

.. parsed-literal::

  :maroon:`def` apply(name: String, min: :doc:`Index <fixpt>`, max: :doc:`Index <fixpt>`): :doc:`counter`

Creates a named Counter with given min and max, and step size of 1


*********

.. parsed-literal::

  :maroon:`def` apply(name: String, min: :doc:`Index <fixpt>`, max: :doc:`Index <fixpt>`, step: :doc:`Index <fixpt>`): :doc:`counter`

Creates a named Counter with given min, max and step size


*********

.. parsed-literal::

  :maroon:`def` apply(name: String, min: :doc:`Index <fixpt>`, max: :doc:`Index <fixpt>`, step: :doc:`Index <fixpt>`, par: Int): :doc:`counter`

Creates a named Counter with given min, max, step size, and parallelization factor


