
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

.. _Array:

Array
=====

Unsynthesizable helper object for creating arrays on the CPU

Static methods
--------------

.. parsed-literal::

  :maroon:`def` empty(length: :doc:`Index <fixpt>`): :doc:`forgearray`\[T\]

Creates an empty array with given length 


*********

.. parsed-literal::

  :maroon:`def` fill(length: :doc:`Index <fixpt>`)(f:  => T): :doc:`forgearray`\[T\]

 Creates an array with given length whose elements are determined by the supplied function 


*********

.. parsed-literal::

  :maroon:`def` tabulate(length: :doc:`Index <fixpt>`)(f: (:doc:`Index <fixpt>`) => T): :doc:`forgearray`\[T\]

Creates an array with the given length whose elements are determined by the supplied indexed function 


