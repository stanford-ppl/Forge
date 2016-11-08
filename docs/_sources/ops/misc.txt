
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

.. _Misc:

Misc
====


Miscellaneous operations, including assertions, console printing, and exception throwing.


Related methods
---------------

.. parsed-literal::

  :maroon:`def` \_\_ifThenElse(x: Rep[:doc:`boolean`], y:  => Rep[T], z:  => Rep[T]): Rep[T]




*********

.. parsed-literal::

  :maroon:`def` \_\_whileDo(x:  => Rep[:doc:`boolean`], y:  => Rep[Unit]): Rep[Unit]




*********

.. parsed-literal::

  :maroon:`def` exit(x: Rep[:doc:`int`]): Rep[Nothing]




*********

.. parsed-literal::

  :maroon:`def` fassert(cond: Rep[:doc:`boolean`], message: Rep[:doc:`string`]): Rep[Unit]

If the condition is true, does nothing. If it is false, terminates the program with the specified exception message. Assertions are only evaluated in the library backend and during compiler debugging. Otherwise, they are ignored to enable more fusion and code motion opportunities. 

	* **cond** \- condition to be checked
	* **message** \- exception message to be shown to the user if the assertion fails

*********

.. parsed-literal::

  :maroon:`def` fatal(x: Rep[:doc:`string`]): Rep[Nothing]




*********

.. parsed-literal::

  :maroon:`def` getMaxHeapSize(): Rep[:doc:`long`]

Returns the maximum heap size from the JVM


*********

.. parsed-literal::

  :maroon:`def` print(x: Rep[Any]): Rep[Unit]




*********

.. parsed-literal::

  :maroon:`def` println(value: Rep[Any]): Rep[Unit]

Prints the specified value to the console *with* a newline. At runtime, this will attempt to use the toString method to cast the value to string if one exists 


*********

.. parsed-literal::

  :maroon:`def` println(): Rep[Unit]

Prints a newline to the console


