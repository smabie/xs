Installation
============

Since xs is written in ocaml, it is necessary to first install
``opam``, the Ocaml package manager. ``opam`` is included with all
major distributions; for more information on installing opam, check
out the installation instructions `here`_

.. _here: https://opam.ocaml.org/doc/Install.html

Note that ``opam`` version 2.0 or above is required. Also, *xs* has
only been tested with OCaml 4.10.0, though it might work with other
versions. If the included OCaml with your distribution is out-of-date,
run: ::

  opam switch create 4.10.0+flamda 4.10.0+flambda

This creates a new opam switch based on the latest release of the
OCaml compiler. Also, this enables aggressive compilation
optimizations that might improve the performance of *xs*.

After ``opam`` is installed, run the following command::

  opam pin add xs https://github.com/smabie/xs.git

After the installation sucessfully completes, run ``eval $(opam env)``
to update your environment variables. ``xs`` should now be installed,
which you can check by running ``which xs``. You also probably want
``rlwrap`` for movement functions in the editor. Install it with your
package manager and then run ``xs`` with: ::

  rlwrap xs
