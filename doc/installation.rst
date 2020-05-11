Installation
============

Since xs is written in ocaml, it is necessary to first install
``opam``, the Ocaml package manager. ``opam`` is included with all
major distributions; for more information on installing opam, check
out the installation instructions `here`_

.. _here: https://opam.ocaml.org/doc/Install.html

After ``opam`` is installed, run the following command::

  opam pin add xs https://github.com/smabie/xs.git

After the installation sucessfully completes, run ``eval $(opam env)``
to update your environment variables. ``xs`` should now be installed,
which you can check by running ``which xs``.
