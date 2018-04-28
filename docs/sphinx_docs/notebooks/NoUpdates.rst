
**************
No Updates
**************

DRAFT

1. Joy doesn't need to change.

  A. The interpreter doesn't need to change, ``viewer`` function can customize mainloop.  Or use a sub-interpreter (Joy in Joy.)  The base interpreter remains static.
  B. Once a function has been named and defined *never change that name*.  It's just not allowed.  If you need to change a function ``foo`` you have to call it ``foo_II`` or something.  Once a function (name mapped to behavior) is released to the public *that's it*, it's done.
  C. The language evolves by adding new definitions and refactoring, always choosing new names for new functions.

2. Following `Semantic Versioning`_ there will never be a version 2.0.

  A. `Major version must be incremented if any backwards incompatible changes are introduced to the public API. <https://semver.org/#spec-item-8>`__
  B. We never implement any backwards incompatible changes, so...
  C. We could see e.g. Thun version 1.273.3!


.. _Semantic Versioning: https://semver.org

