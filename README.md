
RochePlot R Package
===================

This package contains simple wrappers for plots we repeatedly do.

Help using functions
--------------------

Please see the *Functions* link above for an HTML version of the help functions. You can also use `?` in R to get help, as the functions have help files built in.

For developers
--------------

Steps to update the package

1.  Make a branch off master
2.  Make your changes
3.  Test your changes
4.  Run `devtools::document()` to rebuild the docs.
5.  Run `pkgdown::pkgdown::build_site()`
    -   If you added a function, you'll get an error now. Add the function to \_pkgdown.yaml as it says
6.  Make a merge request to the master branch.
