# So you want to compile packages from source on your Mac?

Then please follow these instructions and advice. *However,* note
that:

+ These instructions were developed with R versions 3.4 and 3.5 in
mind. If you are using a version of R prior to 3.4, please disregard
this document as it is not relevant to you.

+ It is possible that these instructions will no longer be valid for
future versions of R. So if you are using R > 3.5, *follow these
instructions at your risk.*

## Introduction to the problem

If you are using version R >= 3.4 in macOS, you should find that
installing packages with the usual approach (e.g., using
`install.packages` or `devtools::install_github`) works for most
packages. However, you may find that you get strange errors when
trying to install some packages. One possible reason for this error is
that installing these packages involves compiling C++ and/or Fortran
code, and this compilation step failed.  (There are many, many other
reasons why installing a package may fail.)

As of R 3.4.0, the R development team decided to use a compiler that
is no longer provided by Apple. There were good
reasons for making this change:

> Since R 3.4.0 release, we are now providing binaries for OS X 10.11
> (El Capitan) and higher using non-Apple toolkit to provide support
> for OpenMP and C++17 standard features.

This has unfortunately made package installation and development more
complicated on Mac computers.

**Key point:** If you are using R >= 3.4 on Mac, and you have not
installed the [C and Fortran compilers recommended for your version of
R][cran-macos-tools], then you will have trouble installing some packages
from source. Here I explain how to set up your Mac for installing R packages.

## What I don't recommend doing

## General advice

## Links

https://cran.r-project.org/doc/manuals/r-release/R-admin.html#macOS-packages

[cran-macos-tools]: https://cran.r-project.org/bin/macosx/tools
[coatless-prof]: https://thecoatlessprofessor.com/programming/openmp-in-r-on-os-x
