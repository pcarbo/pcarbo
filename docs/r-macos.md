# So you want to compile packages on your Mac?

Then please follow these instructions and advice. *However,* please
note that:

1. These instructions were developed with R versions 3.4 and 3.5 in
mind. If you are using a version of R prior to 3.4, please disregard
this document as it is not relevant to you.

2. It is possible that these instructions will no longer be valid for
future versions of R. So if you are using R > 3.5, *follow these
instructions at your risk.*

## Introduction to the problem

If you are using version R >= 3.4 in macOS, you should find that
installing packages with the usual approach (e.g., using
`install.packages` or `devtools::install_github`) works for most
packages. However, you may find that you get strange errors when
trying to install some packages. One possible reason (among many!) for
this error is that installing these packages involves compiling C++
and/or Fortran code, and this compilation step failed.

> Installing source packages which contain C/C++/Fortran code requires
> that compilers and related tools be installed.

As of R 3.4.0, the R development team decided to use a compiler that
is no longer provided by Apple. There were good
reasons for making this change:

> Since R 3.4.0 release, we are now providing binaries for OS X 10.11
> (El Capitan) and higher using non-Apple toolkit to provide support
> for OpenMP and C++17 standard features.

But, tis has unfortunately made package installation and development
more complicated for people with a Mac.

**Key point:** If you are using R >= 3.4 on Mac, and you have not
installed the [C and Fortran compilers recommended for your version of
R][cran-macos-tools], then you will have trouble installing some packages
from source.

Here I explain how to properly (and safely) set up your Mac for
installing R packages from source.

## What I don't recommend doing

Do not download and install the
[tools provided by CRAN][cran-macos-tools].  This will install these
compilers in the system directories on your computer and could cause
issues that will be difficult to fix.

## My recommendation

Use package managers (Homebrew, MacPorts, Anaconda, Miniconda, _etc._)
to install the necessary compilers. This will allow you to install
compilers without affecting the system directories. And if you make a
mistake, you can easily reverse the changes.

## Setup instructions

I've made these instructions general enough that they should work for
different versions of R. However, I can't guarantee that they will
work. This advice is based on my experience.

## Links

https://cran.r-project.org/doc/manuals/r-release/R-admin.html#macOS-packages

[cran-macos-tools]: https://cran.r-project.org/bin/macosx/tools
[coatless-prof]: https://thecoatlessprofessor.com/programming/openmp-in-r-on-os-x
[package-compilation-macos]: https://cran.r-project.org/doc/manuals/r-release/R-admin.html#macOS-packages
