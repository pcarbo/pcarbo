# So you want to install source packages on your Mac?

Then please follow these instructions and advice. *However,* please
note that:

1. These instructions were developed with R versions 3.4 and 3.5 in
mind. If you are using a version of R prior to 3.4, please *disregard
this document as it is not (immediately) relevant to you.*

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

As of R 3.4, the R development team decided to use a compiler that
is no longer provided by Apple. There were good reasons for making
this change:

> Since R 3.4.0 release, we are now providing binaries for OS X 10.11
> (El Capitan) and higher using non-Apple toolkit to provide support
> for OpenMP and C++17 standard features.

But... this has unfortunately made package installation and development
more complicated for people with a Mac.

**Key point:** If you are using R >= 3.4 on Mac, and you have not
installed the [C and Fortran compilers recommended for your version of
R][cran-macos-tools], then you will have trouble installing some packages
from source.

Here I explain how to properly (and safely) set up your Mac for
installing R packages from source.

## What I don't recommend doing

Do **not** download and install the [tools provided by
CRAN][cran-macos-tools].  This will install these
compilers in the system directories on your computer and could cause
issues that will be difficult to fix.

## My recommendation

Use package managers (Homebrew, MacPorts, Anaconda, Miniconda, _etc._)
to install the necessary compilers. This will allow you to install
compilers without affecting the system directories. And if you make a
mistake, you can easily reverse the changes.

## Barebones setup instructions

I've made these instructions general enough that they should work for
different versions of R. (I can't guarantee that they will work,
however.) If you are unfamiliar with the tools I recommend using (e.g.,
Homebrew), you will have to learn the basics of how to use them.

I also give the specific example of my setup, which is R 3.4.3 on a
MacBook Pro (3.5 GHz Intel Core i7 CPU, macOS 10.13.4).

### 1. Install Homebrew.

Follow the installation instructions [here][homebrew].

### 2. Install MacPorts.

Follow the installation instructions [here][macports].

### 3. Install the recommended versions of clang and gfortran

Search Homebrew and MacPorts for the recommended version of **clang**
and **gfortran**, and install these packages (also called
"ports"). (Note that clang may be bundled with an llvm package, and
gfortran may be bundled with a gcc package.) For example, on my
computer, I have R 3.4.3, so I should install clang 4.0.0 and
gfortran 6.1 (according to [here][stackoverflow]—I can't find this
information on the CRAN website any more). I found clang 4.0.1 and
gfortran 6.4 (close enough, hopefully!) in MacPorts, so I ran these
commands to install them:

```bash
sudo port install clang-4.0
sudo port install gcc6
```

You may also find suitable packages in Homebrew.

Confusingly, R 3.5 switched to clang 6.0!

### 4. Edit your Makevars file to tell R use the new clang and gfortran

Edit file `~/.R/Makevars` to point to the newly installed versions of
clang and gfortran. For example, on my computer with R 3.4.3, I have:

```bash
$ cat ~/.R/Makevars
CC       = /opt/local/bin/clang-mp-4.0
CXX      = /opt/local/bin/clang++-mp-4.0
CXX11    = $CXX
CXX17    = $CXX
CXX98    = $CXX
F77      = /opt/local/bin/gfortran-mp-6
FC       = $F77
LDFLAGS  = -L/opt/local/lib
FLIBS    = -L/opt/local/lib/gcc6 -lgfortran -lquadmath -lm
```

Note that all MacPorts packages are installed in `/opt/local`.

For more detailed advice on configuring your `Makevars` file, see
[here][package-compilation-macos].

### 5. Test your new setup by installing a source package with C/Fortran code

For example, install [glmnet][glmnet] 2.0-16 from source by running
the following in R:

```R
install.packages("https://cran.r-project.org/src/contrib/glmnet_2.0-16.tar.gz")
```

The glmnet package has both C and Fortran source code, and indeed when
I run this command on my computer I see that it is using gfortran and
clang to compile and build the package:

```
/opt/local/bin/gfortran-mp-6 -fPIC -g -O2 -c glmnet5dp.f -o glmnet5dp.o
/opt/local/bin/clang-mp-4.0 -I/Library/Frameworks/R.framework/Resources/include -DNDEBUG -I/usr/local/include -fPIC -Wall -g -O2 -c glmnet_init.c -o glmnet_init.o
/opt/local/bin/clang-mp-4.0 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/opt/local/lib -o glmnet.so glmnet5dp.o glmnet_init.o -L/opt/local/lib/gcc6 -lgfortran -lquadmath -lm -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
```

## More info

+ See [here][cran-macos-notes] for more macOS-specific details on R.

[homebrew]: https://brew.sh
[macports]: http://macports.org
[glmnet]: https://CRAN.R-project.org/package=glmnet
[cran-macos-tools]: https://cran.r-project.org/bin/macosx/tools
[coatless-prof]: https://thecoatlessprofessor.com/programming/openmp-in-r-on-os-x
[package-compilation-macos]: https://cran.r-project.org/doc/manuals/r-release/R-admin.html#macOS-packages
[cran-macos-notes]: https://cran.r-project.org/doc/manuals/r-release/R-admin.html#macOS
[stackoverflow]: https://stackoverflow.com/questions/44439620/installing-r-3-4-0-on-macos-mac-os-x-10-9-5
