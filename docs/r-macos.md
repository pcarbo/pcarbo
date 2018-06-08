# So you want to compile packages from source on your Mac?

Please follow these instructions and advice. However, note that:

+ These instructions have been developed with R versions 3.4 and 3.5
in mind. If you are using a version of R prior to 3.4, please
disregard this document as it is not relevant to you.

+ It is possible that these instructions will no longer be valid for
future versions of R. So if you are using R > 3.5, *follow these
instructions at your risk.*

## Introduction to the problem

If you are using version R 3.4.0 `install.packages`

As of R 3.4.0, the R development team decided to use a compiler that
is no longer provided by Apple. I'm sure that there were very good
reasons for making this change, but this 
