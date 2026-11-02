# src/ holds the Python package (src/hydroloom/), not compiled R code.
# R CMD INSTALL sources this file in place of copying a shared library; it
# installs nothing, so the install succeeds with no DLL (Windows) or .so (Unix).
# See src/Makefile.
invisible(NULL)
