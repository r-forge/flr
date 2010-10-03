#!/bin/bash

# Convert the LaTeX to HTML
rm -fr OpenSourceICA; # necessary to avoid the latex2html interactive mode prompted by -noreuse
latex2html OpenSourceICA.tex

