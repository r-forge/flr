#!/bin/bash

# Provide an example used to test OpenICAMINUIT
cd /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT/Documentation/Website/OpenSourceICA

if [ ! -d "Examples" ]; then
   mkdir Examples
fi

cd /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT/Data/Simulated/Absolute-NoVariability-NoSSBbias-NoCatchBias-SSBsurveyNVNVNV/1

tar -pczf /tmp/UsingICAsettings.tar.gz Canum Caton Fleet Fprop ICAsettings.txt Index Matprop Mprop Natmor ssb Weca West

cp /tmp/UsingICAsettings.tar.gz /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT/Documentation/Website/OpenSourceICA/Examples/.

# Provide all simulated dataset used for testing
cd /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT/Documentation/Website/OpenSourceICA

if [ ! -d "test" ]; then
   mkdir test
fi

if [ ! -d "test/datasets" ]; then
   mkdir test/datasets
fi

cd /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT/Data/Simulated
tar -pczf /tmp/SimAbsolute.tar.gz Absolute-NoVariability-NoSSBbias-NoCatchBias-SSBsurveyNVNVNV --exclude-vcs

tar -pczf /tmp/SimRelative.tar.gz Relative-NoVariability-NoSSBbias-NoCatchBias-SSBsurveyNVNVNV --exclude-vcs

cd /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT/Documentation/Website/OpenSourceICA
cp /tmp/SimAbsolute.tar.gz test/datasets/.
cp /tmp/SimRelative.tar.gz test/datasets/.


# Provide OpenICAMINUIT sources
cd /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT/Documentation/Website/OpenSourceICA
if [ ! -d "src" ]; then
   mkdir src
fi

cd /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT
tar -pczf /tmp/OpenICAMINUIT.tar.gz makefile src/*
cp /tmp/OpenICAMINUIT.tar.gz /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT/Documentation/Website/OpenSourceICA/src/.

# Provide the makefile
cp /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT/makefile /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT/Documentation/Website/OpenSourceICA/src/.

# Provide binaries
cd /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT/Documentation/Website/OpenSourceICA
if [ ! -d "bin" ]; then
   mkdir bin; mkdir bin/Ubuntu10.04;
fi

cp /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT/OpenICAMINUIT.exe /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT/Documentation/Website/OpenSourceICA/bin/Ubuntu10.04/.


