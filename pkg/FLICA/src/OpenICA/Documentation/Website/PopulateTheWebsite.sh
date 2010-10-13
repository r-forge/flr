#!/bin/bash

# Provide an example used to test OpenICAMINUIT
cd /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT/Documentation/Website/OpenSourceICA

if [ ! -d "Examples" ]; then
   mkdir Examples
fi

cd /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT/Data/Simulated/Absolute-NoVariability-NoSSBbias-NoCatchBias-SSBsurveyNVNVNV/1

tar -pczf /tmp/UsingICAsettings.tar.gz Canum Caton Fleet Fprop ICAsettings.txt Index Matprop Mprop Natmor ssb Weca West

cp /tmp/UsingICAsettings.tar.gz /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT/Documentation/Website/OpenSourceICA/Examples/.


# Provide OpenICAMINUIT sources
cd /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT/Documentation/Website/OpenSourceICA
if [ ! -d "src" ]; then
   mkdir src
fi

cd /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT
tar -pczf /tmp/OpenICAMINUIT.tar.gz makefile src/*
cp /tmp/OpenICAMINUIT.tar.gz /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICA/Documentation/Website/OpenSourceICA/src/.

# Provide binaries
cd /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT/Documentation/Website/OpenSourceICA
if [ ! -d "bin" ]; then
   mkdir bin; mkdir bin/Ubuntu10.04;
fi

cp /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT/OpenICA.exe /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICA/Documentation/Website/OpenSourceICA/bin/Ubuntu10.04/.


