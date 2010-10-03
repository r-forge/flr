#!/bin/bash

# Provide an example used to test OpenICA
cd /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICA/Documentation/Website/OpenSourceICA

if [ ! -d "Examples" ]; then
   mkdir Examples
fi

cd /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICA/Data/Simulated/Absolute-NoVariability-NoSSBbias-NoCatchBias-SSBsurveyNVNVNV/1

tar -pczf /tmp/UsingICAsettings.tar.gz Canum Caton Fleet Fprop ICAsettings.txt Index Matprop Mprop Natmor ssb Weca West

cp /tmp/UsingICAsettings.tar.gz /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICA/Documentation/Website/OpenSourceICA/Examples/.


# Provide OpenICA sources
cd /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICA/Documentation/Website/OpenSourceICA
if [ ! -d "src" ]; then
   mkdir src
fi

cd /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICA
tar -pczf /tmp/OpenICA.tar.gz makefile src/*
cp /tmp/OpenICA.tar.gz /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICA/Documentation/Website/OpenSourceICA/src/.

# Provide binaries
cd /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICA/Documentation/Website/OpenSourceICA
if [ ! -d "bin" ]; then
   mkdir bin; mkdir bin/Ubuntu10.04;
fi

cp /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICA/OpenICA.exe /home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICA/Documentation/Website/OpenSourceICA/bin/Ubuntu10.04/.


