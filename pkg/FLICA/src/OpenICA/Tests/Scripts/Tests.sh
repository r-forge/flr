#!/bin/bash

# Test OpenICA using the option of fitting the survey as an absolute estimate of biomass
for i in `seq 1 10`; do
cd ~/CSIRO/cmar_projects/Fortran/ICA/OpenICA/Data/Simulated/Absolute-NoVariability-NoSSBbias-NoCatchBias-SSBsurveyNVNVNV/$i; 
OpenICA.exe < ICAsettings.txt > ica.log;
echo "Fitted OpenICA using survey as absolute estimate of abundance on dataset" $i 
done

# Test OpenICA using the option of fitting the survey as a relative estimate of biomass
for i in `seq 1 10`; do 
cd ~/CSIRO/cmar_projects/Fortran/ICA/OpenICA/Data/Simulated/Relative-NoVariability-NoSSBbias-NoCatchBias-SSBsurveyNVNVNV/$i; 
OpenICA.exe < ICAsettings.txt > ica.log; 
echo "Fitted OpenICA using survey as relative estimate of abundance on dataset" $i
done

# Create the graph that compare OpenICA estimates to parameters used to create the simulated dataset
cd ~/CSIRO/cmar_projects/Fortran/ICA/OpenICA/Tests/Scripts
perl ComparisonBtwICAestimatesAndSimulatedData.pl -d=Absolute-NoVariability-NoSSBbias-NoCatchBias-SSBsurveyNVNVNV -v=OpenICA > /tmp/trash
perl ComparisonBtwICAestimatesAndSimulatedData.pl -d=Relative-NoVariability-NoSSBbias-NoCatchBias-SSBsurveyNVNVNV -v=OpenICA > /tmp/trash


echo "Created graphical comparison of OpenICA estimates against parameters used to simulate datasets"

# Assemble the results into a website
cd ~/CSIRO/cmar_projects/Fortran/ICA/OpenICA/Documentation/Website
./TranslateIntoHTML.sh > /tmp/trash

echo "Created ~/CSIRO/cmar_projects/Fortran/ICA/OpenSourceICA/Documentation/Website/OpenICA/OpenICA.html"

# go back where you started
cd ~/CSIRO/cmar_projects/Fortran/ICA/OpenICA/Tests/Scripts

