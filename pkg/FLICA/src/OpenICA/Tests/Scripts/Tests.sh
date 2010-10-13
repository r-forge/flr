#!/bin/bash

# Test OpenICAMINUIT using the option of fitting the survey as an absolute estimate of biomass
for i in `seq 1 10`; do
cd ~/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT/Data/Simulated/Absolute-NoVariability-NoSSBbias-NoCatchBias-SSBsurveyNVNVNV/$i; 
OpenICAMINUIT.exe < ICAsettings.txt > ica.log;
echo "Fitted OpenICAMINUIT using survey as absolute estimate of abundance on dataset" $i 
done

# Test OpenICAMINUIT using the option of fitting the survey as a relative estimate of biomass
for i in `seq 1 10`; do 
cd ~/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT/Data/Simulated/Relative-NoVariability-NoSSBbias-NoCatchBias-SSBsurveyNVNVNV/$i; 
OpenICAMINUIT.exe < ICAsettings.txt > ica.log; 
echo "Fitted OpenICAMINUIT using survey as relative estimate of abundance on dataset" $i
done

# Create the graph that compare OpenICAMINUIT estimates to parameters used to create the simulated dataset
cd ~/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT/Tests/Scripts
perl ComparisonBtwICAestimatesAndSimulatedData.pl -d=Absolute-NoVariability-NoSSBbias-NoCatchBias-SSBsurveyNVNVNV -v=OpenICAMINUIT > /tmp/trash
perl ComparisonBtwICAestimatesAndSimulatedData.pl -d=Relative-NoVariability-NoSSBbias-NoCatchBias-SSBsurveyNVNVNV -v=OpenICAMINUIT > /tmp/trash


echo "Created graphical comparison of OpenICAMINUIT estimates against parameters used to simulate datasets"

# Assemble the results into a website
cd ~/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT/Documentation/Website
./TranslateIntoHTML.sh > /tmp/trash

echo "Created ~/CSIRO/cmar_projects/Fortran/ICA/OpenSourceICA/Documentation/Website/OpenICAMINUIT/OpenICA.html"

# go back where you started
cd ~/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT/Tests/Scripts

