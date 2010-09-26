#!/bin/bash

#CREATION: 11 July 2007
#MODIFIED: 31 August 2007
#PURPOSE: fit ICAv1.4x to the data and compare with ICAv1.4w
#AUTHOR: marco.kienzle@gmail.com

# Make sure that ICAv1.4x used is the latest compiled
echo "Copying ICAv1.4x from ICA/ModifiedSource to /home/kienzle/bin"
cp /home/mkienzle/Programs/Fortran/ICA/ModifiedSource/ICA.exe /home/mkienzle/bin/.

# Make sure the binary directory is in the executant PATH
source ~/.bash_profile

# Looping over the different type of datasets
#for SimulatedDataset in {'Absolute-NoVariability-NoSSBbias-NoCatchBias-SSBsurveyNVNVNV','Relative-NoVariability-NoSSBbias-NoCatchBias-SSBsurveyNVNVNV','Absolute-FisheryVariability-NoSSBbias-NoCatchBias-SSBsurveyNANACS','Relative-FisheryVariability-NoSSBbias-NoCatchBias-SSBsurveyNANACS','Absolute-FisheryVariability-NoSSBbias-NoCatchBias-SSBsurveyCSNANA','Relative-FisheryVariability-NoSSBbias-NoCatchBias-SSBsurveyCSNANA'}; do
#for SimulatedDataset in {'Absolute-NoVariability-NoSSBbias-NoCatchBias-SSBsurveyNVNVNV','Relative-NoVariability-NoSSBbias-NoCatchBias-SSBsurveyNVNVNV'}; do
for SimulatedDataset in 'Relative-FisheryVariability-NoSSBbias-NoCatchBias-SSBsurveyNANACS'; do

echo "Start working on $SimulatedDataset simulated dataset"
cd /home/mkienzle/Websites/OpenSourceICA/OpenSourceICA/test/datasets/ICAv1.4x/$SimulatedDataset;

# Fit ICAv1.4x to all datasets
echo "Fit ICAv1.4x to all datasets"
sh commands.sh;

##### Compare ICA v1.4w to v1.4x using R

echo "Compare ICA v1.4w to v1.4x using R"

# Compare the results of 2 differents ICA versions
perl /home/mkienzle/Websites/OpenSourceICA/OpenSourceICA/test/Scripts/Comparison.pl -d $SimulatedDataset;

# Compare the results of 2 ICA versions against simulated values
perl /home/mkienzle/Websites/OpenSourceICA/OpenSourceICA/test/Scripts/R-analysis.pl -d $SimulatedDataset;

echo "testICA.sh script is finished"
done;
