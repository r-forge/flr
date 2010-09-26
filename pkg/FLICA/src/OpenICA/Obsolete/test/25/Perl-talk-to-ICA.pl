#!/usr/local/bin/perl -w

# PURPOSE: This script open ICA and pass to it the settings stored in the ICAsettings.txt file
# DATE: 13 June 2005
# AUTHOR: kienzle_marco@bigfoot.com

$pid = open(ICAprompt, "| ICA.exe");

open(SETTINGS, '< /home/kienzlem/Websites/OpenSourceICA/OpenSourceICA/test/datasets/ICAv1.4x/Absolute-NoVariability-NoSSBbias-NoCatchBias-SSBsurveyNVNVNV/ICAsettings.txt');

while(<SETTINGS>){print ICAprompt "$_";}

close(ICAprompt);
