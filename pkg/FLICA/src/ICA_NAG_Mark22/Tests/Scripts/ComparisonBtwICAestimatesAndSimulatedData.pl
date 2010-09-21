#!/usr/bin/perl -w

# PURPOSE: compare parameter estimates from ICA fit and simulated data
# DATE: 1 June 2005
# LAST MODIFIED: 21 Sept 2010
# AUTHOR: marco.kienzle@gmail.com

# STATUS not working

#DEAL WITH THE OPTION

use Getopt::Long;

GetOptions("h"    => \$help,
           "d=s"  => \$DirPath,
           "v=s"  => \$ICAVersionTested);

if($help){print "OPTION: this program need the path of the directory containing the data files\n";
          print "d: the directory containing the files\n";
          print "USAGE:  -d=Absolute-NoVariability-NoSSBbias-NoCatchBias-SSBsurveyNVNVNV -v=ICA_NAG22\n";
          exit();}


$DataPath = "/scratch/cmar_projects/Fortran/ICA/ICA_NAG_Mark22/Data/Simulated/$DirPath";
#$NewVersionPath = "/home/mkienzle/Websites/OpenSourceICA/OpenSourceICA/test/datasets/ICAv1.4x/$DirPath";

# Get the number of subdirectories
$SubDirNb=`tree -d $DataPath | grep directories`; chomp($SubDirNb); $SubDirNb =~ /(\d{1,5})\sdirectories/;
$last=$1;

# Begin to talk to R
open(R, "| R --vanilla ");

# Useful to see the graphical output while running
# print R "x11()\n"; 

print R "source(\"/scratch/cmar_projects/Fortran/ICA/ICA_NAG_Mark22/Tests/Scripts/ExtractSummaryTableFromICAoutput.R\")\n";

print R "for(i in 1:1){\n";
print R "ica.data <- read.ica(paste(\"$DataPath/\",i,\"\/ica.out\",sep=\"\"))\n";
print R "simulated.recruitment <- read.table(file = paste(\"$DataPath/\",i,\"/simulated.recruitment\", sep = \"\"), header = TRUE)\n";

#print R "new<-read.ica(paste(\"$NewVersionPath/\",i,\"\/ica.out\",sep=\"\"))\n";

print R "all.ica.data <- ica.data\$data\n";
print R "all.simulated.rec <- simulated.recruitment[simulated.recruitment\$year %in% 1972:2002, \"number\"] * 1e-3\n";
#print R "newdata<-new\$data\n";

print R "}\n";

print R "for(i in 2:$last){\n";
print R "ica.data<-read.ica(paste(\"$DataPath/\",i,\"\/ica.out\",sep=\"\"))\n";
print R "simulated.recruitment <- read.table(file = paste(\"$DataPath/\",i,\"/simulated.recruitment\", sep=\"\"), header = TRUE)\n";
#print R "new<-read.ica(paste(\"$NewVersionPath/\",i,\"\/ica.out\",sep=\"\"))\n";

print R "all.ica.data<-rbind(all.ica.data,ica.data\$data)\n";
print R "all.simulated.rec <- c(all.simulated.rec, simulated.recruitment[simulated.recruitment\$year %in% 1972:2002, \"number\"] * 1e-3)\n";
#print R "newdata<-rbind(newdata,new\$data)\n";

print R "}\n";

#print R "print(ica.datadata)\n";
#print R "print(newdata)\n";

# COMPARE THE RECRUITMENT
print R "ps.options(horizontal=FALSE, onefile=FALSE, paper=\"special\", width=8, height=6)\n";
print R "postscript(\"/scratch/cmar_projects/Fortran/ICA/ICA_NAG_Mark22/Tests/Results/$DirPath/$ICAVersionTested/comparison-recruitment.ps\", onefile=FALSE)\n";

print R "print(dim(all.ica.data))\n";
print R "print(length(all.simulated.rec))\n";

print R "plot(all.ica.data[,1], all.simulated.rec,xlab=\"ICA estimates\", ylab=\"Simulated\", las=1)\n";
print R "title(\"Recruitment (thousands of individuals)\")\n";
print R "abline(0,1)\n";

print R "dev.off()\n";

# # COMPARE THE TSB
# print R "ps.options(horizontal=FALSE, onefile=FALSE, paper=\"special\", width=8, height=6)\n";
# print R "postscript(\"$NewVersionPath/comparison-tsb.ps\", onefile=FALSE)\n";

# print R "plot(ica.datadata[,2],newdata[,2],xlab=\"version 1.4w\",ylab=\"version 1.4x\", las=1)\n";
# print R "title(\"Total stock biomass\")\n";
# print R "abline(0,1)\n";

# print R "dev.off()\n";

# # COMPARE THE SSB
# print R "ps.options(horizontal=FALSE, onefile=FALSE, paper=\"special\", width=8, height=6)\n";
# print R "postscript(\"$NewVersionPath/comparison-ssb.ps\", onefile=FALSE)\n";

# print R "plot(ica.datadata[,3],newdata[,3],xlab=\"version 1.4w\",ylab=\"version 1.4x\", las=1)\n";
# print R "title(\"Spawning stock biomass\")\n";
# print R "abline(0,1)\n";

# print R "dev.off()\n";

# # COMPARE THE Fbar
# print R "ps.options(horizontal=FALSE, onefile=FALSE, paper=\"special\", width=8, height=6)\n";
# print R "postscript(\"$NewVersionPath/comparison-Fbar.ps\", onefile=FALSE)\n";

# print R "plot(ica.datadata[,6],newdata[,6],xlab=\"version 1.4w\",ylab=\"version 1.4x\", las=1)\n";
# print R "title(\"Fbar\")\n";
# print R "abline(0,1)\n";

# print R "dev.off()\n";

# Stop talking to R
print R "q()\n";
print R "y\n";
close(R);
