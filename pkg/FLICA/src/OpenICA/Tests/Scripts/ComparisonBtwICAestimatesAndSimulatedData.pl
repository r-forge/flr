#!/usr/bin/perl -w

# PURPOSE: compare parameter estimates from OpenICAMINUIT (ICA version 1.4 x) to simulated data
# DATE: 1 June 2005
# LAST MODIFIED: 25 Sept 2010
# AUTHOR: marco.kienzle@gmail.com

# STATUS working

#DEAL WITH THE OPTION

use Getopt::Long;

GetOptions("h"    => \$help,
           "d=s"  => \$DirPath,
           "v=s"  => \$ICAVersionTested);

if($help){print "OPTION: this program need the path of the directory containing the data files\n";
          print "d: the directory containing the files\n";
	  print "v: the version of ICA used\n";
          print "USAGE:  -d=Absolute-NoVariability-NoSSBbias-NoCatchBias-SSBsurveyNVNVNV -v=OpenICAMINUIT\n";
          exit();}


$DataPath = "/home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT/Data/Simulated/$DirPath";

# Get the number of subdirectories
$SubDirNb=`tree -d $DataPath | grep directories`; chomp($SubDirNb); $SubDirNb =~ /(\d{1,5})\sdirectories/;
$last=$1;

# Begin to talk to R
open(R, "| R --vanilla ");

# Useful to see the graphical output while running
# print R "x11()\n"; 

print R "source(\"/home/mkienzle/Programs/R/ExtractSummaryTableFromICAoutput.R\")\n";

print R "for(i in 1:1){\n";
print R "ica.data <- read.ica(paste(\"$DataPath/\",i,\"/ica.out\",sep=\"\"))\n";
print R "simulated.recruitment <- read.table(file = paste(\"$DataPath/\",i,\"/simulated.recruitment\", sep = \"\"), header = TRUE)\n";
print R "simulated.ssb <- read.table(file = paste(\"$DataPath/\",i,\"/simulated.ssb\", sep = \"\"), skip = 2, header = TRUE)\n";
print R "simulated.tsb <- read.table(file = paste(\"$DataPath/\",i,\"/simulated.TSB\", sep = \"\"), skip = 1)\n";

print R "all.ica.data <- ica.data\$data\n";
print R "all.simulated.rec <- simulated.recruitment[simulated.recruitment\$year %in% 1972:2002, \"number\"] * 1e-3\n";
print R "all.simulated.ssb <- simulated.ssb\$INDEX\n";
print R "all.simulated.tsb <- simulated.tsb\$V2\n";

print R "}\n";

print R "for(i in 2:$last){\n";
print R "ica.data<-read.ica(paste(\"$DataPath/\",i,\"\/ica.out\",sep=\"\"))\n";
print R "simulated.recruitment <- read.table(file = paste(\"$DataPath/\",i,\"/simulated.recruitment\", sep=\"\"), header = TRUE)\n";
print R "simulated.ssb <- read.table(file = paste(\"$DataPath/\",i,\"/simulated.ssb\", sep = \"\"), skip = 2, header = TRUE)\n";
print R "simulated.tsb <- read.table(file = paste(\"$DataPath/\",i,\"/simulated.TSB\", sep = \"\"), skip = 1)\n";

print R "all.ica.data<-rbind(all.ica.data,ica.data\$data)\n";
print R "all.simulated.rec <- c(all.simulated.rec, simulated.recruitment[simulated.recruitment\$year %in% 1972:2002, \"number\"] * 1e-3)\n";
print R "all.simulated.ssb <- c(all.simulated.ssb, simulated.ssb\$INDEX)\n";
print R "all.simulated.tsb <- c(all.simulated.tsb, simulated.tsb\$V2)\n";

print R "}\n";

# COMPARE THE RECRUITMENT
print R "ps.options(horizontal=FALSE, onefile=FALSE, paper=\"special\", width=8, height=6)\n";
print R "postscript(\"/home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT/Tests/Results/$DirPath/comparison-recruitment.ps\", onefile=FALSE)\n";

print R "print(dim(all.ica.data))\n";
print R "print(length(all.simulated.rec))\n";

print R "plot(all.ica.data[,1], all.simulated.rec,xlab=\"Open ICA estimates\", ylab=\"Simulated\", las=1)\n";
print R "title(\"Recruitment (thousands of individuals)\")\n";
print R "abline(0,1)\n";

print R "dev.off()\n";

# # COMPARE THE TSB
print R "ps.options(horizontal=FALSE, onefile=FALSE, paper=\"special\", width=8, height=6)\n";
print R "postscript(\"/home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT/Tests/Results/$DirPath/comparison-tsb.ps\", onefile=FALSE)\n";

print R "plot(all.ica.data[,2], all.simulated.tsb,xlab=\"Open ICA estimates\",ylab=\"Simulated\", las=1)\n";
print R "title(\"Total stock biomass\")\n";
print R "abline(0,1)\n";

print R "dev.off()\n";

# COMPARE THE SSB
print R "ps.options(horizontal=FALSE, onefile=FALSE, paper=\"special\", width=8, height=6)\n";
print R "postscript(\"/home/mkienzle/CSIRO/cmar_projects/Fortran/ICA/OpenICAMINUIT/Tests/Results/$DirPath/comparison-ssb.ps\", onefile=FALSE)\n";

print R "plot(all.ica.data[,3], all.simulated.ssb,xlab=\"Open ICA estimates\",ylab=\"Simulated\", las=1)\n";
print R "title(\"Spawning stock biomass\")\n";
print R "abline(0,1)\n";

print R "dev.off()\n";

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
