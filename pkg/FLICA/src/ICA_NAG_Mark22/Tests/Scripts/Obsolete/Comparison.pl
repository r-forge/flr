#!/usr/bin/perl -w

# PURPOSE: compare the fit between the old and new ICA version
# DATE: 1 June 2005
# LAST MODIFIED: 15 July 2005
# AUTHOR: kienzle_marco@bigfoot.com

#DEAL WITH THE OPTION

use Getopt::Long;

GetOptions("h"    => \$help,
           "d=s"  => \$DirPath);

if($help){print "OPTION: this program need the path of the directory containing the data files\n";
          print "d: the directory containing the files\n";
          print "USAGE:  -d=Absolute-NoVariability-NoSSBbias-NoCatchBias-SSBsurveyNVNVNV\n";
          exit();}


$OldVersionPath = "/home/mkienzle/Websites/OpenSourceICA/OpenSourceICA/test/datasets/ICAv1.4w/$DirPath";
$NewVersionPath = "/home/mkienzle/Websites/OpenSourceICA/OpenSourceICA/test/datasets/ICAv1.4x/$DirPath";

# Get the number of subdirectories
$SubDirNb=`tree -d $NewVersionPath | grep directories`; chomp($SubDirNb); $SubDirNb =~ /(\d{1,5})\sdirectories/;
$last=$1;

# Begin to talk to R
open(R, "| R --vanilla ");

# Useful to see the graphical output while running
#print R "x11()\n";

print R "source(\"/home/mkienzle/Programs/R/ExtractSummaryTableFromICAoutput.R\")\n";

print R "for(i in 1:1){\n";
print R "old<-read.ica(paste(\"$OldVersionPath/\",i,\"\/ica.out\",sep=\"\"))\n";
print R "new<-read.ica(paste(\"$NewVersionPath/\",i,\"\/ica.out\",sep=\"\"))\n";

print R "olddata<-old\$data\n";
print R "newdata<-new\$data\n";

print R "}\n";

print R "for(i in 2:$last){\n";
print R "old<-read.ica(paste(\"$OldVersionPath/\",i,\"\/ica.out\",sep=\"\"))\n";
print R "new<-read.ica(paste(\"$NewVersionPath/\",i,\"\/ica.out\",sep=\"\"))\n";

print R "olddata<-rbind(olddata,old\$data)\n";
print R "newdata<-rbind(newdata,new\$data)\n";

print R "}\n";

#print R "print(olddata)\n";
#print R "print(newdata)\n";

# COMPARE THE RECRUITMENT
print R "ps.options(horizontal=FALSE, onefile=FALSE, paper=\"special\", width=8, height=6)\n";
print R "postscript(\"$NewVersionPath/comparison-recruitment.ps\", onefile=FALSE)\n";

print R "print(dim(olddata))\n";
print R "print(dim(newdata))\n";

print R "plot(olddata[,1],newdata[,1],xlab=\"version 1.4w\",ylab=\"version 1.4x\", las=1)\n";
print R "title(\"Recruitment\")\n";
print R "abline(0,1)\n";

print R "dev.off()\n";

# COMPARE THE TSB
print R "ps.options(horizontal=FALSE, onefile=FALSE, paper=\"special\", width=8, height=6)\n";
print R "postscript(\"$NewVersionPath/comparison-tsb.ps\", onefile=FALSE)\n";

print R "plot(olddata[,2],newdata[,2],xlab=\"version 1.4w\",ylab=\"version 1.4x\", las=1)\n";
print R "title(\"Total stock biomass\")\n";
print R "abline(0,1)\n";

print R "dev.off()\n";

# COMPARE THE SSB
print R "ps.options(horizontal=FALSE, onefile=FALSE, paper=\"special\", width=8, height=6)\n";
print R "postscript(\"$NewVersionPath/comparison-ssb.ps\", onefile=FALSE)\n";

print R "plot(olddata[,3],newdata[,3],xlab=\"version 1.4w\",ylab=\"version 1.4x\", las=1)\n";
print R "title(\"Spawning stock biomass\")\n";
print R "abline(0,1)\n";

print R "dev.off()\n";

# COMPARE THE Fbar
print R "ps.options(horizontal=FALSE, onefile=FALSE, paper=\"special\", width=8, height=6)\n";
print R "postscript(\"$NewVersionPath/comparison-Fbar.ps\", onefile=FALSE)\n";

print R "plot(olddata[,6],newdata[,6],xlab=\"version 1.4w\",ylab=\"version 1.4x\", las=1)\n";
print R "title(\"Fbar\")\n";
print R "abline(0,1)\n";

print R "dev.off()\n";

# Stop talking to R
print R "q()\n";
print R "y\n";
close(R);
