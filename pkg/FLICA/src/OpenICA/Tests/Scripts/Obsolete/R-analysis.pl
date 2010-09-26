#!/usr/bin/perl -w

# PURPOSE: compare the estimated fishing mortality from the old and new ICA version and the simulated data
# DATE: 9 September 2005
# LAST MODIFIED: 22 August 2007
# AUTHOR: kienzle_marco@hotmail.com

#DEAL WITH THE OPTION

use Getopt::Long;

GetOptions("h"    => \$help,
           "d=s"  => \$DirPath);

if($help){print "OPTION: this program need the path of the directory containing the data files\n";
          print "d: the directory containing the files\n";
          print "USAGE:  -d=Relative-FisheryVar-NoNatMort-NoSSBbias-NoCatchBias-SSBsurveyNANACS\n";
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
print R "for(number in 1:$last){\n";


print R "ps.options(horizontal=FALSE, onefile=FALSE, paper=\"special\", width=8, height=6)\n";
print R "postscript(paste(\"$NewVersionPath/\",number,\"/Cmp-FishMort.ps\",sep=\"\"), onefile=FALSE)\n";

print R "simulated <- read.table(paste(\"$OldVersionPath/\",number,\"/simulated.fishing.mortality\",sep=\"\"))\n";

print R "estimated.by.x <- read.table(paste(\"$NewVersionPath/\",number,\"/ica.f\",sep=\"\"), skip=5, colClasses=\"numeric\")\n";

print R "estimated.by.w <- read.table(paste(\"$OldVersionPath/\",number,\"/ica.f\",sep=\"\"), skip=5, colClasses=\"numeric\")\n";

print R "par(mfrow=c(2,2))\n";
print R "plot(as.matrix(simulated[1,1:9]), as.matrix(estimated.by.w[1,1:9]), xlab=\"simulated\", ylab=\"version w\");for(i in 2:9){points(as.matrix(simulated[i,1:9]), as.matrix(estimated.by.w[i,1:9]))}\n";
print R "abline(0,1)\n";

print R "plot(as.matrix(simulated[1,1:9]), as.matrix(estimated.by.x[1,1:9]),xlab=\"simulated\", ylab=\"version x\");for(i in 2:9){points(as.matrix(simulated[i,1:9]), as.matrix(estimated.by.x[i,1:9]))}\n";
print R "abline(0,1)\n";

print R "plot(as.matrix(estimated.by.w[1,1:9]), as.matrix(estimated.by.x[1,1:9]),xlab=\"version w\", ylab=\"version x\");for(i in 2:9){points(as.matrix(estimated.by.w[i,1:9]), as.matrix(estimated.by.x[i,1:9]))}\n";
print R "abline(0,1)\n";

print R "dev.off()\n";

##### Compare the SSB
print R "ps.options(horizontal=FALSE, onefile=FALSE, paper=\"special\", width=8, height=6)\n";
print R "postscript(paste(\"$NewVersionPath/\",number,\"/Cmp-ssb.ps\",sep=\"\"), onefile=FALSE)\n";

print R "source(\"/home/mkienzle/Programs/R/ExtractSummaryTableFromICAoutput.R\")\n";

print R "old<-read.ica(paste(\"$OldVersionPath/\",number,\"/ica.out\",sep=\"\"))\n";
print R "new<-read.ica(paste(\"$NewVersionPath/\",number,\"/ica.out\",sep=\"\"))\n";

print R "simulated <- read.table(paste(\"$OldVersionPath/\",number,\"/simulated.ssb\",sep=\"\"), skip=2, header=TRUE)\n";

print R "par(mfrow=c(2,2))\n";
print R "plot(simulated[,3], old\$data[,3], xlab=\"simulated\", ylab=\"version w\",ylim=c(min(old\$data[,3],new\$data[,3]),max(old\$data[,3],new\$data[,3])))\n";
print R "library(modreg)\n";
print R "lines(supsmu(simulated[,3], old\$data[,3]),col=2,lwd=2)\n";
print R "abline(0,1)\n";


print R "plot(simulated[,3], new\$data[,3], xlab=\"simulated\", ylab=\"version x\", ylim=c(min(old\$data[,3],new\$data[,3]),max(old\$data[,3],new\$data[,3])))\n";
print R "lines(supsmu(simulated[,3], new\$data[,3]),col=2,lwd=2)\n";
print R "abline(0,1)\n";

print R "plot(old\$data[,3], new\$data[,3], xlab=\"version w\", ylab=\"version x\")\n";
print R "lines(supsmu(old\$data[,3], new\$data[,3]),col=2,lwd=2)\n";
print R "abline(0,1)\n";

print R "dev.off()\n";

# ANCOVA
print R "id=c(rep(1,length((new\$data[,3]))),rep(2,length((old\$data[,3]))))\n";
print R "xx<-c(new\$data[,3],old\$data[,3])\n";
print R "yy<-rep(simulated[,3],2)\n";
print R "mod.e=lm(yy~factor(id)*xx)\n";
print R "mod.p=lm(yy~factor(id)/xx-1)\n";

#print R "print(summary(mod.e)); print(summary(mod.p))\n";
print R "results <- cbind(number,summary(mod.e)\$coefficients[4,4], summary(mod.p)\$coefficients[3,1],summary(mod.p)\$coefficients[4,1], sum((new\$data[,3]-simulated[,3])^2), sum((old\$data[,3]-simulated[,3])^2))\n";

print R "write.table(results,file=paste(\"$NewVersionPath/\",number,\"/Ancova.txt\",sep=\"\"), col.names=FALSE, row.names=FALSE, quote=FALSE)\n";

print R "}\n"; # End of the for loop
 
# Stop talking to R
print R "q()\n";
print R "y\n";
close(R);

# GROUP TOGETHER ALL THE RESULTS OF THE SIMULATION
system("rm -f $NewVersionPath/Ancova.txt");
system("echo \"This file contains the quantification of the differences between the SSB estimation from ICA version x and w\" > $NewVersionPath/Ancova.txt");

system("echo \"The format is: dataset number, probability of interaction, slope version x, slope version w\" >> $NewVersionPath/Ancova.txt");


system("cd $NewVersionPath; cat */Ancova.txt >> Ancova.txt");
