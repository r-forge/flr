#### R scripts for reading in data, will become part of FLR in release 3.0
source("\\\\192.168.100.101\\flr_2011\\FLR\\Data\\createFL.R")

## Multifan-CL
alb<-read.mfcl(c("\\\\192.168.100.101\\flr_2011\\FLR\\Data\\inputs\\MFCL\\plot-09.par.rep",
                 "\\\\192.168.100.101\\flr_2011\\FLR\\Data\\inputs\\MFCL\\plot-09.par.rep"))

