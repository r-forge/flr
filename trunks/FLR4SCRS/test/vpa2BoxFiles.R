#### VPA2Box Files #############################################################
VPA2Files<-list()
#### Main IO files
VPA2Files$main<-c(
   "Bfte2010.c1",  #### Control file
   "bfte2010.csv", #### Excel interface file
   "Bfte2010.d1",  #### Inputs
   "bfte2010.e1",  #### Parameter estimates
   "Bfte2010.p1")  #### Parameter control file

#### Retrospective files
VPA2Files$retro<-c(
  "minus0.r",
  "minus0.est",
  "minus0.spd")

#### Binary output files for bootstrap results
VPA2Files$out<-c(
  "NAA.OUT"        ## numbers-at-age
  "OBJ.OUT"
  "TERM.OUT"
  "BAD.OUT"        ## bad bootstrap by iter
  "BOOTSTRP.OUT"
  "CAA.OUT"        ## catch-at-age
  "FAA.OUT"        ## F-at-age
  "IND.OUT"        ## numbers-at-age
  "MAA.OUT")       ## M-at-age

###### Pro2Box Files ###########################################################
"Prj10.ctl" ## control file
#####
VPA2Files$prjI<-c(
  "caa.txt",      ## catch-at-age
  "discards.txt", ## dscards-at-age
  "maa.txt",      ## M at age
  "Quotas.txt",   ## catch/effort scenarios to run.
  "recruit.txt",  ## recruitment options
  "sel.txt",      ## selectivity
  "Trans.txt",    ## mixing
  "waa.txt")      ## wts-at-age

#### results by iteration, year & age
VPA2Files$prjOaa<-c(
  "faa.out",      ## F-at-age
  "naa.out")      ## N-at-age

#### Summary results by iteration
VPA2Files$prjO<-c(
  "BENCH-1.OUT",  ## Reference points
  "BIO_f-1.OUT",  ## Biomass ? by iteration and year
  "BIO_t-1.OUT",  ## Biomass ? by iteration and year
  "Fapex-1.OUT",  ## F Apex by iteration and year
  "Fcurr-1.OUT",  ## F ? by iteration and year
  "RECRT-1.OUT",  ## Recuits by iteration and year
  "sel.OUT",      ## F Apex by iteration and year
  "SR_PARMS.OUT", ## SRR by ?
  "SSBIO-1.OUT",  ## SSB by iteration and year
  "SSNUM-1.OUT",  ## ? by iteration and year
  "YIELD-1.OUT")  ## Yield by iteration and year

#### summarised across iterations
VPA2Files$prjOsta<-c(
  "BENCH-1.STA",  ## Reference points
  "BIO_f-1.STA",  ## Biomass ? by iteration and year
  "BIO_t-1.STA",  ## Biomass ? by iteration and year
  "Fapex-1.STA",  ## F Apex by iteration and year
  "Fcurr-1.STA",  ## F ? by iteration and year
  "RECRT-1.STA",  ## Recuits by iteration and year
  "SSBF01-1.STA", ## F Apex by iteration and year
  "SSBF20-1.STA", ## SSB / F20% Virgin SPR
  "SSBF30-1.STA", ## SSB / F30% Virgin SPR
  "SSBF40-1.STA", ## SSB / F40% Virgin SPR
  "SSBIO-1.STA",  ## SSB
  "SSBMSY-1.STA", ## SSB / BMSY
  "SSNUM-1.STA",  ## SSB numbers?
  "YIELD-1.STA")  ## Yield
