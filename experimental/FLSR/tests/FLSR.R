library(odfWeave)

odfWeaveControl(zipCmd = c("C:/Program Files/WinRAR/rar A $$file$$ .", "C:/Program Files/WinRAR/rar X $$file$$"))

iFLSR  <- "C:/Stuff/FLR/help/Sweave/FLSRSweave.odt"
oFLSR <- "C:/Stuff/FLR/help/text/FLSR.odt"

odfWeave(iFLSR, oFLSR)
