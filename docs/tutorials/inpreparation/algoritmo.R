library(odfWeave)
odfWeave("fileIn.odt", "fileOut.odt")


# panel for compare stock assessment methods

pfun <- function(...){
	panel.bwplot(...)
	dd <- as.data.frame(eval.parent(ssbTrue, 0))
	dd$year <- as.numeric(factor(dd$year))
	panel.lines(dd$data~dd$year, type="l", col=1)
}


