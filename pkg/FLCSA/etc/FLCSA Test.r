library(FLCore)

source("C:\\Stuff\\FLR\\pkg\\FLCSA\\R\\FLCSA.r")
source("C:\\Stuff\\FLR\\pkg\\FLCSA\\R\\FLCSA IO.r")

file.   <-"C:\\Stuff\\FLR\\pkg\\FLCSA\\etc\\CSAData.csv"
s.      <-readCSAFile(file.)
i.      <-readIndicesCSA(file.)

FLCSA(s.,i.)
