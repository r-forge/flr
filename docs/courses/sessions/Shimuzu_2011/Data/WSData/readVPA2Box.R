#### R scripts for reading in data, will become part of FLR in release 3.0
source("\\\\192.168.100.101\\flr_2011\\FLR\\Data\\createFL.R")

#### R scripts for reading in data as FLStocks
yft      <-FLStocks()
yft$cont <-readVPA2Box("//192.168.100.101/flr_2011/FLR/Data/inputs/VPA2Box/YFT/CONT/yft2008.ctl")
yft$run3 <-readVPA2Box("//192.168.100.101/flr_2011/FLR/Data/inputs/VPA2Box/YFT/Run 3/yft2008.ctl")
yft$run4 <-readVPA2Box("//192.168.100.101/flr_2011/FLR/Data/inputs/VPA2Box/YFT/Run 4/yft2008.ctl")
yft$run5 <-readVPA2Box("//192.168.100.101/flr_2011/FLR/Data/inputs/VPA2Box/YFT/Run 5/yft2008.ctl")
yft$run6 <-readVPA2Box("//192.168.100.101/flr_2011/FLR/Data/inputs/VPA2Box/YFT/Run 6/yft2008.ctl")
yft$run7 <-readVPA2Box("//192.168.100.101/flr_2011/FLR/Data/inputs/VPA2Box/YFT/Run 7/yft2008.ctl")
yft$run8 <-readVPA2Box("//192.168.100.101/flr_2011/FLR/Data/inputs/VPA2Box/YFT/Run 8/yft2008.ctl")
yft$run9 <-readVPA2Box("//192.168.100.101/flr_2011/FLR/Data/inputs/VPA2Box/YFT/Run 9/yft2008.ctl")
yft$run10<-readVPA2Box("//192.168.100.101/flr_2011/FLR/Data/inputs/VPA2Box/YFT/Run 10/yft2008.ctl")

##### some data are not easily found in vpa files so add now
## function
missingStuff<-function(x) {
              mat(x)[]         <-c(0,0,0.5,1,1,1)
              m.spwn(x)[]      <-0.5
              harvest.spwn(x)[]<-0.5
              m(x)             <-0.1

              return(x)}

### use apply, i.e. run function on each element of list object
yft<-lapply(yft,missingStuff)

plot(yft)

