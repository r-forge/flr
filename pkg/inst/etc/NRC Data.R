    nrc  <-read.FLStock("d:\\FLR\\Packages\\FLBREM\\inst\\etc\\NRCind.txt")
    nrc.1<-read.FLStock("d:\\FLR\\Packages\\FLBREM\\inst\\etc\\set1NRCind.txt")
    nrc.2<-read.FLStock("d:\\FLR\\Packages\\FLBREM\\inst\\etc\\set2NRCind.txt")
    nrc.3<-read.FLStock("d:\\FLR\\Packages\\FLBREM\\inst\\etc\\set3NRCind.txt")
    nrc.4<-read.FLStock("d:\\FLR\\Packages\\FLBREM\\inst\\etc\\set4NRCind.txt")
    nrc.5<-read.FLStock("d:\\FLR\\Packages\\FLBREM\\inst\\etc\\set5NRCind.txt")
    
    nrc.cpue  <-readIndices.VPA("d:\\FLR\\Packages\\FLBREM\\inst\\etc\\NRCcpue.txt")
    nrc.cpue.1<-readIndices.VPA("d:\\FLR\\Packages\\FLBREM\\inst\\etc\\set1NRCcpue.txt")
    nrc.cpue.2<-readIndices.VPA("d:\\FLR\\Packages\\FLBREM\\inst\\etc\\set2NRCcpue.txt")
    nrc.cpue.3<-readIndices.VPA("d:\\FLR\\Packages\\FLBREM\\inst\\etc\\set3NRCcpue.txt")
    nrc.cpue.4<-readIndices.VPA("d:\\FLR\\Packages\\FLBREM\\inst\\etc\\set4NRCcpue.txt")
    nrc.cpue.5<-readIndices.VPA("d:\\FLR\\Packages\\FLBREM\\inst\\etc\\set5NRCcpue.txt")
    
    ## save data
    save(adult,     file="D:\\FLR\\Packages\\FLBREM\\data\\adult.RData")
    save(rec,       file="D:\\FLR\\Packages\\FLBREM\\data\\rec.RData")
    save(nrc,       file="D:\\FLR\\Packages\\FLBREM\\data\\nrc.RData")
    save(nrc.1,     file="D:\\FLR\\Packages\\FLBREM\\data\\nrc.1.RData")
    save(nrc.2,     file="D:\\FLR\\Packages\\FLBREM\\data\\nrc.2.RData")
    save(nrc.3,     file="D:\\FLR\\Packages\\FLBREM\\data\\nrc.3.RData")
    save(nrc.4,     file="D:\\FLR\\Packages\\FLBREM\\data\\nrc.4.RData")
    save(nrc.5,     file="D:\\FLR\\Packages\\FLBREM\\data\\nrc.5.RData")
    save(nrc.cpue,  file="D:\\FLR\\Packages\\FLBREM\\data\\nrc.cpue.RData")
    save(nrc.cpue.1,file="D:\\FLR\\Packages\\FLBREM\\data\\nrc.1.cpue.RData")
    save(nrc.cpue.2,file="D:\\FLR\\Packages\\FLBREM\\data\\nrc.2.cpue.RData")
    save(nrc.cpue.3,file="D:\\FLR\\Packages\\FLBREM\\data\\nrc.3.cpue.RData")
    save(nrc.cpue.4,file="D:\\FLR\\Packages\\FLBREM\\data\\nrc.4.cpue.RData")
    save(nrc.cpue.5,file="D:\\FLR\\Packages\\FLBREM\\data\\nrc.5.cpue.RData")

## set up adult index based upon data in biomass.dat
adult          <-new("FLIndexSurvey")
adult@index    <-FLQuant(c(11321.158,6453.406, 2757.817, 6628.615,
                           3382.021, 4883.180, 2928.896, 3580.033,
                           2213.013, 3804.548, 4446.052, 4283.051,
                           2662.460, 2445.235, 2837.943, 2179.990,
                           1543.419, 1406.579, 1945.253, 1540.172,
                           1600.833, 1176.406, 1517.689,  864.215, 
                           757.389,   574.303,  350.916,  642.183, 
                           508.770,   416.287), 
                         dimnames=list(age="all",year=1:30,unit="unique",area="all",season="unique"))

