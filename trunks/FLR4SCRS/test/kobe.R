##### Kobe I ############################################################################################3
ggplotKobe<-function(x,xlim=c(0,2),ylim=xlim){
       quads<- rbind(data.frame(x=c(-Inf,-Inf,Inf,Inf), y=c(-Inf,Inf,Inf,-Inf), fill=as.factor("yellow")),
                     data.frame(x=c(   1,   1,Inf,Inf), y=c(-Inf,  1,  1,-Inf), fill=as.factor("green")),
                     data.frame(x=c(-Inf,-Inf,  1,  1), y=c(   1,Inf,Inf,   1), fill=as.factor("red")))

       ggplot(x)+geom_polygon(data=quads,aes(x,y,fill=fill)) +
                   scale_fill_manual(values = c("yellow","green","red"), legend=FALSE) +
                   ylab(expression(F/F[MSY]))        +
                   xlab(expression(SSB/B[MSY]))      +
                   scale_y_continuous(limits=ylim)   +
                   scale_x_continuous(limits=xlim)}

t.<-TS[TS$Quantile=="50%" & TS$TAC %in% c(0,4000,8000,13500,16000,20000) & TS$Year %in% 2009:2020 & TS$VPA %in% c(13,15),]

kobe(t.) + geom_line(aes(SSB,F,group=Recruits:Implementation, lty=Implementation, col=Recruits, size=1))   +
           facet_grid(VPACatch~TAC) +
           scale_size(legend=FALSE)

kobe(t.) + geom_line(aes(SSB,F,group=Recruits:Implementation, lty=Implementation, col=Recruits, size=1))   +
           facet_grid(VPACatch~TAC) +
           scale_size(legend=FALSE) +scale_x_continuous(limits=c(0,5))

mc<-dbGetQuery(conMC, "SELECT Catch, Recruits, VPA, Implementation, year, fapex, TAC, f0_1, b0_1 FROM 'prj' WHERE VPA in (13,15) and Year = 2020")

##### Kobe Rasta ###########################################################################################

##### Kobe matrix ###########################################################################################
