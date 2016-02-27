library(plotrix)
importancia=attStats(f.boruta)
y1=importancia$medianImp[order(importancia$medianImp,decreasing = T)]
x1=row.names(importancia[order(importancia$medianImp,decreasing = T),])
y2=apply(train,2,function(i){length(which(is.na(i)))})

y2=y2[match(x1,names(y2))]

twoord.plot(lx=c(1:100),rx=c(1:100),ly=y1[1:100],ry=log(y2[1:100]+1),lylim=c(0,max(y1[1:100])*1.1),
            rylim=c(0,log(max(y2[1:100]))*1.1),xlim=c(0,100),xticklab=x1[1:100])
