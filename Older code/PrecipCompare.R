jules.precip<-readRDS('TRIFFID/TRIFFID.precipf.rds')
ed.precip<-readRDS('ED2/ED2.precipf.rds')

edpix<-which(ed.precip[1,]!=-9999)
julespix<-which(jules.precip[1,]!=-9999)
  
jules.precip1<-jules.precip[jules.precip[,13]!=-9999,]
#jules.precip2<-jules.precip1[,jules.precip1[1,]!=-9999]

#ed.precip2<-ed.precip[,ed.precip[1,]!=-9999]


for(p in julespix[which(julespix %in% edpix)]){
plot(jules.precip1[,p]~ed.precip[1:13920,p],main=paste(georef$lat[p],georef$lon[p]))
abline(0,1,col='red', lwd=2)
  }
