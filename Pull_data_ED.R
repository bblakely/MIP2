source('Pull_fcomp_ED_v3.R')
library(abind)

georef.name<-load('PalEON_siteInfo_all.RData')
georef<-paleon


var.want<-c('SW_albedo','LAI', 'SWE')
  #c('SW_albedo','swdown','LWnet', 'lwdown', 'Qh', 'Qle','LAI','SWE','evap','tair','precipf','SoilMoist','qair','wind', 'transp','SnowDepth')
varchunk<-list()

for (v in 1:length(var.want)){
  name<-paste('ED2/ED2.',var.want[v],'.rds', sep='')
  datvar<-readRDS(name)
  
  if(length(dim(datvar))==3){  #If PFT level variable, collapse to whole grid cell
    datvar<-apply(datvar,c(1:2),FUN=sum, na.rm=TRUE)
    beep(7)
  }
  
  #goodpix<-which((datvar[1,]!=(-9999)))  
  goodpix<-which(fcomp.slice[1,]!=-9999)  #This is weird - two more no data columns in fcomp than in datvar
  
  goodlon<-georef$lon[goodpix]
  goodlat<-georef$lat[goodpix]
  
  npix<-length(goodpix) #To use later for easier subsetting
  
  
  datvar.good<-datvar[,goodpix]
  rm('datvar')
  
  datvar.trim<-datvar.good[datvar.good[,1]!=-9999,]
  rm('datvar.good')
  
  datvar.match<-datvar.trim[601:13800,]
  
  datvar.df<-data.frame(datvar.match)
  rm('datvar.match')
  
  varchunk[[v]]<-datvar.df
  print(v)
}

names(varchunk)<-var.want

master<-array(unlist(varchunk), c(nrow(varchunk[[1]]),npix,length(var.want)))

rm('varchunk')

timeref<-DummyTS
months<-rep(1:12,1100)

eg.ancil<-eg.comp.match[,regimeshift==1]
dc.ancil<-dc.comp.match[,regimeshift==1]


series.cells<-abind(master[,regimeshift==1,], eg.ancil, dc.ancil)


datasubset<-list()
for (l in 1:(ncol(series.cells))){
  shiftdat<-list()
  sub<-varset[[l]] #Each of these are the starts, ends, and lengths of a shift
  if(nrow(sub)!=0){
    for (c in 1:nrow(sub)){
      shiftdat[[c]]<-series.cells[which(timeref %in% ann.ts[sub[c,1]:sub[c,2]]),l,]  #gives one more record than needed
      timestamp.all<-cbind(timeref, months)
      timestamp<-timestamp.all[which(timeref %in% ann.ts[sub[c,1]:sub[c,2]]),]
      shiftdat[[c]]<-cbind(shiftdat[[c]],timestamp)
      colnames(shiftdat[[c]])<-c(var.want,'egcomp','dccomp','year','month')
       }
    
    datasubset[[l]]<-shiftdat
  }else{datasubset[[l]]<-NA}
}

rm('master')
#Cleanup loop

goodvar<-rep(0, length(varset))
for(i in 1:length(varset)){
  if(!is.na(datasubset[[i]])){goodvar[i]<-1} #Throws errors, but they don't impair function (all it does is clip to the number of existing shift locations)
}

datasubset<-datasubset[which(goodvar==1)]
coords.use<-coords[which(goodvar==1),]

datasubset->datasubset.ed
coords.use->coords.ed
