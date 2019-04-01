source('Pull_fcomp_JULES_v2.R')
library(abind)

georef.name<-load('PalEON_siteInfo_all.RData')
georef<-paleon

var.want<-c('SW_albedo','LAI', 'SWE', 'SoilMoist')
#c('SW_albedo','LWnet', 'lwdown', 'Qh', 'Qle','LAI','SWE','evap','tair','precipf','SoilMoist','qair','wind')
varchunk<-list()

comp.weight<-readRDS(paste('TRIFFID/TRIFFID.','fcomp','.rds', sep=''))[,,1:5]


for (v in 1:length(var.want)){
  name<-paste('TRIFFID/TRIFFID.',var.want[v],'.rds', sep='')
  datvar<-readRDS(name)
  
  if(length(dim(datvar))==3& var.want[v]!="SoilMoist"){  #If PFT level variable, collapse to whole grid cell
    datvar<-datvar[,,1:5]
    datvar.weight<-comp.weight*datvar
    datvar<-apply(datvar.weight,c(1:2),FUN=sum, na.rm=TRUE)
  }
  
  if(var.want[v]=="SoilMoist"){
    datvar<-datvar[,,4]
  }

  #goodpix<-which((datvar[1,]!=(-9999)))  
  goodpix<-which(fcomp.slice[1,]>-9999)  #This is weird - two more no data columns in fcomp than in datvar
  
  goodlon<-georef$lon[goodpix]
  goodlat<-georef$lat[goodpix]
  
  npix<-length(goodpix) #To use later for easier subsetting
  
  
  datvar.good<-datvar[,goodpix]
  datvar.trim<-datvar.good[datvar.good[,1]!=-9999,]
  datvar.match<-datvar.trim[601:13800,]
 
  datvar.df<-data.frame(datvar.match)
  varchunk[[v]]<-datvar.df
  print(v)
}

rm('comp.weight', 'datvar.weight', 'datvar.good','datvar')

names(varchunk)<-var.want

master<-array(unlist(varchunk), c(nrow(varchunk[[1]]),npix,length(var.want)))

timeref<-DummyTS
months<-rep(1:12,1100)

eg.ancil<-eg.comp.match[,regimeshift==1]
dc.ancil<-dc.comp.match[,regimeshift==1]

series.cells<-abind(master[,regimeshift==1,], eg.ancil, dc.ancil)


datasubset<-list()
for (l in 1:(ncol(series.cells))){
  shiftdat<-list()
  sub<-varset[[l]]
  if(nrow(sub)!=0){
    for (c in 1:nrow(sub)){
      shiftdat[[c]]<-series.cells[which(timeref %in% ann.ts[sub[c,1]:sub[c,2]]),l,]  #gives one more record than needed (don't care that much)
      timestamp.all<-cbind(timeref, months)
      timestamp<-timestamp.all[which(timeref %in% ann.ts[sub[c,1]:sub[c,2]]),]
      shiftdat[[c]]<-cbind(shiftdat[[c]],timestamp)
      colnames(shiftdat[[c]])<-c(var.want,'egcomp','dccomp','year','month')
    }
    
    datasubset[[l]]<-shiftdat
  }else{datasubset[[l]]<-NA}
}


#Cleanup loop

goodvar<-rep(0, length(varset))
for(i in 1:length(varset)){
  if(!is.na(datasubset[[i]])){goodvar[i]<-1}
}

datasubset<-datasubset[which(goodvar==1)]
coords.use<-coords[which(goodvar==1),]


datasubset->datasubset.jules
coords.use->coords.jules
