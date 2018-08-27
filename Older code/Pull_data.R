library(ncdf4)
#regismeshift, DummyTS

files<-list.files(pattern='.nc')

master.series.dat<-matrix(NA,0,40)  #Empty matrices for whole timeseries

var.want<-c("SW_albedo", "Qh")
varchunk<-list()
  #array(data=NA,c(1200*length(files),40,length(var.want)))

for (f in 1:length(files)){   #length(files)
  
  file<-files[f]
  ex.open<-nc_open(file)  #open the file. Takes some time
  
  lon<-ncvar_get(ex.open, "lon")
  lat<-ncvar_get(ex.open, "lat")
  
  time<-ncvar_get(ex.open, "time")
  vars<-names(ex.open$var)
  #comp<-ncvar_get(ex.open, "Fcomp")  #also takes some time
  
  coords.lon<-matrix(data=rep(lon,30),nrow=80,ncol=30)
  coords.lat<-matrix(data=rep(lat,each=80),nrow=80,ncol=30)
  
  
  #variable loop
  
  for (v in 1:length(var.want)){
    
  datvar<-ncvar_get(ex.open, var.want[v])
  
  goodpix<-which(!is.na(datvar[,,1]), arr.ind=T) #where is comp @ all lons, all lats, PFT6, and timestep 1 real data
  goodpix.dumb<-which(!is.na(datvar[,,1]))  #Same thing, but leveraging R's way of indexing matrices as vectors; counts down column 1 then moves onto column 2 and counts down that... 
  
  goodlat<-coords.lat[goodpix.dumb]  
  goodlon<-coords.lon[goodpix.dumb]
  
  datvar.df<-array(data=9999,dim=c(1200,nrow(goodpix)))  #Empty matrix with each column being a good grid cell, each row a timepoint and each slice a PFT
  
  for(i in 1:nrow(goodpix)){
    #plot(datvar[unname(goodpix[i,1]), unname(goodpix[i,2]),], type='l')
    datvar.df[,i]<-t(datvar[unname(goodpix[i,1]), unname(goodpix[i,2]),])  #Crazy indexing
  }
  
  if(f!=1){varchunk[[v]]<-rbind(varchunk[[v]],datvar.df)}else{varchunk[[v]]<-datvar.df}

  }
  
  names(varchunk)<-var.want
    
  print(f)
  nc_close(ex.open)
  
  #master.series.dat<-rbind(master.series.dat, datvar.df)  
}

#unlist varchunk

master<-array(unlist(varchunk), c(1200*length(files),40,length(var.want)))
series.cells<-master[,regimeshift==1,]
  
  #series.cells<-data.frame(series.cells)
  timeref<-DummyTS
  
  datasubset<-list()
  for (l in 1:(ncol(series.cells))){  #-1 because the last column in series.cells is the time
  shiftdat<-list()
  sub<-varset[[l]]
  if(nrow(sub)!=0){
  for (c in 1:nrow(sub)){
  shiftdat[[c]]<-series.cells[which(timeref %in% ann.ts[sub[c,1]:sub[c,2]]), l,]  #gives one more record than needed (don't care that much)
  }
  datasubset[[l]]<-shiftdat
  }
  }

  