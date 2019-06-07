
if (!exists('datasubset.ed')){source('Pull_data_ED.R')}
if (!exists('datasubset.jules')){source('Pull_data_JULES.R')}

model<-"ed"
  "ed"
  "jules"
#library("abind")

if(model=="ed"){DAT<-datasubset.ed}
if(model=="jules"){DAT<-datasubset.jules}
#var.want[14]<-"none"  #makes ed and jules same length
#counters
skipcount<-0 #For shifts that don't meet criteria
goodcount<-0 #For shifts that do meet criteria
colnum<-0 #used with storage

#storage
laicheck<-matrix(data=NA,nrow=3, ncol=100)  #Holds min LAI 
coords.rec<-matrix(data=NA,nrow=100, ncol=2)
databin<-array(data=-9999, dim=c(12,100,length(var.want)+2)) #Holds all seasonal profiles; +2 is for added-on dc/eg shift percentages


#storage for longwave
lwdown.eg<-matrix(data=NA,nrow=12, ncol=100)
lwdown.dc<-matrix(data=NA,nrow=12, ncol=100)
LWnet.eg<-matrix(data=NA,nrow=12, ncol=100)
LWnet.dc<-matrix(data=NA,nrow=12, ncol=100)

#storage for H and LE
Qh.eg<-matrix(data=NA,nrow=12, ncol=100)
Qh.dc<-matrix(data=NA,nrow=12, ncol=100)
Qle.eg<-matrix(data=NA,nrow=12, ncol=100)
Qle.dc<-matrix(data=NA,nrow=12, ncol=100)

#storage for swdown and albedo
sd.eg<-matrix(data=NA,nrow=12, ncol=100)
sd.dc<-matrix(data=NA,nrow=12, ncol=100)
sa.eg<-matrix(data=NA,nrow=12, ncol=100)
sa.dc<-matrix(data=NA,nrow=12, ncol=100)

#par(mfrow=c(2,3))
par(mfrow=c(2,2), mar=c(2,2,3,1))

#Cutoffs
#if(model=='ed'){dominance<-0.6};if(model=='JULES'){dominance<-0.55}  #Proportion needed to count as dominance

dc.dom.val<-0.615;eg.dom.val<-0.415
gap<-24 #Number of allowable months between consecutive dominant pixels
pers<-60 #Number of months dominance must last
canopy<-1.0 #Lowest acceptable peak season LAI
peakgs<- 7 #Month which designates peak GS


for(c in 1:length(DAT)){  #Number of pixels
  for(l in 1:length(DAT[[c]])){ #Number of shifts in each pixel

dat.ex<-data.frame(DAT[[c]][l])

dc.dom<-which(dat.ex$dccomp>=dc.dom.val) #.625 for mx - dc mod, 'dominance' originally'
eg.dom<-which(dat.ex$egcomp>=eg.dom.val) # .425 for mx - dc mod

#This bit finds the last section of dominance, removing points of short fluctuations
#Can tinker with condition on breaks (i.e. use <3 if a one-year gap is acceptable)
breaks<-which(diff(dc.dom)>gap)
if(length(breaks!=0)){
last<-breaks[length(breaks)]+1
dc.fin<-dc.dom[last:length(dc.dom)]
}else{dc.fin<-dc.dom}

breaks<-which(diff(eg.dom)>gap)
if(length(breaks!=0)){
  last<-breaks[length(breaks)]+1
  eg.fin<-eg.dom[last:length(eg.dom)]
}else{eg.fin<-eg.dom}


if(length(eg.fin)>pers & length(dc.fin)>pers){
dc.profs<-aggregate(dat.ex[dc.fin,], by=list(dat.ex$month[dc.fin]), FUN=mean)
eg.profs<-aggregate(dat.ex[eg.fin,], by=list(dat.ex$month[eg.fin]), FUN=mean)


colnum<-colnum+1
#var.care<-var.want[c(1:2,4:8)]


to.plot<-c(which(colnames(dc.profs) %in% var.want), #variable cols
          which(colnames(dc.profs)=="egcomp"), which(colnames(dc.profs)=='dccomp')) #plus the EG and DC dominance cols; these will always be cbound on right after the varcols

for(p in to.plot){ 
  
  plot(dc.profs[,p], type='l', col='green', lwd=2, 
       ylim=c(min(min(dc.profs[,p]),min(eg.profs[,p])), max(max(dc.profs[,p]), max(eg.profs[,p]))),
       main=paste('Cell',c,'shift',l,colnames(dc.profs)[p]))
  lines(eg.profs[,p], type='l', col='purple', lwd=2) #For both plots on one
  #plot(eg.profs[,p], type='l', col='purple', lwd=2, #For seperate plots
       # ylim=c(min(min(dc.profs[,p]),min(eg.profs[,p])), max(max(dc.profs[,p]), max(eg.profs[,p]))),
       # main=paste('Cell',c,'shift',l,colnames(dc.profs)[p]))
  diff<-eg.profs[,p]-dc.profs[,p]
  #par(mfrow=c(1,1))
  plot(diff, type='l', main=paste('Cell',c,'shift',l,colnames(dc.profs)[p], 'diff'))
  abline(h=0)
  
  databin[,colnum,p-1]<-diff
  
  #special: If LAI, chack for closed canopy
  if(p==(which(var.want=="LAI")+1)){ 
    laicheck[1,colnum]<-(dc.profs[7,p])  #6 is for june
    laicheck[2,colnum]<-(eg.profs[7,p])
    laicheck[3,colnum]<- mean(c(dc.profs[7,p],eg.profs[7,p]))
  }
  
  #special: If lwdown or lwnet, store
  if("LWnet"%in%var.want & 'lwdown'%in%var.want){
  if (p==(which(var.want=="LWnet")+1)){
    LWnet.dc[,colnum]<-dc.profs[,p]
    LWnet.eg[,colnum]<-eg.profs[,p]
  }
  
  if (p==(which(var.want=="lwdown")+1)){
    lwdown.dc[,colnum]<-dc.profs[,p]
    lwdown.eg[,colnum]<-eg.profs[,p]
  }
  }#closes if lwdown/lwnet exists statement
  
  #special: If H and LE included, store
  if("Qh"%in%var.want & 'Qle'%in%var.want){
    if (p==(which(var.want=="Qh")+1)){
      Qh.dc[,colnum]<-dc.profs[,p]
      Qh.eg[,colnum]<-eg.profs[,p]
    }
    if (p==(which(var.want=="Qle")+1)){
      Qle.dc[,colnum]<-dc.profs[,p]
      Qle.eg[,colnum]<-eg.profs[,p]
    
    }
  }#Closes if Qh/Qle exists statement
  
  
  #special: If swdown, albedo included, store
  if("swdown"%in%var.want & 'SW_albedo'%in%var.want){
    if (p==(which(var.want=="swdown")+1)){
      sd.dc[,colnum]<-dc.profs[,p]
      sd.eg[,colnum]<-eg.profs[,p]
    }
    if (p==(which(var.want=="SW_albedo")+1)){
      sa.dc[,colnum]<-dc.profs[,p]
      sa.eg[,colnum]<-eg.profs[,p]
    }
  }#Closes if swdown/albedo exists statement
  
  
}  #Closes p-loop
goodcount<-goodcount+1
if(model=="ed"){coords.rec[colnum,]<-coords.ed[c,]}else{coords.rec[colnum,]<-coords.jules[c,]}

}else{skipcount<-skipcount+1}

} #Closes l-loop
  print(paste('cell',c,'complete'))
} #Closes c-loop


#Thin out databin to good data
databin<-databin[,databin[1,,1]!=(-9999),] #Clips off excess rows and cells with no data
#databin<-databin[,coords.rec.trim[,2]>(-99),]
closed<-apply(laicheck,FUN=min,2)[!is.na(apply(laicheck,FUN=min,2))]
databin<-databin[,closed>=1,]#Clips out low-LAI cells


#Plot diffs
par(mfrow=c(1,2))
for(d in 1:dim(databin)[3]){
  plot(-rowMeans(databin[,,d]), type='l', main=var.want[d])
  abline(h=0)
}

#Clean up LW files
lw.dc<-abind(lwdown.dc, LWnet.dc, along=3)
lw.dc<-lw.dc[,which(!is.na(lw.dc[1,,1])),]
lw.dc<-lw.dc[,closed>=1,]#Clips out low-LAI cells
lw.dc<-lw.dc[,,1]-lw.dc[,,2]

lw.eg<-abind(lwdown.eg, LWnet.eg, along=3)
lw.eg<-lw.eg[,which(!is.na(lw.eg[1,,1])),]
lw.eg<-lw.eg[,closed>=1,]#Clips out low-LAI cells
lw.eg<-lw.eg[,,1]-lw.eg[,,2]

#Clean up H/LE files
br.dc<-abind(Qh.dc, Qle.dc, along=3)
br.dc<-br.dc[,which(!is.na(br.dc[1,,1])),]
br.dc<-br.dc[,closed>=1,]#Clips out low-LAI cells
br.dc<-br.dc[,,1]/br.dc[,,2]

br.eg<-abind(Qh.eg, Qle.eg, along=3)
br.eg<-br.eg[,which(!is.na(br.eg[1,,1])),]
br.eg<-br.eg[,closed>=1,]#Clips out low-LAI cells
br.eg<-br.eg[,,1]/br.eg[,,2]


#Clean up rad files
swn.dc<-abind(sd.dc, sa.dc, along=3)
swn.dc<-swn.dc[,which(!is.na(swn.dc[1,,1])),]
swn.dc<-swn.dc[,closed>=1,]#Clips out low-LAI cells
swn.dc<-swn.dc[,,1]*(1-swn.dc[,,2])

swn.eg<-abind(sd.eg, sa.eg, along=3)
swn.eg<-swn.eg[,which(!is.na(swn.eg[1,,1])),]
swn.eg<-swn.eg[,closed>=1,]#Clips out low-LAI cells
swn.eg<-swn.eg[,,1]*(1-swn.eg[,,2])



rm("lwdown.dc","lwdown.eg","LWnet.dc","LWnet.eg", "Qh.dc","Qh.eg","Qle.dc","Qle.eg")
#clean up lai and coords
laicheck.good<-laicheck[,which(!is.na(laicheck[1,]))]

#if(model=='ed'){ #Temporary patch; this works for ED but not JULES for some reason. Won't affect final #s, mostly for mapping.
coords.rec.trim<-coords.rec[which(!is.na(coords.rec[,1])),]

lai.symb.all<-data.frame(cbind(coords.rec.trim,laicheck.good[3,]))
lai.symb<-data.frame(lai.symb.all[closed>=1,])
#}

rm("coords.rec")

#if(model=='ed'){
coords.rec1<-unique(coords.rec.trim)[(1:nrow(unique(coords.rec.trim))-1),]
points.ed.used<-data.frame(coords.rec1)
#}

if(model=="ed"){
  databin.ed<-databin
  lw.dc.ed<-lw.dc
  lw.eg.ed<-lw.eg
  br.dc.ed<-br.dc
  br.eg.ed<-br.eg
  swn.dc.ed<-swn.dc
  swn.eg.ed<-swn.eg}

if(model=="jules"){
  databin.jules<-databin
  lw.dc.jules<-lw.dc
  lw.eg.jules<-lw.eg
  br.dc.jules<-br.dc
  br.eg.jules<-br.eg
  swn.dc.jules<-swn.dc
  swn.eg.jules<-swn.eg}
#Negative: EG lower than DC
#Positive: EG higher than DC

