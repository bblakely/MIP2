
#102 year transition
explore.succ<-data.frame(datasubset.jules[[1]][[1]])

plot(explore.succ$dccomp, main='dccomp')

#WAS  for dataset 34
#explore.smooth<-explore.succ[250:nrow(explore.succ),]

#plot(explore.smooth$dccomp)

explore.ann<-aggregate(explore.smooth, by=list(explore.smooth$year), FUN=mean)

datcount<-rep(0, length(varset.jules))
for (s in 1:length(varset.ed)){
  if(nrow(varset.jules[[s]])!=0){datcount[s]<-1}
}
varset.jules.thin<-varset.jules[datcount==1]
plot(explore.ann[c(2:3,5:13,15:18)])

datasubset.ed.east<-datasubset.ed[which(coords.ed[,2]>-97)]
varset.ed.east<-varset.ed.thin[which(coords.ed[,2]>-97)]

poc<-data.frame(datasubset.jules[[2]][[1]])
poc.ann<-aggregate(poc, by=list(poc$year), FUN=mean)

colorramp<-colorRampPalette(c("black","blue","green", "yellow"))#c("blue","green", "yellow")
plot(poc.ann[c(2:3,5:7, 15:18)], main='effect', col=colorramp(nrow(poc.ann)), cex=0.5)
plot(poc.ann[c(10:13,16:18)], main='cause',col=colorramp(nrow(poc.ann)), cex=0.5)

#Observations
#easternmost site (3, 4) has less explanation of compshift


datasubset.ed.west<-datasubset.ed[which(coords.ed[,2]<=-97)]
varset.ed.west<-varset.ed.thin[which(coords.ed[,2]<=-97)]

laick<-data.frame(datasubset.ed.west[[23]][2])




