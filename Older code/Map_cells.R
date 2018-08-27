#Learning maps
library(maps)
library(ggplot2)

jules.pts<-data.frame(coords.jules)
ed.pts<-data.frame(coords.ed)


#gg.tet<-ggplot()+geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill='gray') + coord_fixed(1.3)


#gg.tet+geom_point(data=jules.pts, aes(x=X2,y=X1), color='red',size=2)


states<-map_data("state")
Domain<-subset(states, region %in% c("ohio","michigan","indiana","illinois","wisconsin",
                                     "minnesota", "iowa", "nebraska", "north dakota", "south dakota", 
                                     "kansas", "missouri", "kentucky","west virginia", "pennsylvania",
                                     "oklahoma", "arkansas","virginia", "tennessee"))
gg.d<-ggplot()+geom_polygon(data=Domain, aes(x=long, y=lat, group=group),fill='gray2')+coord_fixed(1.3)
    

#gg.d+geom_point(data=jules.pts, aes(x=X2,y=X1), color='red',size=2)

ed.east<-ed.pts[ed.pts$X2>-97,]
ed.west<-ed.pts[ed.pts$X2<=-97,]

colramp.lai<-colorRampPalette(c("blue","green","yellow","red"))
colorkey<-colramp.lai(35)[as.numeric(cut(lai.symb[,3],breaks = 35))]

gg.d+geom_point(data=ed.east, aes(x=X2,y=X1), color='light green',size=4)+
  geom_point(data=jules.pts, aes(x=X2,y=X1), color='orange',size=4)+
  geom_point(data=ed.west, aes(x=X2,y=X1), color="white", size=4)+
  geom_point(data=lai.symb, aes(x=X2,y=X1), color=colorkey, size=2)


                   