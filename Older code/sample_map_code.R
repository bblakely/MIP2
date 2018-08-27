library(raster)
library(maps)


#ggplot(slope.xy, aes(x = lon, y=lat, fill= slope))+geom_raster()


states <- map_data("state")
states <- subset(states, ! region  %in% c("california", 'nevada','arizona','utah','oregon','washington','new mexico','colorado','montana','wyoming','idaho') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
states <- spTransform(states,CRSobj = '+init=epsg:4326')
mapdata <- data.frame(states)

cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")

# slope.xy is a df with column for lat, lon, slope


# map out the correlations 
png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/ED_",var,"_inc_map.png"))
print(ggplot(slope.xy, aes(x = lon, y=lat, fill= slope))+geom_raster()+
        scale_fill_gradient(low = "blue", high = "red", name ="slope (WUE increase/year)", na.value = 'darkgrey')+
        geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle('Slope of relative WUE increase/year'))
dev.off()