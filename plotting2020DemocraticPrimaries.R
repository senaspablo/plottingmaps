library(usmap)
library(ggplot2)
library(ggthemes)


primariasEEUU<-function(candidatos,update,colors=1:(length(candidatos)+1)){
  us_states<-map_data("state")
  region<-sort(c(unique(us_states$region),"alaska","hawaii"))
  ganadores<-rep("Elections yet to come",length(region))
  
  nombres<-names(candidatos)
  no.candidatos<-length(candidatos)
  for (i in 1:no.candidatos){
    ganadores[candidatos[[i]]]<-nombres[i]
  }
  primarias<-data.frame(region,ganadores)
  us_states_primarias<- dplyr::left_join(us_states, primarias)
  p<- ggplot(data = us_states_primarias,
               mapping = aes(x = long, y = lat,
                             group = group, fill = ganadores))
  p <- p+ geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) 
  p<- p+ scale_fill_manual(values = colors) +
    labs(title = "2020 Democratic Party presidential primaries",
         caption="Author: Pablo Señas Peón",
         subtitle=paste("Last update:", update), fill = NULL)
  p+theme_map()
}

candidatos<-list("Pete Buttigieg"=c(16),
                 "Bernie Sanders"=c(30,29,5,6,45,46,35),
                 "Joe Biden"=c(41,1,4,20,22,24,34,37,43,44,47,13,23,25,26,48))
colors<-c("darksalmon","black","blue","yellow")
update="March 13"
primariasEEUU(candidatos,update,colors)
ggsave(paste0(update,".pdf"))





