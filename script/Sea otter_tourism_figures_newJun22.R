## Fujii et al. SeaOtters_Economic_Value
##Survey Data Summary Figures

library(tidyverse)
library(patchwork)
library(reshape2)
library(ggpubr)
library(cowplot)
library(here)
library(Ecdat)
library(lmtest)
library(scales)
library(ggplot2)
library(ggthemes)



themeKV <- theme_few() +
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        axis.title.x= element_text(size= 12,margin = margin(0.2, unit = "cm")),
        axis.text.x = element_text(size=12,margin = margin(0.2, unit = "cm")),
        axis.title.y= element_text(size= 12,margin = margin(0.2, unit = "cm")),
        axis.text.y = element_text(size=12,margin = margin(c(1, 0.2), unit = "cm")),
        axis.ticks.length=unit(-0.15, "cm"),element_line(colour = "black", size=.5),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.title=element_blank(),
        strip.text=element_text(hjust=0))


#####Figure 3  visitor survey results #####

dem<- read.csv(paste0("C:/Users/",Sys.info()[7],"/Seaotter_tourism_econ_impact/data/SurveyData_edits.csv"), header=T)
str(dem)


# See sea otters donut plot
Ottr<- dem %>% filter(!is.na(OtterYN))

Otrsum<-Ottr %>% group_by(OtterYN)%>% summarise(count=length(OtterYN) )
                                                
Otrsum<- Otrsum %>% mutate (frac = count/ sum(count), ymax= cumsum(frac), ymin=c(0,head(ymax,n=-1)))                                                
Otrsum$labelPosition <- (Otrsum$ymax +Otrsum$ymin)/2
Otrsum$label <- paste0(Otrsum$OtterYN, "\n value: ", Otrsum$count)


ottrplot<-ggplot(Otrsum, aes(ymax=ymax, ymin=ymin, xmax= 4, xmin=3, fill=factor(OtterYN))) + geom_rect()+
  coord_polar(theta="y")+ xlim(c(1,4))+ scale_fill_manual(values =c("#026AA4", "azure4" )) +
  theme_void() + theme(legend.position = "none")



### Fig 3 Tourist Activity bar plot
typ<- dem %>% filter (!is.na(Visit_mode))


typsum<-typ %>% group_by(Visit_mode) %>% summarise(count=length(Visit_mode))
typsum<-typsum%>% mutate(prop=count/(sum(count)))

typplot<-ggplot(typsum,aes(x=reorder(Visit_mode, prop), y= prop )) + geom_bar(stat="identity", fill="azure4" )+ 
  scale_y_continuous(limits=c(0,0.4), name="Proportion of visitors")+
  scale_x_discrete (name= " ",label=c("Undetermined", "Motor boat tour" , "Personal watercraft","Hiking", "Kayak tour")) +
  themeKV + coord_flip()

# Colored by Water vs shore activity
typsum<- typsum %>% mutate(Color= ifelse(Visit_mode =="4"|Visit_mode=="5","azure4","#026AA4"))  

typplot<-ggplot(typsum,aes(x=reorder(Visit_mode, prop), y= prop, fill=Color)) + geom_bar(stat="identity")+ 
  scale_y_continuous(limits=c(0,0.4), name="Proportion of visitors")+ scale_fill_manual(values =c("#026AA4", "azure4" ),guide="none")+
  scale_x_discrete (name= " ",label=c("Undetermined", "Motor boat tour" , "Personal watercraft","Kayak tour", "Hiking")) +
  themeKV + coord_flip()


## Trip Length donut plot
tripl<- dem %>% filter(!is.na(TripType))

trpl<-tripl %>% group_by(TripType)%>% summarise(count=length(TripType) )

trpl<- trpl %>% mutate (frac = count/ sum(count), ymax= cumsum(frac), ymin=c(0,head(ymax,n=-1)))                                                


trpplot<-ggplot(trpl, aes(ymax=ymax, ymin=ymin, xmax= 4, xmin=3, fill=factor(TripType))) + geom_rect()+
  coord_polar(theta="y")+ xlim(c(1,4))+ scale_fill_manual(values =c("#026AA4", "azure4" )) +
  theme_void() + theme(legend.position = "none")



## Visit Home Bar Plot
hm<- read.csv(paste0("C:/Users/",Sys.info()[7],"/Seaotter_tourism_econ_impact/data/SurveyData_edits.csv"), header=T, na.strings=c("", " ", "NA"))
str(dem)

home<- hm %>% filter (!is.na(RegionName))


Homsum<-home %>% group_by(RegionName) %>% summarise(count=length(RegionName))
Homsum<-Homsum%>% mutate(prop=count/(sum(count)))

Homplot<-ggplot(Homsum,aes(x=reorder(RegionName, prop), y= prop )) + geom_bar(stat="identity", fill="azure4" )+ 
  scale_y_continuous(limits=c(0,0.4), name="Proportion of visitors")+
  scale_x_discrete (name= "") +
  themeKV + coord_flip()

#Plotting together and exporting figures

(ottrplot + typplot)/ (Homplot + trpplot)

ggsave("Figure_X_sawotr.pdf",width= 90,height=90, units="mm", dpi=800, ottrplot)
ggsave("Figure_X_activty.pdf",width= 90,height=90, units="mm", dpi=800, typplot)
ggsave("Figure_X_home.pdf",width= 90,height=90, units="mm", dpi=800, Homplot)
ggsave("Figure_X_trpl.pdf",width= 90,height=90, units="mm", dpi=800, trpplot)

### Figure 4 Sankey Diagram and financial flow
library(networkD3)
san<- read.csv(paste0("C:/Users/",Sys.info()[7],"/Seaotter_tourism_econ_impact/data/Sankey_Econ.csv"), header=T)
str(san)

links<-san


nodes <- data.frame(
  name=c(as.character(links$Source), 
         as.character(links$Target)) %>% unique()
)

links$IDsource <- match(links$Source, nodes$name)-1 
links$IDtarget <- match(links$Target, nodes$name)-1

my_color<-'d3.scaleOrdinal()  .range(["cadetblue","darkturquoise", "steelblue","seagreen","green","palegreen", ""red" ])'

# .domain(["Direct", "indirect","induced","labor", "value added", "output"])
#
p2 <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "Value", NodeID = "name", fontsize =12, nodeWidth = 30,
                   #colourScale= my_color
                   )

p2

saveNetwork(p2,"p2.html")

library(webshot)

#install phantom:
webshot::install_phantomjs()
# Make a webshot in pdf : high quality but can not choose printed zone
webshot("Insert html address here" , "sankey2.pdf", delay = 0.2)








