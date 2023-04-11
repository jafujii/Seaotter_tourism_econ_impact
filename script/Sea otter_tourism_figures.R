## Fujii et al. SeaOtters_Economic_Value
## Journal of Ocean and Coastal Economics

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


### ggplot Theme ###

library(ggthemes)
themeKV <- theme_few() +
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        axis.title.x= element_text(size= 14,margin = margin(0.2, unit = "cm")),
        axis.text.x = element_text(size=12,margin = margin(0.2, unit = "cm")),
        axis.title.y= element_text(size= 14,margin = margin(0.2, unit = "cm")),
        axis.text.y = element_text(size=12,margin = margin(c(1, 0.2), unit = "cm")),
        axis.ticks.length=unit(-0.15, "cm"),element_line(colour = "black", size=.5),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.title=element_blank(),
        strip.text=element_text(hjust=0))

##### Figure 2/S1 Elkhorn Slough Recreational Businesses ########

# Will look for repository folder under C drive user folder automatically. Repeated below for other data files. 
bus<- read.csv(paste0("C:/Users/",Sys.info()[7],"/Seaotter_tourism_econ_impact/data/ES_businesses.csv"), header=T) ## for PC
str(bus)

bus<- read.csv('./data/ES_businesses.csv', header=T) ## for Mac

## times series plot
## number of tourism businesses in the Elkhorn SLough NERR
## obtained from BBB (https://www.bbb.org/us/ca/moss-landing) and CA state BE (https://businesssearch.sos.ca.gov/), on this date: 10 Feb 2021

Bus_yr<-ggplot(bus, aes(x=Year, y= NoRecBus)) +
  geom_point(color="Black", shape=21, size= 4) + 
  scale_y_continuous(name="No. recreational businesses", breaks=c(2,4,6,8)) + themeKV +
  scale_x_continuous(name="Year", limits = c(1990,2020), breaks=c(1990,1995,2000,2005,2010,2015,2020))

## print(Bus_yr)
ggsave("Figure_2_business.eps",width= 183,height=183, units="mm", dpi=800, Bus_yr)
ggsave("Figure_2_business.png",width= 183, height=183,units="mm",dpi=800, Bus_yr)


## Figure S1 - alternative version of this plot as scatter of otters vs businesses
Otr_bus<-ggplot(bus, aes(x=NoOtters, y= NoRecBus)) + 
  themeKV + 
  geom_point(olor="Black",shape = 21, size = 4) + 
  geom_smooth(method = "loess", formula = y ~ x, span = 0.5,color= "#026AA4", se = FALSE) + 
  scale_y_continuous(name="No. recreational businesses", limits = c(0,8), breaks=c(0,2,4,6,8)) + 
  scale_x_continuous(name="No. Sea otters", limits = c(0,150), breaks=c(0,50,100,150))

S1<- Bus_yr+ Otr_bus  # Both versions side by side
ggsave("Figure_S1_business_otters.eps",width= 183,height=90, units="mm", dpi=800, S1)
ggsave("Figure_S1_business_otters.png",width= 183,height=90, units="mm", dpi=800, S1)

#####Figure 3  visitor survey results #####

dem<- read.csv(paste0("C:/Users/",Sys.info()[7],"/Seaotter_tourism_econ_impact/data/SurveyData_edits.csv"), header=T)
str(dem)

## Fig 3A Visit Home Bar Plot
hm<- read.csv(paste0("C:/Users/",Sys.info()[7],"/Seaotter_tourism_econ_impact/data/SurveyData_edits.csv"), header=T, na.strings=c("", " ", "NA"))
str(dem)

home<- hm %>% filter (!is.na(RegionName))


Homsum<-home %>% group_by(RegionName) %>% summarise(count=length(RegionName))
Homsum<-Homsum%>% mutate(prop=count/(sum(count)))

Homplot<-ggplot(Homsum,aes(x=reorder(RegionName, prop), y= prop )) + geom_bar(stat="identity", fill="azure4" )+ 
  scale_y_continuous(limits=c(0,0.4), name="Proportion of visitors")+
  scale_x_discrete (name= "") +
  themeKV + coord_flip()

## Fig 3B Trip Length donut plot
tripl<- dem %>% filter(!is.na(TripType))

trpl<-tripl %>% group_by(TripType)%>% summarise(count=length(TripType) )

trpl<- trpl %>% mutate (frac = count/ sum(count), ymax= cumsum(frac), ymin=c(0,head(ymax,n=-1)))                                                

  #Blue= Day Trip #Grey= Multi-day trip
trpplot<-ggplot(trpl, aes(ymax=ymax, ymin=ymin, xmax= 4, xmin=3, fill=factor(TripType))) + geom_rect()+
  coord_polar(theta="y")+ xlim(c(1,4))+ scale_fill_manual(values =c("#026AA4", "azure4" )) +
  theme_void() + theme(legend.position = "none")

# Fig 3C See sea otters donut plot
Ottr<- dem %>% filter(!is.na(OtterYN))

Otrsum<-Ottr %>% group_by(OtterYN)%>% summarise(count=length(OtterYN) )

Otrsum<- Otrsum %>% mutate (frac = count/ sum(count), ymax= cumsum(frac), ymin=c(0,head(ymax,n=-1)))                                                
Otrsum$labelPosition <- (Otrsum$ymax +Otrsum$ymin)/2
Otrsum$label <- paste0(Otrsum$OtterYN, "\n value: ", Otrsum$count)


ottrplot<-ggplot(Otrsum, aes(ymax=ymax, ymin=ymin, xmax= 4, xmin=3, fill=factor(OtterYN))) + geom_rect()+
  coord_polar(theta="y")+ xlim(c(1,4))+ scale_fill_manual(values =c("#026AA4", "azure4" )) +
  theme_void() + theme(legend.position = "none")

### Fig 3D Tourist Activity bar plot
typ<- dem %>% filter (!is.na(Visit_mode))

typsum<-typ %>% group_by(Visit_mode) %>% summarise(count=length(Visit_mode))
typsum<-typsum%>% mutate(prop=count/(sum(count)))


# Colored by Water vs shore activity
typsum<- typsum %>% mutate(Color= ifelse(Visit_mode =="4"|Visit_mode=="5","azure4","#026AA4"))  

typplot<-ggplot(typsum,aes(x=reorder(Visit_mode, prop), y= prop, fill=Color)) + geom_bar(stat="identity")+ 
  scale_y_continuous(limits=c(0,0.4), name="Proportion of visitors")+ scale_fill_manual(values =c("#026AA4", "azure4" ),guide="none")+
  scale_x_discrete (name= " ",label=c("Undetermined", "Motor boat tour" , "Personal watercraft","Kayak tour", "Hiking")) +
  themeKV + coord_flip()


#Plotting together and exporting individual figures

Visitplot<-(Homplot + trpplot)/ (ottrplot + typplot)

ggsave("Figure_3A_home.eps",width= 90,height=90, units="mm", dpi=800, Homplot)
ggsave("Figure_3B_trpl.eps",width= 90,height=90, units="mm", dpi=800, trpplot)
ggsave("Figure_3C_sawotr.eps",width= 90,height=90, units="mm", dpi=800, ottrplot)
ggsave("Figure_3D_activty.eps",width= 90,height=90, units="mm", dpi=800, typplot)


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

#my_color<-'d3.scaleOrdinal()  .range(["cadetblue","darkturquoise", "steelblue","seagreen","green","palegreen", ""red" ])'

# .domain(["Direct", "indirect","induced","labor", "value added", "output"])
#
p2 <- sankeyNetwork(Links = links, Nodes = nodes,
                    Source = "IDsource", Target = "IDtarget",
                    Value = "Value", NodeID = "name",  nodeWidth = 30)

p2
#Labeling and coloring done in post-processing

saveNetwork(p2,"p2.html") #saves html file of Sankey plot. Copy address and insert below

library(webshot)

#install phantom:
webshot::install_phantomjs()
# Make a webshot in pdf : high quality but can not choose printed zone
webshot("paste_your_html_here.html" , "Fig4_Sankey.pdf", delay = 0.2)

###### Fig 5 Summary and plotting of attributes rank scores #######

dat2<- read.csv(paste0("C:/Users/",Sys.info()[7],"/Seaotter_tourism_econ_impact/data/AttributeRank.csv"), header=T)

dat2<-gather(dat2,attribute, rank, Attrib_Unique:Attrib_Convenience)
tbl<-table(dat2$attribute, dat2$rank) # Count of ranking scores by attribute
chisq.test(tbl) # Chi-Square test 


## Fig 5A Attribute Rank overall
dat2<- dat2 %>%  mutate(name=factor(attribute, levels= c( "Attrib_Otter", "Attrib_Unique","Attrib_Convenience",
                                                          "Attrib_Wildlife", "Attrib_Birds","Attrib_Fish")))
datA<- dat2 %>% group_by(name) %>% summarize (mean= mean(rank),sd=sd(rank), se=(sd(rank)/sqrt(length(rank)))) %>% arrange(mean)
datA$lo <- datA$mean - datA$sd
datA$hi<- datA$mean + datA$sd
datA<- datA %>%mutate( hi= case_when(hi >= 10 ~ 10,TRUE ~ as.numeric(hi)))

pA<-ggplot(datA,aes(x=name, y= mean )) + geom_bar(stat="identity", fill="azure4" )+ geom_errorbar(aes(ymin= lo, ymax=hi, width=0.5))+
  scale_y_continuous(limits=c(0,10), name="Ranking Score")+
  scale_x_discrete (name= "Attribute",label=c("Sea Otter", "Uniqueness", "Convenience", "Other Wildlife", "Birds", "Fish")) +
  themeKV + coord_flip()

##   Attribute Rank by Viewing of sea otters
dat4<- dat2 %>% group_by(OtterYN, name) %>% filter(OtterYN >= 1) %>%
  summarize (mean= mean(rank),sd= sd(rank), se=(sd(rank)/sqrt(length(rank)))) %>% arrange(mean)
labels<- c("1"= "Saw sea otters", "2"= "Did not see sea otters")

p2<-ggplot(dat4,aes(x=name, y= mean)) + geom_bar(stat="identity")+
  facet_wrap(~OtterYN, labeller=labeller(OtterYN=labels),ncol=2) + scale_y_continuous(limits=c(0,10), name="Ranking Score")+ 
  scale_x_discrete (name= "Attribute",label=c("Fish","Birds","Other Wildlife","Convenience","Uniqueness","Sea Otter"), limits=rev(levels(dat4$name))) +themeKV + coord_flip()


####Fig 5B Difference of seeing otters ####
## Shows the difference in rank based on seeing sea otters in a single plot
datdif<-dat4 %>% left_join(dat4, suffix=c("1","2"), by = c("name")) %>% filter(mean1 != mean2) %>% mutate(dif= mean1-mean2, hi= (mean1+ sd1)-(mean2 + sd2), lo= (mean1 - sd1)-(mean2 - sd2)) %>% filter(OtterYN1==1)  

datdif<- datdif %>% mutate(Color= ifelse(dif <0,"red","green"))  
pdif<- ggplot(datdif, aes(x=reorder(name, -dif), y=dif, fill=Color))+ geom_bar(stat="identity") + scale_y_continuous(name="Change in Ranking Score") + geom_errorbar(aes(ymin=lo, ymax=hi, width=0.5))+
  geom_hline(yintercept= 0) + themeKV + theme(axis.text.y= element_blank(), axis.title.y= element_blank()) + 
  scale_fill_manual(values =c("#00b159", "azure4" ), guide= "none") + coord_flip()

compplot<- (pA + pdif) # Combine total randing plot and score different with otters observed

 
# Save plot 
ggsave("Figure_4_Rank_redo.eps", width=183,height=90, units="mm",dpi=800, plot=compplot)
ggsave("Figure_4_Rank_redo.png", width=183,height=90, units="mm",dpi=800, plot=compplot)


## Attribute Rank by Survey Location- not using 
# Shows the effect of Water versus Shore based activities, basically the same as seeing otters versus not seeing otters
#dat2$Location_Summary[dat2$Location_Summary==2]<-1  ## Combine North and South harbor locations           
#dat3<- dat2 %>% group_by(Location_Summary,name) %>% filter(Location_Summary != "9") %>% 
# summarize (mean= mean(rank), se=(sd(rank)/sqrt(length(rank)))) %>% arrange(mean)

#p1<-ggplot(dat3,aes(x=name, y= mean)) + geom_bar(stat="identity")+ geom_errorbar(aes(ymin=mean-se, ymax= mean + se),width=.2)+
# facet_wrap(~Location_Summary) + themeKV

### See "Tourism_scripts.R" for Figure 6 script###

#####Figure S2 Density plots of visitor demographics #####

dem<- read.csv(paste0("C:/Users/",Sys.info()[7],"/Seaotter_tourism_econ_impact/data/SurveyData_edits.csv"), header=T)
str(dem)

# Fig S2A Distribution of respondents' age
dem2<- dem %>% filter(!is.na(AGE))

ageden<-ggplot(dem2, aes(x= AGE))+geom_density(alpha=0.3, color= "#026AA4",fill= "#026AA4")+ 
  scale_y_continuous(limits=c(0,0.025), name=" ", breaks=c(0.00,0.01,0.02))+
  geom_rug(aes(x=AGE, y=0), sides="b", position="jitter") + scale_x_continuous(name="Age (years)")+ themeKV


# Fig S2B Density plot visitor reported income on Log scale with "rug" to show sample data

incden<- ggplot(dem, aes(x= Income))+geom_density(alpha=0.3, color= "#026AA4",fill= "#026AA4")+ 
  geom_rug(aes(x=Income, y=0), sides="b", position="jitter") + scale_y_continuous(limits=c(0,0.65), breaks=c(0.0,0.3,0.6), name="") +
  scale_x_continuous(name= "Log (Annual Income)", trans="log1p", labels=scales::label_number(), breaks=c(10000,50000,150000)) + themeKV 

## Fig S2C Density plot previous visits on Log scale with "rug" to show sample data
visden<-ggplot(dem2, aes(x= NVisits))+geom_density( alpha=0.3, color= "#026AA4",fill= "#026AA4")+ 
  geom_rug(aes(x=NVisits, y=0), sides="b", position="jitter") + scale_y_continuous(limits=c(0,0.65), breaks=c(0.0,0.3,0.6),name="")+
  scale_x_continuous(name= "Log (Number previous visits)", trans="log1p", labels=scales::label_number(), breaks=c(0, 10,50,100,200)) + 
  themeKV

##Fig S2D Party Size density plot with "rug" to show sample data
# Outliers of Party Size were not included in reported averages by Colgan, so also removed here. Likely data entry errors  
dem3<- dem %>% filter(Partysize <30) 
Psden<- ggplot(dem3, aes(x= Partysize))+geom_density(alpha=0.3, color= "#026AA4",fill= "#026AA4")+ 
  geom_rug(aes(x=Partysize, y=0), sides="b", position="jitter") + scale_y_continuous(limits=c(0,0.45), name="", breaks=c(0.0,0.2,0.4)) +
  scale_x_continuous(name= "Party Size") + themeKV

## Combining plots together and exporting
denp<- (ageden + Psden)/(incden + visden ) # square layout
denp2<-(ageden / incden / visden / Psden) # vertical layout
ggsave("Figure_S2_density.eps",width= 140,height=140, units="mm",dpi=800, denp)
ggsave("Figure_S2_density.png",width= 140,height=140, units="mm",dpi=800, denp)
