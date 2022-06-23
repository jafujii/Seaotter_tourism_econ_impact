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


### ggplot Theme

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

##### Figure 2 Elkhorn Slough Businesses ########

# Will look for repository folder under C drive user folder automatically. Repeated below for other data files. From Kisei  

bus<- read.csv(paste0("C:/Users/",Sys.info()[7],"/Seaotter_tourism_econ_impact/data/ES_businesses.csv"), header=T) ## for PC
str(bus)

bus<- read.csv('./data/ES_businesses.csv', header=T) ## for Mac

## times series plot
## number of tourism businesses in the Elkhorn SLough NERR
## obtained from BBB (https://www.bbb.org/us/ca/moss-landing) and CA state BE (https://businesssearch.sos.ca.gov/), on this date: 10 Feb 2021

Bus_yr<-ggplot(bus, aes(x=Year, y= NoRecBus))+geom_point(color="Black", shape=21, size= 4, alpha= 0.5)+ geom_smooth(color= "#026AA4",se=TRUE) + 
  scale_y_continuous(name="No. recreational businesses", breaks=c(2,4,6,8)) + themeKV +
  scale_x_continuous(name="Year", limits = c(1990,2020), breaks=c(1990,1995,2000,2005,2010,2015,2020))

## print(bplot)
ggsave("Figure_2_business.pdf",width= 183,height=183, units="mm", dpi=800, Bus_yr)
ggsave("Figure_2_business.png",width= 183, height=183,units="mm",dpi=800, Bus_yr)

ggsave("Figure_2_business_sm.pdf",width= 90,height=90, units="mm", dpi=800,Bus_yr)
ggsave("Figure_2_business_sm.png",width= 90, height=90,units="mm", dpi=800,Bus_yr)

## Figure S1 - alternative version of this plot as scatter of otters vs businesses
Otr_bus<-ggplot(bus, aes(x=NoOtters, y= NoRecBus)) + 
  themeKV + 
  geom_point(shape = 21, size = 4, alpha=0.5) + 
  geom_smooth(method = "loess", formula = y ~ x, span = 0.5,color= "#026AA4", se = TRUE) + 
  scale_y_continuous(name="No. Businesses", limits = c(0,8), breaks=c(0,2,4,6,8)) + 
  scale_x_continuous(name="No. Sea otters", limits = c(0,150), breaks=c(0,50,100,150))

Otr_bus + Bus_yr # Both versions side by side


ggsave("Figure_S1_business_otters.pdf",width= 90,height=90, units="mm", dpi=800, Otr_bus)
ggsave("Figure_S1_business_otters.png",width= 90, height=90,units="mm", dpi=800, Otr_bus)

#####Figure 3 Density plots of visitor demographics #####

dem<- read.csv(paste0("C:/Users/",Sys.info()[7],"/Seaotter_tourism_econ_impact/data/SurveyData_edits.csv"), header=T)
str(dem)

# Fig 3A Distribution of respondents' age
dem2<- dem %>% filter(!is.na(AGE))

ageden<-ggplot(dem2, aes(x= AGE))+geom_density(alpha=0.3, color= "#026AA4",fill= "#026AA4")+ 
  scale_y_continuous(limits=c(0,0.025), name=" ", breaks=c(0.00,0.01,0.02))+
  geom_rug(aes(x=AGE, y=0), sides="b", position="jitter") + scale_x_continuous(name="Age (years)")+ themeKV


# Fig 3B Density plot visitor reported income on Log scale with "rug" to show sample data

incden<- ggplot(dem, aes(x= Income))+geom_density(alpha=0.3, color= "#026AA4",fill= "#026AA4")+ 
  geom_rug(aes(x=Income, y=0), sides="b", position="jitter") + scale_y_continuous(limits=c(0,0.65), breaks=c(0.0,0.3,0.6), name="") +
  scale_x_continuous(name= "Log (Annual Income)", trans="log1p", labels=scales::label_number(), breaks=c(10000,50000,150000)) + themeKV 


## Fig 3C Density plot previous visits on Log scale with "rug" to show sample data
visden<-ggplot(dem2, aes(x= NVisits))+geom_density( alpha=0.3, color= "#026AA4",fill= "#026AA4")+ 
  geom_rug(aes(x=NVisits, y=0), sides="b", position="jitter") + scale_y_continuous(limits=c(0,0.65), breaks=c(0.0,0.3,0.6),name="")+
  scale_x_continuous(name= "Log (Number previous visits)", trans="log1p", labels=scales::label_number(), breaks=c(0, 10,50,100,200)) + 
  themeKV



##Fig 3D Party Size density plot with "rug" to show sample data
# Outliers of Party Size were not included in reported averages by Colgan, so also removed here. Likely data entry errors  
dem3<- dem %>% filter(Partysize <30) 
Psden<- ggplot(dem3, aes(x= Partysize))+geom_density(alpha=0.3, color= "#026AA4",fill= "#026AA4")+ 
  geom_rug(aes(x=Partysize, y=0), sides="b", position="jitter") + scale_y_continuous(limits=c(0,0.45), name="", breaks=c(0.0,0.2,0.4)) +
  scale_x_continuous(name= "Party Size") + themeKV




## Combining plots together and exporting
denp<- (ageden + Psden)/(incden + visden ) # square layout
denp2<-(ageden / incden / visden / Psden) # vertical layout
ggsave("Figure_3_density_Feb11.pdf",width= 140,height=140, units="mm",dpi=800, denp)
ggsave("Figure_3_density_Feb11.png",width= 140,height=140, units="mm",dpi=800, denp)

ggsave("Figure_3_vertical.pdf", width=90, height=190,units="mm",dpi=800, denp2)



###### Fig 4 Summary and plotting of attributes rank scores #######

dat2<- read.csv(paste0("C:/Users/",Sys.info()[7],"/Seaotter_tourism_econ_impact/data/AttributeRank.csv"), header=T)

dat2<-gather(dat2,attribute, rank, Attrib_Unique:Attrib_Convenience)
tbl<-table(dat2$attribute, dat2$rank) # Count of ranking scores by attribute
chisq.test(tbl) # Chi-Square test 


## Fig 4A Attribute Rank overall
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



## Attribute Rank by Survey Location- not using 
# Shows the effect of Water versus Shore based activities, basically the same as seeing otters versus not seeing otters
dat2$Location_Summary[dat2$Location_Summary==2]<-1  ## Combine North and South harbor locations           
dat3<- dat2 %>% group_by(Location_Summary,name) %>% filter(Location_Summary != "9") %>% 
  summarize (mean= mean(rank), se=(sd(rank)/sqrt(length(rank)))) %>% arrange(mean)

p1<-ggplot(dat3,aes(x=name, y= mean)) + geom_bar(stat="identity")+ geom_errorbar(aes(ymin=mean-se, ymax= mean + se),width=.2)+
  facet_wrap(~Location_Summary) + themeKV


##  Attribute Rank by Viewing of sea otters
dat4<- dat2 %>% group_by(OtterYN, name) %>% filter(OtterYN >= 1) %>%
  summarize (mean= mean(rank),sd= sd(rank), se=(sd(rank)/sqrt(length(rank)))) %>% arrange(mean)
labels<- c("1"= "Saw sea otters", "2"= "Did not see sea otters")

p2<-ggplot(dat4,aes(x=name, y= mean)) + geom_bar(stat="identity")+
  facet_wrap(~OtterYN, labeller=labeller(OtterYN=labels),ncol=1) + scale_y_continuous(limits=c(0,10), name="Ranking Score")+ 
  scale_x_discrete (name= "Attribute",label=c("Fish","Birds","Other Wildlife","Convenience","Uniqueness","Sea Otter"), limits=rev(levels(dat4$name))) +themeKV + coord_flip()


####Fig 4B Difference of seeing otters ####
## Shows the difference in rank based on seeing sea otters in a single plot
datdif<-dat4 %>% left_join(dat4, suffix=c("1","2"), by = c("name")) %>% filter(mean1 != mean2) %>% mutate(dif= mean1-mean2, hi= (mean1+ sd1)-(mean2 + sd2), lo= (mean1 - sd1)-(mean2 - sd2)) %>% filter(OtterYN1==1)  

datdif<- datdif %>% mutate(Color= ifelse(dif <0,"red","green"))  
pdif<- ggplot(datdif, aes(x=reorder(name, -dif), y=dif, fill=Color))+ geom_bar(stat="identity") + scale_y_continuous(name="Change in Ranking Score") + geom_errorbar(aes(ymin=lo, ymax=hi, width=0.5))+
  geom_hline(yintercept= 0) + themeKV + theme(axis.text.y= element_blank(), axis.title.y= element_blank()) + 
  scale_fill_manual(values =c("#00b159", "azure4" ), guide= "none") + coord_flip()

compplot<- (pA + pdif)

 
# Save plot 
ggsave("Figure_4_Ranking_change.pdf", width=183,height=90, units="mm",dpi=800, plot=pdif)
ggsave("Figure_4_Ranking_change.png", width=183, height=90, units="mm",dpi=800, plot=pdif)
ggsave("Figure_4_Rank_redo.pdf", width=183,height=90, units="mm",dpi=800, plot=compplot)
ggsave("Figure_4_Rank_redo.png", width=183,height=90, units="mm",dpi=800, plot=compplot)
