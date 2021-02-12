## Fujii et al. SeaOtters_Economic_Value
##Survey Data Summary Figures

library(tidyverse)
library(patchwork)
library(reshape2)
library(ggpubr)
library(cowplot)
library(here)
library(DCchoice)
library(Ecdat)
library(lmtest)
library(AICcmodavg)
library(scales)
library(ggplot2)



# Package required for DCchoice that no longer installs automatically
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.12")

BiocManager::install("Icens")
library(Incens)


### ggplot Theme from Becker
theme_themeo <- function () { 
  theme_classic()+
    theme(strip.background = element_blank(),
          axis.line = element_blank(),
          text=element_text(size=15),
          axis.text.x = element_text(margin = margin( 0.2, unit = "cm")),
          axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
          axis.ticks.length=unit(-0.1, "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=.5),
          legend.title=element_blank(),
          legend.position="bottom",
          strip.text=element_text(hjust=0) )}

library(ggthemes)
themeKV <- theme_few() +
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(margin = margin(0.2, unit = "cm")),
        axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
        axis.ticks.length=unit(-0.15, "cm"),element_line(colour = "black", size=.5),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.title=element_blank(),
        strip.text=element_text(hjust=0))


# Will look for repository folder under C drive user folder automatically. Repeated below for other data files. From Kisei  

bus<- read.csv(paste0("C:/Users/",Sys.info()[7],"/Seaotter_tourism_econ_impact/data/ES_businesses.csv"), header=T) ## for PC
str(bus)


## times series plot
## number of tourism businesses in the Elkhorn SLough NERR
## obtained from BBB (http:// ______), on this date: _______

bplot<-ggplot(bus, aes(x=Year, y= NoRecBus))+geom_point(color="#026AA4")+ geom_smooth(color= "#026AA4",se=FALSE) + 
  scale_y_continuous(name="No recreational businesses", breaks=c(2,4,6,8)) +
  theme_themeo()
## print(bplot)
ggsave("Figure_2_business.pdf",width= 183, units="mm", bplot)
ggsave("Figure_2_business.png",width= 183, units="mm", bplot)

## alternative version of this plot with same variables
bus<- read.csv('./data/ES_businesses.csv', header=T) ## for Mac
ggplot(bus, aes(x=Year, y= NoRecBus)) + 
  themeKV + 
  geom_point(shape = 21, size = 3, alpha=0.5) + 
  geom_smooth(method = "loess", formula = y ~ x, span = 0.5, se = TRUE) + 
  scale_y_continuous(name="no. businesses", limits = c(0,9), breaks=c(0,2,4,6,8)) + 
  scale_x_continuous(name="year", limits = c(1990,2020), breaks=c(1990,1995,2000,2005,2010,2015,2020))

## alternative version of this plot as scatter of otters vs businesses
ggplot(bus, aes(x=NoOtters, y= NoRecBus)) + 
  themeKV + 
  geom_point(shape = 21, size = 3, alpha=0.5) + 
  geom_smooth(method = "loess", formula = y ~ x, span = 0.7, se = TRUE) ## + 
##  scale_y_continuous(name="no. businesses", limits = c(0,8), breaks=c(0,2,4,6,8)) + 
##  scale_x_continuous(name="year", limits = c(0,150), breaks=c(0,50,100,150))






dem<- read.csv(paste0("C:/Users/",Sys.info()[7],"/Seaotter_tourism_econ_impact/data/SurveyData_edits.csv"), header=T)
str(dem)
# Distribution of respondents' age
dem2<- dem %>% filter(!is.na(AGE))

ageden<-ggplot(dem2, aes(x= AGE))+geom_density(alpha=0.3, color= "#026AA4",fill= "#026AA4")+ scale_y_continuous(limits=c(0,0.025), name="Proportion of respondents", breaks=c(0.00,0.01,0.02,0.03))+
  geom_rug(aes(x=AGE, y=0), sides="b", position="jitter") + scale_x_continuous(name="Age (years)")+ theme_themeo()

ageden1<-ggplot(dem2, aes(x= AGE))+ geom_density(alpha=0.3, color= "#026AA4",fill= "#026AA4")+ scale_y_continuous(limits=c(0,0.03), name="Proportion of respondents", breaks=c(0.00,0.01,0.02,0.03))+
  geom_rug(aes(x=AGE, y=0), sides="b", position="jitter") + scale_x_continuous(name="Age (years)")+ theme_themeo()



## Density plot previous visits on Log scale with "rug" to show sample data
visden<-ggplot(dem2, aes(x= NVisits))+geom_density( alpha=0.3, color= "#026AA4",fill= "#026AA4")+ 
  geom_rug(aes(x=NVisits, y=0), sides="b", position="jitter") + scale_y_continuous(limits=c(0,0.65), name="")+
  scale_x_continuous(name= "Log (Number previous visits)", trans="log1p", labels=scales::label_number(), breaks=c(0, 10,50,100,200)) + 
  theme_themeo()
  theme_classic()+
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        text=element_text(size=15),
        axis.text.x = element_text(margin = margin( 0.2, unit = "cm")),
        #axis.text.y = element_blank(),
        axis.ticks.length=unit(-0.1, "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.title=element_blank(),
        legend.position="bottom",
        strip.text=element_text(hjust=0) )



# Density plot visitor reported income on Log scale with "rug" to show sample data

incden<- ggplot(dem, aes(x= Income))+geom_density(alpha=0.3, color= "#026AA4",fill= "#026AA4")+ 
  geom_rug(aes(x=Income, y=0), sides="b", position="jitter") + scale_y_continuous(limits=c(0,0.65), name="Proportion of respondents") +
  scale_x_continuous(name= "Log (Annual Income)", trans="log1p", labels=scales::label_number(), breaks=c(10000,50000,150000)) + theme_themeo() 

# Bar plot of origin of visit
# Not currently using
home<- dem %>%  filter(RegionName != " ") %>% mutate(name=factor(RegionName, levels= c( "Monterey Bay", "Outside California","East SF Bay",
                                                                                        "Southern California", "Northern California","West SF Bay"))) %>% group_by(name) %>% summarise(n= n()) %>% 
  mutate (prop=n/sum(n)*100) %>% arrange(desc(prop))

hm<- ggplot(home, aes(x=name, y= prop))+ geom_bar(stat="identity")+ theme_themeo ()+
  scale_y_continuous(name= "Percent") + scale_x_discrete(name= "", limits= rev(levels(home$name))) +coord_flip()


##Party Size density plot with "rug" to show sample data
  # Outliers of Party Size were not included in reported averages by Colgan, so also removed here. Likely data entry errors  
dem3<- dem %>% filter(Partysize <30) 
Psden<- ggplot(dem3, aes(x= Partysize))+geom_density(alpha=0.3, color= "#026AA4",fill= "#026AA4")+ 
  geom_rug(aes(x=Partysize, y=0), sides="b", position="jitter") + scale_y_continuous(limits=c(0,0.45), name="", breaks=c(0.0,0.2,0.4)) +
  scale_x_continuous(name= "Party Size") +
  theme_classic()+
  theme(strip.background = element_blank(),
        axis.line = element_blank(),
        text=element_text(size=15),
        axis.text.x = element_text(margin = margin( 0.2, unit = "cm")),
        axis.text.y = element_text(margin = margin( c(1,0.2), unit = "cm")),
        axis.ticks.length=unit(-0.1, "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        legend.title=element_blank(),
        legend.position="bottom",
        strip.text=element_text(hjust=0) )

#Grouped by Day trip or multiday. Heavily overlapped. Not using
Psden2<- ggplot(dem3, aes(x= Partysize, group=TripType))+geom_density(alpha=0.3, color= "#026AA4",fill= "#026AA4")+ 
  geom_rug(aes(x=Partysize, y=0), sides="b", position="jitter") + scale_y_continuous(limits=c(0,0.45), name="") +
  scale_x_continuous(name= "Party Size") +
  theme_classic()

## Combining plots together and exporting
denp<- (ageden + visden)/(incden + Psden )
ggsave("Figure_2_density_Feb11.pdf",width= 183, units="mm", denp)
ggsave("Figure_2_density_Feb11.png",width= 183, units="mm", denp)


####################### Summary and plotting of attributes rank scores ########################
dat2<- read.csv(paste0("C:/Users/",Sys.info()[7],"/Seaotter_tourism_econ_impact/data/AttributeRank.csv"), header=T)
#dat2<-read.csv("AttributeRank.csv", header=T)
dat2<-gather(dat2,attribute, rank, Attrib_Unique:Attrib_Convenience)
tbl<-table(dat2$attribute, dat2$rank) # Count of ranking scores by attribute
chisq.test(tbl) # Chi-Square test 


  ## Attribute Rank overall
  dat2<- dat2 %>%  mutate(name=factor(attribute, levels= c( "Attrib_Otter", "Attrib_Unique","Attrib_Convenience",
                                                          "Attrib_Wildlife", "Attrib_Birds","Attrib_Fish")))
  datA<- dat2 %>% group_by(name) %>% summarize (mean= mean(rank), se=(sd(rank)/sqrt(length(rank)))) %>% arrange(mean)

  pA<-ggplot(datA,aes(x=name, y= mean)) + geom_bar(stat="identity")+
    scale_y_continuous(limits=c(0,10), name="Ranking Score")+
    scale_x_discrete (name= "Attribute",label=c("Sea Otter", "Uniqueness", "Convenience", "Other Wildlife", "Birds", "Fish")) +theme_themeo ()


  ## Attribute Rank by Survey Location
  # Shows the effect of Water versus Shore based activities, basically the same as seeing otters versus not seeing otters
    dat2$Location_Summary[dat2$Location_Summary==2]<-1  ## Combine North and South harbor locations           
    dat3<- dat2 %>% group_by(Location_Summary,name) %>% filter(Location_Summary != "9") %>% 
      summarize (mean= mean(rank), se=(sd(rank)/sqrt(length(rank)))) %>% arrange(mean)

    p1<-ggplot(dat3,aes(x=name, y= mean)) + geom_bar(stat="identity")+ geom_errorbar(aes(ymin=mean-se, ymax= mean + se),width=.2)+
    facet_wrap(~Location_Summary) + theme_themeo ()


  ## Attribute Rank by Viewing of sea otters
    dat4<- dat2 %>% group_by(OtterYN, name) %>% filter(OtterYN >= 1) %>%
      summarize (mean= mean(rank),sd= sd(rank), se=(sd(rank)/sqrt(length(rank)))) %>% arrange(mean)
    labels<- c("1"= "Saw sea otters", "2"= "Did not see sea otters")

    p2<-ggplot(dat4,aes(x=name, y= mean)) + geom_bar(stat="identity")+
      facet_wrap(~OtterYN, labeller=labeller(OtterYN=labels),ncol=1) + scale_y_continuous(limits=c(0,10), name="Ranking Score")+ 
      scale_x_discrete (name= "Attribute",label=c("Fish","Birds","Other Wildlife","Convenience","Uniqueness","Sea Otter"), limits=rev(levels(dat4$name))) +theme_themeo () + coord_flip()


  #### Difference of seeing otters ############
  ## Shows the difference in rank based on seeing sea otters in a single plot
   datdif<-dat4 %>% left_join(dat4, suffix=c("1","2"), by = c("name")) %>% filter(mean1 != mean2) %>% mutate(dif= mean1-mean2, hi= (mean1+ sd1)-(mean2 + sd2), lo= (mean1 - sd1)-(mean2 - sd2)) %>% filter(OtterYN1==1)  
    pdif<- ggplot(datdif, aes(x=reorder(name, -dif), y=dif))+ geom_bar(stat="identity") + scale_y_continuous(name="Change in Ranking Score") + geom_errorbar(aes(ymin=lo, ymax=hi, width=0.5))+
     scale_x_discrete(name= "Attribute",label=c("Sea Otter","Convenience"," Uniqueness","Birds","Other Wildlife","Fish")) + geom_hline(yintercept= 0) +
     theme_themeo()

  # Save plot 
    ggsave("Ranking_change.pdf", width=183, units="mm", plot=pdif)
    ggsave("Ranking_change.png", width=183, units="mm", plot=pdif)

############## Willingness to Pay Analyses ###############################################
datWTP<-read.csv(paste0("C:/Users/",Sys.info()[7],"/Seaotter_tourism_econ_impact/data/WTPsurvey.csv"), header=T)

    table(datWTP$ESR1, datWTP$ESBID_1)
    
    # Data exploration, average proportion of 'yes' responses for first bid amount
    round(tapply(datWTP$ESR1, datWTP$ESBID_1, mean), 2)
    round(tapply(datWTP$OTR1, datWTP$OTBID_1, mean), 2)
    
    
    ## Double bid of Elkhorn Slough WTP Question
    
    dat1<- datWTP %>% dplyr::select(ESR1, ESR2,Income,AGE, ESBID_1,ESBID_2, Attrib_Birds,Attrib_Unique,Attrib_Otter,Attrib_Fish, Attrib_Convenience, NVisits,MBA) %>% drop_na()
    
    # Test of error distributions used in full model
    E.log<-  dbchoice(ESR1 +ESR2 ~ 1 + log(Income) +log(AGE)+ Attrib_Otter + Attrib_Unique +
                        Attrib_Convenience +NVisits + MBA| ESBID_1 + ESBID_2,dist="logistic",na.rm=TRUE, data=dat1)
    E.loglog<-dbchoice(ESR1 +ESR2 ~ 1 + log(Income) +log(AGE)+ Attrib_Otter + Attrib_Unique +
                         Attrib_Convenience +NVisits + MBA| log(ESBID_1) + log(ESBID_2),dist="log-logistic",na.rm=TRUE, data=dat1)
    E.lognormal<-dbchoice(ESR1 +ESR2 ~ 1 + log(Income) +log(AGE)+ Attrib_Otter + Attrib_Unique +
                            Attrib_Convenience +NVisits + MBA| log(ESBID_1) + log(ESBID_2),dist="log-normal",na.rm=TRUE, data=dat1)
    
    AIC(E.log, E.loglog,E.lognormal) # Logistic distribution has lowest AIC
    
    
    ## Double bid dichotomous choice of Elkhorn Slough WTP Question
    
    datES<- datWTP %>% dplyr::select(ESR1, ESR2,Income, ESBID_1,ESBID_2, Attrib_Birds,Attrib_Unique,Attrib_Otter,
                                     Attrib_Fish, Attrib_Convenience, NVisits,MBA, AGE) %>% drop_na()
    
    ESd0<- dbchoice(ESR1 +ESR2 ~ 1 | ESBID_1 + ESBID_2,dist="logistic",na.rm=TRUE, data=datES)  # Null Model
    Esdfull<- dbchoice(ESR1 +ESR2 ~ 1 + log(Income) + log(AGE)+ Attrib_Otter + Attrib_Unique + 
                         Attrib_Convenience +NVisits + MBA| ESBID_1 + ESBID_2,dist="logistic",na.rm=TRUE, data=datES) # All reported factors
    Esd1<-dbchoice(ESR1 +ESR2 ~ 1 + log(Income) | ESBID_1 + ESBID_2,dist="logistic",na.rm=TRUE, data=datES) # Reported model
    Esd2<-dbchoice(ESR1 +ESR2 ~ 1 + log(Income) + Attrib_Otter  + 
                     Attrib_Convenience  | ESBID_1 + ESBID_2,dist="logistic", na.rm=TRUE, data=datES)
    Esd3<-dbchoice(ESR1 +ESR2 ~ 1 + log(Income) + Attrib_Otter | ESBID_1 + ESBID_2,dist="logistic", na.rm=TRUE, data=datES)
    AIC(ESd0,Esd1,Esdfull, Esd2, Esd3) # Esd3 had lowest AIC 
    
    summary(Esd3)
    
    # Bootstrap confidence intervals about mean Willingness to pay for best fit model
    set.seed(123)
    bootCI(Esd3, nboot = 1000, CI = 0.95, individual = NULL)
    
    
    ## Sea Otter survey
    datS<- datWTP %>% dplyr::select(OTR1, OTR2,Income, OTBID_1,OTBID_2, Attrib_Birds,Attrib_Unique,Attrib_Otter,Attrib_Fish, Attrib_Convenience, NVisits,MBA) %>% drop_na()
    Sdb0<- dbchoice(OTR1 +OTR2 ~ 1 | OTBID_1 + OTBID_2,dist="logistic",na.rm=TRUE, data=datS)
    Sdbfull<- dbchoice(OTR1 +OTR2 ~ 1 + log(Income) + Attrib_Otter + Attrib_Unique + Attrib_Birds + Attrib_Fish + 
                         Attrib_Convenience + NVisits +MBA  | OTBID_1 + OTBID_2,dist="logistic", data=datS)
    Sdb1<-dbchoice(OTR1 +OTR2 ~ 1 + log(Income) | OTBID_1 + OTBID_2,dist="logistic", data=datS)
    Sdb2<-dbchoice(OTR1 +OTR2 ~ 1 + log(Income) + Attrib_Otter  + 
                     Attrib_Convenience  | OTBID_1 + OTBID_2,dist="logistic", data=datS)
    Sdb3<-dbchoice(OTR1 +OTR2 ~ 1 + log(Income) + Attrib_Otter  | OTBID_1 + OTBID_2,dist="logistic",na.rm=TRUE, data=datS)
    Sdb4<-dbchoice(OTR1 +OTR2 ~ 1 + Attrib_Otter  | OTBID_1 + OTBID_2,dist="logistic", data=datS)
    AIC(Sdb0, Sdb1, Sdbfull, Sdb2, Sdb3, Sdb4)
      summary(Sdb3)
      summary(Sdb4)
    
    # Bootstrap confidence intervals about mean Willingness to pay for best fit model
    set.seed(123)
    bootCI(Sdb3, nboot = 1000, CI = 0.95, individual = NULL)
    
      
    ## Plot both WTP on single graph
        ## Calculate probabilities with mean otter rank and income, varying bid amount only
        tst.data<- data.frame(Attrib_Otter= mean(datS$Attrib_Otter), Income= mean(datS$Income), OTBID_1=c(5:50))
        Spred<- predict(Sdb3, newdata= tst.data, type="probability")
      
        df<-cbind(tst.data,Spred)
        
        tst.data1<- data.frame(Income=mean(datES$Income), Attrib_Otter=mean(datES$Attrib_Otter), ESBID_1=c(5:50))
        Spred1<- Esd3 %>% predict(tst.data1, type="probability")
        df1<-cbind(df,Spred1)
        
        Preds<-melt(df1,id.vars=c("Attrib_Otter", "OTBID_1"))
        
        Pred1<-ggplot(Preds, aes(x=OTBID_1, y= value, color=variable)) +geom_smooth(method="loess", size=2) +scale_x_continuous(name="Proposed Fee Amount (USD)") +theme_themeo() +
          scale_color_manual(breaks= c("Spred1","Spred"), labels=c("Elkhorn Slough", "Sea Otter"), values= c("#026AA4", "#990000" )) +
          scale_y_continuous(limits= c(0,1), name="Probability of voting yes") + theme( legend.justification= c(0.9,0.9), legend.position=c(0.9,0.9))
        
        ggsave("Fig4_WTP.pdf",Pred1, width=183, scale =1,units="mm")
        ggsave("Fig4_WTP.png",Pred1, width=183, scale =1,units="mm")
        
        ## Plot by otter ranking instead of bid amount 
        tdat<- data.frame(Attrib_Otter= c(1:10), Income= mean(datS$Income), OTBID_1= mean(datS$OTBID_1))
        Spr<-predict(Sdb3, newdata=tdat, type= "probability")
        df<- cbind(tdat, Spr)
        
        tdat1<- data.frame(Attrib_Otter= c(1:10), Income= mean(datES$Income), ESBID_1= mean(datES$ESBID_1))
        Spr1<-predict(Esd3, newdata=tdat1, type= "probability")
        df1<- cbind(df, Spr1)
        
        ggplot(df, aes(x= Attrib_Otter, y= Spr)) +geom_line() + theme_themeo ()
        
        Preds<-melt(df1,id.vars=c("Attrib_Otter", "Income", "OTBID_1"))
        
        Pred2<-ggplot(Preds, aes(x=Attrib_Otter, y= value, color=variable)) +geom_line(size=1) +scale_x_continuous(name="Ranking Score of Sea Otters") +theme_themeo() +
          scale_color_manual(breaks= c("Spr","Spr1"), labels=c("Elkhorn Slough", "Sea Otter"), values= c("#026AA4", "#990000" )) +
          scale_y_continuous(limits= c(0,0.9), name="Probability of voting yes") + theme( legend.justification= c(0.9,0.95), legend.position=c(0.9,0.95))
        
        ggsave("Fig4_OtterRank.pdf",Pred2, width=183, scale =1,units="mm")
        ggsave("Fig4_OtterRank.png",Pred2, width=183, scale =1,units="mm")
