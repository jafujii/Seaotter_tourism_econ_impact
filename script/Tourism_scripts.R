## MIIS Survey Data Summary Figures

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


# Package required for DCchoice that no longer installs automatically
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("Icens")

### ggplot Theme from Becker
theme_themeo <- function () { 
  theme_classic()+
    theme(strip.background = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_text(margin = margin( 0.2, unit = "cm")),
          axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
          axis.ticks.length=unit(-0.1, "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=.5),
          legend.title=element_blank(),
          legend.position="bottom",
          strip.text=element_text(hjust=0) )}

#Edited Survey Response data (cleaned up for clarity)
#dem<-read.csv("SurveyData_edits.csv", header=T)

# Will look for repository folder under C drive user folder automatically. Repeated below for other data files. From Kisei  
dem<- read.csv(paste0("C:/Users/",Sys.info()[7],"/tourism/data/SurveyData_edits.csv"), header=T)
str(dem)
# Distribution of respondents' age
dem2<- dem %>% filter(!is.na(AGE))

ageden<-ggplot(dem2, aes(x= AGE))+geom_density(aes(y=..scaled..), alpha=0.2, color= "Blue",fill= "Blue")+ scale_y_continuous(limits=c(0,1), name="proportion")+
  geom_rug(aes(x=AGE, y=0), sides="b", position="jitter") + scale_x_continuous(name="Age(years)")+ theme_themeo()


## Density plot previous visits on Log scale with "rug" to show sample data
visden<-ggplot(dem2, aes(x= NVisits))+geom_density(aes(y=..scaled..), alpha=0.2, color= "Blue",fill= "Blue")+ 
  geom_rug(aes(x=NVisits, y=0), sides="b", position="jitter") + scale_y_continuous(limits=c(0,1), name="")+
  scale_x_continuous(name= "Log (Number previous visits)", trans="log1p", labels=scales::label_number(), breaks=c(0,50,100,200)) + theme_themeo() 


# Density plot visitor reported income on Log scale with "rug" to show sample data

incden<- ggplot(dem, aes(x= Income))+geom_density(aes(y=..scaled..), alpha=0.2, color= "Blue",fill= "Blue")+ 
  geom_rug(aes(x=Income, y=0), sides="b", position="jitter") + scale_y_continuous(limits=c(0,1), name="proportion") +
  scale_x_continuous(name= "Log (Annual Income)", trans="log10", labels=scales::label_number()) + theme_themeo() 

# Bar plot of origin of visit
# Not currently using
home<- dem %>%  filter(RegionName != " ") %>% mutate(name=factor(RegionName, levels= c( "Monterey Bay", "Outside California","East SF Bay",
                                                                                        "Southern California", "Northern California","West SF Bay"))) %>% group_by(name) %>% summarise(n= n()) %>% 
  mutate (prop=n/sum(n)*100) %>% arrange(desc(prop))

hm<- ggplot(home, aes(x=name, y= prop))+ geom_bar(stat="identity")+ theme_themeo ()+
  scale_y_continuous(name= "Percent") + scale_x_discrete(name= "", limits= rev(levels(home$name))) +coord_flip()


##Party Size density plot with "rug" to show sample data
  # Outliers of Party Size were not included in reported averages by Colgan, so also removed here. Likely data entry errors  
dem2<- dem %>% filter(Partysize <30)
Psden<- ggplot(dem2, aes(x= Partysize))+geom_density(aes(y=..scaled..), alpha=0.2, color= "Blue",fill= "Blue")+ 
  geom_rug(aes(x=Partysize, y=0), sides="b", position="jitter") + scale_y_continuous(limits=c(0,1), name="") +
  scale_x_continuous(name= "Party Size") + theme_themeo() 

## Combining plots together and exporting
denp<- (ageden + Psden)/(incden + visden)
ggsave("Figure2_density.pdf",width= 183, units="mm", denp)



####################### Summary and plotting of attributes rank scores ########################
dat2<- read.csv(paste0("C:/Users/",Sys.info()[7],"/tourism/data/AttributeRank.csv"), header=T)
#dat2<-read.csv("AttributeRank.csv", header=T)
dat2<-gather(dat2,attribute, rank, Attrib_Unique:Attrib_Convenience)

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

############## Willingness to Pay Analyses ###############################################
datWTP<-read.csv(paste0("C:/Users/",Sys.info()[7],"/tourism/data/WTPsurvey.csv"), header=T)
#datWTP<- read.csv("WTPsurvey.csv", header=T)
    
    ## Double bid dichotomous choice of Elkhorn Slough WTP Question. same models described by Colgan report
    
    datES<- datWTP %>% dplyr::select(ESR1, ESR2,Income, ESBID_1,ESBID_2, Attrib_Birds,Attrib_Unique,Attrib_Otter,
                                     Attrib_Fish, Attrib_Convenience, NVisits,MBA) %>% drop_na()
    
    ESd0<- dbchoice(ESR1 +ESR2 ~ 1 | ESBID_1 + ESBID_2,dist="logistic",na.rm=TRUE, data=datES)  # Null Model
    Esd1<-dbchoice(ESR1 +ESR2 ~ 1 + log(Income) | ESBID_1 + ESBID_2,dist="logistic",na.rm=TRUE, data=datES) # Reported model
    Esdfull<- dbchoice(ESR1 +ESR2 ~ 1 + log(Income) + Attrib_Otter + Attrib_Unique + Attrib_Birds + Attrib_Fish + 
                         Attrib_Convenience +NVisits + MBA| ESBID_1 + ESBID_2,dist="logistic",na.rm=TRUE, data=datES) # All reported factors
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
    
    Sdb1<-dbchoice(OTR1 +OTR2 ~ 1 + log(Income) | OTBID_1 + OTBID_2,dist="logistic", data=datS)
    Sdbfull<- dbchoice(OTR1 +OTR2 ~ 1 + log(Income) + Attrib_Otter + Attrib_Unique + Attrib_Birds + Attrib_Fish + 
                         Attrib_Convenience + NVisits +MBA  | OTBID_1 + OTBID_2,dist="logistic", data=datS)
    Sdb2<-dbchoice(OTR1 +OTR2 ~ 1 + log(Income) + Attrib_Otter  + 
                     Attrib_Convenience  | OTBID_1 + OTBID_2,dist="logistic", data=datS)
    Sdb3<-dbchoice(OTR1 +OTR2 ~ 1 + log(Income) + Attrib_Otter  | OTBID_1 + OTBID_2,dist="logistic", data=datS)
    Sdb4<-dbchoice(OTR1 +OTR2 ~ 1 + Attrib_Otter  | OTBID_1 + OTBID_2,dist="logistic", data=datS)
    AIC(Sdb0, Sdb1, Sdbfull, Sdb2, Sdb3, Sdb4)
      summary(Sdb3)
      summary(Sdb4)
    
    # Bootstrap confidence intervals about mean Willingness to pay for best fit model
    set.seed(123)
    bootCI(Sdb4, nboot = 1000, CI = 0.95, individual = NULL)
    
    ### Plot Probability for both questions based on model output
    Sp<-plot(Sdb4, xlab="Bid Amount (USD)",ylab="") 
      abline(h=0.5, col="grey")
      segments(x0=27.6, y0=-1, x1=27.6, y1=0.5, col="red")
    
    Ep<- plot(Esd3, xlab="Bid Amount (USD)")
      abline(h=0.5, col="grey")
      segments(x0=29.8, y0=-1, x1=29.8, y1=0.5, col="red")
    
        Sp + Ep
      
    ## Plot both WTP on single graph
        ## Calculate probabilities with mean otter rank and income, varying bid amount only
        tst.data<- data.frame(Attrib_Otter=mean(datS$Attrib_Otter), OTBID_1=c(5:50))
        Spred<-Sdb4 %>% predict(tst.data, type="probability")
        df<-cbind(tst.data,Spred)
        tst.data1<- data.frame(Income=mean(datES$Income), Attrib_Otter=mean(datES$Attrib_Otter), ESBID_1=c(5:50))
        Spred1<- Esd3 %>% predict(tst.data1, type="probability",)
        df1<-cbind(df,Spred1)
        
        
        Preds<-melt(df1,id.vars=c("Attrib_Otter", "OTBID_1"))
        
        Pred1<-ggplot(Preds, aes(x=OTBID_1, y= value, color=variable)) +geom_smooth(method="loess", size=2) +scale_x_continuous(name="Bid Amount") +theme_themeo() +
          scale_color_discrete(breaks= c("Spred1","Spred"), labels=c("Elkhorn Slough", "Sea Otter")) +
          scale_y_continuous(name="Probability of voting yes") 
        
        ggsave("Fig4_WTP.pdf",Pred1, width=183, scale =1,units="mm")
