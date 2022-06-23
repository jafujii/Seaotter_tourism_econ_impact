## Fujii et al. SeaOtters_Economic_Value
##WTP Analyses and Figures

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
library(ggthemes)



# Package required for DCchoice that no longer installs automatically
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.12")

BiocManager::install("Icens")
library(Incens)

install.packages("DCchoice",
                 repos = c("http://www.bioconductor.org/packages/release/bioc",
                           "https://cran.rstudio.com/"),
                 dep = TRUE)

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

############## Willingness to Pay Analyses ############
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
    
    AICc<- AIC(Esd3)+ (24/140)
    
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
        ## Calculate probabilities with mean otter rank and income, varying bid amount only. Does not calculate SE
        tst.data<- data.frame(Attrib_Otter= mean(datS$Attrib_Otter), Income= mean(datS$Income), OTBID_1=c(5:50))
        Spred<- predict(Sdb3, newdata= tst.data,  type="probability")
      
        
        df<-cbind(tst.data,Spred)
        
        tst.data1<- data.frame(Income=mean(datES$Income), Attrib_Otter=mean(datES$Attrib_Otter), ESBID_1=c(5:50))
        Spred1<- Esd3 %>% predict(tst.data1, type="probability")
        df1<-cbind(df,Spred1)
        
        Preds<-melt(df1,id.vars=c("Attrib_Otter", "OTBID_1"))
        
        Pred1<-ggplot(Preds, aes(x=OTBID_1, y= value, color=variable)) +geom_smooth(method="loess", size=2) +scale_x_continuous(name="Proposed Fee Amount (USD)")  +
          scale_color_manual(breaks= c("Spred1","Spred"), labels=c("Elkhorn Slough", "Sea Otter"), values= c("#026AA4", "#990000" )) +
          scale_y_continuous(limits= c(0,1), name="Probability of voting yes") + theme(legend.justification= c(0.9,0.9), legend.position=c(0.9,0.9)) +themeKV
        
        ggsave("FigX_WTP.pdf",Pred1, width=183, scale =1,units="mm")
        ggsave("FigX_WTP.png",Pred1, width=183, scale =1,units="mm")
        
        ### Figure 5 Plot by otter ranking rather  than  bid amount 
        tdat<- data.frame(Attrib_Otter= c(1:10), Income= mean(datS$Income), OTBID_1= mean(datS$OTBID_1))
        SOpr<-predict(Sdb3, newdata=tdat, type= "probability")
        df<- cbind(tdat, SOpr)
        
        tdat1<- data.frame(Attrib_Otter= c(1:10), Income= mean(datES$Income), ESBID_1= mean(datES$ESBID_1))
        ESpr1<-predict(Esd3, newdata=tdat1, type= "probability")
        df1<- cbind(df, ESpr1)
        
        ggplot(df, aes(x= Attrib_Otter, y= SOpr)) +geom_line() + themeKV
        
        Preds<-melt(df1,id.vars=c("Attrib_Otter", "Income", "OTBID_1"))
        
        Pred2<-ggplot(Preds, aes(x=Attrib_Otter, y= value, color=variable)) +geom_line(size=0.95) +scale_x_continuous(name="Ranking score of sea otters") +
          scale_color_manual(breaks= c("SOpr","ESpr1"), labels=c("Sea Otter", "Elkhorn Slough"), values= c("#026AA4", "#990000" )) +
          scale_y_continuous(limits= c(0,0.9), name="Probability of voting yes") + themeKV + 
          theme(legend.text= element_text(size= 10),legend.justification= c(0.95,0.95),legend.position= c(0.95,0.3),
                legend.title=element_blank(), strip.text=element_text(hjust=0))
        
        ggsave("Fig5_OtterRank.pdf",Pred2, width=90,height=90, scale =1,dpi=800, units="mm")
        ggsave("Fig5_OtterRank.png",Pred2, width=90, height=90,scale =1,dpi=800, units="mm")
