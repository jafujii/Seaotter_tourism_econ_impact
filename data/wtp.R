library(tidyverse)
library(patchwork)
library(reshape2)
library(DCchoice)
library(Ecdat)
library(lmtest)
library(AICcmodavg)
library(here)


## Loading older version of requried package
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("Icens")

### Sample data
data(NaturalPark, package = "Ecdat") # Access the example data file
NP <- NaturalPark                    # Rename the data file to something short
head(NP, n = 3)                      # Display the first three rows of data 
NP$ans1 <- ifelse(NP$answers == "yy" | NP$answers == "yn", 1, 0)  ## This is already done in our dataset
table(NP$ans1, NP$bid1) 
round(tapply(NP$ans1, NP$bid1, mean), 2)
sb1 <- sbchoice(ans1 ~ 1 | bid1,  dist = "logistic", data = NP)
coeftest(sb1)
summary(sb1)

## MIIS Data
dat<- read.csv("WTPsurvey.csv", header=T)
str(dat)
table(dat$ESR1, dat$ESBID_1)
round(tapply(dat$ESR1, dat$ESBID_1, mean), 2)
round(tapply(dat$OTR1, dat$OTBID_1, mean), 2)

sb1<-sbchoice(ESR1~ 1+ AGE + NVisits + log(Income) + Attrib_Otter|ESBID_1, dist="logistic", na.rm=TRUE, data =dat)

plot(sb1, las = 1) 


## Double bid of Elkhorn Slough WTP Q. same models described by Colgan report

dat1<- dat %>% dplyr::select(ESR1, ESR2,Income, ESBID_1,ESBID_2, Attrib_Birds,Attrib_Unique,Attrib_Otter,Attrib_Fish, Attrib_Convenience, NVisits,MBA) %>% drop_na()

ESd0<- dbchoice(ESR1 +ESR2 ~ 1 | ESBID_1 + ESBID_2,dist="logistic",na.rm=TRUE, data=dat1)
Esd1<-dbchoice(ESR1 +ESR2 ~ 1 + log(Income) | ESBID_1 + ESBID_2,dist="logistic",na.rm=TRUE, data=dat1)
Esdfull<- dbchoice(ESR1 +ESR2 ~ 1 + log(Income) + Attrib_Otter + Attrib_Unique + Attrib_Birds + Attrib_Fish + 
                    Attrib_Convenience +NVisits + MBA| ESBID_1 + ESBID_2,dist="logistic",na.rm=TRUE, data=dat1)
Esd2<-dbchoice(ESR1 +ESR2 ~ 1 + log(Income) + Attrib_Otter  + 
                Attrib_Convenience  | ESBID_1 + ESBID_2,dist="logistic", na.rm=TRUE, data=dat1)
Esd3<-dbchoice(ESR1 +ESR2 ~ 1 + log(Income) + Attrib_Otter | ESBID_1 + ESBID_2,dist="logistic", na.rm=TRUE, data=dat1)
AIC(ESd0,Esd1,Esdfull, Esd2, Esd3)

set.seed(123)
bootCI(Esd3, nboot = 1000, CI = 0.95, individual = NULL)

                                                    
## Sea Otter survey
dat2<- dat %>% dplyr::select(OTR1, OTR2,Income, OTBID_1,OTBID_2, Attrib_Birds,Attrib_Unique,Attrib_Otter,Attrib_Fish, Attrib_Convenience, NVisits,MBA) %>% drop_na()
Sdb0<- dbchoice(OTR1 +OTR2 ~ 1 | OTBID_1 + OTBID_2,dist="logistic",na.rm=TRUE, data=dat2)

Sdb1<-dbchoice(OTR1 +OTR2 ~ 1 + log(Income) | OTBID_1 + OTBID_2,dist="logistic", data=dat2)
Sdbfull<- dbchoice(OTR1 +OTR2 ~ 1 + log(Income) + Attrib_Otter + Attrib_Unique + Attrib_Birds + Attrib_Fish + 
                    Attrib_Convenience + NVisits +MBA  | OTBID_1 + OTBID_2,dist="logistic", data=dat2)
Sdb2<-dbchoice(OTR1 +OTR2 ~ 1 + log(Income) + Attrib_Otter  + 
                Attrib_Convenience  | OTBID_1 + OTBID_2,dist="logistic", data=dat2)
Sdb3<-dbchoice(OTR1 +OTR2 ~ 1 + log(Income) + Attrib_Otter  | OTBID_1 + OTBID_2,dist="logistic", data=dat2)
set.seed(123)
bootCI(Sdb3, nboot = 1000, CI = 0.95, individual = NULL)

### Plot Probability for both questions
Sp<-plot(Sdb3, xlab="Bid Amount (USD)",ylab="") 
  abline(h=0.5, col="grey")
segments(x0=27.6, y0=-1, x1=27.6, y1=0.5, col="red")


Ep<- plot(Esd3, xlab="Bid Amount (USD)")
abline(h=0.5, col="grey")
segments(x0=29.8, y0=-1, x1=29.8, y1=0.5, col="red")

Sp + Ep

## Calculate probabilities for plotting on a single frame
tst.data<- data.frame(Income=mean(dat2$Income), Attrib_Otter=mean(dat2$Attrib_Otter), OTBID_1=c(5:50))
Spred<-Sdb3 %>% predict(tst.data, type="probability")
df<-cbind(tst.data,Spred)
tst.data1<- data.frame(Income=mean(dat1$Income), Attrib_Otter=mean(dat1$Attrib_Otter), ESBID_1=c(5:50))
Spred1<- Esd3 %>% predict(tst.data1, type="probability",)
df1<-cbind(df,Spred1)


Preds<-melt(df1,id.vars=c("Income", "Attrib_Otter", "OTBID_1"))

Pred<- ggplot(tst.data, aes(x=OTBID_1, y= Spred)) +geom_smooth(size=2) +geom_smooth(tst.data1, aes(x=ESBID_1, y= Spred1))
Pred1<-ggplot(Preds, aes(x=OTBID_1, y= value, color=variable)) +geom_smooth(method="loess", size=2) +scale_x_continuous(name="Bid Amount") +theme_themeo() +
  scale_color_discrete(breaks= c("Spred1","Spred"), labels=c("Elkhorn Slough", "Sea Otter")) +
  scale_y_continuous(name="Probability of voting yes") +
  annotate (geom="point", x=28, y=0.50, color="black", size=3)+
  annotate(geom="point", x=30.7, y= 0.5, color="black", size=3)

ggsave("Fig4_WTP.pdf",Pred1, width=183, scale =1,units="mm")
