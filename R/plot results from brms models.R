rm(list=ls()) 


library(corrplot)
library(ggcorrplot)
library(ggplot2)
library(reshape2)
library(tidyverse)
col.scheme.realm<-c(  "Freshwater"  = "dodgerblue2", "Terrestrial" = "chocolate4")
theme_clean<- theme_grey() + theme(panel.grid.major = element_blank(), 
                                   panel.grid.minor = element_blank(),
                                   panel.background = element_blank(), 
                                   axis.line = element_line(colour = "black") , 
                                   legend.key=element_blank())
studies<-read.csv(file = "C:\\Dropbox\\Insect Biomass Trends/csvs/studies 5.2.csv", header = T); dim(studies)



filenames <- list.files("D:/work/2017 iDiv/2018 insect biomass/Insect-trends-correlations/model-outputs/prior sd1/", pattern="*corSum*", full.names=TRUE)
ldf <- lapply(filenames, readRDS)
cors<- do.call(rbind.data.frame, ldf); dim(cors)
cors<- subset(cors, numberOfGoodDatasets >4); dim(cors) # exclude crappy data  57 -> 46


filenamesSlopes <- list.files("D:/work/2017 iDiv/2018 insect biomass/Insect-trends-correlations/model-outputs/prior sd1/", pattern="slope*", full.names=TRUE)
ldf <- lapply(filenamesSlopes, readRDS)
slopes<- do.call(rbind.data.frame, ldf)
dim(slopes)
slopes<- subset(slopes, task.id %in% cors$task.id); dim(slopes)

filenamesCorraw <- list.files("D:/work/2017 iDiv/2018 insect biomass/Insect-trends-correlations/model-outputs/prior sd1/", pattern="cors_*", full.names=TRUE)
ldf <- lapply(filenamesCorraw, readRDS)
corSamples<- do.call(rbind.data.frame, ldf)
dim(corSamples)
corSamples<- subset(corSamples, task.id %in% cors$task.id); dim(corSamples)

print("prior sd = 1")
sd(cors$meanCor)
sd(slopes$trend_T1)
sd(slopes$trend_T2)
sd(corSamples$cor)

cors$evidence<- "none"
cors$evidence[cors$lower80Cor>0 | cors$upper80Cor <0] <- "weak"
cors$evidence[cors$lower90Cor>0 | cors$upper90Cor <0] <- "medium"
cors$evidence[cors$lower95Cor>0 | cors$upper95Cor <0] <- "strong"
cors$evidencenum<- 1
cors$evidencenum[cors$lower80Cor>0 | cors$upper80Cor <0] <- 0.04
cors$evidencenum[cors$lower90Cor>0 | cors$upper90Cor <0] <- 0.009
cors$evidencenum[cors$lower95Cor>0 | cors$upper95Cor <0] <- 0.0001


table( slopes$Realm)
length(unique(subset(slopes, Realm == "Terrestrial" )$Datasource_ID))
length(unique(subset(slopes, Realm == "Terrestrial" )$Plot_ID))
length(unique(subset(slopes, Realm == "Freshwater"  )$Datasource_ID))
length(unique(subset(slopes, Realm == "Freshwater"  )$Plot_ID))

# get some elemetary info on the data : 
# Where are the data from? 
includedStudies<- studies [studies$Datasource_ID %in% unique(slopes$Datasource_ID) , ]
includedPlots<- studies [studies$Datasource_ID %in% unique(slopes$Datasource_ID) , ]
table(includedStudies$Continent, includedStudies$Realm)



# Fig 1 Data avialability #####
ggplot(subset(cors, numberOfGoodDatasets >4), aes(Taxon1, Taxon2, fill= numberOfGoodDatasets)) + 
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red", name = "Datasets") +
  facet_wrap(.~ Realm, scales = 'free')+
  ggtitle("A. Data availability per order")+
  theme_clean +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        legend.key=element_blank(), 
        legend.position="right")

par(mfrow = c(1,2))#, 
   # mar = c())
# using corrplot

dataMatrF<- dcast(subset(cors, numberOfGoodDatasets >4 & Realm == "Freshwater"),   Taxon1 ~ Taxon2, value.var = "numberOfGoodDatasets")
rownames(dataMatrF) <- dataMatrF$Taxon1
dataMatrF<- as.matrix(dataMatrF[, -(1)])
dataMatrT<- dcast(subset(cors, numberOfGoodDatasets >4 & Realm == "Terrestrial"),   Taxon1 ~ Taxon2, value.var = "numberOfGoodDatasets")
rownames(dataMatrT) <- dataMatrT$Taxon1
dataMatrT<- as.matrix(dataMatrT[, -(1)])
col3 = hcl.colors(7, "YlOrRd", rev = TRUE)
corrplot(dataMatrF,   col = col3,   is.corr = FALSE, tl.col = 'black', type = 'lower',   col.lim = c(5, 19), 
         method = 'color',  diag = T, na.label = " ", 
         title = "Freshwater", mar=c(0,0,1,0))
corrplot(dataMatrT,   col = col3,   is.corr = FALSE, tl.col = 'black', type = 'lower', col.lim = c(5, 19), 
         method = 'color', diag = T, 
         title = "Terrestrial", mar=c(0,0,1,0),
         na.label = " ") #



# calculate men and sd of slope estimates
mnSlopes<- aggregate(.~task.id +Taxon1 +Taxon2  +Plot_ID + Datasource_ID+Realm, data = slopes,  FUN = "mean"); dim(mnSlopes)
sdSlopes<- aggregate(.~ task.id +Taxon1 +Taxon2 +Plot_ID + Datasource_ID+Realm, data = slopes,  sd); dim(sdSlopes)
mnSlopes$sdTrend_T1<- sdSlopes$trend_T1
mnSlopes$sdTrend_T2<- sdSlopes$trend_T2
cor(mnSlopes$sdTrend_T1, mnSlopes$sdTrend_T2) # 0.866 high correlation so we might just use the mean sd of the two trends for weighting 
mnSlopes$mnSD<- apply(mnSlopes[, c("sdTrend_T1", "sdTrend_T2")], 1, mean) # 
merge(slopes, mnSlopes[, c("task.id", "Taxon1","Taxon2", "Plot_ID", "mnSD")]) # merge sd's into original slopes so that these can be used for weighting 

ggplot(mnSlopes, aes(x = Realm, y = trend_T1)) + # variability for fw is much higher... 
  geom_point(position = "jitter")+
  ylab ("mean")
ggplot(sdSlopes, aes(x = Realm, y = trend_T1)) + 
  geom_point(position = "jitter")+
  ylab ("Standard deviation")
boxplot(sdSlopes$trend_T1 ~ sdSlopes$Realm )



 

#Fig 2a?  overall correlations #####
mnSlpFw<- subset(mnSlopes , Realm == "Freshwater")
mnSlpT<- subset(mnSlopes , Realm == "Terrestrial")
cor.test(mnSlpFw$trend_T1, mnSlpFw$trend_T2) # r = 0.241, p = <0.0001
cor.test(mnSlpT$trend_T1,  mnSlpT$trend_T2) # r = 0.20, p = <0.0001
# try weighted correlation 
library(wCorr)
weightedCorr(mnSlpFw$trend_T1, mnSlpFw$trend_T2 , weights = 1/mnSlpFw$mnSD) # 0.28
weightedCorr(mnSlpT$trend_T1, mnSlpT$trend_T2 , weights = 1/mnSlpT$mnSD) # 0.27


table(mnSlopes$trend_T1>0, mnSlopes$trend_T2>0, mnSlopes$Realm )
plot(density(mnSlopes$trend_T1))
hist(slopes$trend_T2) # Almost equal increaes and decreases, but parallel increases ad declines are more common 
#plot(slopes$trend_T1~ slopes$trend_T2)


#extract percentages in each corner
dat_text<- as.data.frame(table(mnSlopes$trend_T1>0, mnSlopes$trend_T2>0, mnSlopes$Realm ))
dat_text$prop<- round( dat_text$Freq /  rep(table(mnSlopes$Realm ), each = 4), 2)
dat_text$x<-  c(-0.20, 0.25, -0.20, 0.25,     -0.07,  0.06, -0.07, 0.06 )
dat_text$y <- c(-0.20, -0.20, 0.20, 0.20,     -0.11, -0.11, 0.06,  0.06)
dat_text$Realm <- dat_text$Var3
dat_text$txt<- paste0 (dat_text$prop*100 , "%")


# plot all sampled slopes
ggplot(slopes , aes(x = trend_T1, y = trend_T2)) + 
  geom_bin2d(bins=50)+ 
  xlab("") + ylab("")+
  geom_abline(intercept = 0 , slope = 1)+
  geom_hline(yintercept = 0)+ geom_vline(xintercept = 0)+
  facet_wrap(.~Realm, scales = "free") # this looks bad because of high uncertainty in fw. 
# also, there are some absurd slopes...

# only mean slopes
ggplot(mnSlopes , aes(x = trend_T1, y = trend_T2)) + 
  geom_bin2d(bins=50)+ 
  xlab("Mean trend taxon 1") + ylab("Mean trend taxon 2")+
  geom_abline(intercept = 0 , slope = 1)+
  geom_hline(yintercept = 0)+ geom_vline(xintercept = 0)+
  facet_wrap(.~Realm, scales = "free") +# slopes with multiple pairwise comparisons are plotted multiple times here. 
  geom_text(
    data    = dat_text,
    mapping = aes(x = x, y = y, label = prop)
  )




# for making sure that no slopes are plotted twice (because of multiple pairwise comparisons)
library(splitstackshape)
library(wCorr)
slopes$index1<- paste0(slopes$Plot_ID, slopes$Taxon1)# make indices on which comparisons in each plot can be excluded after they were selected
slopes$index2<- paste0(slopes$Plot_ID, slopes$Taxon2)

slopesX<- slopes # this takes some time 
slopesRandom<- NULL
while (nrow(slopesX)>0) {
  slopesRandom2<- stratified(slopesX, "Plot_ID", 1); dim(slopesRandom2) # take 1 slope per plot from all mean slopes
  slopesRandom<- rbind(slopesRandom, slopesRandom2); dim(slopesRandom) # glue selected random dfs  together
  slopesX<- slopesX [!slopesX$index1 %in% slopesRandom$index1 &  # remove the randomly selected taxa from each plot 
                           !slopesX$index1 %in% slopesRandom$index2&
                           !slopesX$index2 %in% slopesRandom$index1&
                           !slopesX$index2 %in% slopesRandom$index2, ] ; dim(slopesX) #  repeat until no rows left 
}; dim(slopesRandom)
slopesRandom <- merge(slopesRandom, mnSlopes[, c("task.id", "Taxon1","Taxon2", "Plot_ID", "mnSD")]); dim(slopesRandom)  # merge in matching mean sd as weight 

        ggplot(subset(slopesRandom, task.id %in% subset(cors, numberOfGoodDatasets >4)$task.id)  , aes(x = trend_T1, y = trend_T2)) + 
          geom_bin2d(bins=50)+ 
          xlab("") + ylab("")+
          geom_abline(intercept = 0 , slope = 1)+
          geom_hline(yintercept = 0)+ geom_vline(xintercept = 0)+
          facet_wrap(.~Realm, scales = "free") # as expected still looks like shite

slopesRandomFw<- subset(slopesRandom, Realm == "Freshwater" )
cor.test(slopesRandomFw$trend_T1,slopesRandomFw$trend_T2) # r
weightedCorr(slopesRandomFw$trend_T1, slopesRandomFw$trend_T2, weights = 1/slopesRandomFw$mnSD)

slopesRandomT<- subset(slopesRandom, Realm == "Terrestrial")
cor.test(slopesRandomT$trend_T1, slopesRandomT$trend_T2) # r =
weightedCorr(slopesRandomT$trend_T1,slopesRandomT$trend_T2, weights = 1/slopesRandomT$mnSD)

slopesT<- subset(slopes, Realm == "Terrestrial")



# including all independent comparisons (i.e. no slopes included twice) 
mnSlopes$index1<- paste0(mnSlopes$Plot_ID, mnSlopes$Taxon1)
mnSlopes$index2<- paste0(mnSlopes$Plot_ID, mnSlopes$Taxon2)

mnSlopesX<- mnSlopes
mnSlopesRandom<- NULL
while (nrow(mnSlopesX)>0) {
  mnSlopesRandom2<- stratified(mnSlopesX, "Plot_ID", 1); dim(mnSlopesRandom2) # take 1 slope per plot from all mean slopes
  mnSlopesRandom<- rbind(mnSlopesRandom, mnSlopesRandom2); dim(mnSlopesRandom) # glue selected dfs  pogether
  mnSlopesX<- mnSlopesX [!mnSlopesX$index1 %in% mnSlopesRandom$index1 &  # remove the randomly selected taxa from each plot 
                        !mnSlopesX$index1 %in% mnSlopesRandom$index2&
                        !mnSlopesX$index2 %in% mnSlopesRandom$index1&
                        !mnSlopesX$index2 %in% mnSlopesRandom$index2, ] ; dim(mnSlopesX) #  stng  left 
}; dim(mnSlopesRandom)

# only mean slopes
#dat_text<- as.data.frame(table(mnSlopesRandom$trend_T1>0, mnSlopesRandom$trend_T2>0, mnSlopesRandom$Realm ))
#dat_text$prop<- round( dat_text$Freq /  rep(table(mnSlopesRandom$Realm ), each = 4), 2)
#dat_text$x<-  c(-0.12, 0.12, -0.12, 0.12,     -0.08,  0.05, -0.08, 0.05 )
#dat_text$y <- c(-0.12, -0.12, 0.15, 0.15,     -0.08, -0.08, 0.06,  0.06)
#dat_text$Realm <- dat_text$Var3

# fig 2a#####
ggplot(mnSlopesRandom, aes(x = trend_T1, y = trend_T2)) + 
  geom_bin2d(bins=50)+ 
  xlab("") + ylab("")+
  xlab("Trend slope taxon 1") + ylab("Trend slope taxon 2")+
  geom_hline(yintercept = 0)+ geom_vline(xintercept = 0)+
  facet_wrap(.~Realm, scales = "free")+
  #ggtitle("Mean slopes")+
  geom_text(
    data    = dat_text,
    mapping = aes(x = x, y = y, label = txt) )+
  theme_clean


# overall correlations (results par 2)
aggregate( .~     Realm, data= cors[4:14], FUN = "mean")
aggregate( .~     Realm, data= cors[4:14], FUN = "sd")

#THrashed: 
mnSlopesRandomFw<- subset(mnSlopesRandom,  Realm == "Freshwater")
cor.test(mnSlopesRandomFw$trend_T1,mnSlopesRandomFw$trend_T2) # r = 0.237, p = <0.0001
#fwcors<- c(fwcors,  cor(mnSlopesRandomFw$trend_T1, mnSlopesRandomFw$trend_T2)) # In case I wanna loop this x times. but this will only give the same as the overall r   r = 0.19, p = <0.0001
weightedCorr(mnSlopesRandomFw$trend_T1,mnSlopesRandomFw$trend_T2, weights = 1/mnSlopesRandomFw$mnSD)

mnSlopesRandomT<- subset(mnSlopesRandom,  Realm == "Terrestrial")
cor.test(mnSlopesRandomT$trend_T1, mnSlopesRandomT$trend_T2) # r = 0.19, p = <0.0001
#Tercors<- c(Tercors,  cor(mnSlopesRandomT$trend_T1, mnSlopesRandomT$trend_T2)) # r = 0.19, p = <0.0001
weightedCorr(mnSlopesRandomT$trend_T1,mnSlopesRandomT$trend_T2, weights = 1/mnSlopesRandomT$mnSD)

median(Tercors)  # only works in case of loop. 
cor.test(subset(mnSlopes, Realm == "Terrestrial")$trend_T1, subset(mnSlopes, Realm == "Terrestrial")$trend_T2)
weightedCorr(subset(mnSlopes, Realm == "Terrestrial")$trend_T1, subset(mnSlopes, Realm == "Terrestrial")$trend_T2, weights = 1/subset(mnSlopes, Realm == "Terrestrial")$mnSD)

median(fwcors)
cor.test(subset(mnSlopes, Realm == "Freshwater")$trend_T1, subset(mnSlopes, Realm == "Freshwater")$trend_T2)
weightedCorr(subset(mnSlopes, Realm == "Freshwater")$trend_T1, subset(mnSlopes, Realm == "Freshwater")$trend_T2, weights = 1/subset(mnSlopes, Realm == "Freshwater")$mnSD)
# almost the same! 
# not sure what this tells us. 
# but this is almost a necessity! because negative covariance can only occur in maximum half of all comparisons
# additionally, several slopes will be both on x and on y repeatedly! 










# What about comparisons where at least one of the two trends is significant? 
# expectation 

sigSlopes<-  subset(slopes, task.id %in% subset(cors, numberOfGoodDatasets >4)$task.id) %>% 
  group_by(task.id, Taxon1, Taxon2, Plot_ID, Datasource_ID, Realm) %>%
  summarise(
    meanSlpT1 = mean(trend_T1),
    medianSlpT1 = median(trend_T1),
    lower80SlpT1 = quantile(trend_T1,0.10),
    upper80SlpT1 = quantile(trend_T1,0.90),
    lower90SlpT1 = quantile(trend_T1,0.05),
    upper90SlpT1 = quantile(trend_T1,0.95),
    lower95SlpT1 = quantile(trend_T1,0.025),
    upper95SlpT1 = quantile(trend_T1,0.975),
    
    meanSlpT2 = mean(trend_T2),
    medianSlpT2 = median(trend_T2),
    lower80SlpT2 = quantile(trend_T2,0.10),
    upper80SlpT2 = quantile(trend_T2,0.90), 
    lower90SlpT2 = quantile(trend_T2,0.05),
    upper90SlpT2 = quantile(trend_T2,0.95),
    lower95SlpT2 = quantile(trend_T2,0.025),
    upper95SlpT2 = quantile(trend_T2,0.975))

mnSlopesSig<- list(
  allSlopes = sigSlopes, 
sigSlopes80= subset(sigSlopes,  lower80SlpT1>0 | upper80SlpT1 <0 | lower80SlpT2>0 | upper80SlpT2 <0),
sigSlopes90= subset(sigSlopes,  lower90SlpT1>0 | upper90SlpT1 <0 | lower90SlpT2>0 | upper90SlpT2 <0),
sigSlopes95= subset(sigSlopes,  lower95SlpT1>0 | upper95SlpT1 <0 | lower95SlpT2>0 | upper95SlpT2 <0),

sigSlopes80both= subset(sigSlopes,  (lower80SlpT1>0 | upper80SlpT1 <0) & (lower80SlpT2>0 | upper80SlpT2 <0)) ,
sigSlopes90both= subset(sigSlopes,  (lower90SlpT1>0 | upper90SlpT1 <0) & (lower90SlpT2>0 | upper90SlpT2 <0)) ,
sigSlopes95both= subset(sigSlopes,  (lower95SlpT1>0 | upper95SlpT1 <0) & (lower95SlpT2>0 | upper95SlpT2 <0)) 
)



lapply(mnSlopesSig, function(x) { 
ggplot( x  , 
       aes(x = meanSlpT1, y = meanSlpT2)) + 
  geom_bin2d(bins=50)+ 
  xlab("") + ylab("")+
  geom_abline(intercept = 0 , slope = 1)+
  geom_hline(yintercept = 0)+ geom_vline(xintercept = 0)+
  facet_wrap(.~Realm, scales = "free")+ 
  ggtitle (deparse(substitute(x)))
  } )


ldf<- lapply(mnSlopesSig, function(x) {cor(x$meanSlpT1, x$meanSlpT2)})
do.call(rbind, ldf)

frequencies<- NULL
for (i in 1: length(mnSlopesSig)){
df1<- mnSlopesSig[[i]]
name<- names(mnSlopesSig)[i]
test<-as.data.frame(table(df1$meanSlpT1 >0, df1$meanSlpT2>0, df1$Realm ))# / (table(sigSlopes$Realm) )
test$prop <- test$Freq/rep(table(df1$Realm), each = 4)
test$dfName<- name
frequencies<- rbind(frequencies, test)
}

# make df of wrongly allocated directions under assumption of parity  
falseFreq<- aggregate (.~ dfName + Var3, data = subset(frequencies, Var1 != Var2  ), sum)
falseFreq$dfName <-factor(falseFreq$dfName, levels = rev(sort(unique(falseFreq$dfName))))
falseFreq$propcorrect<- 1-falseFreq$prop


ggplot(falseFreq, aes (x = dfName, y = propcorrect,   color = Var3))+ 
  geom_point(size = 4)+
  coord_flip()+
  ylim ( 0.5, 1)+
  scale_color_manual(values = col.scheme.realm)+
  ylab ("Proportion slopes in same direction")+
  theme_clean
# this is actually not correct. The correlations for both significant are not 85%, but unknown. 








#correlation coefficients

#Fig 2B #####
cors_good<- subset(cors, numberOfGoodDatasets >4 & numberOfGoodPlots >19)
ggplot(cors_good, aes(Taxon1, Taxon2, fill= medianCor)) + 
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid  = "grey90", high = "red", na.value = "grey50",) +
  facet_wrap(.~ Realm, scales = 'free')+
  theme_classic()+
  ggtitle("At least five datasets")+
  theme_clean +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        legend.key=element_blank(), 
        legend.position="right")
plot(density(cors_good$medianCor), main = "Density of correlation coefficients")



# try other visualization 

testmatr<- dcast(subset(cors, Realm == "Freshwater" & numberOfGoodDatasets >4 & numberOfGoodPlots >19 ) , Taxon1 ~ Taxon2, value.var = "medianCor")
testmatrT<- dcast(subset(cors, Realm == "Terrestrial" & numberOfGoodDatasets >4 & numberOfGoodPlots >19 ) , Taxon1 ~ Taxon2, value.var = "medianCor")
rownames(testmatr) <- testmatr$Taxon1
testmatr<- as.matrix(testmatr[, -(1)])
rownames(testmatrT) <- testmatrT$Taxon1
testmatrT<- as.matrix(testmatrT[, -(1)])
min(testmatr , na.rm = T); max(testmatr , na.rm = T)
min(testmatrT , na.rm = T); max(testmatrT , na.rm = T)

pMatrFW<- dcast(subset(cors, Realm == "Freshwater" & numberOfGoodDatasets >4 & numberOfGoodPlots >19 ) , Taxon1 ~ Taxon2, value.var = "evidencenum")
rownames(pMatrFW) <- pMatrFW$Taxon1
pMatrFW<- as.matrix(pMatrFW[, -(1)])
pMatrT<- dcast(subset(cors, Realm == "Terrestrial" & numberOfGoodDatasets >4 & numberOfGoodPlots >19 ) , Taxon1 ~ Taxon2, value.var = "evidencenum")
rownames(pMatrT) <- pMatrT$Taxon1
pMatrT<- as.matrix(pMatrT[, -(1)])



par(mfrow = c(1,2))
#corrplot(as.matrix(testmatr))
#corrplot(testmatr, method = 'ellipse')#, order = 'AOE', type = 'upper')
corrplot(testmatr,p.mat = pMatrFW,   is.corr =F, type = 'lower',tl.col = 'black', na.label = ".",
         col.lim = c(-0.10, 0.4), insig = 'label_sig', sig.level = c(0.001, 0.01, 0.05), 
         pch.cex = 1.5, pch.col = 'grey20')
corrplot(testmatrT, p.mat = pMatrT,  is.corr = F, type = 'lower',tl.col = 'black', na.label = ".",
         col.lim = c(-0.10, 0.4), insig = 'label_sig', sig.level = c(0.001, 0.01, 0.05), 
         pch.cex = 1.5, pch.col = 'grey20')









# Fig 2c plot correlations for by  taxon #####


a<- unique((corSamples[, c("Taxon1", "Realm")]))  
b<- unique((corSamples[, c("Taxon2", "Realm")]))  
names(b)<- names(a) # to have the same colum names
taxaRealm<- unique(rbind(a,b))

meanCorPerTaxon <- NULL
allcors<- NULL
for (i in 1 : nrow(taxaRealm)){ 
  subs<- subset(cors, Taxon1 == taxaRealm[i,"Taxon1"] & Realm == taxaRealm[i,"Realm"] | 
                  Taxon2 == taxaRealm[i,"Taxon1"] & Realm == taxaRealm[i,"Realm"])  
  subs$Taxon<- taxaRealm[i,"Taxon1"]
  res<-data.frame(
    Taxon = taxaRealm[i,"Taxon1"],
    Realm = taxaRealm[i,"Realm"],
    nrCorrelations = nrow(subs),
    meanCor = mean(subs$medianCor), 
    sdCor = sd(subs$medianCor)  )
  
  meanCorPerTaxon<- rbind (meanCorPerTaxon, res)
  allcors<- rbind(allcors, subs)
}
arrange(meanCorPerTaxon, Realm, Taxon)

allcorSamples<- NULL
for (i in 1 : nrow(taxaRealm)){ 
  subs<- subset(corSamples, Taxon1 == taxaRealm[i,"Taxon1"] & Realm == taxaRealm[i,"Realm"] | 
                  Taxon2 == taxaRealm[i,"Taxon1"] & Realm == taxaRealm[i,"Realm"])  # grab all lines that have taxon x
  subs$Taxon<- taxaRealm[i,"Taxon1"]

  allcorSamples<- rbind(allcorSamples, subs)
}
dim(allcorSamples)
head(arrange(allcorSamples, Realm, Taxon))

# exclude samples outside the 95% CI for each comparison
#what are the CI's?
corCIs<-  allcorSamples %>% 
  group_by(task.id, Taxon1, Taxon2, Realm) %>%
  summarise(
    meanCor = mean(cor),
    medianCor = median(cor),
    lower80cor = quantile(cor,0.10),
    upper80cor = quantile(cor,0.90),
    lower90cor = quantile(cor,0.05),
    upper90cor = quantile(cor,0.95),
    lower95cor = quantile(cor,0.025),
    upper95cor = quantile(cor,0.975))
    
allcorSamples$good<- FALSE # a sort of blunt force way of figuring out which values to include in the plot 
for (i in 1: nrow(allcorSamples)){

  ii<- allcorSamples$task.id[i]
    if(  allcorSamples$cor[i] < corCIs$upper95cor[corCIs$task.id == ii] & 
         allcorSamples$cor[i] > corCIs$lower95cor[corCIs$task.id == ii])
  allcorSamples$good[i]<- TRUE
    
}


allcorSamples$Taxon <-factor(allcorSamples$Taxon, levels = rev(sort(unique(allcorSamples$Taxon))))

ggplot()+
  geom_violin(data =subset(allcorSamples, good == TRUE)  , aes(x = Taxon, y = cor, color = Realm, fill = Realm), 
              position=position_dodge(width= 0.6)) +
  geom_point(data = allcors, aes(x = Taxon, y = medianCor, group = Realm), shape = 1, color = "black", #fill = "black",  
             position=position_dodge(width= 0.6)) +
  geom_point(data =meanCorPerTaxon , aes(x = Taxon, y = meanCor,  group = Realm) ,  size=3, fill="black", shape = 18, 
               position=position_dodge(width= 0.6))+
  geom_hline(yintercept = 0)+
  ylab ("Correlation coefficient")+
  scale_color_manual(values = col.scheme.realm)+
  scale_fill_manual(values = col.scheme.realm)+
  coord_flip()+
  theme_clean +
  theme(legend.key=element_blank(), 
        legend.position="bottom")


















# Rest taxa #####
#############

filenames <- list.files("D:/work/2017 iDiv/2018 insect biomass/Insect-trends-correlations/model-outputs/rest taxa prior sd1/", pattern="*corSum*", full.names=TRUE)
ldf <- lapply(filenames, readRDS)
Gcors<- do.call(rbind.data.frame, ldf); dim(Gcors)
Gcors<- subset(Gcors, numberOfGoodDatasets >4 & numberOfGoodPlots > 19); dim(Gcors) # exclude crappy data  57 -> 46

filenamesSlopes <- list.files("D:/work/2017 iDiv/2018 insect biomass/Insect-trends-correlations/model-outputs/rest taxa prior sd1/", pattern="slope*", full.names=TRUE)
ldf <- lapply(filenamesSlopes, readRDS)
slopes<- do.call(rbind.data.frame, ldf); dim(slopes)
slopes<- subset(slopes, task.id %in% Gcors$task.id); dim(slopes)

filenamesCorraw <- list.files("D:/work/2017 iDiv/2018 insect biomass/Insect-trends-correlations/model-outputs/rest taxa prior sd1/", pattern="cors_*", full.names=TRUE)
ldf <- lapply(filenamesCorraw, readRDS)
corSamples<- do.call(rbind.data.frame, ldf); dim(corSamples)
corSamples<- subset(corSamples, task.id %in% Gcors$task.id); dim(corSamples)





Gcors$evidence<- "none"
Gcors$evidence[Gcors$lower80Cor>0 | Gcors$upper80Cor <0] <- "weak"
Gcors$evidence[Gcors$lower90Cor>0 | Gcors$upper90Cor <0] <- "medium"
Gcors$evidence[Gcors$lower95Cor>0 | Gcors$upper95Cor <0] <- "strong"
Gcors$evidencenum<- 1
Gcors$evidencenum[Gcors$lower80Cor>0 | Gcors$upper80Cor <0] <- 0.04
Gcors$evidencenum[Gcors$lower90Cor>0 | Gcors$upper90Cor <0] <- 0.009
Gcors$evidencenum[Gcors$lower95Cor>0 | Gcors$upper95Cor <0] <- 0.0001


# REST: data availability #####
Gcors2<- Gcors
Gcors$Taxon1[Gcors2$Taxon1 == "Simuliidae"] <- Gcors$Taxon2[Gcors$Taxon1 == "Simuliidae"] # replace column of simuliidae with paired taxa
Gcors$Taxon2[Gcors2$Taxon1 == "Simuliidae"] <-  "Simuliidae" # put simuliidae in their places 


GdataMatrF<- dcast(subset(Gcors, numberOfGoodDatasets >4 & Realm == "Freshwater"& numberOfGoodPlots >19),   Taxon1 ~ Taxon2, value.var = "numberOfGoodDatasets")
rownames(GdataMatrF) <- GdataMatrF$Taxon1
GdataMatrF<- as.matrix(GdataMatrF[, -(1)])
GdataMatrT<- dcast(subset(Gcors, numberOfGoodDatasets >4 & Realm == "Terrestrial" & numberOfGoodPlots >19),   Taxon2 ~ Taxon1, value.var = "numberOfGoodDatasets")
rownames(GdataMatrT) <- GdataMatrT$Taxon2
GdataMatrT<- as.matrix(GdataMatrT[, -(1)])
col3 = hcl.colors(7, "YlOrRd", rev = TRUE)
par(mfrow = c(1,2))
corrplot(GdataMatrF,   col = col3,   is.corr = FALSE, tl.col = 'black', col.lim = c(5, 19), 
         method = 'color',  diag = T,  na.label = " ", 
         title = "Freshwater", mar=c(0,0,1,0))
corrplot(GdataMatrT,   col = col3,   is.corr = FALSE, tl.col = 'black', col.lim = c(5, 19), 
         method = 'color', diag = T, 
         title = "Terrestrial", mar=c(0,0,1,0),
         na.label = " ") #



GtestmatrF<- dcast(subset(Gcors, Realm == "Freshwater" & numberOfGoodDatasets >4 & numberOfGoodPlots >19 ) , Taxon1 ~ Taxon2, value.var = "medianCor")
rownames(GtestmatrF) <- GtestmatrF$Taxon1
GtestmatrF<- as.matrix(GtestmatrF[, -(1)])
GtestmatrT<- dcast(subset(Gcors, Realm == "Terrestrial" & numberOfGoodDatasets >4  & numberOfGoodPlots >19 ) , Taxon1 ~ Taxon2, value.var = "medianCor")
rownames(GtestmatrT) <- GtestmatrT$Taxon1
GtestmatrT<- as.matrix(GtestmatrT[, -(1)])
min(GtestmatrF , na.rm = T); max(GtestmatrF , na.rm = T)
min(GtestmatrT , na.rm = T); max(GtestmatrT , na.rm = T)

GpMatrFW<- dcast(subset(Gcors, Realm == "Freshwater" & numberOfGoodDatasets >4 & numberOfGoodPlots >19 ) , Taxon1 ~ Taxon2, value.var = "evidencenum")
rownames(GpMatrFW) <- GpMatrFW$Taxon1
GpMatrFW<- as.matrix(GpMatrFW[, -(1)])
GpMatrT<- dcast(subset(Gcors, Realm == "Terrestrial" & numberOfGoodDatasets >4& numberOfGoodPlots >19 ) , Taxon1 ~ Taxon2, value.var = "evidencenum")
rownames(GpMatrT) <- GpMatrT$Taxon1
GpMatrT<- as.matrix(GpMatrT[, -(1)])



par(mfrow = c(1,2))
#corrplot(as.matrix(testmatr))
#corrplot(testmatr, method = 'ellipse')#, order = 'AOE', type = 'upper')
corrplot(GtestmatrF, p.mat = GpMatrFW,   is.corr =F, tl.col = 'black', na.label = " ",
         col.lim = c(-0.02, 0.2), insig = 'label_sig', sig.level = c(0.001, 0.01, 0.05), 
         pch.cex = 1.5, pch.col = 'grey20', title = "Freshwater", mar=c(0,0,1,0))
corrplot(t(GtestmatrT), p.mat = t(GpMatrT),  is.corr = F, tl.col = 'black', na.label = ".",
         col = colorRampPalette(c(#"#67001F", "#B2182B", 
                                  #"#D6604D", "#F4A582", "#FDDBC7", 
                                  "#FFFFFF", "#D1E5F0", "#92C5DE", 
                                  "#4393C3", "#2166AC", "#053061"))(200), 
         col.lim = c(-0, 0.20), insig = 'label_sig', sig.level = c(0.001, 0.01, 0.05), 
         pch.cex = 1.5, pch.col = 'grey20', title = "Terrestrial", mar=c(0,0,1,0))



badmatrFW<- dcast(subset(Gcors, Realm == "Freshwater" & numberOfGoodDatasets <5 ) , Taxon1 ~ Taxon2, value.var = "medianCor")
rownames(badmatrFW) <- badmatrFW$Taxon1
badmatrFW<- as.matrix(badmatrFW[, -(1)])
badmatrT<- dcast(subset(Gcors, Realm == "Terrestrial" & numberOfGoodDatasets <5 & numberOfGoodPlots >19 ) , Taxon1 ~ Taxon2, value.var = "medianCor")
rownames(badmatrT) <- badmatrT$Taxon1
badmatrT<- as.matrix(badmatrT[, -(1)])
min(badmatrFW , na.rm = T); max(badmatrFW , na.rm = T)
min(badmatrT , na.rm = T); max(badmatrT , na.rm = T)

pbadMatrFW<- dcast(subset(Gcors, Realm == "Freshwater" & numberOfGoodDatasets <5 ) , Taxon1 ~ Taxon2, value.var = "evidencenum")
rownames(pbadMatrFW) <- pbadMatrFW$Taxon1
pbadMatrFW<- as.matrix(pbadMatrFW[, -(1)])
pbadMatrT<- dcast(subset(Gcors, Realm == "Terrestrial" & numberOfGoodDatasets <5 ) , Taxon1 ~ Taxon2, value.var = "evidencenum")
rownames(pbadMatrT) <- pbadMatrT$Taxon1
pbadMatrT<- as.matrix(pbadMatrT[, -(1)])


corrplot(t(badmatrFW), p.mat = t(pbadMatrFW),  is.corr = FALSE,tl.col = 'black', na.label = "na",
         col.lim = c(-0.10, 0.40), insig = 'label_sig', sig.level = c(0.001, 0.01, 0.05), 
         pch.cex = 1.5, pch.col = 'grey20', title = "Freshwater", mar=c(0,0,1,0))
corrplot((badmatrT), p.mat = (pbadMatrT),   is.corr = FALSE, tl.col = 'black', na.label = "na",
         col.lim = c(-0.3, 0.5), insig = 'label_sig', sig.level = c(0.001, 0.01, 0.05), 
         pch.cex = 1.5, pch.col = 'grey20', title = "Terrestrial", mar=c(0,0,1,0))






# Fig 2c plot correlations for by  taxon 

a<- unique((Gcors1[, c("Taxon1", "Realm")]))  
b<- unique((Gcors1[, c("Taxon2", "Realm")]))  
names(b)<- names(a)
taxaRealm<- unique(rbind(a,b))


meanCorPerTaxon <- NULL
allGcors<- NULL
for (i in 1 : nrow(taxaRealm)){ 
  subs<- subset(Gcors, Taxon1 == taxaRealm[i,"Taxon1"] & Realm == taxaRealm[i,"Realm"] | 
                  Taxon2 == taxaRealm[i,"Taxon1"] & Realm == taxaRealm[i,"Realm"])  
  subs$Taxon<- taxaRealm[i,"Taxon1"]
  res<-data.frame(
    Taxon = taxaRealm[i,"Taxon1"],
    Realm = taxaRealm[i,"Realm"],
    nrCorrelations = nrow(subs),
    meanCor = mean(subs$medianCor), 
    sdCor = sd(subs$medianCor)  )
  
  meanCorPerTaxon<- rbind (meanCorPerTaxon, res)
  allGcors<- rbind(allGcors, subs)
}
arrange(meanCorPerTaxon, Realm, Taxon)
ggplot(meanCorPerTaxon, aes(x = Taxon, y = meanCor, color = Realm))+
  geom_point(position=position_dodge(width= 0.6)) +
  geom_errorbar(aes( ymin = meanCor + sdCor, ymax = meanCor - sdCor),  position=position_dodge(width= 0.6), width = 0.6)+
  coord_flip()+
  #  geom_hline()
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(subset(allGcors, numberOfGoodDatasets >4 & numberOfGoodPlots >19), aes(x = Taxon, y = medianCor, color = Realm, fill = Realm))+
  geom_violin(position=position_dodge(width= 0.6), alpha = 0.5) +
  geom_point(  position=position_dodge(width= 0.6)) +
  geom_errorbar(aes(ymin = lower95Cor, ymax = upper90Cor, color = Realm),  position=position_dodge(width= 0.6), width = 0)+
  geom_hline(yintercept = 0)+
  scale_color_manual(values = col.scheme.realm)+
  scale_fill_manual(values = col.scheme.realm)+
  coord_flip()+
  theme_clean +
  theme(legend.key=element_blank(), 
        legend.position="bottom")


# same thing on cor samples: 
corSamples1<- subset(corSamples, task.id %in% subset(Gcors, numberOfGoodDatasets >4)$task.id)

a<- unique((corSamples1[, c("Taxon1", "Realm")]))  
b<- unique((corSamples1[, c("Taxon2", "Realm")]))  
names(b)<- names(a) # to have the same colum names
taxaRealm<- unique(rbind(a,b))

allcorSamples<- NULL
for (i in 1 : nrow(taxaRealm)){ 
  subs<- subset(corSamples, Taxon1 == taxaRealm[i,"Taxon1"] & Realm == taxaRealm[i,"Realm"] | 
                  Taxon2 == taxaRealm[i,"Taxon1"] & Realm == taxaRealm[i,"Realm"])  # grab all lines that have taxon x
  subs$Taxon<- taxaRealm[i,"Taxon1"]
  
  allcorSamples<- rbind(allcorSamples, subs)
}
dim(allcorSamples)
arrange(allcorSamples, Realm, Taxon)

orders<- c("Odonata", "Ephemeroptera",  "Trichoptera",   "Coleoptera" ,    "Hemiptera"   ,   
 "Plecoptera"    , "Araneae"   )    

allcorSamples<- subset(allcorSamples, !Taxon  %in% orders )


# exclude samples outside the 95% CI for each comparison
#what are the CI's?
corCIs<-  allcorSamples %>% 
  group_by(task.id, Taxon1, Taxon2, Realm) %>%
  summarise(
    meanCor = mean(cor),
    medianCor = median(cor),
    lower80cor = quantile(cor,0.10),
    upper80cor = quantile(cor,0.90),
    lower90cor = quantile(cor,0.05),
    upper90cor = quantile(cor,0.95),
    lower95cor = quantile(cor,0.025),
    upper95cor = quantile(cor,0.975))

allcorSamples$good<- FALSE
for (i in 1: nrow(allcorSamples)){
  
  ii<- allcorSamples$task.id[i]
  if(  allcorSamples$cor[i] < corCIs$upper95cor[corCIs$task.id == ii] & 
       allcorSamples$cor[i] > corCIs$lower95cor[corCIs$task.id == ii])
    allcorSamples$good[i]<- TRUE
  
}

ggplot()+
  geom_violin(data =subset(allcorSamples, good == TRUE)  , aes(x = Taxon, y = cor, color = Realm, fill = Realm), 
              position=position_dodge(width= 0.6)) +
  geom_point(data = allGcors, aes(x = Taxon, y = medianCor, group = Realm), color = "black", fill = "black",  
             position=position_dodge(width= 0.6)) +
  stat_summary(data =subset(allcorSamples, good == TRUE) , aes(x = Taxon, y = cor,  group = Realm) , fun=median, geom="point", size=2, fill="red", shape = 23, 
               position=position_dodge(width= 0.6))+
  #  geom_errorbar(aes(ymin = lower95Cor, ymax = upper90Cor, color = Realm),  position=position_dodge(width= 0.6), width = 0)+
  geom_hline(yintercept = 0)+
  scale_color_manual(values = col.scheme.realm)+
  scale_fill_manual(values = col.scheme.realm)+
  coord_flip()+
  theme_clean +
  theme(legend.key=element_blank(), 
        legend.position="bottom")



















# excursion: why is teh variance in the fw so much higher than in terr???



# relation corelation and sd of trends
dim(slopes)

test<- merge(cors, mnSlopes); dim(test)

ggplot(test, aes (x = mnSD, y = meanCor, color = Realm))+
  geom_point()+
  scale_fill_manual(values = col.scheme.realm)
 # nothing interesting here

#read in data
myData<- readRDS("C:\\Dropbox\\Insect Biomass Trends/csvs/taxon correlations/Fulldata allorders.rds")

mySummary <- myData %>%
  group_by(Plot_ID) %>%
  summarise(nuPeriod = length(unique(Period)),
            nuYears = length(unique(Year)))

hist(mySummary$nuYears)
#reorganise data wide so each taxa is on a column
mydata_wide <- myData %>%
  pivot_wider(.,names_from="Order",
              values_from="Number")


minNrYrsV<- c(3, 4,5,6,7, 8 ,9, 10, 15)

par(mfrow = c(5,4), 
    mar = c(3.1, 4.1, 4.1, 2.1))
corSubsets<- NULL
slopeSubsets <- NULL
realms<- c("Terrestrial", "Freshwater")
 
for(i in 1: length(minNrYrsV)){

   
  for(j in (1:2)){
      minNrYrs <- minNrYrsV[i]
      rlm<- realms[j] 
      sel<- subset(mySummary, nuYears >= minNrYrs  ) 
      
      dim(slopes)
      slopesMin<- subset(slopes, Plot_ID %in% sel$Plot_ID & Realm == rlm);   print(nrow(slopesMin))
      print(paste("nrPlots:", length(unique(slopesMin$Polt_ID))))
      
      hist(slopesMin$trend_T1, main = paste("minimum nr Yrs",  minNrYrs, rlm))#, slopesMin$trend_T2)

slopedesc<- data.frame(
  Realm = rlm,
  minNrYrs = minNrYrs, 
  meanT1 = mean(slopesMin$trend_T1),
  sdT1 = sd(slopesMin$trend_T1),
  medianT1 = median(slopesMin$trend_T1),
  meanT2 = mean(slopesMin$trend_T2),
  sdT2 = sd(slopesMin$trend_T2),
  medianT2 = median(slopesMin$trend_T2)
  )
slopeSubsets<- rbind(slopeSubsets, slopedesc)

cor <- sapply(1:1000,function(i){
  cor(slopesMin$trend_T1[slopesMin$sample==i], slopesMin$trend_T2[slopesMin$sample==i])
  })

#hist(cor, main = paste("minimum nr Yrs",  minNrYrs))

corDesc <-data.frame(
    Realm = rlm,
    minNrYrs = minNrYrs, 
    meanCor = mean(cor),
    sdCor = sd(cor),
    varCor = var(cor),
    medianCor = median(cor),
    lower80cor = quantile(cor,0.10),
    upper80cor = quantile(cor,0.90),
    lower90cor = quantile(cor,0.05),
    upper90cor = quantile(cor,0.95),
    lower95cor = quantile(cor,0.025),
    upper95cor = quantile(cor,0.975))

corSubsets<- rbind(corSubsets, corDesc)
}}
corSubsets

plot(slopeSubsets$sdT1 ~ slopeSubsets$minNrYrs)
lines(subset(slopeSubsets, Realm == "Terrestrial")$sdT1~ subset(slopeSubsets, Realm == "Terrestrial")$minNrYrs ,    col = "brown")
lines(subset(slopeSubsets, Realm == "Freshwater")$sdT1~ subset(slopeSubsets, Realm == "Freshwater")$minNrYrs ,    col = "blue")


plot(corSubsets$sdCor ~ corSubsets$minNrYrs , main("sd of cor coef"))
lines(subset(corSubsets, Realm == "Terrestrial")$sdCor~ subset(corSubsets, Realm == "Terrestrial")$minNrYrs ,    col = "brown")
lines(subset(corSubsets, Realm == "Freshwater")$sdCor~ subset(corSubsets, Realm == "Freshwater")$minNrYrs ,    col = "blue")

plot( corSubsets$meanCor ~ corSubsets$minNrYrs, main = "mean cor coef")
lines(subset(corSubsets, Realm == "Terrestrial")$meanCor ~ subset(corSubsets, Realm == "Terrestrial")$minNrYrs ,    col = "brown")
lines(subset(corSubsets, Realm == "Freshwater")$meanCor ~ subset(corSubsets, Realm == "Freshwater")$minNrYrs ,    col = "blue")




















# TRASH #####


badmatrFW<- dcast(subset(cors, Realm == "Freshwater" & numberOfGoodDatasets <5 ) , Taxon1 ~ Taxon2, value.var = "medianCor")
badmatrT<- dcast(subset(cors, Realm == "Terrestrial" & numberOfGoodDatasets <5 & numberOfGoodPlots >19 ) , Taxon1 ~ Taxon2, value.var = "medianCor")
rownames(badmatrFW) <- badmatrFW$Taxon1
badmatrFW<- as.matrix(badmatrFW[, -(1)])
rownames(badmatrT) <- badmatrT$Taxon1
badmatrT<- as.matrix(badmatrT[, -(1)])
min(badmatrFW , na.rm = T); max(badmatrFW , na.rm = T)
min(badmatrT , na.rm = T); max(badmatrT , na.rm = T)

pbadMatrFW<- dcast(subset(cors, Realm == "Freshwater" & numberOfGoodDatasets >4 & numberOfGoodPlots >19 ) , Taxon1 ~ Taxon2, value.var = "evidencenum")
rownames(pbadMatrFW) <- pbadMatrFW$Taxon1
pbadMatrFW<- as.matrix(pbadMatrFW[, -(1)])
pbadMatrT<- dcast(subset(cors, Realm == "Terrestrial" & numberOfGoodDatasets >4 & numberOfGoodPlots >19 ) , Taxon1 ~ Taxon2, value.var = "evidencenum")
rownames(pbadMatrT) <- pbadMatrT$Taxon1
pbadMatrT<- as.matrix(pbadMatrT[, -(1)])


corrplot(t(badmatrT), p.mat = t(pbadMatrT),   is.corr = FALSE, tl.col = 'black', na.label = "na",
         col.lim = c(-0.4, 0.10), insig = 'label_sig', sig.level = c(0.001, 0.01, 0.05), 
         pch.cex = 1.5, pch.col = 'grey20')
corrplot(t(badmatrFW), p.mat = t(pbadMatrFW),  is.corr = FALSE,tl.col = 'black', na.label = "na",
         col.lim = c(-0.10, 0.40), insig = 'label_sig', sig.level = c(0.001, 0.01, 0.05), 
         pch.cex = 1.5, pch.col = 'grey20')


library(ggcorrplot)


a<- unique((cors[, c("Taxon1", "Realm")]))  
b<- unique((cors[, c("Taxon2", "Realm")]))  
names(b)<- names(a)
taxaRealm<- unique(rbind(a,b))


meanCorPerTaxon <- NULL
allcors<- NULL
for (i in 1 : nrow(taxaRealm)){ 
  subs<- subset(cors, Taxon1 == taxaRealm[i,"Taxon1"] & Realm == taxaRealm[i,"Realm"] | 
                  Taxon2 == taxaRealm[i,"Taxon1"] & Realm == taxaRealm[i,"Realm"])  
  subs$Taxon<- taxaRealm[i,"Taxon1"]
  res<-data.frame(
    Taxon = taxaRealm[i,"Taxon1"],
    Realm = taxaRealm[i,"Realm"],
    nrCorrelations = nrow(subs),
    meanCor = mean(subs$medianCor), 
    sdCor = sd(subs$medianCor)  )
  
  meanCorPerTaxon<- rbind (meanCorPerTaxon, res)
  allcors<- rbind(allcors, subs)
}
arrange(meanCorPerTaxon, Realm, Taxon)

ggplot(subset(allcors, numberOfGoodDatasets >4 & numberOfGoodPlots >19), aes(x = Taxon, y = medianCor, color = Realm, fill = Realm))+
  geom_violin(position=position_dodge(width= 0.6), alpha = 0.5) +
  geom_point(  position=position_dodge(width= 0.6)) +
  geom_errorbar(aes(ymin = lower95Cor, ymax = upper90Cor, color = Realm),  position=position_dodge(width= 0.6), width = 0)+
  geom_hline(yintercept = 0)+
  scale_color_manual(values = col.scheme.realm)+
  scale_fill_manual(values = col.scheme.realm)+
  coord_flip()+
  theme_clean +
  theme(legend.key=element_blank(), 
        legend.position="bottom") # ugly 


