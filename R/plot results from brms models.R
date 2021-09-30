

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



filenames <- list.files("D:/work/2017 iDiv/2018 insect biomass/Insect-trends-correlations/model-outputs/narrower prior test/", pattern="*corSum*", full.names=TRUE)
ldf <- lapply(filenames, readRDS)
cors<- do.call(rbind.data.frame, ldf)

filenamesSlopes <- list.files("D:/work/2017 iDiv/2018 insect biomass/Insect-trends-correlations/model-outputs/narrower prior test/", pattern="slope*", full.names=TRUE)
ldf <- lapply(filenamesSlopes, readRDS)
slopes<- do.call(rbind.data.frame, ldf)
dim(slopes)


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
length(unique(subset(slopes, Realm == "Freshwater" )$Datasource_ID))
length(unique(subset(slopes, Realm == "Freshwater" )$Plot_ID))

# get some elemetary info on the data : 
# Where are the data from? 
includedStudies<- studies [studies$Datasource_ID %in% unique(slopes$Datasource_ID) , ]
table(includedStudies$Continent, includedStudies$Realm)
# a map here could actualy be nice 


# Fig 1 Data avialability #####
ggplot(subset(cors, numberOfGoodDatasets >4), aes(Taxon1, Taxon2, fill= numberOfGoodDatasets)) + 
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  facet_wrap(.~ Realm, scales = 'free')+
  ggtitle("Available datasets")+
  theme_clean +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        legend.key=element_blank(), 
        legend.position="right")

# using corrplot
dataMatr<- dcast(cors , Taxon1 ~ Taxon2, value.var = "numberOfGoodDatasets")
rownames(dataMatr) <- dataMatr$Taxon1
dataMatr<- as.matrix(dataMatr[, -(1)])
col3 = hcl.colors(10, "YlOrRd", rev = TRUE)
corrplot(dataMatr,   col = col3,   is.corr = FALSE, tl.col = 'black', col.lim = c(5, 7), method = 'color', type = 'lower', diag = FALSE, na.label = "na")
ggcorrplot(dataMatr,   col = col3,   is.corr = FALSE, tl.col = 'black', col.lim = c(5, 7), method = 'color', type = 'lower', diag = FALSE, na.label = "na")


# is there a fundamental diffrence between fw and terr? 
head(slopes)
mnSlopes<- aggregate(.~task.id +Taxon1 +Taxon2  +Plot_ID + Datasource_ID+Realm, data = slopes,  FUN = "mean"); dim(mnSlopes)
sdSlopes<- aggregate(.~ task.id +Taxon1 +Taxon2 +Plot_ID + Datasource_ID+Realm, data = slopes,  sd); dim(sdSlopes)
ggplot(mnSlopes, aes(x = Realm, y = trend_T1)) + 
  geom_point(position = "jitter")+
  ylab ("mean")
ggplot(sdSlopes, aes(x = Realm, y = trend_T1)) + 
  geom_point(position = "jitter")+
  ylab ("Standard deviation")
boxplot(sdSlopes$trend_T1 ~ sdSlopes$Realm )



#Fig 2a?  overall correlations #####

table(mnSlopes$trend_T1>0, mnSlopes$trend_T2>0, mnSlopes$Realm )
plot(density(mnSlopes$trend_T1))
hist(slopes$trend_T2) # Almost equal increaes and decreases, but parallel increases ad declines are more common 
#plot(slopes$trend_T1~ slopes$trend_T2)

ggplot(slopes, aes(x = trend_T1, y = trend_T2)) + 
  geom_bin2d(bins=50)+ 
  xlab("") + ylab("")+
  geom_abline(intercept = 0 , slope = 1)+
  geom_hline(yintercept = 0)+ geom_vline(xintercept = 0)+
  facet_wrap(.~Realm, scales = "free")

# only mean slopes
ggplot(mnSlopes, aes(x = trend_T1, y = trend_T2)) + 
  geom_bin2d(bins=50)+ 
  xlab("") + ylab("")+
  geom_abline(intercept = 0 , slope = 1)+
  geom_smooth(method = "lm")+
  geom_hline(yintercept = 0)+ geom_vline(xintercept = 0)+
  facet_wrap(.~Realm, scales = "free")

cor.test(subset(mnSlopes, Realm == "Freshwater")$trend_T1, subset(mnSlopes, Realm == "Freshwater")$trend_T2) # r = 0.233, p = <0.0001
cor.test(subset(mnSlopes, Realm == "Terrestrial")$trend_T1, subset(mnSlopes, Realm == "Terrestrial")$trend_T2) # r = 0.09, p = <0.0001

sma(trend_T1 ~trend_T2, data = mnSlopes)
# not sure what this tells us. 
# but this is almost a necessity! because negative covariance can only occur in maximum half of all comparisons

# What about comparisons where at least one of the two trends is significant? 
# expectation 

sigSlopes<-  slopes %>% 
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

mnSlopes<- list(
  allSlopes = sigSlopes, 
sigSlopes80= subset(sigSlopes,  lower80SlpT1>0 | upper80SlpT1 <0 | lower80SlpT2>0 | upper80SlpT2 <0),
sigSlopes90= subset(sigSlopes,  lower90SlpT1>0 | upper90SlpT1 <0 | lower90SlpT2>0 | upper90SlpT2 <0),
sigSlopes95= subset(sigSlopes,  lower95SlpT1>0 | upper95SlpT1 <0 | lower95SlpT2>0 | upper95SlpT2 <0),

sigSlopes80both= subset(sigSlopes,  (lower80SlpT1>0 | upper80SlpT1 <0) & (lower80SlpT2>0 | upper80SlpT2 <0)) ,
sigSlopes90both= subset(sigSlopes,  (lower90SlpT1>0 | upper90SlpT1 <0) & (lower90SlpT2>0 | upper90SlpT2 <0)) ,
sigSlopes95both= subset(sigSlopes,  (lower95SlpT1>0 | upper95SlpT1 <0) & (lower95SlpT2>0 | upper95SlpT2 <0)) 
)

lapply(mnSlopes, function(x) { 
ggplot( x  , 
       aes(x = meanSlpT1, y = meanSlpT2)) + 
  geom_bin2d(bins=50)+ 
  xlab("") + ylab("")+
  geom_abline(intercept = 0 , slope = 1)+
  geom_hline(yintercept = 0)+ geom_vline(xintercept = 0)+
  facet_wrap(.~Realm, scales = "free")+ 
  ggtitle (deparse(substitute(x)))
  } )


ldf<- lapply(mnSlopes, function(x) {cor(x$meanSlpT1, x$meanSlpT2)})
do.call(rbind, ldf)

frequencies<- NULL
for (i in 1: length(mnSlopes)){
df1<- mnSlopes[[i]]
name<- names(mnSlopes)[i]
test<-as.data.frame(table(df1$meanSlpT1 >0, df1$meanSlpT2>0, df1$Realm ))# / (table(sigSlopes$Realm) )
test$prop <- test$Freq/rep(table(df1$Realm), each = 4)
test$dfName<- name
frequencies<- rbind(frequencies, test)
}

# make df of wrongly allocated directions under assumption of parity  
falseFreq<- aggregate (.~ dfName + Var3, data = subset(frequencies, Var1 != Var2  ), sum)
ggplot(falseFreq, aes (x = dfName, y = prop,   color = Var3))+ 
  geom_point()+
  coord_flip()+
  ylab ("proportion opposite direction")



#What about 






#correlation coefficients
all.relations$cor.rEdit<-all.relations$cor.r 
all.relations$cor.rEdit[all.relations$cor.p>0.05] <- NA

#Fig 2B #####
cors_good<- subset(cors, numberOfGoodDatasets >4 & numberOfGoodPlots >19)
ggplot(cors_good, aes(Taxon1, Taxon2, fill= medianCor)) + 
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid  = "grey90", high = "red", na.value = "grey50",) +
  facet_wrap(.~ Realm, scales = 'free')+
  theme_classic()+
  ggtitle("correlation coefficients")+
  theme_clean +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        legend.key=element_blank(), 
        legend.position="right")
plot(density(cors_good$medianCor), main = "Density of correlation coefficients")

cors_medium<- subset(cors, numberOfGoodPlots <20 | numberOfGoodDatasets < 5)
ggplot(cors_medium, aes(Taxon1, Taxon2, fill= medianCor)) + 
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid  = "grey90", high = "red", na.value = "grey50",) +
  facet_wrap(.~ Realm, scales = 'free')+
  theme_classic()+
  ggtitle("correlation coefficients")+
  theme_clean +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        legend.key=element_blank(), 
        legend.position="right")
plot(density(cors_medium$medianCor), main = "Density of correlation coefficients")






# plot correlations for by  taxon 
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
ggplot(meanCorPerTaxon, aes(x = Taxon, y = meanCor, color = Realm))+
  geom_point(position=position_dodge(width= 0.6)) +
  geom_errorbar(aes( ymin = meanCor + sdCor, ymax = meanCor - sdCor),  position=position_dodge(width= 0.6), width = 0.6)+
  coord_flip()+
#  geom_hline()
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

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
        legend.position="bottom")





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
corrplot(testmatr,p.mat = pMatrFW,   is.corr = FALSE, type = 'lower',tl.col = 'black', na.label = "na",
         col.lim = c(-0.1, 0.40), insig = 'label_sig', sig.level = c(0.001, 0.01, 0.05), 
         pch.cex = 1.5, pch.col = 'grey20')
corrplot(testmatrT, p.mat = pMatrT,  is.corr = FALSE, type = 'lower',tl.col = 'black', na.label = "na",
         col.lim = c(-0.10, 0.40), insig = 'label_sig', sig.level = c(0.001, 0.01, 0.05), 
         pch.cex = 1.5, pch.col = 'grey20')



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



