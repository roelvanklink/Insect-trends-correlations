library(tidyverse)
library(brms)
library(reshape2)
library(lme4)
library(smatr)
setwd("E:\\Dropbox\\Insect Biomass Trends/csvs/taxon correlations/")
setwd("C:\\Dropbox\\Insect Biomass Trends/csvs/taxon correlations/")
studies<-read.csv(file = "E:\\Dropbox\\Insect Biomass Trends/csvs/studies 5.2.csv", header = T); dim(studies)


col.scheme.realm<-c(  "Freshwater"  = "dodgerblue2", "Terrestrial" = "chocolate4")
theme_clean<- theme_grey() + theme(panel.grid.major = element_blank(), 
                                   panel.grid.minor = element_blank(),
                                   panel.background = element_blank(), 
                                   axis.line = element_line(colour = "black") , 
                                   legend.key=element_blank())


myDataG<- readRDS("Fulldata allgroups.rds"); dim(myDataG)
myData<- readRDS("Fulldata allorders.rds"); dim(myData)
length(unique(subset(myDataG, Realm == "Terrestrial" )$Datasource_ID))
length(unique(subset(myDataG, Realm == "Terrestrial" )$Plot_ID))
length(unique(subset(myDataG, Realm == "Freshwater" )$Datasource_ID))
length(unique(subset(myDataG, Realm == "Freshwater" )$Plot_ID))


#compare datasets used in these dataframes 
setdiff(unique(myDataG$Datasource_name), unique(myData$Datasource_name)) # 6 more datasets, mostly on beetles 
setdiff(unique(myData$Datasource_name), unique(myDataG$Datasource_name)) # no difs 




good_comparisons_order<-  read.csv( "g:/work/2017 iDiv/2018 insect biomass/Insect-trends-correlations/R/submit scripts and jobs/comparison_jobs.csv")

ok_comparisons_order<-    read.csv("G:/work/2017 iDiv/2018 insect biomass/Insect-trends-correlations/R/submit scripts and jobs/comparison_jobs_less_good.csv")
comparisons <- rbind(good_comparisons_order, ok_comparisons_order)

good_comparisons_group <- read.csv( "G:/work/2017 iDiv/2018 insect biomass/Insect-trends-correlations/R/submit scripts and jobs/comparison_jobs_groups.csv")
ok_comparisons_group <-   read.csv( "G:/work/2017 iDiv/2018 insect biomass/Insect-trends-correlations/R/submit scripts and jobs/comparison_jobs_groups_less_good.csv")
comparisons_group <- rbind(good_comparisons_group, ok_comparisons_group)

#data check: 
metadata_per_date_per_plot<-  myData %>% 
#  mutate(sample = paste(Year, Period, Date)) %>%
  group_by(  Plot_ID, Year, Period, Order) %>%
  summarise(
    #Realm = unique(Realm), 
    Datasource_ID = unique(Datasource_ID), 
    Datasource_name = unique(Datasource_name), 
    Location = unique(Location),
    NumberOfRecords = length(Number),
    NumberOfIndividuals = sum(Number, na.rm = T ),
  )
dim(metadata_per_date_per_plot)

ss <- subset(metadata_per_date_per_plot, NumberOfRecords>1)
sample_n(as.data.frame(ss), 20) 
unique(ss$Datasource_name) # should only be Arizona, greenland, New Zealand 

#Hemi_Aran_T<- readRDS("Hemi_Aran_T_model.rds")
##summary(Hemi_Aran_T)
#plot(Hemi_Aran_T)

#Lepi_Hemi_T<- readRDS("Lepi_Hemi_T_model.rds")
#summary(Lepi_Hemi_T)
#plot(Lepi_Hemi_T)

#Orth_Hemi_T<- readRDS("Orth_Hemi_T_model.rds")
#summary(Orth_Hemi_T)
#plot(Orth_Hemi_T)



mydata_wide <- myData %>%
  pivot_wider(.,names_from="Order",
              values_from="Number")

mydata_wide <- myDataG %>%
  pivot_wider(.,names_from="commonGroup",
              values_from="Number")


taxonindex<- comparisons
taxonindex <- comparisons_group
all.relations<- NULL
all.all.ests<- NULL


for (k in 1:nrow(taxonindex)) { # c(12,23,37,43)

Taxon1<- taxonindex[k, "Taxon1"]; print(Taxon1)
Taxon2<- taxonindex[k, "Taxon2"]; print(Taxon2)
realm <- taxonindex[k, "Realm"]; print(realm)

#realm<-  "Terrestrial"#comparisons[i, "Realm"]; print(realm)
#Taxon1<- "Hemiptera"
#Taxon2<- "Orthoptera"

# which columns do we use?  
indexTaxon1 <-which(names(mydata_wide) == Taxon1)
indexTaxon2 <-which(names(mydata_wide) == Taxon2)

mydata_taxasubset <- subset(mydata_wide, Realm == realm &  !is.na(mydata_wide[indexTaxon1]) & !is.na(mydata_wide[indexTaxon2]))
mydata_taxasubset$log_T1 <- log10(pull(mydata_taxasubset[indexTaxon1])+1  )
mydata_taxasubset$log_T2 <- log10(pull(mydata_taxasubset[indexTaxon2])+1)   

mydata_taxasubset <- mydata_taxasubset[, c("Plot_ID", "Date",   "Datasource_ID",  "Year", "Period", "Location", 
                                           "Datasource_name", "Realm" , "Flag_taxonomy" , Taxon1, Taxon2, "log_T1", "log_T2")]



# do we have enough data in each plot to actually compare these taxa? 
# threshold: at least present in half of all years 

mydataAggSubset<- subset(myData, commonGroup == Taxon1& !is.na(Number)  | commonGroup == Taxon2 & !is.na(Number)  )
mydataAggSubset<- subset(mydataAggSubset, Realm == realm)
#pltQltyCheck<- dcast(mydataAggSubset, Realm + Plot_ID + Year ~ Order, value.var = "Number",  fill = -999 , sum)



metadata_per_group_per_plot<-  mydataAggSubset %>% 
  mutate(sample = paste(Year, Period, Date)) %>%
  group_by(  Plot_ID, commonGroup) %>%
  summarise(
    Datasource_ID = unique(Datasource_ID), 
    NumberOfIndPerGroup = sum(Number, na.rm = T ),
    NumberOfOccPerGroup = sum(Number != 0, na.rm = T ),
    NumberOfYears = length(unique(Year)),
    NumberOfSamples = length(unique(sample))
  ) %>% mutate(meanIndPerSample = NumberOfIndPerGroup / NumberOfSamples, 
               meanOccurence    = NumberOfOccPerGroup / NumberOfSamples )


widemetadata<-   dcast(metadata_per_group_per_plot, Datasource_ID + Plot_ID ~ commonGroup, value.var = "meanOccurence" ) 

GoodPlots<- subset(widemetadata, widemetadata[3]>0.5 & widemetadata[4]> 0.5 & !is.na(widemetadata[3]) & !is.na(widemetadata[4]))

nGoodPlots <- nrow(GoodPlots); print(nGoodPlots)
nGoodDatasets<- length(unique(GoodPlots$Datasource_ID)); print(nGoodDatasets)


#if (nGoodDatasets < 5) print(paste("WARNING:", taxonindex[k, "modelName"],   "HAS < 5 DATASETS"))  #if less than 20 plots or less than 5 atasets, skip comparison 
#if (nGoodDatasets < 5 & nGoodPlots < 20) print(paste("WARNING:", taxonindex[k, "modelName"],   "SKIPPED"))  #if less than 20 plots or less than 5 atasets, skip comparison 
#if (nGoodDatasets < 5 & nGoodPlots < 20) next
# for less good datasets:
if (nGoodPlots < 15) print(paste("WARNING:", taxonindex[k, "modelName"],   "HAS < 20 PLOTS"))  #if less than 20 plots or less than 5 atasets, skip comparison 
if (nGoodDatasets < 1 | nGoodPlots < 15) print(paste("WARNING:", taxonindex[k, "modelName"],   "SKIPPED"))  #if less than 20 plots or less than 5 atasets, skip comparison 
if (nGoodDatasets < 1 | nGoodPlots < 15) next

# select the good data (each taxon is present in at least half of all years in each plot ):
mydata_taxasubset<-  mydata_taxasubset[mydata_taxasubset$Plot_ID %in% GoodPlots$Plot_ID, ]



# just plotting trends against eachother 


# at plot level 
plts<- unique(mydata_taxasubset$Plot_ID)



all.ests<- NULL

      for(i in 1:length(plts)){
      dat<- subset(mydata_taxasubset, Plot_ID == plts[i])
      
      
      
      
      if(length(unique(dat$Period))==1){
        
        mod1<- summary(lm(log_T1 ~ Year , data = dat  )) 
        mod2<- summary(lm(log_T2 ~ Year, data = dat  ))    
        
       est<- data.frame(Taxon1 = Taxon1,
                       Taxon2 = Taxon2,
                       Plot = plts[i],
                       glmm = FALSE,
                       NumberOfYears = length(unique(dat$Year)),
                       estimateT1 = mod1$coefficients[2], 
                       stdErrorT1 = mod1$coefficients[4],
                       estimateT2 = mod2$coefficients[2], 
                       stdErrorT2 = mod2$coefficients[4],
                       Plot_ID = unique(dat$Plot_ID),
                       Datasource_ID = unique(dat$Datasource_ID),
                       Realm = realm)
       all.ests<- rbind(all.ests, est) 
        
      }  
        
      if(length(unique(dat$Period))>1){
        
      mod1x<- summary(lmer(log_T1 ~ Year + (1|Period), data = dat  ))  
      mod2x<- summary(lmer(log_T2 ~ Year + (1|Period), data = dat  ))  
      
      est<- data.frame(Taxon1 = Taxon1,
                       Taxon2 = Taxon2,
                       Plot = plts[i],
                       glmm = TRUE, 
                       NumberOfYears = length(unique(dat$Year)),
                       estimateT1 = mod1x$coefficients[2], 
                       stdErrorT1 = mod1x$coefficients[4],
                       estimateT2 = mod2x$coefficients[2], 
                       stdErrorT2 = mod2x$coefficients[4],
                       Plot_ID = unique(dat$Plot_ID),
                       Datasource_ID = unique(dat$Datasource_ID),
                       Realm = realm)
      
      
        
      all.ests<- rbind(all.ests, est)
      mod1x<- NULL
      mod2x<- NULL
      }}
# add conf int of estimates
all.ests$T1upper<- all.ests$estimateT1 + all.ests$stdErrorT1
all.ests$T1lower<- all.ests$estimateT1 - all.ests$stdErrorT1
all.ests$T2upper<- all.ests$estimateT2 + all.ests$stdErrorT2
all.ests$T2lower<- all.ests$estimateT2 - all.ests$stdErrorT2

all.all.ests<- rbind(all.all.ests, all.ests)

# put in major axis regression here. 
MA<- ma(estimateT2~ estimateT1, data = all.ests)

ct<- cor.test(all.ests$estimateT1, all.ests$estimateT2)


relations<- data.frame(
  ModelNr = i,
  Taxon1 = Taxon1, 
  Taxon2 = Taxon2, 
  Realm = realm, 
  estTaxon1 = est$estimateT1,
  estTaxon2 = est$estimateT2,  
  nGoodPlots  = nGoodPlots,
  nGoodDatasets = nGoodDatasets, 
  MAelevation = MA$coef[[1]][1,1], 
  MAslope = MA$coef[[1]][2,1],
  MAlower = MA$coef[[1]][2,2],
  MAupper = MA$coef[[1]][2,3],
  MAp.value = round(MA$pval[[1]], 5),  
  MAr2 = round(MA$r2[[1]], 5),
  cor.r = round(ct$estimate, 4),
  cor.p = round(ct$p.value, 5),
  corLower = ct$conf.int[1],
  corUpper= ct$conf.int[2]
  )
all.relations<- rbind(all.relations, relations)

print(paste("p value:" , round(ct$p.value, 5)))
print(paste("r:",round(ct$estimate, 4)))




print(
ggplot(all.ests, aes(x = estimateT1, y = estimateT2, color = as.factor(Datasource_ID)))+ 
  geom_point()+ 
  geom_hline(yintercept = 0)+ 
  geom_vline(xintercept = 0)+ 
  geom_errorbar(aes(ymin = T2lower, ymax = T2upper))+
  geom_errorbarh(aes(xmin = T1lower, xmax = T1upper))+ 
  geom_smooth(method=lm, na.rm = TRUE, fullrange= TRUE,
              aes(group=1),colour="black")+
  ylab(paste("Trend", Taxon2))+ xlab(paste("Trend" ,Taxon1))+
  geom_abline(intercept = MA$coef[[1]][1,1], slope = MA$coef[[1]][2,1], col = "red", size = 2)+
  ggtitle ( taxonindex[k, "modelName"])#+
  #facet_wrap(.~Datasource_ID)
)



}


table( all.all.ests$Realm)
length(unique(subset(all.all.ests, Realm == "Terrestrial" )$Datasource_ID))
length(unique(subset(all.all.ests, Realm == "Terrestrial" )$Plot_ID))
length(unique(subset(all.all.ests, Realm == "Freshwater" )$Datasource_ID))
length(unique(subset(all.all.ests, Realm == "Freshwater" )$Plot_ID))

# Where are the data from? 
includedStudies<- studies [studies$Datasource_ID %in% unique(all.all.ests$Datasource_ID) , ]
table(includedStudies$Continent, includedStudies$Realm)
all.all.ests<- (merge(all.all.ests, studies))


all.relationsSel<- subset(all.relations, !cor.r %in% all.relationsOrders$cor.r )




# Fig 1 Data avialability #####
ggplot(all.relations, aes(Taxon1, Taxon2, fill= nGoodDatasets)) + 
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  facet_wrap(.~ Realm, scales = 'free')+
  ggtitle("Available datasets")+
  theme_clean +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        legend.key=element_blank(), 
        legend.position="right")



table(all.all.ests$estimateT1>0, all.all.ests$estimateT2>0, all.all.ests$Realm )
hist(all.all.ests$estimateT1)
hist(all.all.ests$estimateT2) # Almost equal increaes and decreases, but parallel increases ad declines are more common 
plot(all.all.ests$estimateT1~ all.all.ests$estimateT2)

ggplot(all.all.ests, aes(x = estimateT1, y = estimateT2)) + 
    geom_bin2d(bins=25)+ 
    xlab("") + ylab("")+
  geom_abline(intercept = 0 , slope = 1)+
  geom_hline(yintercept = 0)+ geom_vline(xintercept = 0)+
  facet_wrap(.~Realm)


cor.test(subset(all.all.ests, Realm == "Freshwater")$estimateT1, subset(all.all.ests, Realm == "Freshwater")$estimateT2) # r = 0.233, p = <0.0001
cor.test(subset(all.all.ests, Realm == "Terrestrial")$estimateT1, subset(all.all.ests, Realm == "Terrestrial")$estimateT2) # r = 0.233, p = <0.0001


  all.relations
  hist(all.relations$MAslope)
  hist(all.relations$MAp.value)
  hist(all.relations$MAr2)
  hist(all.relations$cor.r)
  hist(all.relations$cor.p)
  
  

  #correlation coefficients
  all.relations$cor.rEdit<-all.relations$cor.r 
  all.relations$cor.rEdit[all.relations$cor.p>0.05] <- NA
  
  all.relations_good<- subset(all.relations, nGoodDatasets >4 & nGoodPlots >19)
    ggplot(all.relations_good, aes(Taxon1, Taxon2, fill= cor.r)) + 
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid  = "grey90", high = "red", na.value = "grey50",) +
    facet_wrap(.~ Realm, scales = 'free')+
    theme_classic()+
      ggtitle("correlation coefficients")+
      theme_clean +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
            legend.key=element_blank(), 
            legend.position="right")
  plot(density(all.relations_good$cor.r), main = "Density of correlation coefficients")
  
  all.relations_medium<- subset(all.relations, nGoodPlots <20 | nGoodDatasets < 5)
  ggplot(all.relations_medium, aes(Taxon1, Taxon2, fill= cor.r)) + 
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid  = "grey90", high = "red", na.value = "grey50",) +
    facet_wrap(.~ Realm, scales = 'free')+
    theme_classic()+
    ggtitle("correlation coefficients")+
    theme_clean +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
          legend.key=element_blank(), 
          legend.position="right")
  plot(density(all.relations_medium$cor.r), main = "Density of correlation coefficients")
  
ggplot(all.relations, aes(Taxon1, Taxon2, fill= cor.r)) + 
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid  = "grey90", high = "red", na.value = "grey50",) +
    facet_wrap(.~ Realm, scales = 'free')+
    theme_classic()+
    ggtitle("correlation coefficients")+
    theme_clean +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
          legend.key=element_blank(), 
          legend.position="right")
  plot(density(all.relations_medium$cor.r), main = "Density of correlation coefficients")
  
  
  
  
  
  
a<- unique((all.relations[, c("Taxon1", "Realm")]))  
b<- unique((all.relations[, c("Taxon2", "Realm")]))  
names(b)<- names(a)
taxaRealm<- unique(rbind(a,b))

meanCorPerTaxon <- NULL
allcors<- NULL
for (i in 1 : nrow(taxaRealm)){ 
subs<- subset(all.relations, Taxon1 == taxaRealm[i,"Taxon1"] & Realm == taxaRealm[i,"Realm"] | 
         Taxon2 == taxaRealm[i,"Taxon1"] & Realm == taxaRealm[i,"Realm"])  
subs$Taxon<- taxaRealm[i,"Taxon1"]
res<-data.frame(
  Taxon = taxaRealm[i,"Taxon1"],
  Realm = taxaRealm[i,"Realm"],
  nrCorrelations = nrow(subs),
  meanCor = mean(subs$cor.r), 
  sdCor = sd(subs$cor.r)  )

meanCorPerTaxon<- rbind (meanCorPerTaxon, res)
allcors<- rbind(allcors, subs)
}
arrange(meanCorPerTaxon, Realm, Taxon)
ggplot(meanCorPerTaxon, aes(x = Taxon, y = meanCor, color = Realm))+
  geom_point(position=position_dodge(width= 0.6)) +
  geom_errorbar(aes( ymin = meanCor + sdCor, ymax = meanCor - sdCor),  position=position_dodge(width= 0.6), width = 0.6)+
  coord_flip()+
  geom_hline()
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
ggplot(allcors, aes(x = Taxon, y = cor.r, color = Realm, fill = Realm))+
  geom_violin(position=position_dodge(width= 0.6), alpha = 0.5) +
  geom_point(  position=position_dodge(width= 0.6)) +
  geom_errorbar(aes(ymin = corLower....ct.conf.int.1., ymax = corUpper....ct.conf.int.2., color = Realm),  position=position_dodge(width= 0.6), width = 0)+
  geom_hline(yintercept = 0)+
  scale_color_manual(values = col.scheme.realm)+
  scale_fill_manual(values = col.scheme.realm)+
  coord_flip()+
  theme_clean +
  theme(legend.key=element_blank(), 
        legend.position="bottom")



# try other visualization 

testmatr<- dcast(subset(all.relations, Realm == "Freshwater") , Taxon1 ~ Taxon2, value.var = "cor.r")
rownames(testmatr) <- testmatr$Taxon1
testmatr<- testmatr[, -(1)]
as.matrix(testmatr) 
library(corrplot)
corrplot(as.matrix(testmatr))





  plot(all.relations$MAslopeEdit, all.relations$Cor.rEdit)
  
  fit3 <- brm(
    mvbind(log_T1, log_T2) ~ Year + 
      #(1|p|Datasource_ID) +
      #(1|r|Datasource_ID:Plot_ID) +
      (1|t|Plot_ID) +
      #(0 + Year|q|Datasource_ID) +
      (0 + Year|s|Plot_ID) ,
    data = mydata_taxasubset, 
    prior = c(set_prior("normal(0, 1)",  class = "b",  resp = "logT1"),
              set_prior("normal(0, 10)", class = "Intercept",  resp = "logT1"),
              set_prior("normal(0, 1)",  class = "b",  resp = "logT2"),
              set_prior("normal(0, 10)", class = "Intercept",  resp = "logT2")),
    warmup = 500, 
    iter   = 2000, 
    chains = 2, 
    #inits  = "random",
    cores  = 2, 
    #set_rescor = TRUE,
    control = list(adapt_delta = 0.8)) # There were 1027 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup 
  
  plot(fit3)
  
  
  
  