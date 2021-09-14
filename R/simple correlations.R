library(tidyverse)
library(brms)
library(reshape2)
library(smatr)
setwd("C:\\Dropbox\\Insect Biomass Trends/csvs/taxon correlations/")

comparisons<-read.csv( "D:/work/2017 iDiv/2018 insect biomass/Insect-trends-correlations/R/submit scripts and jobs/comparison_jobs.csv")

myData<- readRDS("Fulldata allorders.rds"); dim(myData)


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



all.relations<- NULL
all.all.ests<- NULL

for (k in c(1:nrow(comparisons))) { 

Taxon1<- comparisons[k, "Taxon1"]; print(Taxon1)
Taxon2<- comparisons[k, "Taxon2"]; print(Taxon2)
realm <- comparisons[k, "Realm"]; print(realm)

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

mydataAggSubset<- subset(myData, Order == Taxon1& !is.na(Number)  | Order == Taxon2 & !is.na(Number)  )
mydataAggSubset<- subset(mydataAggSubset, Realm == realm)
#pltQltyCheck<- dcast(mydataAggSubset, Realm + Plot_ID + Year ~ Order, value.var = "Number",  fill = -999 , sum)



metadata_per_order_per_plot<-  mydataAggSubset %>% 
  mutate(sample = paste(Year, Period, Date)) %>%
  group_by(  Plot_ID, Order) %>%
  summarise(
    Datasource_ID = unique(Datasource_ID), 
    NumberOfIndPerOrder = sum(Number, na.rm = T ),
    NumberOfOccPerOrder = sum(Number != 0, na.rm = T ),
    NumberOfYears = length(unique(Year)),
    NumberOfSamples = length(unique(sample))
  ) %>% mutate(meanIndPerSample = NumberOfIndPerOrder / NumberOfSamples, 
               meanOccurence    = NumberOfOccPerOrder / NumberOfSamples )


widemetadata<-   dcast(metadata_per_order_per_plot, Datasource_ID + Plot_ID ~ Order, value.var = "meanOccurence" ) 

GoodPlots<- subset(widemetadata, widemetadata[3]>0.5 & widemetadata[4]> 0.5 & !is.na(widemetadata[3]) & !is.na(widemetadata[4]))

nGoodPlots <- nrow(GoodPlots); print(nGoodPlots)
nGoodDatasets<- length(unique(GoodPlots$Datasource_ID)); print(nGoodDatasets)


if (nGoodDatasets < 5) print(paste("WARNING:", comparisons[k, "modelName"],   "HAS < 5 DATASETS"))  #if less than 20 plots or less than 5 atasets, skip comparison 
if (nGoodDatasets < 5 & nGoodPlots < 20) print(paste("WARNING:", comparisons[k, "modelName"],   "SKIPPED"))  #if less than 20 plots or less than 5 atasets, skip comparison 
if (nGoodDatasets < 5 & nGoodPlots < 20) next
# select the good data (each taxon is present in at least half of all years in each plot ):
mydata_taxasubset<-  mydata_taxasubset[mydata_taxasubset$Plot_ID %in% GoodPlots$Plot_ID, ]



# just plotting trends against eachother 


# at plot level 
plts<- unique(mydata_taxasubset$Plot_ID)



all.ests<- NULL

for(i in 1:length(plts)){
dat<- subset(mydata_taxasubset, Plot_ID == plts[i])
mod1<- summary(lm(log_T1 ~ Year, data = dat  ))  
mod2<- summary(lm(log_T2 ~ Year, data = dat  ))  

est<- data.frame(Taxon1 = Taxon1,
                 Taxon2 = Taxon2,
                 Plot = plts[i],
                 estimateT1 = mod1$coefficients[2], 
                 stdErrorT1 = mod1$coefficients[4],
                 estimateT2 = mod2$coefficients[2], 
                 stdErrorT2 = mod2$coefficients[4],
                 Plot_ID = unique(dat$Plot_ID),
                 Datasource_ID = unique(dat$Datasource_ID),
                 Realm = realm)

  
all.ests<- rbind(all.ests, est)
}
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
  cor.p = round(ct$p.value, 5)
    )
all.relations<- rbind(all.relations, relations)

print(paste("p value:" , round(MA$pval[[1]], 5)))
print(paste("r2:", round(MA$r2[[1]], 5)))

all.ests$corLower <- ct$conf.int[1]
all.ests$corUpper <- ct$conf.int[2]
all.ests$T1upper<- all.ests$estimateT1 + all.ests$stdErrorT1
all.ests$T1lower<- all.ests$estimateT1 - all.ests$stdErrorT1
all.ests$T2upper<- all.ests$estimateT2 + all.ests$stdErrorT2
all.ests$T2lower<- all.ests$estimateT2 - all.ests$stdErrorT2



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
  ggtitle ( comparisons[k, "modelName"])#+
  #facet_wrap(.~Datasource_ID)
)
}
table(all.all.ests$estimateT1>0, all.all.ests$estimateT2>0)
hist(all.all.ests$estimateT1)
hist(all.all.ests$estimateT2) # Almost equal increaes and decreases, but parallel increases ad declines are more common 
plot(all.all.ests$estimateT1~ all.all.ests$estimateT2)

ggplot(all.all.ests, aes(x = estimateT1, y = estimateT2)) + 
    geom_bin2d(bins=25)+ 
    xlab("") + ylab("")+
  facet_wrap(.~Realm)


  all.relations
  hist(all.relations$MAslope)
  hist(all.relations$MAp.value)
  hist(all.relations$MAr2)
  hist(all.relations$cor.r)
  hist(all.relations$cor.p)
  
  
  all.relations$MAslopeEdit<-all.relations$MAslope 
  all.relations$MAslopeEdit[all.relations$MAp.value>0.05] <- NA
  #all.relations$CorslopeEdit[all.relations$cor.p>0.05] <- 0
  hist(all.relations$MAslopeEdit)
  
  library(viridis)
  ggplot(all.relations, aes(Taxon1, Taxon2, fill= MAslopeEdit)) + 
    geom_tile() +
    scale_fill_gradient2(low = "red", mid  = "grey90", high = "blue", na.value = "grey50", trans = "log10") +
    facet_wrap(.~ Realm, scales = 'free')+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    ggtitle("major axis regression")
  plot(density(all.relations$MAslope))
  
  #correlation coefficients
  all.relations$Cor.rEdit<-all.relations$cor.r 
  all.relations$Cor.rEdit[all.relations$cor.p>0.05] <- NA
  
    ggplot(all.relations, aes(Taxon1, Taxon2, fill= cor.r)) + 
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid  = "grey90", high = "red", na.value = "grey50",) +
    facet_wrap(.~ Realm, scales = 'free')+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      ggtitle("correlation coefficients")
  plot(density(all.relations$cor.r), main = "Density of correlation coefficients")
  
  
a<- unique((all.relations[, c("Taxon1", "Realm")]))  
b<- unique((all.relations[, c("Taxon2", "Realm")]))  
names(b)<- names(a)
taxaRealm<- unique(rbind(a,b))

meanCorPerTaxon <- NULL
for (i in 1 : nrow(taxaRealm)){ 
subs<- subset(all.relations, Taxon1 == taxaRealm[i,"Taxon1"] & Realm == taxaRealm[i,"Realm"] | 
         Taxon2 == taxaRealm[i,"Taxon1"] & Realm == taxaRealm[i,"Realm"])  

res<-data.frame(
  Taxon = taxaRealm[i,"Taxon1"],
  Realm = taxaRealm[i,"Realm"],
  nrCorrelations = nrow(subs),
  meanCor = mean(subs$cor.r), 
  sdCor = sd(subs$cor.r)  )

meanCorPerTaxon<- rbind (meanCorPerTaxon, res)
}
arrange(meanCorPerTaxon, Realm, Taxon)
ggplot(meanCorPerTaxon, aes(x = Taxon, y = meanCor, color = Realm))+
  geom_point(position=position_dodge(width= 0.6)) +
  geom_errorbar(aes( ymin = meanCor + sdCor, ymax = meanCor - sdCor),  position=position_dodge(width= 0.6), width = 0.6)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  

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
  
  
  
  