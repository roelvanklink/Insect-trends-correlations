library(tidyverse)
library(brms)
library(reshape2)
library(smatr)

#Diana:
setwd("C:/Users/db40fysa/Dropbox/Insect Biomass Trends/csvs/taxon correlations")

#Roel:
setwd("C:\\Dropbox\\Insect Biomass Trends/csvs/taxon correlations/")

#read in data
myData<- readRDS("Fulldata allorders.rds")

#check models
Hemi_Aran_T<- readRDS("Hemi_Aran_T_model.rds")
summary(Hemi_Aran_T)
plot(Hemi_Aran_T)

Lepi_Hemi_T<- readRDS("Lepi_Hemi_T_model.rds")
summary(Lepi_Hemi_T)
plot(Lepi_Hemi_T)

Orth_Hemi_T<- readRDS("Orth_Hemi_T_model.rds")
summary(Orth_Hemi_T)
plot(Orth_Hemi_T)


#reorganise data wide so each taxa is on a column
mydata_wide <- myData %>%
  pivot_wider(.,names_from="Order",
              values_from="Number")

all.relations<- NULL
all.all.ests<- NULL

#all comparison to make
comparisons<-read.csv( "D:/work/2017 iDiv/2018 insect biomass/Insect-trends-correlations/R/submit scripts and jobs/comparison_jobs.csv")

for (k in c(1:71)) { 

Taxon1<- comparisons[k, "Taxon1"]; print(Taxon1)
Taxon2<- comparisons[k, "Taxon2"]; print(Taxon2)
realm <- comparisons[k, "Realm"]; print(realm)

#example comparison for testing:
realm<-  "Terrestrial"#comparisons[i, "Realm"]; print(realm)
Taxon1<- "Hemiptera"
Taxon2<- "Orthoptera"

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


# at plot level if there is enough data

plts<- unique(mydata_taxasubset$Plot_ID)
all.ests <- NULL

#loop through each plot 
for(i in 1:length(plts)){
  
#first get trends for each species
dat<- subset(mydata_taxasubset, Plot_ID == plts[i])
dat$cYear <- dat$Year - median(dat$Year)
dat$iYear <- dat$Year - min(dat$Year) + 1

#brm trends - we might need to consider more complex models here
prior1 = c(set_prior("normal(0,2)", class = "b"))
mod1<- brm(log_T1 ~ cYear,data = dat, prior = prior1)
mod2<- brm(log_T2 ~ cYear,data = dat, prior = prior1)  

#get 1000 samples from the posterior of each trend estimate
fits1 <- as_draws_df(mod1)
samples1 <- sample(fits1$b_cYear,1000)
fits2 <- as_draws_df(mod2)
samples2 <- sample(fits2$b_cYear,1000)

#put all into a data frame
est <- data.frame(sample = 1:1000,
                  trend_T1 = samples1,
                  trend_T2 = samples2,
                  Plot_ID = unique(dat$Plot_ID),
                  Datasource_ID = unique(dat$Datasource_ID),
                  Realm = realm)

  
all.ests<- rbind(all.ests, est)

}

#monte carlo simulation - get correlation coefficient for each sample
cor_samples <- sapply(1:length(est$sample),function(x){
  
                cor(est$trend_T1[est$sample==i],est$trend_T2[est$sample==i])
  })


#summarise the correlation distributions
corSummary <- data.frame(meanCor = mean(cor_samples),
                         lower95Cor = quantile(cor_samples,0.025),
                         upper95Cor = quantile(cor_samples,0.975))



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

}

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

table(all.all.ests$estimateT1>0, all.all.ests$estimateT2>0)
hist(all.all.ests$estimateT1)
hist(all.all.ests$estimateT2) # Almost equal increaes and decreases, but parallel increases ad declines are more common 
plot(all.all.ests$estimateT1~ all.all.ests$estimateT2)
all.all.ests <- merge(all.all.ests, studies, by = "Datasource_ID")

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
  plot(density(all.relations$MAslopeEdit))
  
  #correlation coefficients
  all.relations$Cor.rEdit<-all.relations$cor.r 
  all.relations$Cor.rEdit[all.relations$cor.p>0.05] <- NA
  
    ggplot(all.relations, aes(Taxon1, Taxon2, fill= Cor.rEdit)) + 
    geom_tile() +
    scale_fill_gradient2(low = "red", mid  = "grey90", high = "blue", na.value = "grey50",) +
  facet_wrap(.~ Realm, scales = 'free')+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      ggtitle("correlation coefficients")
  plot(density(all.relations$cor.r), main = "Density of correlation coefficients")
  
  
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
  
  
  
  