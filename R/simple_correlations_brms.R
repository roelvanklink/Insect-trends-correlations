library(tidyverse)
library(brms)
library(rstan)
library(reshape2)
library(posterior)

#choose HPC folder
myfolder <- "/data/idiv_ess/Roel" #Diana's HPC
myfolder <- "/data/idiv_chase/vanKlink"

#read in data
myData<- readRDS(paste(myfolder,"Fulldata allorders.rds",sep="/"))

mySummary <- myData %>%
                group_by(Plot_ID) %>%
                summarise(nuPeriod = length(unique(Period)),
                          nuYears = length(unique(Year)))


#reorganise data wide so each taxa is on a column
mydata_wide <- myData %>%
  pivot_wider(.,names_from="Order",
              values_from="Number")

all.relations<- NULL
all.all.ests<- NULL

#all comparison to make
good_comparisons_order<-read.csv(paste(myfolder,"comparison_jobs.csv",sep="/"))
ok_comparisons_order<-  read.csv(paste(myfolder,"comparison_jobs_less_good.csv",sep="/"))
comparisons <- rbind(good_comparisons_order, ok_comparisons_order)
comparisons<- arrange(comparisons, desc(Realm), desc(Datasets))

nrow(comparisons)#119
task.id = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", "1"))

#get parameters for this task is
Taxon1<- comparisons[task.id, "Taxon1"]; print(Taxon1)
Taxon2<- comparisons[task.id, "Taxon2"]; print(Taxon2)
realm <- comparisons[task.id, "Realm"]; print(realm)

# which columns do we use?  
indexTaxon1 <-which(names(mydata_wide) == Taxon1)
indexTaxon2 <-which(names(mydata_wide) == Taxon2)

mydata_taxasubset <- subset(mydata_wide, Realm == realm &  !is.na(mydata_wide[indexTaxon1]) & !is.na(mydata_wide[indexTaxon2]))
mydata_taxasubset$log_T1 <- log10(pull(mydata_taxasubset[indexTaxon1])+1)
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

nGoodPlots <- nrow(GoodPlots); print(paste( "number good plots", nGoodPlots))
nGoodDatasets<- length(unique(GoodPlots$Datasource_ID)); print(paste("number good datasets", nGoodDatasets))


if (nGoodDatasets < 5) print(paste("WARNING:", comparisons[task.id, "modelName"],   "HAS < 5 DATASETS"))  #if less than 20 plots or less than 5 atasets, skip comparison 
if (nGoodDatasets < 5 & nGoodPlots < 20) print(paste("WARNING:",  Taxon1, Taxon2, realm,   "SKIPPED"))  #if less than 20 plots or less than 5 datasets, skip comparison 

if (nGoodDatasets < 5 & nGoodPlots < 20){
  write.table(c(realm, Taxon1, Taxon2, "SKIPPED" ),
              file = paste0(task.id,  realm, "_" ,Taxon1, "_" ,Taxon2, "_SKIPPED.txt"),
              sep = "\t",
              row.names = FALSE)}


if (nGoodDatasets < 5 & nGoodPlots < 20) next
# select the good data (each taxon is present in at least half of all years in each plot ):
mydata_taxasubset<-  mydata_taxasubset[mydata_taxasubset$Plot_ID %in% GoodPlots$Plot_ID, ]

#check data per plot
plotSummary <- mydata_taxasubset %>%
  group_by(Plot_ID) %>%
  summarise(nuYears = length(unique(Year)))

#also subset to those with more than 2 years of data
mydata_taxasubset <- filter(mydata_taxasubset, 
                            Plot_ID %in% plotSummary$Plot_ID[plotSummary$nuYears>2])

plts<- sort(unique(mydata_taxasubset$Plot_ID))
all.ests <- NULL

# try to get SLURM_CPUS_PER_TASK from submit script, otherwise fall back to 1
cpus_per_task = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))
rstan_options(auto_write = FALSE)
options(mc.cores = cpus_per_task)

#loop through each plot 
for(i in 1:length(plts)){
  
  #first get trends for each species
  plt<- plts[i]
  print(plt)
  dat<- subset(mydata_taxasubset, Plot_ID == plt)
  dat$cYear <- dat$Year - median(dat$Year)
  dat$iYear <- dat$Year - min(dat$Year) + 1
  dat$Period <- as.numeric(as.factor(dat$Period))
  
  #make sure there is no NAs
  dat <- subset(dat, !is.na(log_T1))
  dat <- subset(dat, !is.na(log_T2))
  
  #brm trends - we might need to consider more complex models here
  prior1 = c(set_prior("normal(0,1)", class = "b"))
  
  #decide on model to run for the plot
  pltSummary <- dat %>%
                  group_by(Plot_ID) %>%
                  summarise(nuPeriod = length(unique(Period)),
                            nuYears = length(unique(Year)))
  
  if(pltSummary$nuPeriod <= 3){
  
    #get data for stan model (made using "make_stancode")
    mod1_data <- make_standata(log_T1 ~ cYear,data = dat, prior = prior1)
    mod2_data <- make_standata(log_T2 ~ cYear,data = dat, prior = prior1) 
    modelfile <- paste(myfolder,"basic_trend.stan",sep="/")#(made using "make_stancode")
  
  }
  
  
  if(pltSummary$nuPeriod > 3){
    #get data for stan model (made using "make_stancode")
    mod1_data <- make_standata(log_T1 ~ cYear + (1|Period), data = dat, prior = prior1)
    mod2_data <- make_standata(log_T2 ~ cYear + (1|Period), data = dat, prior = prior1) 
    modelfile <- paste(myfolder,"basic_period_trend.stan",sep="/")#(made using "make_stancode")
  
  }
  
  #add on men and sd for intercept priors
  mod1_data$meanResponse <- round(median(dat$log_T1), 1)
  mod1_data$sdResponse <- max(round(mad(dat$log_T1), 1), 2.5)
  mod2_data$meanResponse <- round(median(dat$log_T2), 1)
  mod2_data$sdResponse <- max(round(mad(dat$log_T2), 1), 2.5)
  
  #run model
  mod1 <- stan(modelfile, 
               data = mod1_data, 
               chains = 4,
               iter = 5000)
  
  mod2 <- stan(modelfile, 
               data = mod2_data, 
               chains = 4,
               iter = 5000)
  
  #get 1000 samples from the posterior of each trend estimate
  fits1 <- as_draws_df(mod1)
  samples1 <- sample(fits1$`b[1]`,1000)
  fits2 <- as_draws_df(mod2)
  samples2 <- sample(fits2$`b[1]`,1000)
  
  #put all into a data frame
  est <- data.frame(task.id = task.id , 
                    Taxon1 = Taxon1,
                    Taxon2 = Taxon2,
                    sample = 1:1000,
                    trend_T1 = samples1,
                    trend_T2 = samples2,
                    Plot_ID = unique(dat$Plot_ID),
                    Datasource_ID = unique(dat$Datasource_ID),
                    Realm = realm)
  
  all.ests<- rbind(all.ests, est)
  
  
  #write a file during the loop to check where things are crashing
  write.table(data.frame(Plot_ID = plts[i]),
              file = paste0(task.id,  "_output_" ,realm, "_" ,Taxon1, "_" ,Taxon2, ".txt"),
              sep = "\t",
              append = TRUE,
              row.names = FALSE)
  
}

#monte carlo simulation - get correlation coefficient for each sample
cor_samples <- sapply(1:1000,function(i){
  
  cor(all.ests$trend_T1[all.ests$sample==i],all.ests$trend_T2[all.ests$sample==i])
})


#put the 1000 values into a data frame
cors <- data.frame(task.id = task.id,
                         Taxon1 = Taxon1,
                         Taxon2 = Taxon2,
                         Realm = realm,
                         cor = cor_samples)

saveRDS(cors,file=paste0("cors_",  task.id,   realm, "_" , Taxon1, "_" ,Taxon2, ".rds"))

#summarise the correlation distribution for each comparison
corSummary <- data.frame(task.id = task.id,
                         Taxon1 = Taxon1,
                         Taxon2 = Taxon2,
                         Realm = realm,
                         numberOfGoodDatasets = nGoodDatasets,
                         numberOfGoodPlots = nGoodPlots,
                         meanCor = mean(cor_samples),
                         medianCor = median(cor_samples),
                         lower80Cor = quantile(cor_samples,0.10),
                         upper80Cor = quantile(cor_samples,0.90),
                         lower90Cor = quantile(cor_samples,0.05),
                         upper90Cor = quantile(cor_samples,0.95),
                         lower95Cor = quantile(cor_samples,0.025),
                         upper95Cor = quantile(cor_samples,0.975))
print(corSummary)
saveRDS(corSummary,file=paste0("corSummary_",  task.id,   realm, "_" , Taxon1, "_" ,Taxon2, ".rds"))

#save trend estimates
saveRDS(all.ests,file=paste0("slopeEstimates_", task.id,   realm, "_" , Taxon1,"_",Taxon2,".rds"))

