suppressPackageStartupMessages( library (brms))
suppressPackageStartupMessages(library(tidyverse))
library(reshape2)


cpus_per_task = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))
rstan_options(auto_write = TRUE)
options(mc.cores = cpus_per_task)
args <- commandArgs(trailingOnly = T)
output_dir <- args[1]
taskID <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", "1"))


comparisons<-read.csv( "D:/work/2017 iDiv/2018 insect biomass/Insect-trends-correlations/R/submit scripts and jobs/comparison_jobs.csv")
myData<- readRDS( file = "./Fulldata allorders.rds")

print(taskID) 
i<- taskID
print("model name:")
  comparisons$modelName[taskID]

print(Sys.time())


Taxon1<- comparisons[i, "Taxon1"]; print(Taxon1)
Taxon2<- comparisons[i, "Taxon2"]; print(Taxon2)
realm<-  comparisons[i, "Realm"]; print(realm)

mydata_wide <- myData %>%
  pivot_wider(.,names_from="Order",
              values_from="Number")

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


if (nGoodDatasets < 5) print("WARNNING: THIS COMPARISON HAS < 5 DATASETS LEFT")  #if less than 20 plots or less than 5 atasets, skip comparison 

# select the good data (each taxon is present in at least half of all years in each plot ):
mydata_taxasubset<-  mydata_taxasubset[mydata_taxasubset$Plot_ID %in% GoodPlots$Plot_ID, ]



# run model 
fit <- brm(
  mvbind(log_T1, log_T2) ~ Year + 
    (1|p|Datasource_ID) +
    (1|r|Datasource_ID:Plot_ID) +
    (1|t|Datasource_ID:Plot_ID: Period) +
    (0 + Year|q|Datasource_ID) +
    (0 + Year|s|Datasource_ID:Plot_ID) ,
  data = mydata_taxasubset, 
  prior = c(set_prior("normal(0, 1)",  class = "b",  resp = "logT1"),
            set_prior("normal(0, 10)", class = "Intercept",  resp = "logT1"),
            set_prior("normal(0, 1)",  class = "b",  resp = "logT2"),
            set_prior("normal(0, 10)", class = "Intercept",  resp = "logT2")),
  warmup = 1500, 
  iter   = 10000, 
  chains = 4, 
  #inits  = "random",
  cores  = 4, 
  #set_rescor = TRUE,
  control = list(adapt_delta = 0.99)) # There were 1027 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup 

print("done:")
Sys.time() 


model_file <- file.path(output_dir, paste0(as.character(comparisons$modelName[taskID]),"_model.rds"))



saveRDS (fit, file =  model_file)
