#This script is to develop a method to test the correlations in trends of taxa at the same sites

#From Roel:
# Toy data for the taxon comparisons are also in the same folder now. The file is called ‘Toy data taxon comparisons 20210806.rds’ 
# You should use the column ‘Order’ to select which taxa you want to compare. 
# Dataset 1429 looks like a decent one to start with..
# I still need to add zeroes to the years where one taxon was absent, but that shouldn’t really affect your try-outs. 

#libraries
library(tidyverse)  

#get species data
#mydata <- readRDS("C:/Dropbox/Insect Biomass Trends/csvs/Toy data taxon comparisons 20210806.rds")



#fit multivariate model
#https://cran.r-project.org/web/packages/brms/vignettes/brms_multivariate.html
library(brms)

mydata_aggregated <- readRDS(file = "C:/Dropbox/Insect Biomass Trends/taxon correlations/testdata allorders.rds")

#expand orders into different columns
mydata_wide <- mydata_aggregated %>%
  pivot_wider(.,names_from="Order",
              values_from="Number")



mydata_taxasubset <-mydata_wide %>%
  filter(.,!is.na(Hemiptera) & !is.na(Coleoptera) ) %>%
  mutate(log_H = log10(Hemiptera+1), log_C = log10(Coleoptera+1) )  



#or slightly simpler - uncorrelated intercepts and slopes

Sys.time()
fit3 <- brm(
  mvbind(log_H, log_C) ~ Year + (1|p|Datasource_ID) +
                                (1|r|Datasource_ID:Plot_ID) +
                                (1|t|Datasource_ID:Plot_ID: Period) +
                                (0 + Year|q|Datasource_ID) +
                                (0 + Year|s|Datasource_ID:Plot_ID) ,
  data = mydata_taxasubset, 
  prior = c(set_prior("normal(0, 1)", class = "b",  resp = "logC"),
            set_prior("normal(0, 10)", class = "Intercept",  resp = "logC"),
            set_prior("normal(0, 1)", class = "b",  resp = "logH"),
            set_prior("normal(0, 10)", class = "Intercept",  resp = "logH")),
  warmup = 1000, 
  iter   = 5000, 
  chains = 3, 
  #inits  = "random",
  cores  = 3, 
  #set_rescor = TRUE,
  control = list(adapt_delta = 0.99)) # There were 1027 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup 

print(paste("done:", Sys.time() )



summary(fit3)




saveRDS (fit3, file =  "taxa cor test model 1.rds")
