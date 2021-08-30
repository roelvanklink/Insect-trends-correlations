# improvements to be made: 
# selection of raw data should be done after building the whole databse together. Perhaps better to take the December 2020 version, 
# ADD ZEROES TO ALL YEARS WHERE A TAXON WAS NOT REPORTED
# Decide what to doi with taxa at higher taxonomic level (class), since these will have "" at order level 

# come up with an intuitive system for taxa that re assessed: 
# carabidae, Staphylinidae, butterflies, moths, hoppers, bugs, grasshopppers, mites
unique(merge4$Datasource_ID, merge4$Taxon_in_Data)


databaseOrderComp <- readRDS( "database selected for Order comparisons.rds") 
  dim(databaseOrderComp)
names(databaseOrderComp)[names(databaseOrderComp) == "Unit"]<-"Unit_in_data"


dat<- merge(databaseOrderComp, taxa, by = "Taxon")
dat<- merge(dat, samples, by = "Sample_ID")

dim(dat)

setdiff(sort(unique(samplesOrderComp$Datasource_nameREDUNDANT)), sort(unique(dat$Datasource_name)))


dat<- subset(dat, Unit_in_data != "biomass")
length(unique(dat$Datasource_name))





test<- dcast(dat, Datasource_ID+Unit_in_data+Plot_ID+Year+ Plot_name+ Phylum + Class + Subclass +Order ~ "Number", value.var = "Number" , sum)
dim(test)
table(test$Datasource_ID, test$Order)

saveRDS(test, "Toy data taxon comparisons 20210806.rds")

