# improvements to be made: 
# selection of raw data should be done after building the whole databse together. Perhaps better to take the December 2020 version, 
# ADD ZEROES TO ALL YEARS WHERE A TAXON WAS NOT REPORTED
# Decide what to doi with taxa at higher taxonomic level (class), since these will have "" at order level 

# come up with an intuitive system for taxa that re assessed: 
# carabidae, Staphylinidae, butterflies, moths, hoppers, bugs, grasshopppers, mites




# load all datasets ##### 
rm(list=ls()) 
 

library(reshape2)
library(tidyverse)
library(beepr)
setwd("D:/work/2017 iDiv/2018 insect biomass/insect-richness-trends/R/")
source("calculate metrics.R")
source("D:/work/2017 iDiv/2018 insect biomass/insect-richness-trends/R/effort_rarefaction.R")
source("D:/work/2017 iDiv/2018 insect biomass/insect-richness-trends/R/calculate expected beta diversity.R")
source("D:/work/2017 iDiv/2018 insect biomass/insect-richness-trends/R/function_cleaning_taxon_names.R")




  # make alternative color schemes
  col.scheme.cont<-c( "Europe"="green3", "Latin America"= "magenta", "North America"= "orange","Asia" = "purple3", 
                      "Africa" = "blue", "Australia" = "red")
  col.scheme.realm<-c(  "Freshwater"  = "dodgerblue2", "Terrestrial" = "peru")
  columnsProvidedData<- c("Datasource_name" ,   "Plot_ID", "Plot_name", "Sample_ID", "Year", "Period", "Date", "Taxon", "Sex", 
                    "Unit",    "Original_number", "Transformed_number", "Number", "Error", "ExpectedBeta",  "SDexpectedBeta" )


# load sheets of original database
setwd("C:/Users/roelv/Dropbox/Insect Biomass Trends/csvs") # home
setwd("C:\\Dropbox\\Insect Biomass Trends/csvs") # work # work

taxa<-read.csv( file = "C:\\Dropbox\\Insect Biomass Trends/csvs/taxa5.2.csv"); dim(taxa)
plots<-read.csv( file = "C:\\Dropbox\\Insect Biomass Trends/csvs/PlotData 5.0.csv"); dim(plots)
UKfwPlots<- read.csv( file = "C:\\Dropbox\\Insect Biomass Trends/csvs/UKfwSites.csv")
plots<- rbind(plots[, names(UKfwPlots)], UKfwPlots)

samples <-read.csv( file = "Sample_Info 5.2.csv"); dim(samples)
database <-read.csv( file = "Data 5.2.csv"); dim(database)
database<- subset(database, Note != "remove");dim(database)
unique(database$Datasource_name)
studies<-read.csv(file = "studies 5.2.csv", header = T); dim(studies)
#studies1 <-read.table( file = "clipboard", header = T, sep = "\t"); dim(studies1) 
#write.csv(studies1, file = "studies 5.2.csv", row.names = F)

#Add taxonomic level to Taxon table
taxa<- taxa[, 1:14] 
 taxa$Level<- NA
  taxa$Level[taxa$Phylum!= ""]<- "Phylum"
  taxa$Level[taxa$Class!= ""]<- "Class"
  taxa$Level[taxa$Subclass!= ""]<- "Subclass"
  taxa$Level[taxa$Order!= ""]<- "Order"
  taxa$Level[taxa$Suborder!= ""]<- "Suborder"
  taxa$Level[taxa$Family!= ""]<- "Family"
  taxa$Level[taxa$Subfamily!= ""]<- "Subfamily"
  taxa$Level[taxa$Genus!= ""]<- "Genus"
  taxa$Level[taxa$Species!= ""]<- "Species"
  taxa$Level <- factor(taxa$Level, ordered = TRUE, 
                      levels = c("Phylum",  "Class", "Subclass", "Order","Suborder",  "Family",
                                 "Subfamily","Genus" ,"Species" ))
  taxa$Rank<-as.numeric(taxa$Level)        
write.csv(taxa, file = "C:\\Dropbox\\Insect Biomass Trends/csvs/taxa5.2.csv", row.names = F )  
  
  # some changes to groupings
  studies$Continent[studies$Continent == "South America"]  <- "Latin America"
  studies$Continent[studies$Continent == "Central America"]  <- "Latin America"
  studies$Region[studies$Region == "Russia Volga"]  <- "Russia Central & Volga"
  studies$Region[studies$Region == "Russia Central"]  <- "Russia Central & Volga"
  studies$Region[studies$Region == "Russia Ural"]  <- "Russia Ural & Siberia"
  studies$Region[studies$Region== "Russia Siberia"]  <- "Russia Ural & Siberia"
  studies$Region[studies$Region== "Russia Far East"]  <- "Asia East"
  
  # manual groupings of some datasets
  studies$Region[(studies$Region == "Germany" & studies$Realm == "Freshwater" ) ] <- "Europe rest West"
  studies$Region[(studies$Region == "United Kingdom" & studies$Realm == "Freshwater" ) ] <- "Europe rest West"
  studies$Region[(studies$Region == "Russia Northwest" & studies$Realm == "Terrestrial" ) ] <- "Europe rest North"
  
  
  
  # remove repeated column names
  names(studies) # no redundancy
  studies<- studies[, 1:31]
  names(database) # remove redundant columns later
  names(samples) # remove redundant comuns
  samples<- samples[, c("Sample_ID", "Datasource_ID", "Datasource_nameREDUNDANT", "Data_source", "Extraction_method", "Sampling_method", "Stratum", 
                    "Sample_area", "Ref.to.methods", "Number_of_replicates", "Aggregation_of_replicates", "Taxon_in_Data",            
                    "OLDabundance_analysis" , "NEWabundance_analysis", "PurifiedAnalysis", "Biomass_Abundance_analysis", "taxonomic_paper", "non.insects" , 
                    "taxon_comparison", "order_comparison", "family_comparison", "Populations",
                    "non.insect_proportion", "Original_unit", "Calculations", "Unit",  "Richness_precision", "Error_unit", "Flag_taxonomy"  )   ]
  names(plots) # remove redundant columns
  plots<- plots[, c("Plot_ID", "Datasource_ID", "Location", "Plot_name", "Details.plots",  "Experimental_Treatment", "Details_expt_trt",
                   "Process_of_change", "notes_change", "invasives", "Coord_system", "Original_Latitude", "Original_Longitude", "Latitude",
                   "Longitude", "Elevation", "Source_geogr_data")]
  names(taxa) # no redundancy
  taxa<-taxa[, c("ID","Phylum", "Class", "Subclass", "Suborder",  "Order", "Family","Subfamily", "Genus",     "Species",   "Taxon", "Level", "Rank", "Note")]
  
# clean some datasets from the raw database: 
    # remove extra months from harvard forest
  database<- subset(database, Datasource_name != "LTER Harvard forest" | Year != 2006 | Period != 8 ) # remove august in 2006
  database<- subset(database, Datasource_name != "LTER Harvard forest" | Year != 2005 | Period <7  ) # remove  july and august in 2005
  # remove some years in Brazil freshwater 
  database<- subset(database,  Datasource_name != "Brazil freshwater 2" | Year != 1999)  
  database<- subset(database,  Datasource_name != "Brazil freshwater 2" | Year != 1999)
  database<- subset(database,  Datasource_name != "Brazil freshwater 2" | Year != 2001 )
  database<- subset(database,  Datasource_name != "Brazil freshwater 2" | Year != 2002 )
  database<- subset(database,  Datasource_name != "Brazil freshwater 2" | Year != 2005)
  database<- subset(database,  Datasource_name != "Brazil freshwater 2" | Year != 2008 ) 
  database<- subset(database,  Datasource_name != "Brazil freshwater 2" | Plot_name == "Ipatinga"     |  Year != 2006 ) 
  database<- subset(database,  Datasource_name != "Brazil freshwater 2" | Plot_name == "SantaBarbara" |  Year != 2004 ) 
  dim(database)

  
  # load other datasets. later filter them for usefulnes for this analysis

  # We take the new versions, except for rarefied datasets where we cant separate the taxa after rarefaction (AZ 1 & 2, ) 
    Biotime <- read.csv( file = "BioTIMEstd2021.csv"); unique(Biotime$Datasource_name)
  hov.std<- read.csv( file = "Owen hoverflies standardized.csv")
  California.std<- read.csv(file = "California Resh standardized.csv")
  schuch<- read.csv( file = "Schuch data for richness.csv")
    schuch<- subset(schuch, Number >=0) # exclude sight observations
  LTER_NTL<- read.csv(file = "LterNTLcleanSum.csv")
  Lauwersmeer<- read.csv( file = "lauwersmeer final.csv", header = T)
  Breitenbach <- read.csv(file = "Breitenbach2020.csv")
  Finland <- read.csv (file = "Kuusamon_long.csv"); Finland$Unit <- "abundance" #head(Finland)
  CC<- read.csv(file = "CCfullForRichness.csv", header = T) 
      CCsum<- read.csv(file = "cedarcreekBEFsummed.csv", header = T)   # summed over all plots (excluding those where a year is missing)
      
  NZ<- read.csv(file = "NZ river monitoring final.csv", header = T)
  PanamaLeafhoppers<- read.csv(file = "C:\\Dropbox\\Insect Biomass Trends/csvs/Panama leafhoppers full dataset.csv", header = T)   
  Sweden<- read.csv(file ="SEFW final 202106.csv", header = T); dim(Sweden)
  HubbardBrookBeetles<- read.csv( file = "c:\\Dropbox\\Insect Biomass Trends/csvs/HubbardBrookBeetles.csv")
  UKfw <- read.csv( file = "C:\\Dropbox\\Insect Biomass Trends/csvs/UKfwSelectedData.csv"); dim(UKfw)
  NDlakes <- read.csv( file = "C:\\Dropbox\\Insect Biomass Trends/csvs/NDlakes clean.csv")
  HongkongfwMeans<- read.csv( file = "C:\\Dropbox\\Insect Biomass Trends\\csvs/Hongkong fw means.csv")

  GPDD.std <- read.csv( file = "GPDD data_standardized.csv"); unique(GPDD.std$Datasource_name)
  Hungary <- read.csv (file = "Valtonen_long.csv"); #head(Hungary)
  Panamabutt <- read.csv( file = "C:\\Dropbox\\Insect Biomass Trends\\csvs/Panamabutt.csv")
  KoreaMoths <- read.csv( file = "C:/Dropbox/Insect Biomass Trends/csvs/KoreaMoths.csv")	
  walesMoths<- read.csv( file = "C:\\Dropbox\\Insect Biomass Trends/csvs/wales moths.csv")

    # old versions: 
    AZOld<- read.csv(file = "C:/Dropbox/Insect Biomass Trends/csvs/Old data Science paper/LTER Arizona Pitfalls NEW2019.csv", header = T)
  AZOld$Taxon[is.na(AZOld$Taxon)]<-"NONE"
  AZ2<- read.csv(file = "C:/Dropbox/Insect Biomass Trends/csvs/Old data Science paper/sycamore creek formatted.csv", header = T)
  Luquillo<- read.csv(file = "C:/Dropbox/Insect Biomass Trends/csvs/Old data Science paper/LTER Luquillo all canopy arthropods.csv", header = T); Luquillo$Unit <- "abundance"
  levels(Luquillo$Taxon)[levels(Luquillo$Taxon) == "MELA"]<-"MELA1"      # is duplicate name in taxon list
  levels(Luquillo$Taxon)[levels(Luquillo$Taxon) == "CHRYS"]<-"CHRYSOPID" # is duplicate name in taxon list
  Luquillo<-subset(Luquillo, Taxon != "LAM") # remove 'Leaf Area Missing'
  Greenland<- read.csv( file = "C:/Dropbox/Insect Biomass Trends/csvs/Old data Science paper/Greenland2020.csv", header = T); Greenland$Taxon<-gsub(" ", "_", Greenland$Taxon)            # checked 19-12-19



  
#  combine files: 
    allData <- rbind(
  #add richness data
    Biotime[, -(1)],
    Breitenbach[, -(1)],
    Lauwersmeer[, -1],
    PanamaLeafhoppers [, -(1)],
    hov.std[, -(1)],
    LTER_NTL ,
    California.std[, -(1)],
    Finland[, -(1)],
    NZ[, -(1)] ,
    Sweden ,  
    schuch[, -c(1,6 )],
    HubbardBrookBeetles,
    NDlakes[, -(1)] , 
    CCsum[, -(1)],
    UKfw,
    GPDD.std[, -(1:2)],
    Hungary[, -(1:2)],
    Panamabutt[, -c(1,16,17)], 
    KoreaMoths [, -(1)],
   walesMoths[, -c(1, 6, 17,18)],
     AZOld[, -(1)], 
    AZ2[, -(1)],
    Luquillo[, -(1)], 
    Greenland[, -(1)], 
    database[, -c(1,6, 17:20)]  ) ; dim(allData)# 727930


    # remove useless metrics 
unique(allData$Unit)
allData<- subset(allData, Unit == "abundance" |  Unit =="density" ); dim(allData)

    # remove non insects  after merging iwth taxa 
allData<- merge(allData, taxa, by = "Taxon"); dim(allData)
#unique(allData$Taxon)[!unique(allData$Taxon)  %in% test$Taxon   ]
allData<-subset(allData, Class ==  "Insecta" | Class == "Arachnida" | Class == "Entognatha" ); dim(allData)


    # select correct datasets(after merging with SampleData)
allData<- merge(allData, samples, by = "Sample_ID"); dim(allData)
allData<- merge(allData, plots,   by = c("Plot_ID", "Datasource_ID")); dim(allData)
#anti_join(allData2, allData1)[, 1:5]

allData<- merge(allData, studies, by = "Datasource_ID"); dim(allData)

names(allData)

allData<- subset(allData, taxon_comparison == "y"| order_comparison == "y" |  family_comparison == "y")
dim(allData)


  
# make metadata file of number of orders / families AND number of years per plot 
metadata_per_plot<-  allData %>% 
  group_by(  Plot_ID) %>%
  summarise(
    Datasource_ID = unique(Datasource_ID), 
    Datasource_name = unique(Datasource_name.x), 
    Start = min(Year, na.rm = T),
    End = max(Year, na.rm = T),
    Duration = (max(Year, na.rm = T) - min(Year, na.rm = T))+1, 
    NumberOfOrders = length(unique(Order)), 
    NumberOfFamilies = length(unique(Family))
    )

plotfamilyClean<- (subset(metadata_per_plot,   Duration >9 & NumberOfFamilies>1 )) ; dim(plotfamilyClean)
plotorderClean<-  (subset(metadata_per_plot,   Duration >9 & NumberOfOrders>1 )) ; dim(plotorderClean)
    # revove plots with < 10 years 
    # remove PLOTS without multiple taxa / too few individuals of taxa

allDataFamT <-subset(allData, Plot_ID %in% plotfamilyClean$Plot_ID & family_comparison == "y" & Realm == "Terrestrial");dim(allDataFamT)  
allDataFamFW<-subset(allData, Plot_ID %in% plotfamilyClean$Plot_ID & family_comparison == "y" & Realm == "Freshwater");dim(allDataFamFW)  
allDataOrdT <-subset(allData, Plot_ID %in% plotorderClean$Plot_ID  & order_comparison == "y"  & Realm == "Terrestrial");dim(allDataOrdT)  
allDataOrdFW<-subset(allData, Plot_ID %in% plotorderClean$Plot_ID  & order_comparison == "y"  & Realm == "Freshwater");dim(allDataOrdFW)  
    





# now add zeroes #####
allDataOrdTzero<- NULL
for(i in 1:length(unique(allDataOrdT$Plot_ID))){
  
  plt<- sort(unique(allDataOrdT$Plot_ID))[i]
  myData<- allDataOrdT[allDataOrdT$Plot_ID == plt , ]
  
  #expand grid to include 0 counts  # note that the 'date' variable is removed here. 
  # Date plays no role in the analysis, 
  # and in case multiple weeks were sampled within a month, these are thus seen as "replicates" within a month. 
  # month is accounted for as random effect
  constantData <- unique(myData[,c("Datasource_ID","Plot_ID", "Order")])#these are defo unique
  allgrid <- expand.grid(Plot_ID = unique(myData$Plot_ID),
                         Year= unique(myData$Year), 
                         Order = unique(myData$Order))
  
  allgrid <- merge(allgrid,constantData,all.x=T)
  
  #add observed data
  myData1 <- merge(allgrid, myData[, c( "Order", "Taxon", "Year","Plot_ID", "Period", "Date", "Number")],  #"classes",
                   by=c("Order", "Year","Plot_ID" ),all=T)
  # add descriptors
  myData <- merge(myData1, unique(myData[ ,c("Plot_ID",  "Location", "Datasource_name.x", "Realm", "Flag_taxonomy" )]),
                  by="Plot_ID",all=T)
  #print(plt)
  
  myData$Number[is.na(myData$Number)] <- 0 
  if(!all(is.na(myData$Period))){
    myData$Period[is.na(myData$Period)]<-sample(myData$Period[!is.na(myData$Period)],
                                                length(myData$Period[is.na(myData$Period)]),
                                                replace=T) }
  
  
  allDataOrdTzero<-rbind (allDataOrdTzero,myData)
  print(plt)
}

beep(2)


    
metadata_per_order_per_plot<-  allDataOrdTzero %>% 
  mutate(sample = paste(Year, Period, Date)) %>%
  group_by(  Plot_ID, Order) %>%
  summarise(
    Datasource_ID = unique(Datasource_ID), 
    Datasource_name = unique(Datasource_name.x), 
    NumberOfIndPerOrder = sum(Number, na.rm = T ),
    NumberOfOccPerOrder = sum(Number != 0, na.rm = T ),
    NumberOfYears = length(unique(Year)),
    NumberOfSamples = length(unique(sample))
    ) %>% mutate(meanIndPerSample = NumberOfIndPerOrder / NumberOfSamples)
    



exclude<- subset(metadata_per_order_per_plot,  floor(NumberOfIndPerOrder) ==  NumberOfIndPerOrder &   meanIndPerSample<1 & meanIndPerSample != 0)
print(exclude, n = Inf)

# which potential comparisons do we have left? 






metadata_per_family_per_plot<-  allDataFamT %>% 
  mutate(sample = paste(Year, Period, Date)) %>%
  group_by(  Plot_ID, Family) %>%
  summarise(
    Datasource_ID = unique(Datasource_ID), 
    Datasource_name = unique(Datasource_name.x), 
    NumberOfIndPerFamily = sum(Number, na.rm = T ),
    NumberOfOccPerFamily = sum(Number != 0, na.rm = T ),
    NumberOfYears = length(unique(Year)),
    NumberOfSamples = length(unique(sample))
    )

metadata_per_order<-  allDataOrd %>% 
  mutate(sample = paste(Year, Period, Date)) %>%
  group_by( Order) %>%
  summarise(
    NumberOfIndPerOrder = sum(Number, na.rm = T ),
    NumberOfOccPerOrder = sum(Number != 0, na.rm = T ),
    NumberOfYears = length(unique(Year)),
    NumberOfSamples = length(unique(sample))
)

print(metadata_per_order, n = Inf)
# then expand grid for each plot to add zeroes 
  







# then aggregate per taxon (order / class / family / arbitrary)
allDataOrdTzero<- anti_join(allDataOrdTzero, exclude) # Remove rare comparisons  looks reasonable. 

# choose a dataset
mydata<- allDataOrdFW
mydata<- allDataOrdTzero


#select columns we need
mydata_select <- mydata %>%
  select(c(Datasource_ID,Location, Plot_ID, Year, Period, Order, Number)) %>%
  filter(!is.na(Order)) %>%
  filter(Order!="")%>%
  filter(!is.na(Number))

#identify rarely sampled order
rareOrder <- mydata_select %>%
  group_by(Order) %>%
  summarise(nuDatasets = length(unique(Datasource_ID))) %>%
  filter(nuDatasets==1)

#aggregate (across species) to order and remove rarely sampled orders
mydata_aggregated <- mydata_select %>%
  filter(!Order %in% rareOrder$Order) %>%
  group_by(Datasource_ID,Plot_ID,Year,Period, Order) %>%
  summarise(Number = sum(Number)) 
mydata_aggregated$Period[is.na(mydata_aggregated$Period)] <- 1

saveRDS(mydata_aggregated, file = "./taxon correlations/testdata allorders.rds")















#check number of times each order co-occurs within the same dataset
V <- crossprod(table(mydata_aggregated[,c("Datasource_ID","Order")]))
diag(V) <- 0
V_long <- V %>%
  as_tibble() %>%
  add_column(Base = row.names(V)) %>%
  pivot_longer(!Base) %>%
  rename(Order = name) %>%
  mutate(log_value = log(value+1))

#plot co-occurrence (need to shade out upper half since it is repeated)
ggplot(V_long %>% filter(log(value+1)>0))+
  geom_tile(aes(x=Base,y=Order,fill=log_value))+
  scale_fill_viridis_c()+
  theme(axis.text.x = element_text(angle=90))


# now we want to know in how many datasets different orders co-occur 
countDatasets<- dcast(mydata_aggregated, Datasource_ID + Order  ~ "Count", value.var = "Number" , sum)
dim(countDatasets)
countDatasets$Count <- 1
coocD<- crossprod(table(countDatasets[,c("Datasource_ID","Order")]))
diag(coocD) <- 0
coocD<- as.data.frame(coocD)
coocD_long <- reshape2::melt(coocD, #id.vars = rownames( cooc), 
                            value.name = "Datasets",
                            variable.name = "Taxon2")
coocD_long$Taxon1 <- rownames(coocD)



countPlots<- dcast(mydata_aggregated, Plot_ID + Order  ~ "Count", value.var = "Number" , sum)
dim(countPlots)
countPlots$Count <- 1 # convert to only existence of the data
cooc<- crossprod(table(countPlots[,c("Plot_ID","Order")]))
diag(cooc) <- 0 # set diagonal as 0 
cooc<- as.data.frame(cooc)
cooc_long <- reshape2::melt(cooc, #id.vars = rownames( cooc), 
                            value.name = "Plots",
                            variable.name = "Taxon2")
cooc_long$Taxon1 <- rownames(cooc)


cooc_long<- merge(cooc_long, coocD_long)

# checks
sum(cooc_long$Plots< cooc_long$Datasets)
cooc_long[cooc_long$Plots< cooc_long$Datasets, ]


fair_comparisons_orderT<- subset(cooc_long, Datasets > 5 | Plots > 20) # 
fair_comparisons_orderFW<- subset(cooc_long, Datasets > 5 | Plots > 20) # 

# remove duplicates: 
com1 <- paste(fair_comparisons_orderFW$Taxon1, fair_comparisons_orderFW$Taxon2)
com2 <- paste(fair_comparisons_orderFW$Taxon2, fair_comparisons_orderFW$Taxon1)
sum(com1 == com2 )

fair_comparisons_orderT[  ]

#choose 2 groups to compare
ggplot(mydata_wide)+
  geom_line(aes(x=Year, y = log10(Hemiptera+1), group=Plot_ID),color="red")+
  geom_line(aes(x=Year, y = log10(Coleoptera+1), group=Plot_ID),color="blue")+
  facet_wrap(~Datasource_ID,scales="free")

ggplot(mydata_wide)+
  geom_line(aes(x=Year, y = Trichoptera, group=Plot_ID),color="red")+
  geom_line(aes(x=Year, y = Ephemeroptera, group=Plot_ID),color="blue")+
  facet_wrap(~Datasource_name,scales="free")


#get simple correlations at the plot-level
spCors <- mydata_wide %>%
  group_by(Datasource_name,Plot_ID) %>%
  summarise(CorrelationTE = cor(Trichoptera,Ephemeroptera),
            CorrelationDE = cor(Diptera,Ephemeroptera),
            CorrelationDT = cor(Diptera,Trichoptera))

par(mfrow=c(3,1))
hist(spCors$CorrelationTE)
hist(spCors$CorrelationDE)
hist(spCors$CorrelationDT)    
    
    
    
    
    
    
    
  

#what about: Owen bees? 
  # Kellogg lampyridae and Neuroptera
  Kellogg<- read.csv(file = "Kellogg 2020.csv", header = T)
  
  
    
    # can use as is, but plot IDs need to be changed
  ECNbuttStd<- read.csv( file = "ECN butterflies standardized 20210715.csv")
  ECNmothsStd<- read.csv(file = "ECN moths standardized 20210718.csv")
  ECNgbStd<- read.csv( file = "ECN ground beetles standardized 20210718.csv")
# problematic because th eplots are not necesarily at the same location. also, there are sometimes multiple plots per site
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


databaseOrderComp <- readRDS( "database selected for Order comparisons.rds") 
  dim(databaseOrderComp)
names(databaseOrderComp)[names(databaseOrderComp) == "Unit"]<-"Unit_in_data"


dat<- merge(databaseOrderComp, taxa, by = "Taxon")
dat<- merge(dat, samples, by = "Sample_ID")

dim(dat)

setdiff(sort(unique(samplesOrderComp$Datasource_nameREDUNDANT)), sort(unique(dat$Datasource_name)))


dat<- subset(dat, Unit_in_data != "biomass")
length(unique(dat$Datasource_name))




# merge all insect counts at order level
test<- dcast(dat, Datasource_ID+Unit_in_data+Plot_ID+Year+ Plot_name+ Phylum + Class + Subclass +Order ~ "Number", value.var = "Number" , sum)
dim(test)
table(test$Datasource_ID, test$Order)












 # here 


















saveRDS(test, "Toy data taxon comparisons 20210806.rds")

