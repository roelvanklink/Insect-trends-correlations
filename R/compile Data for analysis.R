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

# remove some years 
  database<- subset(database,  Datasource_name != "Alaska freshwater" | Year != 1994) # some taxa not counted  
  database<- subset(database,  Datasource_name != "Alaska freshwater" | Year != 1985) # some taxa not counted
  database<- subset(database,  Datasource_name != "Alaska freshwater" | Year != 1986) # some taxa not counted
  
  
    
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
  AZnw<- read.csv(file = "C:\\Dropbox\\Insect Biomass Trends\\csvs/LTER Arizona Pitfalls per date2021.csv", header = T)
  AZnw$Taxon[is.na(AZOld$Taxon)]<-"NONE"
  AZ2<- read.csv(file = "C:/Dropbox/Insect Biomass Trends/csvs/Old data Science paper/sycamore creek formatted.csv", header = T)
  Luquillo<- read.csv(file = "C:/Dropbox/Insect Biomass Trends/csvs/Old data Science paper/LTER Luquillo all canopy arthropods.csv", header = T); Luquillo$Unit <- "abundance"
  levels(Luquillo$Taxon)[levels(Luquillo$Taxon) == "MELA"]<-"MELA1"      # is duplicate name in taxon list
  levels(Luquillo$Taxon)[levels(Luquillo$Taxon) == "CHRYS"]<-"CHRYSOPID" # is duplicate name in taxon list
  Luquillo<-subset(Luquillo, Taxon != "LAM") # remove 'Leaf Area Missing'
  Greenland<- read.csv( file = "C:/Dropbox/Insect Biomass Trends/csvs/Greenland2021meansPerDay.csv", header = T); 


  
#  combine files: 
    allData <- rbind(
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
     AZnw[, -(1)], 
    AZ2[, -(1)],
    Luquillo[, -(1)], 
    Greenland[, -(1)], 
    database[, -c(1,6, 17:20)]  ) ; dim(allData)#  727690 
names(allData)[names(allData) == "Unit"]<-"Unit_in_data" # rename to avoid confusion 
allData<- allData[, -c( which(names(allData) == "Plot_name"), which(names(allData) == "Datasource_name"))  ]
    




    # remove useless metrics 
unique(allData$Unit_in_data)
allData<- subset(allData, Unit_in_data == "abundance" |  Unit_in_data =="density" ); dim(allData)

    # remove non insects  after merging iwth taxa 
allData<- merge(allData, taxa, by = "Taxon"); dim(allData)
#unique(allData$Taxon)[!unique(allData$Taxon)  %in% test$Taxon   ]
allData<-subset(allData, Class ==  "Insecta" | Class == "Arachnida" | Class == "Entognatha" ); dim(allData)


    # select correct datasets(after merging with SampleData)
allData<- merge(allData, samples, by = "Sample_ID"); dim(allData)
allData<- merge(allData, plots); dim(allData)
#anti_join(allData2, allData1)[, 1:5]

allData<- merge(allData, studies, by = "Datasource_ID"); dim(allData)

names(allData)

dim(allData)
allData<- subset(allData, taxon_comparison == "y"| order_comparison == "y" |  family_comparison == "y")
dim(allData)


# exclude experimental data (locations where researchers manipulated the environment)
exptPlots<- c(5, # alaska
              921, 922, 924,925, #smes
              643, 644, 646, 647, # hemlock removal
              137, 138, 139  #brazil fragmentation experiment
)
exptDatasources<- c(300,1364, 1357,1410) #Kellogg, Luiquillo CTE, Cedar creek big bio, some german grassland

allData<- allData[!allData$Datasource_ID %in% exptDatasources, ]
allData<- allData[!allData$Plot_ID %in% exptPlots, ]
dim(allData)




  
# make metadata file of number of orders / families AND number of years per plot 
metadata_per_plot<-  allData %>% 
  group_by(  Plot_ID) %>%
  summarise(
    Datasource_ID = unique(Datasource_ID), 
    Datasource_name = unique(Datasource_name), 
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

allDataFam <-subset(allData, Plot_ID %in% plotfamilyClean$Plot_ID & family_comparison == "y" );dim(allDataFam)  #525063
allDataOrd <-subset(allData, Plot_ID %in% plotorderClean$Plot_ID  & order_comparison == "y"  );dim(allDataOrd)  #441320
   

# remove rare taxa 

#select columns we need
allDataOrd_select <- allDataOrd %>%
  select(c(Datasource_ID, Datasource_name, Location, Realm, Plot_ID, Year, Period, Date,  Order, Number,  Flag_taxonomy)) %>%
  filter(!is.na(Order)) %>%
  filter(Order!="")%>%
  filter(!is.na(Number))

#identify rarely sampled orders (les sthan 5 datasets and less than 20 plots go out )
rareOrder <- allDataOrd_select %>%
  group_by(Order) %>%
  summarise(nuDatasets = length(unique(Datasource_ID)), 
            nuPlots = length(unique(Plot_ID))) %>%
  filter(nuDatasets<5 & nuPlots <20)

#aggregate (across species) to order and remove rarely sampled orders #####
allDataOrd_aggregated <- allDataOrd_select %>%
  filter(!Order %in% rareOrder$Order) %>%
  group_by(Datasource_ID, Datasource_name, Location, Plot_ID, Realm, Year, Period, Date, Order, Flag_taxonomy) %>%
  summarise(Number = sum(Number)) 
allDataOrd_aggregated$Period[is.na(allDataOrd_aggregated$Period)] <- 1
allDataOrd_aggregated$Date[is.na(allDataOrd_aggregated$Date)] <- 1
dim(allDataOrd_aggregated) #76328













# check which comparisons make sense in each plot regarding data availability per plot 

# TO DO LATER 
metadata_per_family_per_plot<-  allDataFamzero %>% 
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






    
metadata_per_order_per_plot<-  allDataOrd_aggregated %>% 
  mutate(sample = paste(Year, Period, Date)) %>%
  group_by(  Plot_ID, Order) %>%
  summarise(
    Realm = unique(Realm), 
    Datasource_ID = unique(Datasource_ID), 
    Datasource_name = unique(Datasource_name), 
    NumberOfIndPerOrder = sum(Number, na.rm = T ),
    NumberOfOccPerOrder = sum(Number != 0, na.rm = T ),
    NumberOfYears = length(unique(Year)),
    NumberOfSamples = length(unique(sample))
    ) %>% mutate(meanIndPerSample = NumberOfIndPerOrder / NumberOfSamples, 
                 meanOccPerSample = NumberOfOccPerOrder / NumberOfSamples)
    


# list of observations to be excluded, bacause they were observed in less than half of the samples 
exclude<- subset(metadata_per_order_per_plot,  floor(NumberOfIndPerOrder) ==  NumberOfIndPerOrder &    meanOccPerSample <0.5)
dim(exclude) # 295
print(exclude, n = Inf)

# remove these
# remove data deficient orders 
dim(allDataOrd_aggregated)
index1<- paste(allDataOrd_aggregated$Plot_ID, allDataOrd_aggregated$Order)
index2<- paste(exclude$Plot_ID, exclude$Order)
length(index1[! index1 %in% index2]) # to check what the nrow of the df should be 
allDataOrd_aggregated<- anti_join(allDataOrd_aggregated, exclude[, 1:3]); dim(allDataOrd_aggregated) # Remove rare comparisons.   this is correct 


# check other rare species
check<- subset(metadata_per_order_per_plot,  floor(NumberOfIndPerOrder) ==  NumberOfIndPerOrder &    meanOccPerSample >0.5 & meanOccPerSample <1)
print(check, n = Inf)
# this seems fine to me: these are taxa that were present in most years and can be analysed 




# which comparisons are left? ####
# now we want to know in how many datasets different orders co-occur , and remove the comparisons that are too flimsy (<5 datasets or <20 plots)
countDatasets<- dcast(allDataOrd_aggregated, Realm + Datasource_ID + Order  ~ "Count", value.var = "Number" , sum)
coocDfw<- crossprod(table(subset(countDatasets, Realm == "Freshwater") [,c("Datasource_ID","Order")]))
coocDt<-  crossprod(table(subset(countDatasets, Realm == "Terrestrial")[,c("Datasource_ID","Order")]))
diag(coocDfw) <- 0
diag(coocDt) <- 0
coocDfw_long <- reshape2::melt(as.data.frame(coocDfw), 
                             value.name = "Datasets",
                             variable.name = "Taxon2")
coocDfw_long$Taxon1 <- rownames(coocDfw) # add rownames 
coocDfw_long$Realm <- "Freshwater"
coocDt_long <- reshape2::melt(as.data.frame(coocDt), 
                             value.name = "Datasets",
                             variable.name = "Taxon2")
coocDt_long$Taxon1 <- rownames(coocDt)
coocDt_long$Realm <- "Terrestrial"
coocD_long<- rbind(coocDt_long, coocDfw_long); dim(coocD_long)
 length (unique(coocD_long$Taxon1))

countPlots<- dcast(allDataOrd_aggregated, Realm + Plot_ID + Order  ~ "Count", value.var = "Number" , sum)
dim(countPlots)
countPlots$Count <- 1 # convert to only existence of the data
coocPfw<- crossprod(table(subset(countPlots, Realm == "Freshwater")  [,c("Plot_ID","Order")]))
coocPt <- crossprod(table(subset(countPlots, Realm == "Terrestrial") [,c("Plot_ID","Order")]))
diag(coocPfw) <- 0
diag(coocPt) <- 0
coocPfw_long <- reshape2::melt(as.data.frame(coocPfw), 
                               value.name = "Plots",
                               variable.name = "Taxon2")
coocPfw_long$Taxon1 <- rownames(coocPfw) # add rownames 
coocPfw_long$Realm <- "Freshwater"
coocPt_long <- reshape2::melt(as.data.frame(coocPt), 
                              value.name = "Plots",
                              variable.name = "Taxon2")
coocPt_long$Taxon1 <- rownames(coocPt)
coocPt_long$Realm <- "Terrestrial"
coocP_long<- rbind(coocPt_long, coocPfw_long); dim(coocP_long)


cooc_long<- merge(coocP_long, coocD_long)

# checks
sum(cooc_long$Plots< cooc_long$Datasets)
cooc_long[cooc_long$Plots< cooc_long$Datasets, ] # these will all be excluded because of too little data

# remove duplicates: 
cooc_long<-   cooc_long[!duplicated(data.frame(t(apply(cooc_long[1:3], 1, sort)))),]
cooc_long$Taxon2 <- as.character(cooc_long$Taxon2)
dim( cooc_long)

fair_comparisons_order<- subset(cooc_long, Datasets > 5 | Plots > 20) # 

ggplot(subset(cooc_long, Datasets > 5 | Plots > 20) ) +
  geom_tile(aes(x=Taxon1, y=Taxon2 , fill=Datasets))+
  scale_fill_viridis_c()+
  facet_wrap(.~Realm, scales = "free")+
  theme(axis.text.x = element_text(angle=90))

ggplot(subset(cooc_long, Datasets > 5 ) ) +
  geom_tile(aes(x=Taxon1, y=Taxon2 , fill=Datasets))+
  scale_fill_viridis_c()+
  facet_wrap(.~Realm, scales = "free")+
  theme(axis.text.x = element_text(angle=90)) # this looks more reasonable 


ok_comparisons_order<- subset(cooc_long, Datasets > 5 | Plots > 20); dim(ok_comparisons_order) # 226
fair_comparisons_order<- subset(cooc_long, Datasets < 5 & Plots > 20); dim(fair_comparisons_order) # 155
good_comparisons_order<- subset(cooc_long, Datasets >= 5); dim(good_comparisons_order) #71 

# make job array file 
good_comparisons_order$modelName<- paste0(substr(good_comparisons_order$Taxon1,1,4), "_",
                                          substr(good_comparisons_order$Taxon2, 1,4), "_", 
                                          substr(good_comparisons_order$Realm, 1,1))

write.csv(good_comparisons_order, "D:/work/2017 iDiv/2018 insect biomass/Insect-trends-correlations/R/submit scripts and jobs/comparison_jobs.csv")









# now add zeroes (run time 3 minutes or so)  #####

addZeroes<- function (dat )  {
  datZero<- NULL
  
  for(i in 1:length(unique(dat$Plot_ID))){
    
    plt<- sort(unique(dat$Plot_ID))[i]
    myData<- dat[dat$Plot_ID == plt , ]
    
    myData$Period[is.na(myData$Period)] <- 1
    #expand grid to include 0 counts  # note that the 'date' variable is removed here. 
    # Date plays no role in the analysis, 
    # and in case multiple weeks were sampled within a month, these are thus seen as "replicates" within a month. 
    # month is accounted for as random effect
    constantData <- unique(myData[,c("Datasource_ID", "Year", "Period", "Date",  "Plot_ID")])#these are defo unique
    allgrid <- expand.grid(Plot_ID = unique(myData$Plot_ID),
                           Date  = unique(myData$Date),
                           Order = unique(myData$Order))
    
    allgrid <- merge(allgrid,constantData, all.x=T)
    
    #add observed data
    myData1 <- merge(allgrid, myData[, c( "Order",  "Plot_ID", "Date",   "Number")],  #"classes",
                     by=c( "Order",  "Plot_ID", "Date" ),all=T)
    # add descriptors
    myData <- merge(myData1, unique(myData[ ,c("Plot_ID",  "Location", "Datasource_name", "Realm", "Flag_taxonomy" )]),
                    by="Plot_ID",all=T)
    #print(plt)
    
    myData$Number[is.na(myData$Number)] <- 0 
    
    datZero<-rbind (datZero,myData)
    print(plt)
    
  }
  beep(2)
  return(datZero)  }


allDataOrdzero <- addZeroes(allDataOrd_aggregated)


# check for outliers (indicative of taxon not counted )

outlierCheck<-  allDataOrdzero %>% 
  group_by(  Plot_ID, Order) %>%
  summarise(
    Realm = unique(Realm), 
    Datasource_ID = unique(Datasource_ID), 
    Datasource_name = unique(Datasource_name), 
    mean = mean(log10(Number+1)), 
    sd = sd(log10(Number+1)), 
    sd2 = sd(log10(Number+1) *2), 
    zeroes = sum(Number == 0),
    lower2sd  = (mean(log10(Number+1)) - sd(log10(Number+1))*2) 
      ) 
print(subset(outlierCheck, zeroes !=0 & lower2sd >0), n = Inf )
# excessive outliers (>1) in Alaska  are fixed. Only a few leftover in Sweden and new zealand 









saveRDS(allDataOrdzero, file = "./taxon correlations/Fulldata allorders.rds")













# trash  ####################################


mydata_aggregated<- readRDS( file = "./taxon correlations/testdata allorders.rds")


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
  
  
  
  
  
  
# loop for comparative models to run 
  
  
  Taxon1 = "Ephemeroptera"
  Taxon2 = "Megaloptera"
  
  

comparisons<- fair_comparisons_orderFW

for (i in 1: nrow(comparisons)){ 
  
  Taxon1<- comparisons[i, 2]
  Taxon2<- comparisons[i, 1]
  
  mydata_wide <- mydata_aggregated %>%
    pivot_wider(.,names_from="Order",
                values_from="Number")
  
# do we have enough data in each plot to actually compare these taxa? 
  # threshold: at least present in half of all years 
  
  mydata_taxasubset <-mydata_wide %>%
    filter(.,!is.na(Taxon1) & !is.na(Taxon2) ) %>%
    mutate(log_H = log10(Hemiptera+1), log_C = log10(Coleoptera+1) )  
  
  
  
mydataAggSubset<- subset(mydata_aggregated, Order == Taxon1& !is.na(Number)  | Order == Taxon2 & !is.na(Number)  )
  pltQltyCheck<- dcast(mydataAggSubset, Plot_ID + Year ~ Order, value.var = "Number",  fill = -999 , sum)
  pltQltyCheck<- subset(pltQltyCheck, Ephemeroptera != -999 & Megaloptera != -999)
  

  
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
  
GoodPlots<- subset(widemetadata, widemetadata[3]>0.5 & widemetadata[4]> 0.5)

nGoodPlots <- nrow(GoodPlots)
nGoodDatasets<- length(unique(GoodPlots$Datasource_ID))

  
if (nGoodPlots< 20 | nGoodDatasets < 5) next  #if less than 20 plots or less than 5 atasets, skip comparison 

  # select the good data (each taxon is present in at least half of all years in each plot ):
mydata_taxasubset<-  mydata_taxasubset[mydata_taxasubset$Plot_ID %in% GoodPlots$Plot_ID, ]
  
mydata_taxasubset$log_1 <- log10(mydata_taxasubset[3])
mydata_taxasubset$log_2 <- log10(mydata_taxasubset[4])



# run model 
fit <- brm(
  mvbind(log_1, log_2) ~ Year + 
    (1|p|Datasource_ID) +
    (1|r|Datasource_ID:Plot_ID) +
    (1|t|Datasource_ID:Plot_ID: Period) +
    (0 + Year|q|Datasource_ID) +
    (0 + Year|s|Datasource_ID:Plot_ID) ,
  data = mydata_taxasubset, 
  prior = c(set_prior("normal(0, 1)", class = "b",  resp = "logC"),
            set_prior("normal(0, 10)", class = "Intercept",  resp = "logC"),
            set_prior("normal(0, 1)", class = "b",  resp = "logH"),
            set_prior("normal(0, 10)", class = "Intercept",  resp = "logH")),
  warmup = 1500, 
  iter   = 10000, 
  chains = 4, 
  #inits  = "random",
  cores  = 4, 
  #set_rescor = TRUE,
  control = list(adapt_delta = 0.99)) # There were 1027 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup 

print("done:")
Sys.time() 

  
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


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

