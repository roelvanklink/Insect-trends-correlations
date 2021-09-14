
ggplot(mydata_taxasubset)+
  geom_line(aes(x=Year, y = log10(Hemiptera+1), group=Plot_ID),color="red")+
  geom_line(aes(x=Year, y = log10(Coleoptera+1), group=Plot_ID),color="blue")+
  facet_wrap(~Datasource_ID,scales="free")

#correlated plot-level intercepts
fit1 <- brm(
  mvbind(log_T, log_E) ~ Year + (1|p|Plot_ID),
  data = mydata_taxasubset, 
  warmup = 500, 
  iter   = 2000, 
  chains = 3, 
  inits  = "random",
  cores  = 3) 
summary(fit1)


get_prior(mvbind(log_H, log_C) ~ Year + (1 + Year|p|Plot_ID),
          data = mydata_taxasubset)




#correlated plot-level intercepts and slopes
print(Sys.time())
fit2 <- brm(
  mvbind(log_H, log_C) ~ Year + (1 + Year|p|Plot_ID),
  data = mydata_taxasubset, 
  prior = c(set_prior("normal(0, 1)", class = "b"),
            set_prior("normal(0, 10)", class = "Intercept")),
  warmup = 500, 
  iter   = 5000, 
  chains = 3, 
  #  inits  = "random",
  cores  = 3)
print(Sys.time())
summary(fit2)






# load tryout results:
fit<- read_rds("./taxon correlations/taxa cor test model 1.rds")
summary(fit)
