#NCAA week by week

#An analysis file that will pull in football results and run them through LARC Rank

ncaadata<-datascrape("NCAAF")
head(ncaadata)
football<-dataconfigure(ncaadata)

bradter<-LARC.Rank(data)
most<-LARC.Rank(data, func = "Thurston-Mosteller")
