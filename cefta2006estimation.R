#this datafile was the estimation for the CEFTA2006 effect, where the results turned out insignificant
#the table is in the appendix of the thesis.

library(tidyverse)
library(haven)
library(alpaca)

# Larch Database ---------------------------------------------------------------
setwd("D:/Thesis/Code and data/Thesis/")
# I directly delete everything except the country column to save memory:
countries <- readRDS("wiot_1993_gravity.rds")$country %>% unique()
# use coltype to set "integer" where possible, this uses much less memory than 
# "numeric"/"double"
# in your script you get rid of columns 7-11 and 5 after saving the data and
# reloading it. If you do not need these columns anywhere else you can get rid
# of them here as well by replacing "i" with "-"
rtaLarch <- read_csv("Data/rta_20200520.csv",
                     col_types = "cciiiiiiiii") 

# rtaLarch does not have ROW(rest of world), SDS(south sudan) and SUD(sudan) 
# have different iso codes, mutate them in RTA file to make sure they match for
# merging with WIOT, copy for importer!

rtaLarch$exporter[rtaLarch$exporter == "SDN"] <- "SUD"
rtaLarch$exporter[rtaLarch$exporter == "SSD"] <- "SDS"
rtaLarch$importer[rtaLarch$importer == "SDN"] <- "SUD"
rtaLarch$importer[rtaLarch$importer == "SSD"] <- "SDS"
rtaLarch$importer[rtaLarch$importer == "ROM"] <- "ROU"
rtaLarch$exporter[rtaLarch$exporter == "ROM"] <- "ROU"
# do all of the filtering/subsetting in one step
rtaLarch <- rtaLarch %>% 
  filter(exporter %in% countries,   # "," and "&" are the same for "filter" 
         importer %in% countries,
         year > 1989,
         year < 2016) %>% 
  rename(country = exporter)

saveRDS(rtaLarch, "Data/rtaLarch.rds")

# Gravity Data -----------------------------------------------------------------

gravdata <- read_dta("Data/gravdata.dta") %>% 
  # better select with names instad of positions so you no which columns you
  # picked when you come back here after 3 month
  select(country = iso3_o, importer = iso3_d, year, contig, distw,
         comcur) %>% 
  filter(year > 1989) %>% 
  # to save memory:
  mutate(year = as.integer(year),
         contig = as.integer(contig),
         comcur = as.integer(comcur))

gravdata$country[gravdata$country == "SDN"] <- "SUD"
gravdata$country[gravdata$country == "SSD"] <- "SDS"
gravdata$importer[gravdata$importer == "SDN"] <- "SUD"
gravdata$importer[gravdata$importer == "SSD"] <- "SDS"
gravdata$importer[gravdata$importer == "ROM"] <- "ROU"
gravdata$country[gravdata$country == "ROM"] <- "ROU"
gravdata <- gravdata[!(gravdata$country =="CZS" | gravdata$importer =="CZS"),]


# I add the cefta dummy to the gravdata set so that I do not need to calculate
# it in every step of the loop below

CEEC <- c("BGR", "HRV", "CZE", "HUN", "POL", "ROU", "SVK", "SVN")
europe <- c("AUT", "BEL", "CHE", "CYP", "DEU", "DNK", "ESP", "EST", "FIN",
            "FRA", "GBR", "GRC", "IRL", "ITA", "LTU", "LUX", "LVA", "MLT",
            "NLD", "NOR", "PRT", "SWE")

#list of EU members before 1995, list for creating EU dummy

eusmall <- c("BEL","FRA", "GBR", "GRC","IRL","ITA","LUX","DEU", "DNK", "ESP", "NLD", "NOR", "PRT")
 westbalkan <- c("ALB", "MKD", "SRB","BIH","MNE","MDA")
# because analysis is on CEEC countries and their coefficient

##MAKE SURE TO INCLUDE EU DUMMY, for countries that joined the EU after 1990
## countries that joined after 1989: Sweden, Latvia, Lithuania, Finland, Cyprus, Austria?, Estonia, Malta
## likely need to add CEEC to EU once they enter officially



gravdata <- gravdata %>%
  mutate(EU_exp = case_when(country == "POL" & year > 2004 ~ 1,
                               country == "HUN" & year > 2004 ~ 1,
                               country == "CZE" & year > 2004 ~ 1,
                               country == "SVK" & year > 2004 ~ 1,
                               country == "SVN" & year > 2004 ~ 1,
                               country == "ROU" & year > 2007 ~ 1,
                               country == "BGR" & year > 2007 ~ 1,
                               country == "HRV" & year > 2013 ~ 1,
                               country == "AUT" & year > 1995 ~ 1,
                               country == "CyP" & year > 2004 ~ 1,
                               country == "EST" & year > 2004 ~ 1,
                               country == "FIN" & year > 1995 ~ 1,
                               country == "LVA" & year > 2004 ~ 1,
                               country == "LTU" & year > 2004 ~ 1,
                               country == "MLT" & year > 2004 ~ 1,
                               country == "SWE" & year > 1995 ~1,
                               country %in% eusmall ~ 1,
                               TRUE ~ 0), # everything else
         EU_imp = case_when(importer == "POL" & year >2004 ~ 1,
                               importer == "HUN" & year >2004 ~ 1,
                               importer == "CZE" & year > 2004 ~ 1,
                               importer == "SVK" & year > 2004 ~ 1,
                               importer == "SVN" & year > 2004 ~ 1,
                               importer == "ROU" & year > 2007 ~ 1,
                               importer == "BGR" & year > 2007 ~ 1,
                               importer == "HRV" & year > 2013 ~ 1,
                               importer == "AUT" & year > 1995 ~ 1,
                               importer == "CyP" & year > 2004 ~ 1,
                               importer == "EST" & year > 2004 ~ 1,
                               importer == "FIN" & year > 1995 ~ 1,
                               importer == "LVA" & year > 2004 ~ 1,
                               importer == "LTU" & year > 2004 ~ 1,
                               importer == "MLT" & year > 2004 ~ 1,
                               importer == "SWE" & year > 1995 ~ 1,
                               importer %in% eusmall ~ 1,
                             TRUE ~ 0), # everything else
         EU = case_when(EU_exp == 1 & EU_imp == 1 ~ 1,
                           EU_exp == 1 & importer %in% eusmall ~ 1,
                           EU_imp == 1 & country %in% eusmall ~ 1,
                           country %in% eusmall & importer %in% eusmall ~1,
                           TRUE ~ 0))# %>% # everything else
#select(-EU_exp, -EU_imp)

#creation of cefta dummy, dummy = 1 for when both countries are part of cefta or EU, i.e cefta_imp - EU_exp, or cefta_exp - EU_imp


gravdata <- gravdata %>% 
  mutate(cefta_exp = case_when(country == "ALB" & year >= 2007  ~ 1,
                               country == "MKD" & year >= 2006  ~ 1,
                               country == "SRB" & year >= 2007  ~ 1,
                               country == "BIH" & year >= 2007  ~ 1,
                               country == "MNE" & year >= 2007  ~ 1,
                               country == "MDA" & year >= 2007  ~ 1,
                               TRUE ~ 0), # everything else
         cefta_imp = case_when(importer == "ALB" & year >=  2007 ~ 1,
                               importer == "MKD" & year >=  2006 ~ 1,
                               importer == "SRB" & year >=  2007 ~ 1,
                               importer == "BIH" & year >=  2007 ~ 1,
                               importer == "MNE" & year >=  2007 ~ 1,
                               importer == "MDA" & year >=  2007 ~ 1,
                               TRUE ~ 0), # everything else
         cefta = case_when(cefta_exp == 1 & cefta_imp == 1 ~ 1,
                           cefta_exp == 1 & EU_imp == 1 ~ 1,
                           cefta_imp == 1 & EU_exp == 1 ~ 1,
                           TRUE ~ 0)) %>% # everything else
    select(-cefta_exp, -cefta_imp)



# Gravity Estimation -----------------------------------------------------------
# this will take a time and could likely be done better but this is a fast way
# for me to write it

fitppml <- list() #initialize list that will hold results  
for(sector_n in 1:26) {
  wiot <- tibble() # we will initialize an empty dataframe and then bind 
  # all years accordingly
  for(year_n in c(1990:2015)) { # add all years here e.g. 1993:2016
    
    tmp <- readRDS(paste0("wiot_", year_n, "_gravity.rds")) %>%
      filter(sector == sector_n) %>% 
      # ignore negative inventory changes for the estimation
      mutate(flow = ifelse(flow < 0, 0, flow)) %>% 
      # Moving to trade flows we can get rid of the use category (and sector here)
      # as well as ROW
      filter(country != "ROW", importer != "ROW") %>% 
      group_by(country, importer) %>% 
      summarise(flow = sum(flow), .groups = "drop") %>% 
      mutate(year = as.integer(year_n))
    
    wiot <- bind_rows(wiot, tmp)  
  }
  saveRDS(wiot, paste0("Data/wiot_sector_", sector_n, ".rds"))
  # for each year we extracted one sector and bound all years together
  # now we add gravity data to the time series
  wiot <- left_join(wiot, rtaLarch)
  wiot <- left_join(wiot, gravdata)
  
  # add fixed effects
  wiot <- wiot %>% 
    mutate(imp_t_fe = paste(importer, year, sep = "_"),
           exp_t_fe = paste(country, year, sep = "_"),
           # the bilateral FIXED EFFECT is really just dependent on which pair
           # you look at, it automatically captures all the time fixed pair
           # effects, you do NOT assign there value to the bilateral FE)
           bilat_fe = paste(country, importer, sep = "_"))
  
  
  # Now that the data set is ready we perform the estimation
 
  tmp_ppml <- feglm(flow ~ rta +  cefta | 
                      exp_t_fe + imp_t_fe + bilat_fe | 
                      country + importer + year,
                    data = wiot,
                    family = poisson(link = "log"),
                    control = list(trace = TRUE)) # show output each iteration
  
  fitppml[[sector_n]] <- summary(tmp_ppml, 
                                 type = "clustered", 
                                 cluster = ~ country + importer + year)
}


saveRDS(fitppml, "Data/rtaCEFTAonWB.rds")