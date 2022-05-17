# this file creates the figures for thesis text, it also creates the table that shows the percentage changes for EU and WB

library(tidyverse)
library(matrixStats)
library(writexl)
library(reshape2)

mydirectory <- "D:/Thesis/Code and data/Thesis/Data/"
results <- readRDS("finalresultsEU.rds")
results_bilateral <- readRDS( "finalbilateralresultsEU.rds")
countries <- readRDS("countries.RDS")

europe <- c("AUT", "BEL", "BGR", "CHE", "CYP", "CZE", "DEU", "DNK", "ESP",
            "EST", "FIN", "FRA", "GBR", "GRC", "HRV", "HUN", "IRL", "ITA",
            "LTU", "LUX", "LVA", "MLT", "NLD", "NOR", "POL", "PRT", "ROU",
            "SVK", "SVN", "SWE")
westbalkan <-c("ALB", "BIH", "MKD", "SRB", "MNE", "MDA")
combo <- c(europe, westbalkan)

# update country id based on country id position in my data

europe_id <- match(europe, countries)#

westbalkan_id <- match(westbalkan, countries) 

combo_id <- c(europe_id,westbalkan_id)



results <- results %>%
  filter(country %in% westbalkan) %>% #cuts down countries to EU and WB countries
  select(country, lambda_hat, w_hat, P_csmpt_hat,U_hat) %>%
  distinct(country, .keep_all = TRUE) %>%
  group_by(country)



# getting a usable figure
results <- results %>%
  filter(country %in% westbalkan) %>% #cuts down countries to EU and WB countries
  select(country, lambda_hat, w_hat, P_csmpt_hat,U_hat) %>%
  distinct(country, .keep_all = TRUE) %>%
  mutate(lambda_hat_pct = 100*(lambda_hat - 1))%>%
  mutate(w_hat_pct = 100*(w_hat - 1)) %>% 
  mutate(P_csmpt_hat_pct = 100*(P_csmpt_hat -1)) %>%
  mutate(U_hat_pct = 100*(U_hat -1)) %>%
  group_by(country)

#save in excel format to get a table for thesis text
write_xlsx(results,"Data/CEFTAsimvariableschangesfinal.xlsx")

results <- results %>%
  select(country, lambda_hat_pct, w_hat_pct, P_csmpt_hat_pct,U_hat_pct)
#long format
rslt <- melt(results) 
colnames(rslt)[3] <- "pct_change"

ggplot(rslt, aes(fill=variable, y=pct_change, x=country)) + 
  geom_bar(position="dodge", stat="identity") + coord_flip()





