
# This script calculates a base scenario without deficits
#based on EconGeo Brexit Simulation
# Libraries ====================================================================

library(tidyverse)
library(matrixStats) # for colProds, column products of matrices


wiot <- readRDS("wiot_simulation.rds") 

# Parameters ===================================================================

# First we generate values derived from the WIOD

# country-sector revenues ------------------------------------------------------
# in each region and sector, total output - total intermediate use equals value
# added, and with labor as the only factor value added is the compensation of
# labor (i.e., the wagesum)
##gives total revenue for individual sectors in countries

prod_data <- wiot %>%
  group_by(country, sector) %>%
  summarise(tot_revenue = sum(flow)) %>%
  ungroup

# cost shares gamma_nj & gamma_njg ---------------------------------------------
prod_data <- wiot %>% 
  group_by(importer, use) %>% 
  # get total intermediate use in each region and sector
  summarise(tot_intermediate = sum(flow)) %>%
  # we only care about production not final demand
  filter(use != 27) %>% 
  rename(country = importer, sector = use) %>% 
  right_join(prod_data, by = c("country", "sector"))

prod_data <- prod_data %>% 
  mutate(value_added = tot_revenue - tot_intermediate) %>% 
  # gamma_rj is the share of value added (i.e. the wage share) in total costs
  # which are equal to total revenue due to zero profits
  # (take care for cases where tot_revenue is zero to avoid division by 0)
  mutate(gamma_nj = ifelse(tot_revenue == 0, 0, value_added / tot_revenue))


# next we get sectoral use shares (gamma_rjg)
prod_data <- wiot %>% 
  # we care about how much a sector in a region uses from other sectors, 
  # independent of where the intermediates were produced
  group_by(importer, use, sector) %>% 
  summarise(intermediate = sum(flow)) %>% 
  ungroup %>% 
  # rename so we can join this with the production data
  rename(country = importer, sector = use, intermediate_sec = sector) %>% 
  right_join(prod_data, by = c("country", "sector")) %>%
  # tot_revenue = tot_cost, therefore the cost share is
  mutate(gamma_njg = ifelse(tot_revenue == 0, 0, intermediate / tot_revenue))



# consumption shares alpha_nj --------------------------------------------------
cnsmptn_data <- wiot %>% 
  # only consumption data
  filter(use == 27) %>%
  # we want each sectors share (independent of the country) in consumption in
  # each country
  group_by(importer, sector) %>% 
  summarise(cnsmptn = sum(flow)) %>%
  group_by(importer) %>%
  mutate(tot_cnsmptn = sum(cnsmptn)) %>%
  ungroup %>% 
  mutate(alpha_nj = cnsmptn / tot_cnsmptn)


trade <- wiot %>% 
  # get trade flows between countries in each sector, independent of their use
  group_by(country, sector, importer) %>% 
  summarise(flow = sum(flow)) %>% 
  # then transform into shares of total imports in the sector
  group_by(sector, importer) %>% 
  mutate(tot_sec_imports = sum(flow)) %>% 
  ungroup %>% 
  # make sure to account for sectors where nothing is important, i.e. avoid 
  # division by 0
  mutate(pi_nij = ifelse(tot_sec_imports == 0, 0, flow/tot_sec_imports))

trade <- trade %>% 
  group_by(country) %>% 
  mutate(exports = sum(flow)) %>%
  ungroup

trade <- trade %>% 
  group_by(importer) %>% 
  summarise(imports = sum(flow)) %>% 
  ungroup %>% 
  rename(country = importer) %>% 
  right_join(trade) %>% 
  mutate(deficit = imports - exports)

#============================exog variables


# second we set parameter values from the literature


epsilon = 3.3 # cf. Redding 2016, Monte, Redding, Rossi-Hansberg 2018

## sigma_j's are taken from Aichele and Heiland 2016, industries differ between
## tehir data and EORA26, the following are simple averages to match EORA industries
## Service sectors, 13-26, value 5.959 from Egger will be used for services
theta_j <- c(1.4457, #agriculture
             1.4457, #fishing
             12.2792, #mining and quarrying
             2.0467, #food and beverages
             5.40925, #textiles and waring apparel
             8.03565, #wood and paper
             6.4202, # petroleum(chemicals & nonmetal minerals)
             4.4546, #electrical & machinery
             1.9491, #transport equipment
             12.7967, #metal products
             2.7715, #other manufacturing
             2.7715, #recycling
             rep(5.959, 14)) 

alpha_H <- 0.4 # expenditure share on housing (cf. Krebs and Pflüger 2019)

##===================================


## right ILO dataset: Labour force by sex and age - ILO modelled estimates

##make sure to just remove countries out of ILO list and have them drop into WORLD

labor_data <- readRDS("Data/ilostat-2020-10-21.rds")
labor_data <- labor_data[-c(73),]
labor_data <- labor_data%>% 
  select(country = ref_area.label, labor_force =  obs_value) %>% 
  
  # for most countries we can just take the first three letters and capitalize 
  # them, some exceptions corrected by hand
  mutate(country = case_when(country == "Austria" ~ "AUT",
                             country == "Algeria" ~ "DZA",
                             country == "Angola" ~ "AGO",
                             country == "Switzerland" ~ "CHE",
                             country == "China" ~ "CHN",
                             country == "Bulgaria" ~ "BGR",
                             country == "Burundi" ~ "BDI",
                             country == "Burkina Faso" ~ "BFA",
                             country == "Bangladesh" ~"BGD",
                             country == "Bahrain" ~ "BHR",
                             country == "Bahamas" ~ "BHS",
                             country == "Bosnia and Herzegovina" ~ "BIH",
                             country == "Barbados" ~ "BRB",
                             country == "Brunei Darussalam" ~ "BRN",
                             country == "Bhutan" ~ "BTN",
                             country == "Botswana" ~ "BWA",
                             country == "Belize" ~ "BLZ",
                             country == "Central African Republic" ~ "CAF",
                             country == "Channel Islands" ~ "GB1",
                             country == "Chile" ~ "CHL",
                             country == "C?te d'Ivoire" ~ "CIV",
                             country == "Cameroon" ~ "CMR",
                             country == "Congo, Democratic Republic of the" ~ "COD",
                             country == "Congo" ~ "COG",
                             country == "Comoros" ~ "COM",
                             country == "Cape Verde" ~ "CPV",
                             country == "Costa Rica" ~ "CRI",
                             country == "Western Sahara" ~ "ESH",
                             country == "Guinea" ~ "GIN",
                             country == "Fiji" ~ "FJI",
                             country == "Gambia" ~ "GMB",
                             country == "Guinea-Bissau" ~ "GNB",
                             country == "Equatorial Guinea" ~ "GNQ",
                             country == "Guatemala" ~ "GTM",
                             country == "Guam" ~ "GUM",
                             country == "Hong Kong, China" ~ "HKG",
                             country == "Honduras" ~ "HND",
                             country == "Haiti" ~ "HTI",
                             country == "Iraq" ~ "IRQ",
                             country == "Iceland" ~ "ISL",
                             country == "Kyrgyzstan" ~ "KGZ",
                             country == "Cambodia" ~ "KHM",
                             country == "Kuwait" ~ "KWT",
                             country == "Lebanon" ~ "LBN",
                             country == "Liberia" ~ "LBR",
                             country == "Libya" ~ "LBY",
                             country == "Saint Lucia" ~ "LCA",
                             country == "Sri Lanka" ~ "LKA",
                             country == "Lesotho" ~ "LSO",
                             country == "Latvia" ~ "LVA",
                             country == "Morocco" ~ "MAR",
                             country == "Moldova, Republic of" ~ "MDA",
                             country == "Madagascar" ~ "MDG",
                             country == "Maldives" ~ "MDV",
                             country == "North Macedonia" ~ "MKD",
                             country == "Mali" ~ "MLI",
                             country == "Myanmar" ~ "MMR",
                             country == "Montenegro" ~ "MNE",
                             country == "Mongolia" ~ "MNG",
                             country == "Mauritania" ~ "MRT",
                             country == "Mauritius" ~ "MUS",
                             country == "Malawi" ~ "MWI",
                             country == "Malaysia" ~ "MYS",
                             country == "New Caledonia" ~ "NCL",
                             country == "Niger" ~ "NER",
                             country == "Nigeria" ~ "NGA",
                             country == "Nepal" ~ "NPL",
                             country == "New Zealand" ~ "NZL",
                             country == "Oman" ~ "OMN",
                             country == "Philippines" ~ "PHL",
                             country == "Papua New Guinea" ~ "PNG",
                             country == "Puerto Rico" ~ "PRI",
                             country == "Paraguay" ~ "PRY",
                             country == "Korea, Democratic People's Republic of" ~ "PRK",
                             country == "Occupied Palestinian Territory" ~ "PSE",
                             country == "French Polynesia" ~ "PYF",
                             country == "Singapore" ~ "SGP",
                             country == "Solomon Islands" ~ "SLB",
                             country == "Sierra Leone" ~ "SLE",
                             country == "El Salvador" ~ "SLV",
                             country == "Serbia" ~ "SRB",
                             country == "South Sudan" ~ "SDS",
                             country == "Sao Tome and Principe" ~ "STP",
                             country == "Eswatini" ~ "SWZ",
                             country == "Chad" ~ "TCD",
                             country == "Togo" ~ "TGO",
                             country == "Tajikistan" ~ "TJK",
                             country == "Timor-Leste" ~ "TLS",
                             country == "Tonga" ~ "TON",
                             country == "Belarus" ~ "BLR",
                             country == "Trinidad and Tobago" ~ "TTO",
                             country == "Tanzania, United Republic of" ~ "TZA",
                             country == "Turkmenistan" ~ "TKM",
                             country == "Uruguay" ~ "URY",
                             country == "Saint Vincent and the Grenadines" ~ "VCT",
                             country == "United States Virgin Islands" ~ "VIR",
                             country == "Viet Nam" ~ "VNM",
                             country == "Vanuatu" ~ "VUT",
                             country == "Samoa" ~ "WSM",
                             country == "South Africa" ~ "ZAF",
                             country == "Zambia" ~ "ZMB",
                             country == "Zimbabwe" ~ "ZWE",
                             country == "Germany" ~ "DEU",
                             country == "Denmark" ~ "DNK",
                             country == "Spain" ~ "ESP",
                             country == "United Kingdom" ~ "GBR",
                             country == "Greece" ~ "GRC",
                             country == "Croatia" ~ "HRV",
                             country == "Indonesia" ~ "IDN",
                             country == "Ireland" ~ "IRL",
                             country == "Japan" ~ "JPN",
                             country == "Lithuania" ~ "LTU",
                             country == "Malta" ~ "MLT",
                             country == "Netherlands" ~ "NLD",
                             country == "Portugal" ~ "PRT",
                             country == "Romania" ~ "ROU",
                             country == "Eswatini" ~ "SWZ",
                             country == "Slovakia" ~ "SVK",
                             country == "Slovenia" ~ "SVN",
                             country == "Taiwan, China" ~ "TWN",
                             country == "United States" ~ "USA",
                             country == "United Arab Emirates" ~ "ARE",
                             country == "Russian Federation" ~"RUS",
                             country == "World" ~ "ROW",
                             TRUE ~ toupper(substr(country, 1, 3)))) %>% 
  
  

  # instead of "world" we want the "rest of world"
  ##world is currently called world, need to change that for it to work
  mutate(labor_force = ifelse(country == "ROW", 
                              labor_force - sum(labor_force[country != "ROW"]),
                              labor_force)) %>% 
  # get L and lambda
  mutate(L = sum(labor_force),
         lambda = labor_force / L)

labor_data <-labor_data[order(labor_data[,'country']), ]

## several countries in EORA and ILO that don't match
## to check run code below

##`%nin%` = Negate(`%in%`)  
 ### make sure to jsut save a list, this is ridiculous
### to check mismatches 
#countries <- unique(Eora$country)
#comp <- cbind(labor_data$country, countries)
#comp <- as.data.frame(comp)
#mismatch <- which(comp$V1 %nin% comp$countries) ##countries in ILO not in EORA
#mismatch2 <- which(comp$countries %nin% comp$V1)  ## countries in EORA not in ILO, these need to be adjusted in EORA, collapse into ROW
  

### Removing deficits

N <- length(unique(prod_data$country)) # number of countries 
J <- length(unique(prod_data$sector)) # number of sectors


# Set up the shock
T_hat <- matrix(rep(1, J*N), nrow=N) # all unchanged, i.e. = 1
tau_hat <- array(rep(1, N*N*J), dim=c(N, N, J)) # unchanged
D_prime <- rep(0, N) # all deficits set to 0

##========== no changes made here yet



# get variables into vectors, matrices and arrays... this makes it easier to
# program the simulation, i.e. iterative solution of the equilibrium equation 
# system
lambda <- arrange(labor_data, country) %>% .$lambda
R <- prod_data %>% 
  distinct(sector, country, tot_revenue) %>%
  arrange(sector, country) %>% 
  .$tot_revenue %>% matrix(nrow = N)

D <- trade %>%
  distinct(country, deficit) %>% 
  arrange(country) %>% 
  .$deficit %>% as.vector()

alpha_nj <- arrange(cnsmptn_data, sector, importer) %>%
  .$alpha_nj %>%
  matrix(nrow = N) 
alpha_nj <- alpha_nj * (1 - alpha_H)

gamma_njg <- arrange(prod_data, intermediate_sec, sector, country) %>% 
  .$gamma_njg %>% array(dim=c(N,J,J))
gamma_nj <- prod_data %>% 
  distinct(sector, country, gamma_nj) %>% 
  arrange(sector, country) %>% 
  .$gamma_nj %>% matrix(nrow = N)
pi_nij <- arrange(trade, sector, country, importer) %>% 
  .$pi_nij %>% array(dim=c(N,N,J))

##======code works til here, initial guesses 
## below doesnt work when it comes to iteration, makes sense tho because WIOT != ILo data
## need to remove countries so it matches up, ask Oliver how to go about this. 

# Set up initial 'guesses' for variables
lambda_hat <- rep(1, N) 
P_hat <- matrix(rep(1, J*N), nrow=N)
Y_hat <- rep(1, N)
w_hat <- rep(1, N)
c_hat <-  matrix(rep(1, J*N), nrow=N)
pi_hat <- array(1, dim = c(N,N,J))
R_prime <- R
R_hat <- ifelse(R == 0, 1, R_prime / R)

# Solve equilibrium equation system iteratively, i.e. iterate unitl the change
# in lambda_hat (l_diff) and the change in R_hat (r_diff) get smaller than some
# tolerance value kTol
kTol <- 1e-3 # EORA is reported in thousands, so this is a tolerance of 1 dollar
l_diff <- kTol + 1 # make sure the loop starts
r_diff <- kTol + 1
iter <- 1 # keep track of iterations in case something goes wrong do max 1000
while((r_diff > kTol | l_diff > kTol) & iter < 10000) {
  
  # wage changes
  w_hat <- rowSums(R_prime * gamma_nj) / rowSums(R * gamma_nj) / lambda_hat
  
  # price changes
  tmp <- sapply(1:N, function(i) {
    c_hat[i,]^(- theta_j) * T_hat[i,]^(theta_j)
  }) %>% t
  
  P_hat <- sapply(1:N, function(n) {
    pi_hat[n,n,]^(-1 / (- theta_j)) * 
      tmp[n,]^(1 / (- theta_j))
  }) %>% t
  P_hat <- P_hat / P_hat[1]
  
  # cost changes
  c_hat <- w_hat^gamma_nj * 
    sapply(1:J, function(j) {
      rowProds(P_hat^gamma_njg[,j,])
    })  
  
  # import share changes
  tmp <- sapply(1:N, function(i) {
    c_hat[i,]^(- theta_j) * T_hat[i,]^(theta_j)
  }) %>% t
  
  pi_hat <- sapply(1:J, function(j) {
    sapply(1:N, function(i) {
      sapply(1:N, function(n) { 
        tau_hat[n,i,j]^(- theta_j[j]) * tmp[i,j] /
          sum(pi_nij[n,,j] * tau_hat[n,,j]^(- theta_j[j]) * tmp[,j])
      })
    })
  }, simplify = "array")
  pi_hat[pi_nij == 0] <- 1
  
  R_prime_new <- sapply(1:N, function(i) {
    colSums(pi_hat[,i,] * pi_nij[,i,] * 
              (alpha_nj * (rowSums(R_prime * gamma_nj) + D_prime) / (1-alpha_H) +
                 sapply(1:J, function(j) {
                   sapply(1:N, function(n) { sum(R_prime[n,] * gamma_njg[n,,j])})
                 })))
  }) %>% t
  
  # get the maximum change (to compare to the tolerance)
  r_diff <- max(abs(ifelse(R == 0, 1, R_prime_new/R) - R_hat))
  
  # adapt R_prime using R_prime_new and a dampening factor - this ensure that 
  # the iterative procedure converges to a solution
  R_prime <- R_prime + 0.3 * (R_prime_new - R_prime)
  R_hat <- ifelse(R == 0, 1, R_prime / R)
  
  # income change
  Y_hat <- (rowSums(R_prime * gamma_nj) + D_prime) /
    (rowSums(R * gamma_nj) + D) / lambda_hat
  
  # lambda change
  tmp <- (Y_hat^(1-alpha_H) / 
            (lambda_hat^alpha_H * rowProds(P_hat^alpha_nj)))^epsilon
  
  lambda_hat_new = tmp / sum(lambda * tmp) 
  
  # get the maximum change (to compare to the tolerance)
  l_diff <- max(abs(lambda_hat_new - lambda_hat))
  # adapt lambda_hat using the newly calculated value and a dampening factor tp
  # ensure convergence of the iterative procedure
  lambda_hat <- lambda_hat + 0.1 * (lambda_hat_new - lambda_hat)
  
  # print current status 
  print(paste0("iter: ", iter, " l_diff: ", l_diff, " r_diff: ", r_diff))
  iter <- iter + 1
}



variables <- list(R = R_prime, lambda = lambda * lambda_hat, 
                  pi_nij = pi_nij * pi_hat)
parameters <- list(alpha_nj = alpha_nj, alpha_H = alpha_H, gamma_nj = gamma_nj,
                   gamma_njg = gamma_njg, theta_j = theta_j, epsilon = epsilon)

saveRDS(variables, "Data/variablesfinal.RDS")
saveRDS(parameters, "Data/parametersfinal.RDS")


