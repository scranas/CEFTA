library(tidyverse)
library(matrixStats)


# Data =========================================================================

variables <- readRDS("variablesfinal.RDS")
parameters <- readRDS("parametersfinal.RDS")
countries <- readRDS("countries.RDS")


#industries file place holder
industries <- c("Agriculture",
                "Fishing"	,
                "Mining and Quarrying",	
                "Food & Beverages",
                "Textiles and Wearing Apparel",	
                "Wood and Paper",	
                "Petroleum, Chemical and Non-Metallic Mineral Products"	,
                "Metal Products	",
                "Electrical and Machinery	",
                "Transport Equipment",	
                "Other Manufacturing",	
                "Recycling",	
                "Electricity, Gas and Water",	
                "Construction",	
                "Maintenance and Repair",	
                "Wholesale Trade",	
                "Retail Trade",	
                "Hotels and Restraurants",	
                "Transport"	,
                "Post and Telecommunications"	,
                "Finacial Intermediation and Business Activities",	
                "Public Administration",	
                "Education, Health and Other Services",	
                "Private Households",	
                "Others",	
                "Re-export & Re-import")	
# extract the data/objects from the two lists 
list2env(variables, globalenv())
list2env(parameters, globalenv())

europe <- c("AUT", "BEL", "BGR", "CHE", "CYP", "CZE", "DEU", "DNK", "ESP",
            "EST", "FIN", "FRA", "GBR", "GRC", "HRV", "HUN", "IRL", "ITA",
            "LTU", "LUX", "LVA", "MLT", "NLD", "NOR", "POL", "PRT", "ROU",
            "SVK", "SVN", "SWE")
westbalkan <-c("ALB", "BIH", "MKD", "SRB", "MNE", "MDA")

combo <- c(europe, westbalkan)
# symmetrical shock so i should jsut be able to comvbine the two lists, and ids accordingly and have a single tau_hat specification
# the same with their respective country index
# update country id based on country id position in my data

europe_id <- match(europe, countries)

westbalkan_id <- match(westbalkan, countries) 



europepluswb_id <- c(europe_id,westbalkan_id)

# Shock ========================================================================

N <- nrow(R) # number of countries
J <- ncol(R) # number of sectors
kTol <- 1e-3 # WEORA is in thousands so this is a tolerance of 1 Dollar

# set mobility
pop_mobility <- "europepluswb" # set "europe", "europepluswb", "none" should thi sbe none or europe for cefta shock? obv europe for EU shock later

# set technological changes
T_hat <- matrix(1, nrow = N, ncol = J)

# set trade barrier changes
tau_hat <- array(1, dim = c(N, N, J))




tausEU <- c(0.7293, 1,1,0.7564,1,0.8446,1,1,1,1,0.8738,1,1,0.7071,1,0.7734,1,0.6118,0.8639,0.7887,0.8688,0.7226,0.6962,1,1,1)


# for West Balkans

tau_hat[europe_id, westbalkan_id, ] <- rep(tausEU, 
                                           each = length(europe_id) * length(westbalkan_id))

##for EUROPE part

tau_hat[westbalkan_id, europe_id, ] <- rep(tausEU, 
                                           each = length(europe_id) * length(westbalkan_id))



# Run the simulation ===========================================================

# Set up initial 'guesses' for variables
lambda_hat <- rep(1, N)
P_hat <- matrix(rep(1, J*N), nrow=N)
Y_hat <- rep(1, N)
w_hat <- rep(1, N)
c_hat <-  matrix(rep(1, J*N), nrow=N)
pi_hat <- array(1, dim = c(N,N,J))
R_prime <- R
R_hat <- ifelse(R == 0, 1, R_prime / R)

# depending on popmobility correct lambda values
##check if having to redo this as well for europepluswb
if (pop_mobility == "none") {lambda <- rep(1, N)}
if (pop_mobility == "europe") {
  lambda[-europe_id] <- 1
  lambda[europe_id] <- lambda[europe_id] / sum(lambda[europe_id])
}
if (pop_mobility == "europepluswb") {
  lambda[-europepluswb_id] <- 1
  lambda[europepluswb_id] <- lambda[europepluswb_id] / sum(lambda[europepluswb_id])
}
# Solve equilibrium equation system iteratively, i.e. iterate unitl the change
# in lambda_hat (l_diff) and the change in R_hat (r_diff) get smaller than some
# tolerance value kTol
l_diff <- kTol + 1 # make sure the loop starts
r_diff <- kTol + 1
iter <- 1 # keep track of iterations in case something goes wrong do max 1000, gonna have to be larger, maybe ktol5 at 5k
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
              (alpha_nj * rowSums(R_prime * gamma_nj) / (1-alpha_H) +
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
  
  # income change NOTE that deficit no longer shows up in Y because we calibrated it out, so to say
  Y_hat <- rowSums(R_prime * gamma_nj) /
    rowSums(R * gamma_nj)  / lambda_hat
  
  if (pop_mobility == "europe") {
    # lambda change
    tmp <- (Y_hat^(1-alpha_H) / 
              (lambda_hat^alpha_H * rowProds(P_hat^alpha_nj)))^epsilon
    
    lambda_hat_new <- lambda_hat
    lambda_hat_new[europe_id] <- tmp[europe_id] / 
      sum(lambda[europe_id] * tmp[europe_id])  # !!!! If your lambda does not
    # match you lambda_hat or more specifically the order of countries in all
    # the other variables (R, ...) then this creates havoc here.
  }
  if (pop_mobility == "europepluswb") {
    # lambda change
    tmp <- (Y_hat^(1-alpha_H) / 
              (lambda_hat^alpha_H * rowProds(P_hat^alpha_nj)))^epsilon
    
    lambda_hat_new <- lambda_hat
    lambda_hat_new[europepluswb_id] <- tmp[europepluswb_id] / 
      sum(lambda[europepluswb_id] * tmp[europepluswb_id]) 
  }
  if (pop_mobility == "none") {
    lambda_hat_new <- lambda_hat
  }
  # get the maximum change (to compare to the tolerance)
  l_diff <- max(abs(lambda_hat_new - lambda_hat))
  # adapt lambda_hat using the newly calculated value and a dampening factor tp
  # ensure convergence of the iterative procedure
  lambda_hat <- lambda_hat + 0.1 * (lambda_hat_new - lambda_hat)
  
  # print current status 
  print(paste0("iter: ", iter, " l_diff: ", l_diff, " r_diff: ", r_diff))
  iter <- iter + 1
}



# get expenditure
X_nj <- alpha_nj * rowSums(R * gamma_nj) / (1-alpha_H) +
  sapply(1:J, function(j) {
    sapply(1:N, function(n) { sum(R[n,] * gamma_njg[n,,j])})
  })

X_prime <- alpha_nj * rowSums(R_prime * gamma_nj) / (1-alpha_H) +
  sapply(1:J, function(j) {
    sapply(1:N, function(n) { sum(R_prime[n,] * gamma_njg[n,,j])})
  })

P_csmpt_hat <- rowProds(P_hat^alpha_nj)

results <- tibble(country = countries, lambda_hat = lambda_hat, w_hat = w_hat,
                  lambda = lambda, P_csmpt_hat = P_csmpt_hat)
results <- merge(results, industries) %>% as_tibble()
results <- results %>% 
  arrange(y, country) %>% # sector header doesnt show up in results file currently, colname is y, fix and replace y for sector
  mutate(R = as.vector(R),
         R_prime = as.vector(R_prime),
         P_hat = as.vector(P_hat),
         T_hat = as.vector(T_hat),
         X_nj = as.vector(X_nj),
         X_prime = as.vector(X_prime),
         Csmpt_hat = w_hat^(1-alpha_H) / (lambda_hat^alpha_H * P_csmpt_hat),
         U_hat = w_hat^(1-alpha_H) / (lambda_hat^alpha_H * P_csmpt_hat) * 
           lambda_hat^(-1/epsilon))

# put pi_hat into a different file due to its size
results_bilateral <- expand.grid(exporter = countries, importer = countries,
                                 sector = 1:26, stringsAsFactors = FALSE) %>% 
  arrange(importer, exporter, sector) %>% 
  mutate(pi_hat = as.vector(pi_hat),
         pi_nij = as.vector(pi_nij))

saveRDS(results, "Data/finalresultsEU.rds")
saveRDS(results_bilateral, "Data/finalbilateralresultsEU.rds")

#saving results here, so i can just reload them when switching between CEFTA and EU SIM without having to rerun iteration

results <- readRDS("Data/finalresultsEU.rds")
results_bilateral <- readRDS("Data/finalbilateralresultsEU.rds")

results %>% 
  group_by(country) %>% 
  summarise(before = sum(R), after = sum(R_prime)) %>% 
  gather(EU_Accession, Revenue, -country) %>% 
  ggplot(aes(x = country, y = Revenue, fill = EU_Accession)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

results %>% 
  distinct(country, lambda_hat) %>%
  mutate(lambda_pct = 100*(lambda_hat - 1)) %>% 
  ggplot(aes(x = country, y = lambda_pct)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

results %>% 
  distinct(country, w_hat) %>%
  mutate(w_pct = 100*(w_hat - 1)) %>% 
  ggplot(aes(x = country, y = w_pct)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

# indices <- results %>% distinct(country, y, R) %>%
#   rename(producer = country, revenue = R, sector = y) %>% #same as before y should be sector, change back alter
#   filter(producer %in% europe) %>%
#   group_by(producer) %>%
#   mutate(c_rev = sum(revenue)) %>%
#   group_by(sector) %>%
#   mutate(sec_rev = sum(revenue)) %>%
#   ungroup  %>%
#   mutate(s_shr = revenue / c_rev,
#          l_shr = revenue / sec_rev)%>%
#   group_by(producer) %>%
#   mutate(hhi_spec = sum(s_shr^2)) %>%
#   group_by(sector) %>%
#   mutate(hhi_conc = sum(l_shr^2)) %>%
#   ungroup %>%
#   mutate(tot_rev = sum(revenue),
#          l_avg = c_rev / tot_rev,
#          s_avg = sec_rev / tot_rev) %>%
#   group_by(sector) %>%
#   mutate(ks_conc = sum(abs(l_shr - l_avg))) %>%
#   group_by(producer) %>%
#   mutate(ks_spec = sum(abs(s_shr - s_avg))) %>%
#   ungroup
# 
# indices_new <- results %>% distinct(country, y, R_prime) %>% #sector_name replaced with y for now bc colname wrong
#   rename(producer = country, revenue = R_prime, sector = y) %>%
#   filter(producer %in% europe) %>%
#   group_by(producer) %>%
#   mutate(c_rev = sum(revenue)) %>%
#   group_by(sector) %>%
#   mutate(sec_rev = sum(revenue)) %>%
#   ungroup  %>%
#   mutate(s_shr = revenue / c_rev,
#          l_shr = revenue / sec_rev)%>%
#   group_by(producer) %>%
#   mutate(hhi_spec = sum(s_shr^2)) %>%
#   group_by(sector) %>%
#   mutate(hhi_conc = sum(l_shr^2)) %>%
#   ungroup %>%
#   mutate(tot_rev = sum(revenue),
#          l_avg = c_rev / tot_rev,
#          s_avg = sec_rev / tot_rev) %>%
#   group_by(sector) %>%
#   mutate(ks_conc = sum(abs(l_shr - l_avg))) %>%
#   group_by(producer) %>%
#   mutate(ks_spec = sum(abs(s_shr - s_avg))) %>%
#   ungroup
# 
# indices <- left_join(indices, indices_new, by = c("producer", "sector"),
#                      suffix = c("", "_new"))
# indices %>%
#   distinct(sector, ks_conc, ks_conc_new) %>%
#   rename(before = ks_conc, after = ks_conc_new) %>%
#   gather(CEFTA, ks_conc, -sector) %>%
#   ggplot(aes(x = sector, y = ks_conc, fill = CEFTA)) +
#   geom_bar(stat = "identity", position = position_dodge()) +
#   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

results_bilateral <- results %>%
  distinct(country, y, X_nj, X_prime) %>%
  rename(importer = country) %>%
  right_join(results_bilateral)

results_bilateral <- results_bilateral %>%
  mutate(flow = X_nj * pi_nij,
         flow_prime = X_prime * pi_nij * pi_hat)

trade <- results_bilateral %>%
  filter(exporter %in% combo, importer %in% combo) %>% #cuts down countries to EU and WB countries
  group_by(exporter, importer) %>%
  summarise(flow = sum(flow), flow_prime = sum(flow_prime)) %>%
  mutate(pct = 100*(flow_prime - flow) / flow) %>%
  ungroup


trade <- trade %>% ungroup %>%
  mutate(importer = ifelse(importer %in% westbalkan, "West Balkan", "Rest_EU"),
         exporter = ifelse(exporter %in% westbalkan, "West Balkan", "Rest_EU")) %>%
  group_by(exporter, importer) %>%
  summarise(flow = sum(flow), flow_prime = sum(flow_prime)) %>%
  mutate(pct = 100*(flow_prime - flow) / flow)


write_xlsx(trade,"Data/EUtradeflowschangeEUWB.xlsx")