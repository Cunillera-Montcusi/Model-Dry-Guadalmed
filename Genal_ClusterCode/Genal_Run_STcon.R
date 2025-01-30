#Test final UY_ ALPS 
setwd("DGY/Genal_ClusterCode/")

load("SLURM_GenalData.RData")
source("SpaTemp_function_NO_Plot.R")

# 5. STconmat calculation  ####
# Remember that STcon is able to calculate several rivers at the same time! So you just need to have a list object with the 3 elements 
# for each river scenario: 
library(igraph)
library(doParallel)
# We run the function (It takes a while)
Riv_Drift <- spat_temp_index(Inermitence_dataset = Inermitence_dataset_Campaings_To_Run,
                            Sites_coordinates=Sites_coordinates_Campaings_To_Run,
                            Network_stru =Network_stru_Campaings_To_Run,
                            direction="directed", 
                            sense = "out",
                            weighting=T,dist_matrices = Dist_matrix,
                            weighting_links =FALSE,link_weights = NULL,
                            legacy_effect =1, legacy_lenght = 1,
                            value_S_LINK=0.1,
                            value_T_LINK=0.1,
                            value_NO_S_link=1,
                            value_NO_T_link=1,
                            Network_variables=F,print.plots=F,print.directory="Figure/")
save(list = "Riv_Drift",file = "Riv_Drift_STcon.RData")

Riv_Swim <- spat_temp_index(Inermitence_dataset = Inermitence_dataset_Campaings_To_Run,
                           Sites_coordinates=Sites_coordinates_Campaings_To_Run,
                           Network_stru =Network_stru_Campaings_To_Run,
                           direction="directed", sense = "all",
                           weighting=T,dist_matrices = Dist_matrix,
                           weighting_links =FALSE,link_weights = NULL,
                           legacy_effect =1, legacy_lenght = 1,
                           value_S_LINK=0.1,
                           value_T_LINK=0.1,
                           value_NO_S_link=1,
                           value_NO_T_link=1,
                           Network_variables=F,print.plots=F,print.directory="Figure/")
save(list = "Riv_Swim",file = "Riv_Swim_STcon.RData")

# Network structure
Distances <- as.matrix(Dist_matrix[[1]])
hist(as.vector(Distances))
Net_stru <- Network_stru_Campaings_To_Run[[1]]
Net_stru <- ifelse(Distances<1,1,0)
diag(Net_stru) <- 0
Net_stru <- list(Net_stru)
#Net_stru <- replicate(length(Inermitence_dataset_Campaings_To_Run), Net_stru, simplify = FALSE)
# Distance matrix
Dist_matr <- list(as.matrix(Distances)*Net_stru[[1]])
#Dist_matr <- replicate(length(Int_dataset), Dist_matr, simplify = FALSE)

Riv_AerAct <- spat_temp_index(Inermitence_dataset = Inermitence_dataset_Campaings_To_Run,
                              Sites_coordinates=Sites_coordinates_Campaings_To_Run,
                              Network_stru =Net_stru,
                              direction="undirected", sense = "all",
                              weighting=T,dist_matrices = Dist_matr,
                              weighting_links =FALSE,link_weights = NULL,
                              legacy_effect =1, legacy_lenght = 1,
                              value_S_LINK=0.1,
                              value_T_LINK=0.1,
                              value_NO_S_link=1,
                              value_NO_T_link=1,
                              Network_variables=F,print.plots=F,print.directory="Figure/")
save(list = "Riv_AerAct",file = "Riv_AerAct_STcon.RData")




