#Test final UY_ ALPS 

load("SLURM_PreSTcon.RData")
source("SpaTemp_function_NO_Plot.R")

# 5. STconmat calculation  ####
# Remember that STcon is able to calculate several rivers at the same time! So you just need to have a list object with the 3 elements 
# for each river scenario: 
library(igraph)
library(doParallel)

# Intermittence database
Flow_DB_toSTcon[[length(Flow_DB_toSTcon)+1]] <- ifelse(Flow_DB_toSTcon[[1]]==0,1,Flow_DB_toSTcon[[1]])
Int_dataset <- Flow_DB_toSTcon
# Sites coordinates
Sit_coordinates <- nodes_DaFr[,c(1:4)]
Sit_coordinates <- replicate(length(Int_dataset), Sit_coordinates, simplify = FALSE)
# Network structure
Net_stru <- Net_stru_RAW
Net_stru <- replicate(length(Int_dataset), Net_stru, simplify = FALSE)
# Distance matrix
Dist_matr <- distances(g)#((as.matrix(dist(nodes_DaFr[,3:4])))*as.matrix(ocn_TEST$FD$W))
Dist_matr <- replicate(length(Int_dataset), Dist_matr, simplify = FALSE)


Riv_Drift <- spat_temp_index(Inermitence_dataset = Int_dataset,
                             Sites_coordinates=Sit_coordinates,
                             Network_stru =Net_stru,
                             direction="directed", sense = "out",
                             weighting=T,dist_matrices = Dist_matr,
                             weighting_links =FALSE,link_weights = NULL,
                             legacy_effect =1, legacy_lenght = 1,
                             value_S_LINK=0.1,
                             value_T_LINK=0.1,
                             value_NO_S_link=1,
                             value_NO_T_link=1,
                             Network_variables=F,print.plots=F,print.directory="Figure/")
save(list = "Riv_Drift",file = "Riv_Drift_STcon.RData")


Riv_Swim <- spat_temp_index(Inermitence_dataset = Int_dataset,
                            Sites_coordinates=Sit_coordinates,
                            Network_stru =Net_stru,
                            direction="directed", sense = "all",
                            weighting=T,dist_matrices = Dist_matr,
                            weighting_links =FALSE,link_weights = NULL,
                            legacy_effect =1, legacy_lenght = 1,
                            value_S_LINK=0.1,
                            value_T_LINK=0.1,
                            value_NO_S_link=1,
                            value_NO_T_link=1,
                            Network_variables=F,print.plots=F,print.directory="Figure/")
save(list = "Riv_Swim",file = "Riv_Swim_STcon.RData")

# Network structure
Distances <- as.matrix(dist(nodes_DaFr[,3:4]))
summary(as.vector(Distances))
Net_stru<- Net_stru_RAW
Net_stru <- ifelse(Distances<5,1,0)
diag(Net_stru) <- 0
Net_stru <- replicate(length(Int_dataset), Net_stru, simplify = FALSE)
# Distance matrix
Dist_matr <- ((as.matrix(dist(nodes_DaFr[,3:4])))*Net_stru[[1]])
Dist_matr <- replicate(length(Int_dataset), Dist_matr, simplify = FALSE)

Riv_AerAct <- spat_temp_index(Inermitence_dataset = Int_dataset,
                            Sites_coordinates=Sit_coordinates,
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







