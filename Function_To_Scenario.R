
# old <- options(stringsAsFactors = FALSE)
# on.exit(options(old))
# 
# MSI_Old <- "E:/"
# MSI_New <- "C:/Users/David CM/"


# 1. FAKE RIVER CREATION ####
# with OCNet we create a river with a determined dendritic structure
library(OCNet)

# With the following function we create a river of a determined size and charactersitics: 
# Cellsize sets the size of each cell (pixels) -- We can use this as a proxy of "community size"
# expEnergy is defining the "branching" of the river in accordance with the size
ocn_TEST <- create_OCN(dimX = 15,dimY = 15,nOutlet = 1,cellsize = 2, expEnergy=0.5)
# Easy function to draw the river
draw_simple_OCN(ocn_TEST)
# FD$A defines the size (area actually) of each cell based on each position in the stream (order of the reach)
length(ocn_TEST$FD$A)
# FD$W is basically the adjancency matrix that we will have to use to built our graph (from & to matrix)
ncol(as.matrix(ocn_TEST$FD$W))
# Coordinates of each of the reaches
length(ocn_TEST$FD$X)
length(ocn_TEST$FD$Y)

# Through the adjacency matrix we can create a graph object
library(igraph)
g <- graph.adjacency(adjmatrix =as.matrix(ocn_TEST$FD$W),mode = "directed")

library(tidyverse)
library(viridis)
# And later we can use this graph to create a "data frame" friendly format. 
# Warning: There is many other ways to do this but all finish by being the same ;)  
# Transformation to ggplot friendly data - for ploting and managing
edges_DaFr <- as.data.frame(get.edgelist(g))
edges_DaFr <- edges_DaFr %>% dplyr::select("from"=V1, "to"=V2) %>% 
  mutate(X1_coord=2)%>% mutate(Y1_coord=2)%>% 
  mutate(X2_coord=2)%>% mutate(Y2_coord=2) # Create new columns for coordinates, to be filled later

nodes_DaFr <- data.frame("Site_ID"=1:length(ocn_TEST$FD$X),"x"=ocn_TEST$FD$X,"y"=ocn_TEST$FD$Y,"weight"=ocn_TEST$FD$A)

# Fill X and Y coordinate information for edges, using the nodes_DaFr
for (nodes in 1:nrow(nodes_DaFr)) {
  edges_DaFr[which(edges_DaFr$from==nodes),3] <- nodes_DaFr[nodes,]$x
  edges_DaFr[which(edges_DaFr$from==nodes),4] <- nodes_DaFr[nodes,]$y
  edges_DaFr[which(edges_DaFr$to==nodes),5] <- nodes_DaFr[nodes,]$x
  edges_DaFr[which(edges_DaFr$to==nodes),6] <- nodes_DaFr[nodes,]$y
}

Years_Of_Drying <- 4

diff_extent <- c(0.01,0.1,0.25,0.35,0.45,0.5,0.65,0.75,0.9)
diff_extent <- tidyr::crossing(Dry_Ext=diff_extent,
                               Pollut_Ext=diff_extent,
                               Dry_Pattern_End=diff_extent) %>% 
                filter(!Dry_Pattern_End%in%c(0.65))
nrow(diff_extent)

#diff_extent <- diff_extent %>% filter(Dry_Pattern_End%in%c(0.01,0.25,0.75))

Flow_DB_toSTcon <- list()
Dry_nodes_DaFr <- nodes_DaFr
Dry_DataFrame_out <- list()

# Here we decide the extent of drying
Sites_to_Dry<- list()
Selected_Sites <- sample(nodes_DaFr$Site_ID,size = length(nodes_DaFr$Site_ID),replace = F)
for (Sit_To_Dry in 1:length(unique(diff_extent$Dry_Ext))) {
Sites_to_Dry[[Sit_To_Dry]] <-Selected_Sites[1:ceiling(nrow(nodes_DaFr)*unique(diff_extent$Dry_Ext)[Sit_To_Dry])] 
}

for (diff_extent_value in 1:(length(unique(diff_extent$Dry_Ext))*length(unique(diff_extent$Dry_Pattern_End)))) {
Dry_extent <- 1

Dry_diff_extent <- tidyr::crossing(Dry_Ext=unique(diff_extent$Dry_Ext),Dry_Pattern_End=unique(diff_extent$Dry_Pattern_End))

Sites_to_Dry_Position <- which(unique(diff_extent$Dry_Ext)==as.numeric(Dry_diff_extent[diff_extent_value,1]))

# 2. Drying pattern  ####
# Drying patterns are incorporated following different criteria
# Temporal - years or days time span
# Duration can either be a single value or vector
## the values must range from 0 to 1 which correspond to the % of "time" of drying period
# Duration_constancy - responds to making the drying pattern "yearly" and repeating every year
## if FALSE it it will assign the corresponding drying days randomly through the whole time period (random drying)
# Extect - corresponds to the % of selected nodes that will be drying
# Distribution - corresponds to the specific nodes that we want to submit to drying (e.g. only upstream)
# skeweed_distr is a modulator for "skewing" the distribution of durations. If all values are 1 it means 
# that there is the same probability to get any of the duration (it can be useful in case we would like to
# favor some of the values)
#source("Function_to_dry.R")
source(file = paste0(getwd(),"/Function_to_dry.R"))
#duration_for_function <- seq(from=pull(diff_extent[diff_extent_value,3]),to=1,length.out=6)
duration_for_function <- 1-seq(diff_extent$Dry_Pattern_End[diff_extent_value],to=0.9,length.out=6)

Flow_DB <- function_to_dry(River_nodes = Dry_nodes_DaFr,
                           years =Years_Of_Drying,days =F,
                           duration =duration_for_function,
                           duration_constancy = F,
                           extent =as.numeric(Dry_extent),
                           distribution =Sites_to_Dry[[Sites_to_Dry_Position]],
                           skeweed_distr = rep(1,6))

# We add a factor called "Permanence to the "nodes" to plot
Dry_nodes_DaFr <- Dry_nodes_DaFr %>% mutate("Permanence"=apply(Flow_DB[,2:ncol(Flow_DB)],2,sum),
                                            "Drying"=nrow(Flow_DB)-apply(Flow_DB[,2:ncol(Flow_DB)],2,sum),
                                            "Dry_Patter"=sd(duration_for_function))

Dry_DataFrame_out[[diff_extent_value]] <- Dry_nodes_DaFr
Flow_DB_toSTcon[[diff_extent_value]] <- Flow_DB
}

Orig_dispersal_pollution <- read.csv(file = paste0(getwd(), "/data/pollution_dispersal.csv")) %>% drop_na()


Sites_to_Pollut<- list()
Selected_Sites <- sample(nodes_DaFr$Site_ID,size = length(nodes_DaFr$Site_ID),replace = F)
for (Sit_To_Pol in 1:length(unique(diff_extent$Pollut_Ext))) {
  Sites_to_Pollut[[Sit_To_Pol]] <-Selected_Sites[1:ceiling(nrow(nodes_DaFr)*unique(diff_extent$Pollut_Ext)[Sit_To_Pol])] 
}

Poll_DataFrame_out <- list()
Poll_filter_Pollution <- list()

# 3. Pollutino assignation  ####
for (diff_extent_value in 1:length(unique(diff_extent$Pollut_Ext))) {
Poll_nodes_DaFr <- nodes_DaFr
Poll_extent <- 1
  
Poll_diff_extent <- unique(diff_extent$Pollut_Ext)
  
Sites_to_Pol_Position <- which(unique(diff_extent$Pollut_Ext)==as.numeric(Poll_diff_extent[diff_extent_value]))

#Spp_tolerance <- c(rep(0.5,50),rep(0.65,50),rep(0.75,50),rep(0.8,50))
Spp_tolerance <- 1.01-(Orig_dispersal_pollution$IBMWP_score/10)

# This function is very similar to the Dry but with less features (distribution is the same)
#source("Function_to_Pollute.R")
source(file = paste0(getwd(),"/Function_to_Pollute.R"))
filter_Pollution <- function_to_pollute(River_nodes =Poll_nodes_DaFr,
                                        Spp_tolerance =Spp_tolerance ,
                                        extent =Poll_extent,
                                        distribution =Sites_to_Pollut[[Sites_to_Pol_Position]])

# A way to detect polluted sites is by looking at which sites the "filter" does not meet the condition of all 0.99
Pollution <- rep("Non_Poll",nrow(Poll_nodes_DaFr))# replicate "NonPoll" as the row number of nodes
Polluted_Sites <- which(apply(filter_Pollution,2,sum)!=sum(rep(0.99,length(Spp_tolerance))))
Pollution[Polluted_Sites] <- "YES_Poll" #write "YES_Poll" to polluted sites (which were randomly selected above)

Poll_nodes_DaFr <- Poll_nodes_DaFr %>% mutate("Pollution"=Pollution)# merge scenarios and nodes

Poll_DataFrame_out[[diff_extent_value]] <- Poll_nodes_DaFr
Poll_filter_Pollution[[diff_extent_value]] <-list(filter_Pollution,Pollution) 
}

# Checking lengths
length(Dry_DataFrame_out)
length(Flow_DB_toSTcon)
length(Poll_DataFrame_out)

output_to_simulate <- list()
plots_diagnosis <- list()
for (diff_extent_value in 1:(length(Dry_DataFrame_out)*length(Poll_DataFrame_out))) {
  
Merge_diff_extent <- tidyr::crossing(Dry_Ext=unique(diff_extent$Dry_Ext),
                                     Pollut_Ext=unique(diff_extent$Pollut_Ext),
                                     Dry_Pattern_End=unique(diff_extent$Dry_Pattern_End))

Dry_diff_extent <- tidyr::crossing(Dry_Ext=unique(diff_extent$Dry_Ext),Dry_Pattern_End=unique(diff_extent$Dry_Pattern_End))

Sites_to_Dry_Position <- which(unique(diff_extent$Dry_Ext)==as.numeric(Merge_diff_extent[diff_extent_value,1]))
Sites_to_Pattern_Position <- which(unique(diff_extent$Dry_Pattern_End)==as.numeric(Merge_diff_extent[diff_extent_value,3]))
Sites_to_Pollut_Position <- which(unique(diff_extent$Pollut_Ext)==as.numeric(Merge_diff_extent[diff_extent_value,2]))

Loca_Scen_DryPat <- which(duplicated(
  c(which(Dry_diff_extent$Dry_Ext==unique(diff_extent$Dry_Ext)[Sites_to_Dry_Position]),
    which(Dry_diff_extent$Dry_Pattern_End==unique(diff_extent$Dry_Pattern_End)[Sites_to_Pattern_Position]))))

Scen_Position <-   c(which(Dry_diff_extent$Dry_Ext==unique(diff_extent$Dry_Ext)[Sites_to_Dry_Position]),
                     which(Dry_diff_extent$Dry_Pattern_End==unique(diff_extent$Dry_Pattern_End)[Sites_to_Pattern_Position]))[Loca_Scen_DryPat]

cat(Scen_Position,Sites_to_Pollut_Position, "\n")
DataFrame_out_Scen <- left_join(Dry_DataFrame_out[[Scen_Position]],Poll_DataFrame_out[[Sites_to_Pollut_Position]], 
                                by=c("Site_ID","x","y","weight")) %>% 
mutate(Scenario=paste(as.numeric(Merge_diff_extent[diff_extent_value,1]),
                      as.numeric(Merge_diff_extent[diff_extent_value,3]),
                      as.numeric(Merge_diff_extent[diff_extent_value,2]),sep="_"),.before="Site_ID")  

# We set the new filter for the pollution 
filter_Pollution <- Poll_filter_Pollution[[Sites_to_Pollut_Position]][[1]]
Pollution <- Poll_filter_Pollution[[Sites_to_Pollut_Position]][[2]]

# 4. Pollution + Drying  ####
# Rescaling filters according to drying
drying_ranges <- unique(DataFrame_out_Scen$Permanence)[order(unique(DataFrame_out_Scen$Permanence))]
Poll_Dry_Inter <- drying_ranges/max(drying_ranges)
Poll_Dry_Inter <- ifelse(Poll_Dry_Inter==1,0.99,Poll_Dry_Inter)
Poll_Dry_Inter <- ifelse(Poll_Dry_Inter==0,0.0001,Poll_Dry_Inter)
for (dry_range in 1:length(drying_ranges)) {
  if (drying_ranges[dry_range]==0) {
    Modif_Spp_tolerance <- rep(0,length(Spp_tolerance))
  }else{
    Modif_Spp_tolerance <- Spp_tolerance^(drying_ranges[dry_range]/max(DataFrame_out_Scen$Permanence))
  }

  Dry_and_Poll_Spp_tolerance <- scales::rescale(Modif_Spp_tolerance,to=c(min(Spp_tolerance),max(Spp_tolerance)))
  filter_Pollution[,which(Pollution=="YES_Poll" & DataFrame_out_Scen$Permanence==drying_ranges[dry_range])] <- Dry_and_Poll_Spp_tolerance
}
filter_Pollution_test <- t(filter_Pollution)
colnames(filter_Pollution_test) <- paste(seq(1:length(Spp_tolerance)),"Spe",sep="_")

# Final diagnostic plots
Scenario_name <- paste(as.numeric(Merge_diff_extent[diff_extent_value,1]),
      as.numeric(Merge_diff_extent[diff_extent_value,3]),
      as.numeric(Merge_diff_extent[diff_extent_value,2]),sep="_")

# We plot our beloved river colored according to the weight (order)/community size
Plot_A <- ggplot()+
  geom_segment(data=edges_DaFr, 
               aes(x=X1_coord,y=Y1_coord, xend=X2_coord, yend=Y2_coord), 
               arrow =arrow(length=unit(0.01,"cm"), ends="last"), linewidth=0.2, colour="grey50", alpha=1)+
  geom_point(data=nodes_DaFr, aes(x=x, y=y,fill=weight/120000,size=weight/120000), shape=21)+ # here weight/12000 is random decision for plotting
  scale_fill_viridis(option = "D",discrete = F)+
  scale_size(guide = "none") +
  labs(title = paste("River structure", Scenario_name),y="",x="",fill="Comm. Size")+
  theme_void()


# We plot our beloved river colored according to the weight (order)/community size
Alpha_plot <- (edges_DaFr %>% left_join(Dry_DataFrame_out[[Scen_Position]],by=c("from"="Site_ID")) %>% pull(Permanence)/
               (max(edges_DaFr %>% left_join(Dry_DataFrame_out[[Scen_Position]],by=c("from"="Site_ID")) %>% pull(Permanence))*2))+0.5

Plot_B <- edges_DaFr %>% 
  left_join(Dry_DataFrame_out[[Scen_Position]],by=c("from"="Site_ID")) %>% 
  ggplot()+
  geom_curve(aes(x=X1_coord,y=Y1_coord, xend=X2_coord, yend=Y2_coord,colour=Permanence),
                                arrow =arrow(length=unit(0.01,"cm"), ends="last"),
               alpha=Alpha_plot,
               linewidth=1.5, 
               curvature=0.1)+
  geom_point(data=nodes_DaFr,aes(x=x, y=y,size=weight/120000), colour="grey30", shape=16)+
  scale_colour_viridis(option = "D",discrete = F,direction = -1,limits=c(0,48))+
  scale_size(guide = "none")+
  labs(title = paste("Scenario",Scenario_name),y="",x="",fill="Permanence")+
  theme_void()

# We plot our beloved river colored according to the weight (order)/community size
Plot_C <- edges_DaFr %>% left_join(  Poll_DataFrame_out[[Sites_to_Pollut_Position]],by=c("from"="Site_ID")) %>% 
  ggplot()+
  geom_curve(aes(x=X1_coord,y=Y1_coord, xend=X2_coord, yend=Y2_coord,colour=Pollution),
             arrow =arrow(length=unit(0.01,"cm"), ends="last"), 
             linewidth=1.5, 
             alpha=0.5,
             curvature=0.1)+
  geom_point(aes(x=x, y=y,size=weight/120000), colour="grey30", shape=16)+
  scale_color_manual(values = c("blue","red"))+
  scale_size(guide = "none") +
  labs(title = paste("Scenario",Scenario_name),y="",x="",fill="Pollution")+
  theme_void()


plot_D <- gridExtra::arrangeGrob(
  DataFrame_out_Scen %>% 
  bind_cols(as.data.frame(filter_Pollution_test)) %>% 
  pivot_longer(cols=10:ncol(.)) %>% 
  mutate(name=factor(name,levels = paste(seq(1:length(Spp_tolerance)),"Spe",sep="_"))) %>% 
  ggplot()+
  geom_tile(aes(y=as.factor(name),x=Site_ID,fill=value))+
  scale_fill_viridis(direction = -1)+
  theme_classic()+
  labs(fill="Filt")+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right"),
  DataFrame_out_Scen %>% 
  bind_cols(as.data.frame(filter_Pollution_test)) %>% 
  pivot_longer(cols=10:ncol(.)) %>% 
  mutate(name=factor(name,levels = paste(seq(1:200),"Spe",sep="_"))) %>% 
  ggplot()+
  geom_tile(aes(y=as.factor(name),x=Site_ID,fill=Permanence))+
  scale_fill_viridis(option = "A",direction = -1)+
  theme_classic()+
  labs(fill="Perm")+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right"),
top= paste("Scenario",Dry_extent,Poll_extent,sep="_"),ncol=1)

plots_temp <- list(gridExtra::arrangeGrob(Plot_A,Plot_B,Plot_C,ncol=3),plot_D)
plots_diagnosis[[diff_extent_value]] <- plots_temp
output_to_simulate[[diff_extent_value]] <- list(filter_Pollution,DataFrame_out_Scen,Flow_DB_toSTcon[[Scen_Position]])
}# Diff


png(filename = "Scenario.png",width = 6000,height = 6500,units = "px",res =300)
gridExtra::grid.arrange(
plots_diagnosis[[1]][[1]],plots_diagnosis[[6]][[1]],
plots_diagnosis[[12]][[1]],plots_diagnosis[[15]][[1]],
plots_diagnosis[[19]][[1]],plots_diagnosis[[25]][[1]],nrow=6)
dev.off()

png(filename = "Scenario_SppFilt.png",width = 6000,height = 2000,units = "px",res =300)
gridExtra::grid.arrange(
 plots_diagnosis[[1]][[2]],plots_diagnosis[[6]][[2]],
 plots_diagnosis[[12]][[2]],plots_diagnosis[[15]][[2]],
 plots_diagnosis[[19]][[2]],plots_diagnosis[[25]][[2]],ncol=6)
dev.off()

save(plots_diagnosis,file = "Diagnosis_Plots.RData")

# 5. STconmat calculation  ####
# Remember that STcon is able to calculate several rivers at the same time! So you just need to have a list object with the 3 elements 
# for each river scenario: 

# Important to save the image to later plot and use the simulations 
save.image(file="Cluster_Code/PreSTcon.RData")

# We save the objects to send to the SLURM running cluster
Net_stru_RAW <- as.matrix(ocn_TEST$FD$W)

Flow_DB_toSTcon_Sc <- list()
for (diff_extent_value in 1:length(output_to_simulate)) {
Flow_DB_toSTcon_Sc[[diff_extent_value]] <- output_to_simulate[[diff_extent_value]][[3]]
}

Jumps_Per_Pack_Beg <- rep(15,ceiling(length(Flow_DB_toSTcon_Sc)/15)-1)
Jumps_Per_Pack_Beg <- c(0,Jumps_Per_Pack_Beg)
Jumps_Per_Pack_End <- rep(15,ceiling(length(Flow_DB_toSTcon_Sc)/15)-1)
Jumps_Per_Pack_End <- c(0,Jumps_Per_Pack_End)
Beg=1
End=15
for (pack_scenario in 1:ceiling(length(Flow_DB_toSTcon_Sc)/15)) {
Beg=Beg+Jumps_Per_Pack_Beg[pack_scenario]
End=End+Jumps_Per_Pack_End[pack_scenario]
Scenario <- cbind(Beg,End)
Flow_DB_toSTcon <- Flow_DB_toSTcon_Sc[Beg:End]
save(Scenario,Flow_DB_toSTcon,g,Net_stru_RAW,nodes_DaFr,
     file=paste("Cluster_Code/",paste("Sc_",Beg,"_",End, sep = ""),"/SLURM_PreSTcon.RData",sep=""))
}

library(igraph)
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

#some library to be needed
library(shp2graph)
library(doParallel)
library(ggnetwork)
source("https://raw.github.com/Cunillera-Montcusi/Quantifyinig-SpaTem-connectivity/main/SpaTemp_function.R")
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
Net_stru <- Net_stru_RAW
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



# After running in the server we reload the data and prepare it for the model 
# Some fast checkings
length(output_to_simulate)

Flow_DB_toSTcon_Sc <- list()
for (diff_extent_value in 1:length(output_to_simulate)) {
  Flow_DB_toSTcon_Sc[[diff_extent_value]] <- output_to_simulate[[diff_extent_value]][[3]]
}

# After running in the server we reload the data and prepare it for the model 
Jumps_Per_Pack_Beg <- rep(15,ceiling(length(output_to_simulate)/15)-1)
Jumps_Per_Pack_Beg <- c(0,Jumps_Per_Pack_Beg)
Jumps_Per_Pack_End <- rep(15,ceiling(length(output_to_simulate)/15)-1)
Jumps_Per_Pack_End <- c(0,Jumps_Per_Pack_End)
Beg=1; End=15
Riv_Drift_Tot <- list(); Riv_Swim_Tot <- list(); Riv_AerAct_Tot <- list()
for (pack_scenario in 1:ceiling(length(output_to_simulate)/15)) {
  Beg=Beg+Jumps_Per_Pack_Beg[pack_scenario]
  End=End+Jumps_Per_Pack_End[pack_scenario] 
  cat("We are at",Beg ,"and", End, "\n")
  Scenario <- cbind(Beg,End)
  Flow_DB_toSTcon <- Flow_DB_toSTcon_Sc[Beg:End]
  # Charge drifters 
  load(paste("C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/1. FEHM coses al DIA/7. DRY-GUADALMED/SLURM_Model_Dry_Guadalmed/",
             paste("Sc_",Beg,"_",End, sep = ""),"/Riv_Drift_STcon.RData",sep=""))
  Riv_Drift_Tot[Beg:End] <- Riv_Drift$STconmat[1:15]
  # Charge swimmers
  load(paste("C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/1. FEHM coses al DIA/7. DRY-GUADALMED/SLURM_Model_Dry_Guadalmed/",
             paste("Sc_",Beg,"_",End, sep = ""),"/Riv_Swim_STcon.RData",sep=""))
  Riv_Swim_Tot[Beg:End] <- Riv_Swim$STconmat[1:15]
  # Charge flyiers
  load(paste("C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/1. FEHM coses al DIA/7. DRY-GUADALMED/SLURM_Model_Dry_Guadalmed/",
             paste("Sc_",Beg,"_",End, sep = ""),"/Riv_AerAct_STcon.RData",sep=""))
  Riv_AerAct_Tot[Beg:End] <- Riv_AerAct$STconmat[1:15]
}

Riv_Drift_Tot <- Riv_Drift_Tot[1:length(output_to_simulate)]
Riv_Swim_Tot <- Riv_Swim_Tot[1:length(output_to_simulate)]
Riv_AerAct_Tot <- Riv_AerAct_Tot[1:length(output_to_simulate)]


Scen_Drift_STconmat <- list()
for (scen in 1:(length(Riv_Drift_Tot))) {
  Drift_STconmat <-Riv_Drift_Tot[[scen]]#/Riv_Drift$STconmat[[length(Int_dataset)]]
  #to construct a diagonal matrix
  diag(Drift_STconmat) <- 1
  # if Riv_STconmat is 0, write 10000, otherwise write Riv_STconmat
  Scen_Drift_STconmat[[scen]] <-ifelse(Drift_STconmat==0,100,Drift_STconmat)
  #Scen_Drift_STconmat[[scen]] <-ifelse(is.nan(Drift_STconmat)==T,100,Drift_STconmat)
}

Scen_Swim_STconmat <- list()
for (scen in 1:(length(Riv_Swim_Tot))) {
  Swim_STconmat <-Riv_Swim_Tot[[scen]]#/Riv_Swim$STconmat[[length(Int_dataset)]]
  #to construct a diagonal matrix
  diag(Swim_STconmat) <- 1
  # if Riv_STconmat is 0 or NaN, write 10000, otherwise write Riv_STconmat
  Scen_Swim_STconmat[[scen]] <-ifelse(Swim_STconmat==0,100,Swim_STconmat)
  #Scen_Swim_STconmat[[scen]] <-ifelse(is.nan(Swim_STconmat)==T,100,Swim_STconmat)
}

Scen_AAct_STconmat <- list()
for (scen in 1:(length(Riv_AerAct_Tot))) {
  AAct_STconmat <-Riv_AerAct_Tot[[scen]]#/Riv_Swim$STconmat[[length(Int_dataset)]]
  #to construct a diagonal matrix
  diag(AAct_STconmat) <- 1
  # if Riv_STconmat is 0 or NaN, write 10000, otherwise write Riv_STconmat
  Scen_AAct_STconmat[[scen]] <-ifelse(AAct_STconmat==0,100,AAct_STconmat)
  #Scen_Swim_STconmat[[scen]] <-ifelse(is.nan(Swim_STconmat)==T,100,Swim_STconmat)
}


summary(as.vector(Scen_AAct_STconmat[[3]]))
summary(as.vector(Scen_Swim_STconmat[[3]]))
summary(as.vector(Scen_Drift_STconmat[[3]]))

# 6. Coaslescent runs ####
source("H2020_Lattice_expKernel_Jenv_TempMeta_DispStr.R")

###__________________________________
### WE BEGUN TO built all the data that we need to run the simulation
## Distance matrix 
# It is equivalent to the Riv_STconmat 

## Community size
# Area total is the weight. But we must convert it to not take too long computation times 
# We define parameters of Community area to transform weight to community size (J)
Max_Area<-(Years_Of_Drying*12)#*max(nodes_DaFr$weight)
Jmin <- 50 # What will be the minimum size at which we will consider "0" 
J.max<-200+Jmin # What is the maximum J that we want to assign to a community
b.ef<-(0.8) # The coefficient of change. If 1 we do a direct proportion between the two but minimimum becomes "1" (only 1 indiv)

# Species tolerances defined in the 2. script
# Filter of species per site. We will use for tolerance
# Other parameters of the model
id_NOmodule <- rep(1,nrow(nodes_DaFr)) # Modules if we want some sites to belong to the same module. 
pool_200 <- rep(1,nrow(output_to_simulate[[1]][[1]])) # Distribution of the species pool #rlnorm(n = 200,5,1) 
Meta_t0 <- matrix(nrow = length(pool_200), ncol =nrow(nodes_DaFr), 1) #Previous Metacommunity (for considering time relevance)

# Distances are related to the distance matrix. 
# Therefore we must see which values correspond to our connections to set the "D50". Dispersal distance is
# considered here as the distance at which probability of dispersal is 0.5. So "higher" or "lower" do not exclude
# other dispersal abilities. They push "overall connectivity" towards higher or lower connections.
#summary(as.vector(Scen_Swim_STconmat[[pollut]])[-which(as.vector(Scen_Swim_STconmat[[pollut]])==100)]) 

dispersal_test <- c(0.15) # We set the dispersal abilities that we want

Disp_Str <- Orig_dispersal_pollution%>% 
  mutate(Disp_Strateg=case_when(
    str_detect(dispersal_strategy ,"dis4") ~ "3", 
    str_detect(dispersal_strategy,"dis1") ~ "1",
    str_detect(dispersal_strategy,"dis2") ~ "2",
    TRUE ~ "MISTAKE")) %>% 
    mutate(Disp_Strateg=as.numeric(Disp_Strateg)) %>% pull(Disp_Strateg)


library(tictoc)
tic() # This is to count the time 
library(doParallel) # We activate the parallelization
registerDoParallel(cores = detectCores()-1) # We keep 1 core to be able to do something else while running
Diff_scenarios <- foreach(dispersal=1:length(dispersal_test), .combine=rbind)%:% # Parallelize for dispersal
                  foreach(pollut=1:length(output_to_simulate), .combine=rbind)%dopar%{ # Parallellize for pollution

# J creation
FW_area <- output_to_simulate[[pollut]][[2]]$Permanence#*output_to_simulate[[pollut]][[2]]$weight
J.freshwater<-ceiling((-Jmin+(J.max/(Max_Area^b.ef))*FW_area^b.ef))
plot(FW_area,J.freshwater)
J.freshwater <- ifelse(J.freshwater<0,20,J.freshwater)
                    
a <- NULL # We create an output object for each iteration
#b <- list()
for (it in 1:10) { # We repeat 10 times the same process
  output <- H2020_Coalescent.and.lottery.exp.Kernel.J_TempMtcom_tempIT(
    Meta.pool = pool_200, # Species pool
    m.pool = 0.001, # Regional dispersal which is always constant 
    Js = J.freshwater, # Size of the communities (AKA: number of individuals/population contained in each community)
    id.module = id_NOmodule, # id of modules if there are some - NOT used for us
    filter.env = output_to_simulate[[pollut]][[1]], # Pollution scenarios (created at 2. Pollution assignation.R)
    Disp_Strat=Disp_Str,
    Tollerances=Spp_tolerance,
    M.dist =list(Scen_Drift_STconmat[[pollut]],
                 Scen_Swim_STconmat[[pollut]],
                 Scen_AAct_STconmat[[pollut]]), # Distance matrix which corresponds to the STconmat (created at 1. OCnet - STconmat.R)
    D50 = dispersal_test[dispersal], # Dispersal distance scenario 
    m.max = 1, # Maximum migration
    tempo_imp = 0, # Relvance of "temporal" effect
    temp_Metacom = Meta_t0, # Metacommunity at time 0 (all species are equally favored)
    temp_it = 0, # Number of temporal iterations
    id.fixed=NULL, D50.fixed=0, m.max.fixed=0, comm.fixed=pool_200, # If there are some communit. that should be fixed
    Lottery=F, 
    it=100, 
    prop.dead.by.it=0.07, # Lottery parameters, nº iterations and proportion of dead organisms  
    id.obs=1:nrow(nodes_DaFr)) # Information if we would like to keep specific results only
  a <- rbind(a,output[[1]])
  #b[[it]] <- output[[2]]
}
resume.out(a)
}
toc()
save(Diff_scenarios,file = "All_Scenarios.RData")
save(output_to_simulate, file="All_Scenarios_OutputToSimul.RData")
# 13593.65 

# > <
# This part below extracts the S (richness) and B (average Jaccard) from each site and "data frame" it 
Res_nodes_DaFr <- data.frame()
leng_disp <- length(dispersal_test)
Leng_scenarios <- length(output_to_simulate)

for (round in 1:(Leng_scenarios*leng_disp)) {
  round_value <- round
  if (round>Leng_scenarios & round<=(Leng_scenarios*2)) {round_value <- round_value-Leng_scenarios}
  if (round>(Leng_scenarios*2) & round<=(Leng_scenarios*3)) {round_value <- round_value-(Leng_scenarios*2)}
  if (round>(Leng_scenarios*3) & round<=(Leng_scenarios*4)) {round_value <- round_value-(Leng_scenarios*3)}
  if (round>(Leng_scenarios*4) & round<=(Leng_scenarios*5)) {round_value <- round_value-(Leng_scenarios*4)}
  if (round>(Leng_scenarios*5) & round<=(Leng_scenarios*6)) {round_value <- round_value-(Leng_scenarios*5)}
  if (round>(Leng_scenarios*6) & round<=(Leng_scenarios*7)) {round_value <- round_value-(Leng_scenarios*6)}

  n_cols_to_calculate <- (nrow(output_to_simulate[[round_value]][[2]]))
  
  S_site<- Diff_scenarios[[round]][10:(n_cols_to_calculate+9)]
  B_site <- Diff_scenarios[[round]][(n_cols_to_calculate+10):((n_cols_to_calculate*2)+9)]

  S_Drift <- Diff_scenarios[[round]][((n_cols_to_calculate*2)+10):((n_cols_to_calculate*3)+9)]
  S_Swim <- Diff_scenarios[[round]][((n_cols_to_calculate*3)+10):((n_cols_to_calculate*4)+9)]
  S_AAct <- Diff_scenarios[[round]][((n_cols_to_calculate*4)+10):((n_cols_to_calculate*5)+9)]
  
  S_sen <- Diff_scenarios[[round]][((n_cols_to_calculate*5)+10):((n_cols_to_calculate*6)+9)]
  S_tol <- Diff_scenarios[[round]][((n_cols_to_calculate*6)+10):((n_cols_to_calculate*7)+9)]
  
  Mean_IBMWP <- Diff_scenarios[[round]][((n_cols_to_calculate*7)+10):((n_cols_to_calculate*8)+9)]
  IBMWP <- Diff_scenarios[[round]][((n_cols_to_calculate*8)+10):((n_cols_to_calculate*9)+9)]
  
  
  STcon_Drift <- apply(ifelse(Scen_Drift_STconmat[[round_value]]==100,NA,Scen_Drift_STconmat[[round_value]]),1,mean, na.rm=T)
  STcon_Swim <- apply(ifelse(Scen_Swim_STconmat[[round_value]]==100,NA,Scen_Swim_STconmat[[round_value]]),1,mean, na.rm=T)
  STcon_AAct <- apply(ifelse(Scen_AAct_STconmat[[round_value]]==100,NA,Scen_AAct_STconmat[[round_value]]),1,mean, na.rm=T)
  
  STcon_Drift[which(is.nan(STcon_Drift))] <- 1
  Result_df <- bind_cols(output_to_simulate[[round_value]][[2]] %>%
                         data.frame("Dry_ext"= strsplit(unique(output_to_simulate[[round_value]][[2]]$Scenario),split = "_")[[1]][1],
                                    "Dry_patt"=strsplit(unique(output_to_simulate[[round_value]][[2]]$Scenario),split = "_")[[1]][2],
                                    "Pollut_ext"=strsplit(unique(output_to_simulate[[round_value]][[2]]$Scenario),split = "_")[[1]][3],
                                    "Disp"=as.numeric(Diff_scenarios[[round]][4]),
                                    "STcon_Drift"=STcon_Drift,
                                    "STcon_Swim"=STcon_Swim,
                                    "STcon_AAct"=STcon_AAct,
                                    "Mean_STcon"=apply(cbind(STcon_Drift,STcon_Swim,STcon_AAct),1,mean),
                                    "Ratio_S.T"=S_sen/S_tol,
                                    "S_Tol"=S_tol,
                                    "S_Sen"=S_sen,
                                    "S_Drift"=S_Drift,"S_Swim"=S_Swim,"S_AAct"=S_AAct,
                                    "Mean_IBMWP"=Mean_IBMWP,
                                    "IBMWP"=IBMWP,
                                    "S"=S_site,"B"=B_site))
  Res_nodes_DaFr <- bind_rows(Res_nodes_DaFr,Result_df)
}
save(Res_nodes_DaFr,file="All_Results_scenarios.RData")


# 6. Plots and results outputs ####
library(tidyverse);library(viridis)
load("All_Results_scenarios.RData")

# New plots and information that we produced after the meetings with NúriaS :) 
colnames(Res_nodes_DaFr)
Data_To_Plot <- Res_nodes_DaFr %>%
  mutate(Pollution = case_when(
             str_detect(Pollution ,"YES_Poll") ~ "Polluted",
             str_detect(Pollution ,"Non_Poll") ~ "Unpolluted")) %>%
  mutate(Pollution=factor(Pollution,levels=c("Unpolluted","Polluted"))) %>% 
  filter(Disp==0.15) %>% 
  mutate(Dry_ext=as.numeric(Dry_ext),Dry_patt=as.numeric(Dry_patt),
         Pollut_ext=as.numeric(Pollut_ext)) %>% 
  group_by(Dry_ext, Dry_patt, Pollut_ext) %>% 
  mutate(Max_STcon=max(Mean_STcon)) %>% 
  mutate(Sc_STcon=Mean_STcon/Max_STcon)

colnames(Data_To_Plot) 
png(filename = "Figure_3.png",width = 2300,height = 2500,units = "px",res =300)
Data_To_Plot %>% 
  filter(Pollut_ext%in%c(0.01,0.5,0.75), 
         Dry_ext%in%c(0.25,0.5,0.75)) %>% 
  ggplot(aes(x=Sc_STcon, y=IBMWP,
             colour=as.factor(1-Dry_patt),fill=as.factor(1-Dry_patt)))+
  geom_point(alpha=0.1,shape=16,size=0.8)+
  geom_smooth(method="lm",se=F)+
  scale_color_viridis(option="E",discrete = T)+
  scale_fill_viridis(option="E",discrete = T)+
  labs(color="Drying intensity",fill="Drying intensity",x="Dispersal resistance",y="IBMWP")+
  facet_wrap(Pollut_ext~Dry_ext, ncol=3,strip.position = "right",axis.labels = "all",axes = "all")+
  theme_classic()+
  theme(strip.background = element_blank(),strip.text.x = element_blank(),
        legend.position = "bottom")
dev.off()

Poll_plot <- Data_To_Plot %>% 
  filter(Pollut_ext%in%c(0.01,0.5,0.9), 
         Dry_ext%in%c(0.25,0.5,0.9)) %>%
  filter(Pollution=="Polluted")

UnPoll_plot <- Data_To_Plot %>% 
  filter(Pollut_ext%in%c(0.01,0.5,0.9), 
         Dry_ext%in%c(0.25,0.5,0.9)) %>%
  filter(Pollution=="Unpolluted")

colnames(Data_To_Plot) 
#png(filename = "Big_test_POll.png",width = 3200,height = 2500,units = "px",res =300)
  ggplot()+
  geom_point(data=Poll_plot,aes(x=Sc_STcon, y=IBMWP,fill=Pollution),fill="red",alpha=0.2,shape=21,size=0.8)+
  geom_point(data=UnPoll_plot,aes(x=Sc_STcon, y=IBMWP,fill=Pollution),fill="green",alpha=0.2,shape=21,size=0.8)+
  
  geom_smooth(data=Poll_plot, aes(x=Sc_STcon,y=IBMWP,colour=as.factor(1-Dry_patt),group=Dry_patt),
              method="lm",se=F,alpha=0.1,linewidth=1,linetype=1)+
  scale_colour_brewer(palette = "Reds")+  
  labs(colour="Polluted Dry. Int.")+
  ggnewscale::new_scale_colour()+
  geom_smooth(data=UnPoll_plot, aes(x=Sc_STcon,y=IBMWP,colour=as.factor(1-Dry_patt),group=Dry_patt),
              method="lm",se=F,alpha=0.1,linewidth=1,linetype=1)+
  scale_colour_brewer(palette = "Greens")+  

  labs(color="Unpolluted Dry. Int.",fill="Impact",
       x="Dispersal resistance",y="IBMWP")+
  facet_wrap(Pollut_ext~Dry_ext, ncol=3,strip.position = "right",axis.labels = "all",axes = "all")+
  theme(legend.position = "bottom")+
  theme_classic()+
  theme(panel.background = element_rect(fill="grey80"))
#dev.off()


Ref_Performance <- Data_To_Plot %>% 
  group_by(Dry_ext, Dry_patt, Pollut_ext,Pollution) %>% 
  summarise(M_IBMWP=mean(IBMWP)) %>% 
  pivot_wider(names_from = Pollution,values_from = c(M_IBMWP)) %>%
  mutate(Performance=Unpolluted-Polluted) %>%
  filter(Dry_ext==0.01,Dry_patt==0.9,Pollut_ext==0.01) %>%
  ungroup() %>% select(Pollut_ext,Performance) %>% 
  rename(Ref_Performance=Performance)
  
Data_To_Plot %>% 
  group_by(Dry_ext, Dry_patt, Pollut_ext,Pollution) %>% 
  summarise(M_IBMWP=mean(IBMWP)) %>% 
  pivot_wider(names_from = Pollution,values_from = c(M_IBMWP)) %>%
  mutate(Performance=Unpolluted-Polluted) %>% 
  #left_join(Ref_Performance, by="Pollut_ext") %>%
  mutate(Ref_Performance=as.numeric(Ref_Performance[,2])) %>% 
  mutate(Perf_In_Perc=(Performance/Ref_Performance)*100) %>% 
  #filter(Pollut_ext%in%c(0.01,0.25,0.5,0.9)) %>% 
  ggplot()+
  #geom_hline(yintercept = c(-0,-10,-20,-30),colour="grey80")+
  geom_jitter(aes(x=Dry_ext,y=Perf_In_Perc,colour=as.factor(1-Dry_patt)), width = 0.02,height = 0,size=2,alpha=0.6)+
  geom_smooth(aes(x=Dry_ext,y=Perf_In_Perc,colour=as.factor(1-Dry_patt),group=Dry_patt),se=F,method="lm")+
  scale_color_viridis(option="E",discrete = T)+
  labs(colour="Dryint intensity", y="Performance (Diff. between Imp. and Non.Imp)")+
  scale_y_continuous(breaks = c(0,25,50,75,100))+
  facet_wrap(Pollut_ext~., ncol=4,strip.position = "right",axis.labels = "all",axes = "all")+
  theme_classic()+
  theme(legend.position = "bottom")




Data_To_Plot %>% 
  filter(Pollut_ext%in%c(0.01), 
         Dry_ext%in%c(0.5)) %>% 
  ggplot(aes(x=Sc_STcon, y=S,
             colour=as.factor(1-Dry_patt),fill=as.factor(1-Dry_patt)))+
  geom_point(alpha=0.2,shape=16,size=0.8)+
  geom_smooth(method="lm",se=F)+
  scale_color_viridis(option="E",discrete = T)+
  scale_fill_viridis(option="E",discrete = T)+
  labs(color="Drying intensity",fill="Drying intensity",
       x="Dispersal resistance",y="Species richnness")+
  facet_wrap(Pollut_ext~Dry_ext, ncol=3,strip.position = "right",axis.labels = "all",axes = "all")+
  theme(legend.position = "bottom")+
  theme_classic()

# WARNING!!!! 
# Very very very slow loop! 
Out_Coef <-data.frame() 
for (Pollut_extent in 1:length(unique(Data_To_Plot$Pollut_ext))) {
for (D_extent in 1:length(unique(Data_To_Plot$Dry_ext))) {
for (D_Intensity in 1:length(unique(Data_To_Plot$Dry_patt))) {
Model_Data <- Data_To_Plot %>% 
  filter(Pollut_ext==unique(Data_To_Plot$Pollut_ext)[Pollut_extent],
         Dry_ext==unique(Data_To_Plot$Dry_ext)[D_extent]) %>%
  filter(Dry_patt==unique(Data_To_Plot$Dry_patt)[D_Intensity]) %>% 
  select(Dry_patt,S,Sc_STcon) 

Model_Data_Un <- Data_To_Plot %>%
  filter(Pollution=="Unpolluted") %>% 
  filter(Pollut_ext==unique(Data_To_Plot$Pollut_ext)[Pollut_extent],
         Dry_ext==unique(Data_To_Plot$Dry_ext)[D_extent]) %>%
  filter(Dry_patt==unique(Data_To_Plot$Dry_patt)[D_Intensity]) %>% 
  select(Dry_patt,S,Sc_STcon) 

Model_Data_Po <- Data_To_Plot %>%
  filter(Pollution=="Polluted") %>% 
  filter(Pollut_ext==unique(Data_To_Plot$Pollut_ext)[Pollut_extent],
         Dry_ext==unique(Data_To_Plot$Dry_ext)[D_extent]) %>%
  filter(Dry_patt==unique(Data_To_Plot$Dry_patt)[D_Intensity]) %>% 
  select(Dry_patt,S,Sc_STcon) 

Out_Coef <- bind_rows(
            Out_Coef,
            data.frame("Poll.ext"=unique(Data_To_Plot$Pollut_ext)[Pollut_extent],
                        "Dry_ext"=unique(Data_To_Plot$Dry_ext)[D_extent],
                       "Dry_patt"=unique(Data_To_Plot$Dry_patt)[D_Intensity],
                       "Intercept"=coefficients(lm(S~Sc_STcon,data = Model_Data))[1],
                       "Slope"=coefficients(lm(S~Sc_STcon,data = Model_Data))[2],
                       "Un_Intercept"=coefficients(lm(S~Sc_STcon,data = Model_Data_Un))[1],
                       "Un_Slope"=coefficients(lm(S~Sc_STcon,data = Model_Data_Un))[2],
                       "Po_Intercept"=coefficients(lm(S~Sc_STcon,data = Model_Data_Po))[1],
                       "Po_Slope"=coefficients(lm(S~Sc_STcon,data = Model_Data_Po))[2]))
}# D_Intensity
}# D_Ext
}# Pollut ext


#png(filename = "Relation_Interaction.png",width = 4500,height = 3000,units = "px",res =300)
gridExtra::grid.arrange(

Out_Coef %>% 
  filter(Poll.ext!=0.01) %>%
  filter(Dry_patt!=0.9) %>% 
  #drop_na() %>% 
  #mutate(Combi_Fact=paste(Dry_patt,Poll.ext)) %>% 
  ggplot()+
  geom_smooth(aes(x=Dry_ext,y=Intercept,colour=as.factor(1-Dry_patt),group=Dry_patt),method="lm",se=F,alpha=0.1,linewidth=2,linetype=1)+
  #scale_colour_brewer(palette = "Greens")+
  geom_jitter(aes(x=Dry_ext,y=Intercept,fill=as.factor(1-Dry_patt)),shape=21,size=3,width = 0.01,alpha=0.6)+
  scale_color_viridis(option="E",discrete = T)+
  scale_fill_viridis(option="E",discrete = T)+
  labs(colour="Drying Intensity",fill="Drying Intensity",x="Drying extent",y="Intercept between Richness & Connectivity")+
  #scale_fill_gradient2(low = "white",mid = "grey60",high = "darkred",midpoint=0.5)+
  theme_classic(),

Out_Coef %>% 
  filter(Poll.ext!=0.01) %>%
  filter(Dry_patt!=0.9) %>% 
  #drop_na() %>% 
  #mutate(Combi_Fact=paste(Dry_patt,Poll.ext)) %>% 
  ggplot()+
  geom_hline(yintercept = 0,linewidth=2)+
  geom_smooth(aes(x=Dry_ext,y=Slope,colour=as.factor(1-Dry_patt),group=Dry_patt),method="lm",se=F,alpha=0.1,linewidth=2,linetype=1)+
  #scale_colour_brewer(palette = "Greens")+
  geom_jitter(aes(x=Dry_ext,y=Slope,fill=as.factor(1-Dry_patt)),shape=21,size=3,width = 0.01,alpha=0.6)+
  scale_color_viridis(option="E",discrete = T)+
  scale_fill_viridis(option="E",discrete = T)+
  labs(colour="Drying Intensity",fill="Drying Intensity",x="Drying extent",y="Intercept between Richness & Connectivity")+
  #scale_fill_gradient2(low = "white",mid = "grey60",high = "darkred",midpoint=0.5)+
  theme_classic(),

Out_Coef %>% 
  filter(Poll.ext!=0.01) %>%
  filter(Dry_patt!=0.9) %>% 
  #drop_na() %>% 
  #mutate(Combi_Fact=paste(Dry_patt,Poll.ext)) %>% 
  ggplot()+
  geom_smooth(aes(x=Dry_ext,y=Po_Intercept,colour=as.factor(1-Dry_patt),group=Dry_patt),method="lm",se=F,alpha=0.1,linewidth=2,linetype=1)+
  scale_colour_brewer(palette = "Reds")+
  labs(colour="Polluted Dry. Int.")+
  ggnewscale::new_scale_colour()+
  geom_smooth(aes(x=Dry_ext,y=Un_Intercept,colour=as.factor(1-Dry_patt),group=Dry_patt),method="lm",se=F,alpha=0.1,linewidth=2,linetype=1)+
  scale_colour_brewer(palette = "Greens")+
  geom_jitter(aes(x=Dry_ext,y=Un_Intercept),fill="green",shape=21,size=3,width = 0.01,alpha=0.6)+
  geom_jitter(aes(x=Dry_ext,y=Po_Intercept),fill="red",shape=21,size=3,width = 0.01,alpha=0.6)+
  #scale_color_viridis(option="E",discrete = T)+
  labs(colour="Unpolluted Dry. Int.",fill="Anthropogenic Impact",x="Drying extent",y="Slope between Richness & Connectivity")+
  #scale_fill_gradient2(low = "white",mid = "grey60",high = "darkred",midpoint=0.5)+
  theme_classic()+
  theme(panel.background = element_rect(fill="grey80")),


Out_Coef %>% 
  filter(Poll.ext!=0.01) %>% 
  filter(Dry_patt!=0.9) %>% 
  #drop_na() %>% 
  #mutate(Combi_Fact=paste(Dry_patt,Poll.ext)) %>% 
  ggplot()+
  geom_hline(yintercept = 0,linewidth=2)+
  geom_smooth(aes(x=Dry_ext,y=Po_Slope,colour=as.factor(1-Dry_patt),group=Dry_patt),method="lm",se=F,alpha=0.1,linewidth=2,linetype=1)+
  scale_colour_brewer(palette = "Reds")+
  labs(colour="Polluted Dry. Int.")+
  ggnewscale::new_scale_colour()+
  geom_smooth(aes(x=Dry_ext,y=Un_Slope,colour=as.factor(1-Dry_patt),group=Dry_patt),method="lm",se=F,alpha=0.1,linewidth=2,linetype=1)+
  scale_colour_brewer(palette = "Greens")+
  geom_jitter(aes(x=Dry_ext,y=Un_Slope),fill="green",shape=21,size=3,width = 0.01,alpha=0.6)+
  geom_jitter(aes(x=Dry_ext,y=Po_Slope),fill="red",shape=21,size=3,width = 0.01,alpha=0.6)+
  #scale_color_viridis(option="E",discrete = T)+
  labs(colour="Unpolluted Dry. Int.",fill="Anthropogenic Impact",x="Drying extent",y="Slope between Richness & Connectivity")+
  #scale_fill_gradient2(low = "white",mid = "grey60",high = "darkred",midpoint=0.5)+
  theme_classic()+
  theme(panel.background = element_rect(fill="grey80")),
ncol=2)
#dev.off()

png(filename = "Relation_Interaction.png",width = 3500,height = 2500,units = "px",res =300)
gridExtra::grid.arrange(
Out_Coef %>% 
  mutate(Combi_Fact=paste(Dry_patt,Poll.ext)) %>% 
  ggplot(aes(x=Dry_ext,y=Intercept))+
  geom_smooth(aes(colour=as.factor(1-Dry_patt),group=Dry_patt),method="lm",se=F,
              alpha=0.1,linewidth=2,linetype=2)+
  geom_jitter(aes(fill=Poll.ext),shape=21,size=3,width = 0.01,alpha=0.6)+
  scale_color_viridis(option="E",discrete = T)+
  labs(colour="Drying Intensity",fill="Anthropogenic Impact",
       x="Drying extent",y="Intercept between Richness & Connectivity")+
  scale_fill_gradient2(low = "white",mid = "grey60",high = "darkred",midpoint=0.5)+
  theme_classic()+theme(legend.position = "none"),
Out_Coef %>% 
  mutate(Combi_Fact=paste(Dry_patt,Poll.ext)) %>% 
  ggplot(aes(x=Dry_ext,y=Slope))+
  geom_hline(yintercept = 0,linewidth=2)+
  geom_smooth(aes(colour=as.factor(1-Dry_patt),group=Dry_patt),method="lm",se=F,
              alpha=0.1,linewidth=2,linetype=2)+
  geom_jitter(aes(fill=Poll.ext),shape=21,size=3,width = 0.01,alpha=0.6)+
  scale_color_viridis(option="E",discrete = T)+
  labs(colour="Drying Intensity",fill="Anthropogenic Impact",
       x="Drying extent",y="Slope between Richness & Connectivity")+
  scale_fill_gradient2(low = "white",mid = "grey60",high = "darkred",midpoint=0.5)+
  theme_classic(),

#Out_Coef %>% 
#  mutate(Combi_Fact=paste(Dry_patt,Poll.ext)) %>% 
#  ggplot(aes(x=Dry_ext,y=Intercept))+
#  geom_smooth(aes(colour=as.factor(1-Dry_patt),group=Dry_patt),method="lm",se=F,
#              alpha=0.1,linewidth=1,linetype=2)+
#  geom_point(aes(fill=Poll.ext),shape=21,size=3)+
#  scale_color_viridis(option="E",discrete = T)+
#  labs(colour="Drying Intensity",fill="Anthropogenic Impact",
#       x="Drying extent",y="Intercept between Richness & Connectivity")+
#  scale_fill_gradient2(low = "white",mid = "grey60",high = "darkred",midpoint=0.5)+
#  theme_classic()+theme(legend.position = "none"),
#
#Out_Coef %>% 
#  mutate(Combi_Fact=paste(Dry_patt,Poll.ext)) %>% 
#  ggplot(aes(x=Dry_ext,y=Slope))+
#  geom_hline(yintercept = 0,linewidth=2)+
#  geom_smooth(aes(colour=as.factor(1-Dry_patt),group=Dry_patt),method="lm",se=F,
#              alpha=0.1,linewidth=1,linetype=2)+
#  geom_point(aes(fill=Poll.ext),shape=21,size=3)+
#  scale_color_viridis(option="E",discrete = T)+
#  labs(colour="Drying Intensity",fill="Anthropogenic Impact",
#       x="Drying extent",y="Slope between Richness & Connectivity")+
#  scale_fill_gradient2(low = "white",mid = "grey60",high = "darkred",midpoint=0.5)+
#  guides(colour="none")+
#  theme_classic(),

ncol=2)
dev.off()



# End of the previous plots old plots follow from below





## All raster plot with all the reference
# Big raster plot to draw the effect of 
Big_Ref <- Data_To_Plot %>% dplyr::select(Scenario,Site_ID,Pollution,Dry_ext, Pollut_ext, Dry_patt,Mean_STcon,
                                   "Ref_IBMWP"=IBMWP) %>% 
                            filter(Pollut_ext==0.01)

Data_To_Plot %>% 
  select(Scenario, Site_ID,Pollution,Dry_ext, Pollut_ext, Dry_patt,Mean_STcon ,IBMWP) %>% 
  left_join(Big_Ref, by=c("Site_ID","Dry_ext","Dry_patt","Mean_STcon"),
            relationship = "many-to-many") %>% 
  mutate(Med_Diff=IBMWP-Ref_IBMWP) %>% 
  #group_by(Pollut_ext.x,Dry_ext,Dry_patt) %>% 
  #mutate(Med_Diff=((IBMWP  -mean(Ref_IBMWP))/sd(Ref_IBMWP))) %>% 
  mutate(Dry_ext = plyr::round_any(Dry_ext, 0.15), 
         Dry_patt = plyr::round_any(Dry_patt, 0.25)) %>% 
  group_by(Pollution.x,Dry_ext, Pollut_ext.x, Dry_patt) %>% 
  summarise(Med_Diff=mean(Med_Diff)) %>% #Mean_Diff=mean(Diff),Med_Diff=median(Diff),sd_Diff=sd(Diff)) %>% 
  #pivot_longer(6) %>% 
  ggplot()+
  geom_raster(aes(x=Dry_ext,y=Dry_patt,fill=Med_Diff),interpolate = T)+
  geom_contour(aes(x=Dry_ext,y=Dry_patt,z=Med_Diff),colour="black",
               breaks = c(0,-25,-50,75,-100,-125,-150))+
  #scale_fill_viridis(direction=-1)+
  #scale_fill_gradient2(low = viridis(2,option = "H")[2],high=viridis(2,option = "H")[1],midpoint = 0)+
  scale_fill_gradient2(low = viridis(2,option = "H")[2],high=viridis(2,option = "H")[1],midpoint = -40)+
  facet_wrap(Pollut_ext.x~Pollution.x,ncol=2,strip.position = "right")+
  theme(legend.position = "none",
    plot.background = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    #axis.text = element_blank(),
    panel.grid = element_blank(),
    strip.background=element_blank()
  )


Data_To_Plot %>% 
  select(Scenario, Site_ID,Pollution,Dry_ext, Pollut_ext, Dry_patt,Mean_STcon ,IBMWP) %>% 
  select(Scenario, Site_ID,Pollution,Dry_ext, Pollut_ext, Dry_patt,Mean_STcon ,IBMWP) %>% 
  left_join(Big_Ref, by=c("Site_ID","Dry_ext","Dry_patt","Mean_STcon"),
            relationship = "many-to-many") %>% 
  mutate(Med_Diff=IBMWP-Ref_IBMWP) %>%
  mutate(State=ifelse(Med_Diff >= quantile(Med_Diff)[3] ,"G",
               ifelse(Med_Diff < quantile(Med_Diff)[3] & Med_Diff >= quantile(Med_Diff)[2] ,"M",
               ifelse(Med_Diff < quantile(Med_Diff)[2] , "B", "ERROR")))) %>% 
  mutate(State=factor(State,levels=c("G","M","B"))) %>% 
  ggplot(aes(x=Dry_ext,y=IBMWP,fill=State))+
  geom_col(position="fill")+
  scale_fill_manual(values = c("green","gold","red"))+
  facet_wrap(Pollut_ext.x~Pollution.x,ncol=2,strip.position = "right")+
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        strip.background=element_blank()
  )


States_Count <- Data_To_Plot %>% 
  select(Scenario, Site_ID,Pollution,Dry_ext, Pollut_ext, Dry_patt,Mean_STcon ,IBMWP) %>% 
  select(Scenario, Site_ID,Pollution,Dry_ext, Pollut_ext, Dry_patt,Mean_STcon ,IBMWP) %>% 
  left_join(Big_Ref, by=c("Site_ID","Dry_ext","Dry_patt","Mean_STcon"),
            relationship = "many-to-many") %>% 
  mutate(Med_Diff=IBMWP-Ref_IBMWP) %>%
  mutate(State=ifelse(Med_Diff >= quantile(Med_Diff)[3] ,"G",
               ifelse(Med_Diff < quantile(Med_Diff)[3] & Med_Diff >= quantile(Med_Diff)[2] ,"M",
               ifelse(Med_Diff < quantile(Med_Diff)[2] , "B", "ERROR")))) %>% 
  group_by(Pollut_ext.x,Dry_ext,Dry_patt,Pollution.x) %>% 
  summarise(Low_Change=length(which(State=="G")),
            Mid_Change=length(which(State=="M")),
            Large_Change=length(which(State=="B"))) %>% 
  pivot_longer(5:ncol(.)) %>% 
  group_by(Pollut_ext.x,Dry_ext,Dry_patt,Pollution.x) %>% mutate(Total=sum(value)) %>% 
  mutate(value=(value/Total)*100)

States_Count %>% 
  group_by(Dry_ext,Pollution.x,name) %>% 
  filter(Pollut_ext.x!=0.01) %>% 
  mutate(Average=mean(value),SD=sd(value)) %>% 
  mutate(name=factor(name,levels=c("Low_Change","Mid_Change","Large_Change"))) %>% 
  #filter(Pollution.x=="Polluted") %>% 
  mutate(Dry_ext=Dry_ext*100,
         Pollut_ext.x=Pollut_ext.x*100) %>% 
  ggplot()+
  geom_hline(yintercept = 0,linewidth=1)+
  geom_hline(yintercept = c(25,50,75,100),linewidth=0.2,alpha=0.5)+
  geom_jitter(aes(x=Dry_ext,y=value,colour=Pollut_ext.x),shape=16,size=1,alpha=0.4)+
  geom_smooth(aes(x=Dry_ext,y=value,group=Pollut_ext.x,colour=Pollut_ext.x),se=F,alpha=0.2,linewidth=0.5)+
  geom_errorbar(aes(x=Dry_ext,y=Average,ymax=Average+SD,ymin=Average-SD),linewidth=1)+
  geom_point(aes(x=Dry_ext,y=Average,fill=name),shape=21,size=5,stroke=1)+
  #scale_y_continuous(limits = c(-12,100))+
  scale_fill_manual(values = c("green","gold","red"))+
  scale_colour_viridis(direction=1,option = "E")+
  theme_classic()+
  theme(legend.direction = "horizontal",legend.position = "bottom")+
  labs(colour="Pollution extent (%)",fill="Assigned groups (%)",
       x="Drying extent (%)",y="Nº of sites (%)")+
  facet_wrap(Pollution.x~name,ncol=3)


# For Drying extent

grid_Output <- data.frame()
Percentage_Output <- data.frame()
for (Poll_levels in 1:length(unique(Data_To_Plot$Pollut_ext))) {
  for (Poll_type in 1:2) {
  model <- lm(value~Dry_ext*name, data = States_Count %>% 
                filter(Pollut_ext.x==unique(Data_To_Plot$Pollut_ext)[Poll_levels] &
                       Pollution.x==unique(Data_To_Plot$Pollution)[Poll_type]) %>% 
                mutate(Dry_ext=Dry_ext*100, Dry_patt=Dry_patt*100))
  
  #Ls_mean <- multcomp::cld(lsmeans::lsmeans(model,~Dry_ext:name),alpha= 0.05,Letters = letters,adjust = "sidak")
  
  Dry_ext_seq <- seq(1, 90, by = 1)
  Dry_patt_seq <- seq(1, 90, by = 1)
  grid <- bind_rows(data.frame(expand.grid(Dry_ext = Dry_ext_seq, Dry_patt = Dry_patt_seq),name="Low_Change"),
                    data.frame(expand.grid(Dry_ext = Dry_ext_seq, Dry_patt = Dry_patt_seq),name="Mid_Change"),
                    data.frame(expand.grid(Dry_ext = Dry_ext_seq, Dry_patt = Dry_patt_seq),name="Large_Change"))
  grid$State_Count <- predict(model, newdata = grid)
  grid$Pollution <- unique(Data_To_Plot$Pollution)[Poll_type]
  grid$Pollut_ext <- unique(Data_To_Plot$Pollut_ext)[Poll_levels]
  grid_Output <- bind_rows(grid_Output,grid)
  
  outPercvalues <- data.frame("Pollution"=unique(Data_To_Plot$Pollution)[Poll_type],
                              "Pollut_ext"= unique(Data_To_Plot$Pollut_ext)[Poll_levels],
                              data.frame(emmeans::emtrends(model, specs = "name", var = "Dry_ext"))
                              )
  
  Percentage_Output <- bind_rows(Percentage_Output,outPercvalues)
  
  }
}

# # Function to compute hull indices for a group
# # get_ternary_hull <- function(df) {
#   # Normalize just in case
#   comp <- as.matrix(df[, c("Dry_ext", "Pollut_ext", "State_Count")])
#   comp <- comp / rowSums(comp)
#   
#   # Project to 2D ternary coords
#   x <- 0.5 * (2 * comp[, "Pollut_ext"] + comp[, "State_Count"])
#   y <- sqrt(3)/2 * comp[, "State_Count"]
#   coords <- cbind(x, y)
#   
#   # Convex hull on 2D coords
#   hull_idx <- chull(coords)
#   
#   # Return original data in hull order
#   df[hull_idx, ]
# }
# 
# hulls <- test %>%
#   group_by(name) %>%
#   group_modify(~get_ternary_hull(.x))
# 
# ggtern(data=test, aes(x =Pollut_ext , y = Dry_ext, z = State_Count,
#                       fill=as.factor(name),group=as.factor(name))) +
#   #geom_point(aes(color = name), alpha = 0.6) +
#   geom_polygon(data = hulls, aes(fill = as.factor(name), group = as.factor(name)), 
#                alpha = 0.4, color = "black")+
#   theme_bw()+
#   facet_wrap(.~name,ncol=1)


Percentage_Output %>%
  filter(Pollution=="Polluted") %>% 
  mutate(name=factor(name,levels=c("Low_Change","Mid_Change","Large_Change"))) %>% 
  ggplot()+
  geom_vline(xintercept = 0,linewidth=2)+
  geom_linerange(aes(x=Dry_ext.trend,y=as.factor(Pollut_ext),xmax=upper.CL,xmin=lower.CL),linewidth=2)+
  geom_point(aes(x=Dry_ext.trend,y=as.factor(Pollut_ext),fill=name),shape=21, size=4)+
  #geom_boxplot(aes(x=Dry_ext.trend,y=as.factor(Pollut_ext),fill=name))+
  #geom_smooth(aes(x=Pollut_ext,y=value,color=name),method="loess")+
  #facet_wrap(.~Pollution)+
  scale_fill_manual(values = c("green","gold","red"))+ # #669900
  labs(title= "Rate of classification per percentual increase in Drying extent",
       subtitle="Polluted sites",
    x="Rate of loss/Gain of sites", fill="Assigned groups", y="Pollution extent")  +
  theme_minimal()


gridExtra::grid.arrange(
  gridExtra::arrangeGrob(
tidyr::crossing(
  Percentage_Output %>%filter(Pollution=="Polluted") %>% 
    dplyr::select(Pollut_ext,Dry_ext.trend,name), 
  Dry_ext=seq(1, 91, by = 10)) %>% 
  mutate(Pollut_ext=Pollut_ext*100) %>%
  mutate("State_Count"=round((Dry_ext.trend*Dry_ext),2)) %>% 
  #mutate(State_Count = case_when(name == "Low_Change" ~ State_Count, 
  #                               name == "Mid_Change" ~ State_Count, 
  #                               name == "Large_Change" ~ 100+State_Count)) %>% 
  mutate(name=factor(name,levels=c("Low_Change","Mid_Change","Large_Change"))) %>% 
  dplyr::select(Dry_ext,Pollut_ext,State_Count,name)%>% 
  filter(Pollut_ext!=1) %>% 
  ggplot(aes(x=Dry_ext,y=State_Count))+
  geom_hline(yintercept = 0,linewidth=2)+
  geom_point(aes(fill=name),shape=21,size=3,alpha=0.5) + 
  scale_fill_manual(values = c("green","gold","red"))+ # #669900
  labs(title= "Rate of classification per percentual increase in Drying extent",
       subtitle="Polluted sites",y="Loss/Gain of sites", 
       fill="Assigned groups", x="Drying extent")  +
  theme_minimal()+theme(legend.position = "right"),

ggplot()+ggnetwork::theme_blank(),
ncol=2),
    
  # Percentage_Output %>%
  #   filter(Pollution=="Polluted") %>% 
  #   mutate(name=factor(name,levels=c("Low_Change","Mid_Change","Large_Change"))) %>% 
  #   ggplot()+
  #   geom_vline(xintercept = 0,linewidth=2)+
  #   geom_violin(aes(x=Dry_ext.trend,y=name,fill=name),alpha=0.2,trim =F)+
  #   #geom_linerange(aes(x=Dry_ext.trend,y=name,xmax=upper.CL,xmin=lower.CL),linewidth=2)+
  #   geom_jitter(aes(x=Dry_ext.trend,y=name,fill=name),shape=21, size=2,height = 0.1)+
  #   #geom_boxplot(aes(x=Dry_ext.trend,y=name,fill=name))+
  #   #geom_smooth(aes(x=Pollut_ext,y=value,color=name),method="loess")+
  #   #facet_wrap(.~Pollution)+
  #   scale_fill_manual(values = c("green","gold","red"))+ # #669900
  #   labs(title= "Rate of classification per percentual increase in Drying extent",
  #        subtitle="Polluted sites",x="Rate of loss/Gain of sites", 
  #        fill="Assigned groups", y="Pollution extent")  +
  #   theme_minimal()+theme(legend.position = "right")

gridExtra::arrangeGrob(
  
tidyr::crossing(
Percentage_Output %>%filter(Pollution=="Polluted" & name=="Low_Change") %>% 
                     dplyr::select(Pollut_ext,Dry_ext.trend,lower.CL,upper.CL), 
Dry_ext=seq(1, 91, by = 10)) %>% 
mutate(Pollut_ext=Pollut_ext*100) %>% 
mutate("Error"=round((Dry_ext.trend*Dry_ext),2)) %>% 
mutate(Dry_ext=factor(Dry_ext, levels = rev(levels(as.factor(Dry_ext))))) %>% 
mutate(Pollut_ext= factor(Pollut_ext, levels = levels(as.factor(Pollut_ext)))) %>% 
  ggplot(aes(x = as.factor(Pollut_ext), y = as.factor(Dry_ext), fill = abs(Error))) +
  #geom_tile(color = "white",alpha=0.5) +
  geom_raster(interpolate = T,alpha=0.5)+
  geom_text(aes(label = round(Error, 2)),nudge_y = 0.1,size = 4, color="grey20",fontface = "bold")+
  #geom_text(aes(label = paste(round(lower.CL,2),"/",round(upper.CL,2))),nudge_y = -0.15, size = 2.5, color="grey20")+
  #scale_fill_viridis(option = "A",direction = -1,limits=c(0,100))+
  #scale_fill_viridis(option = "D",direction = -1,limits=c(0,100))+
  scale_fill_gradient2(low = "white", mid = "green", high = "darkgreen", midpoint = 50,limits=c(0,100)) +
  labs(title="Percentage of polluted sites missclassified as Low Change",
       x="Pollution extent (%)",y="Catchment drying extent (%)")+theme_minimal() +
  theme(panel.grid = element_blank(),legend.position = "none"),

tidyr::crossing(
  Percentage_Output %>%filter(Pollution=="Polluted" & name=="Mid_Change") %>% 
    dplyr::select(Pollut_ext,Dry_ext.trend,lower.CL,upper.CL), 
  Dry_ext=seq(1, 91, by = 10)) %>% 
  mutate(Pollut_ext=Pollut_ext*100) %>% 
  mutate("Error"=round((Dry_ext.trend*Dry_ext),2)) %>% 
  mutate(Dry_ext=factor(Dry_ext, levels = rev(levels(as.factor(Dry_ext))))) %>% 
  mutate(Pollut_ext= factor(Pollut_ext, levels = levels(as.factor(Pollut_ext)))) %>% 
  ggplot(aes(x = as.factor(Pollut_ext), y = as.factor(Dry_ext), fill = abs(Error))) +
  #geom_tile(color = "white",alpha=0.5) +
  geom_raster(interpolate = T,alpha=0.5)+
  geom_text(aes(label = round(Error, 2)),nudge_y = 0.1,size = 4, color="grey20",fontface = "bold")+
  #geom_text(aes(label = paste(round(lower.CL,2),"/",round(upper.CL,2))),nudge_y = -0.15, size = 2.5, color="grey20")+
  #scale_fill_viridis(option = "A",direction = -1,limits=c(0,100))+
  #scale_fill_viridis(option = "D",direction = -1,limits=c(0,100))+
  scale_fill_gradient2(low = "white", mid = "gold", high = "orange", midpoint = 50,limits=c(0,100)) +
  labs(title="Percentage of polluted sites missclassified as Mid Change",
       x="Pollution extent (%)",y="Catchment drying extent (%)")+theme_minimal() +
  theme(panel.grid = element_blank(),legend.position = "none"),

tidyr::crossing(
  Percentage_Output %>%filter(Pollution=="Polluted" & name=="Large_Change") %>% 
    dplyr::select(Pollut_ext,Dry_ext.trend,lower.CL,upper.CL), 
  Dry_ext=seq(1, 91, by = 10)) %>% 
  mutate(Pollut_ext=Pollut_ext*100) %>% 
  mutate("Error"=round((Dry_ext.trend*Dry_ext),2)) %>% 
  mutate(Dry_ext=factor(Dry_ext, levels = rev(levels(as.factor(Dry_ext))))) %>% 
  mutate(Pollut_ext= factor(Pollut_ext, levels = levels(as.factor(Pollut_ext)))) %>% 
  ggplot(aes(x = as.factor(Pollut_ext), y = as.factor(Dry_ext), fill = abs(Error))) +
  #geom_tile(color = "white",alpha=0.5) +
  geom_raster(interpolate = T,alpha=0.5)+
  geom_text(aes(label = round(Error, 2)),nudge_y = 0.1,size = 4, color="grey20",fontface = "bold")+
  #geom_text(aes(label = paste(round(lower.CL,2),"/",round(upper.CL,2))),nudge_y = -0.15, size = 2.5, color="grey20")+
  #scale_fill_viridis(option = "A",direction = -1,limits=c(0,100))+
  #scale_fill_viridis(option = "D",direction = -1,limits=c(0,100))+
  scale_fill_gradient2(low = "white", mid = "red", high = "darkred", midpoint = 50,limits=c(0,100)) +
  labs(title="Percentage of polluted sites classified as Large Change",
       x="Pollution extent (%)",y="Catchment drying extent (%)")+theme_minimal() +
  theme(panel.grid = element_blank(),legend.position = "none"),
ncol=3),

ncol=1 ,heights=c(1,3))



((500*0.5)+
(((500*0.5)*(50*Percentage_Output %>% filter(Pollut_ext==0.5 & Pollution=="Polluted" & name=="n_B") %>% pull(Dry_ext.trend)))/100)
)/250


# For Drying pattern

grid_Output <- data.frame()
Percentage_Output <- data.frame()
for (Poll_levels in 1:length(unique(Data_To_Plot$Pollut_ext))) {
  for (Poll_type in 1:2) {
    model <- lm(value~Dry_patt*name, data = States_Count %>% 
                  filter(Pollut_ext.x==unique(Data_To_Plot$Pollut_ext)[Poll_levels] &
                           Pollution.x==unique(Data_To_Plot$Pollution)[Poll_type]) %>% 
                  mutate(Dry_ext=Dry_ext*100, Dry_patt=Dry_patt*100))
    #Ls_mean <- multcomp::cld(lsmeans::lsmeans(model,~Dry_ext:name),alpha= 0.05,Letters = letters,adjust = "sidak")
    
    Dry_ext_seq <- seq(1, 90, by = 1)
    Dry_patt_seq <- seq(1, 90, by = 1)
    grid <- bind_rows(data.frame(expand.grid(Dry_ext = Dry_ext_seq, Dry_patt = Dry_patt_seq),name="Low_Change"),
                      data.frame(expand.grid(Dry_ext = Dry_ext_seq, Dry_patt = Dry_patt_seq),name="Mid_Change"),
                      data.frame(expand.grid(Dry_ext = Dry_ext_seq, Dry_patt = Dry_patt_seq),name="Large_Change"))
    grid$State_Count <- predict(model, newdata = grid)
    grid$Pollution <- unique(Data_To_Plot$Pollution)[Poll_type]
    grid$Pollut_ext <- unique(Data_To_Plot$Pollut_ext)[Poll_levels]
    grid_Output <- bind_rows(grid_Output,grid)
    
    outPercvalues <- data.frame("Pollution"=unique(Data_To_Plot$Pollution)[Poll_type],
                                "Pollut_ext"= unique(Data_To_Plot$Pollut_ext)[Poll_levels],
                                data.frame(emmeans::emtrends(model, specs = "name", var = "Dry_patt"))
    )
    
    Percentage_Output <- bind_rows(Percentage_Output,outPercvalues)
    
  }
}

gridExtra::grid.arrange(
  gridExtra::arrangeGrob(
    tidyr::crossing(
      Percentage_Output %>%filter(Pollution=="Polluted") %>% 
        dplyr::select(Pollut_ext,Dry_patt.trend,name), 
      Dry_patt=seq(1, 91, by = 10)) %>% 
      mutate(Pollut_ext=Pollut_ext*100) %>%
      mutate("State_Count"=round((Dry_patt.trend*Dry_patt),2)) %>% 
      #mutate(State_Count = case_when(name == "Low_Change" ~ State_Count, 
      #                               name == "Mid_Change" ~ State_Count, 
      #                               name == "Large_Change" ~ 100+State_Count)) %>% 
      mutate(name=factor(name,levels=c("Low_Change","Mid_Change","Large_Change"))) %>% 
      dplyr::select(Dry_patt,Pollut_ext,State_Count,name)%>% 
      filter(Pollut_ext!=1) %>% 
      ggplot(aes(x=Dry_patt,y=State_Count))+
      geom_hline(yintercept = 0,linewidth=2)+
      geom_point(aes(fill=name),shape=21,size=3,alpha=0.5) + 
      scale_fill_manual(values = c("green","gold","red"))+ # #669900
      labs(title= "Rate of classification per percentual increase in Drying pattern",
           subtitle="Polluted sites",y="Loss/Gain of sites", 
           fill="Assigned groups", x="Drying pattern")  +
      theme_minimal()+theme(legend.position = "right"),
    
    ggplot()+ggnetwork::theme_blank(),
    ncol=2),
  
  # Percentage_Output %>%
  #   filter(Pollution=="Polluted") %>% 
  #   mutate(name=factor(name,levels=c("Low_Change","Mid_Change","Large_Change"))) %>% 
  #   ggplot()+
  #   geom_vline(xintercept = 0,linewidth=2)+
  #   geom_violin(aes(x=Dry_patt.trend,y=name,fill=name),alpha=0.2,trim =F)+
  #   #geom_linerange(aes(x=Dry_patt.trend,y=name,xmax=upper.CL,xmin=lower.CL),linewidth=2)+
  #   geom_jitter(aes(x=Dry_patt.trend,y=name,fill=name),shape=21, size=2,height = 0.1)+
  #   #geom_boxplot(aes(x=Dry_patt.trend,y=name,fill=name))+
  #   #geom_smooth(aes(x=Pollut_ext,y=value,color=name),method="loess")+
  #   #facet_wrap(.~Pollution)+
  #   scale_fill_manual(values = c("green","gold","red"))+ # #669900
  #   labs(title= "Rate of classification per percentual increase in Drying extent",
  #        subtitle="Polluted sites",x="Rate of loss/Gain of sites", 
  #        fill="Assigned groups", y="Pollution extent")  +
  #   theme_minimal()+theme(legend.position = "right")
  
  gridExtra::arrangeGrob(
    
    tidyr::crossing(
      Percentage_Output %>%filter(Pollution=="Polluted" & name=="Low_Change") %>% 
        dplyr::select(Pollut_ext,Dry_patt.trend,lower.CL,upper.CL), 
      Dry_patt=seq(1, 91, by = 10)) %>% 
      mutate(Pollut_ext=Pollut_ext*100) %>% 
      mutate("Error"=round((Dry_patt.trend*Dry_patt),2)) %>% 
      mutate(Dry_patt=factor(Dry_patt, levels = rev(levels(as.factor(Dry_patt))))) %>% 
      mutate(Pollut_ext= factor(Pollut_ext, levels = levels(as.factor(Pollut_ext)))) %>% 
      ggplot(aes(x = as.factor(Pollut_ext), y = as.factor(Dry_patt), fill = abs(Error))) +
      #geom_tile(color = "white",alpha=0.5) +
      geom_raster(interpolate = T,alpha=0.5)+
      geom_text(aes(label = round(Error, 2)),nudge_y = 0.1,size = 4, color="grey20",fontface = "bold")+
      #geom_text(aes(label = paste(round(lower.CL,2),"/",round(upper.CL,2))),nudge_y = -0.15, size = 2.5, color="grey20")+
      #scale_fill_viridis(option = "A",direction = -1,limits=c(0,100))+
      #scale_fill_viridis(option = "D",direction = -1,limits=c(0,100))+
      scale_fill_gradient2(low = "white", mid = "green", high = "darkgreen", midpoint = 50,limits=c(0,100)) +
      labs(title="Percentage of polluted sites missclassified as Low Change",
           x="Pollution extent (%)",y="Drying pattern (%)")+theme_minimal() +
      theme(panel.grid = element_blank(),legend.position = "none"),
    
    tidyr::crossing(
      Percentage_Output %>%filter(Pollution=="Polluted" & name=="Mid_Change") %>% 
        dplyr::select(Pollut_ext,Dry_patt.trend,lower.CL,upper.CL), 
      Dry_patt=seq(1, 91, by = 10)) %>% 
      mutate(Pollut_ext=Pollut_ext*100) %>% 
      mutate("Error"=round((Dry_patt.trend*Dry_patt),2)) %>% 
      mutate(Dry_patt=factor(Dry_patt, levels = rev(levels(as.factor(Dry_patt))))) %>% 
      mutate(Pollut_ext= factor(Pollut_ext, levels = levels(as.factor(Pollut_ext)))) %>% 
      ggplot(aes(x = as.factor(Pollut_ext), y = as.factor(Dry_patt), fill = abs(Error))) +
      #geom_tile(color = "white",alpha=0.5) +
      geom_raster(interpolate = T,alpha=0.5)+
      geom_text(aes(label = round(Error, 2)),nudge_y = 0.1,size = 4, color="grey20",fontface = "bold")+
      #geom_text(aes(label = paste(round(lower.CL,2),"/",round(upper.CL,2))),nudge_y = -0.15, size = 2.5, color="grey20")+
      #scale_fill_viridis(option = "A",direction = -1,limits=c(0,100))+
      #scale_fill_viridis(option = "D",direction = -1,limits=c(0,100))+
      scale_fill_gradient2(low = "white", mid = "gold", high = "orange", midpoint = 50,limits=c(0,100)) +
      labs(title="Percentage of polluted sites missclassified as Mid Change",
           x="Pollution extent (%)",y="Drying pattern (%)")+theme_minimal() +
      theme(panel.grid = element_blank(),legend.position = "none"),
    
    tidyr::crossing(
      Percentage_Output %>%filter(Pollution=="Polluted" & name=="Large_Change") %>% 
        dplyr::select(Pollut_ext,Dry_patt.trend,lower.CL,upper.CL), 
      Dry_patt=seq(1, 91, by = 10)) %>% 
      mutate(Pollut_ext=Pollut_ext*100) %>% 
      mutate("Error"=round((Dry_patt.trend*Dry_patt),2)) %>% 
      mutate(Dry_patt=factor(Dry_patt, levels = rev(levels(as.factor(Dry_patt))))) %>% 
      mutate(Pollut_ext= factor(Pollut_ext, levels = levels(as.factor(Pollut_ext)))) %>% 
      ggplot(aes(x = as.factor(Pollut_ext), y = as.factor(Dry_patt), fill = abs(Error))) +
      #geom_tile(color = "white",alpha=0.5) +
      geom_raster(interpolate = T,alpha=0.5)+
      geom_text(aes(label = round(Error, 2)),nudge_y = 0.1,size = 4, color="grey20",fontface = "bold")+
      #geom_text(aes(label = paste(round(lower.CL,2),"/",round(upper.CL,2))),nudge_y = -0.15, size = 2.5, color="grey20")+
      #scale_fill_viridis(option = "A",direction = -1,limits=c(0,100))+
      #scale_fill_viridis(option = "D",direction = -1,limits=c(0,100))+
      scale_fill_gradient2(low = "white", mid = "red", high = "darkred", midpoint = 50,limits=c(0,100)) +
      labs(title="Percentage of polluted sites classified as Large Change",
           x="Pollution extent (%)",y="Drying pattern (%)")+theme_minimal() +
      theme(panel.grid = element_blank(),legend.position = "none"),
    ncol=3),
  
  ncol=1 ,heights=c(1,3))




Modeled_grid <- data.frame()
for (Poll_levels in 1:length(unique(Data_To_Plot$Pollut_ext))) {
  Comparison_data <- Data_To_Plot %>% 
    select(Scenario, Site_ID,Pollution,Dry_ext, Pollut_ext, Dry_patt,Mean_STcon ,IBMWP) %>% 
    left_join(Big_Ref, by=c("Site_ID","Dry_ext","Dry_patt","Mean_STcon"),relationship = "many-to-many") %>% 
    mutate(Med_Diff=IBMWP-Ref_IBMWP) %>% 
    filter(Pollut_ext.x==unique(Data_To_Plot$Pollut_ext)[Poll_levels])
  
  #model <- mgcv::gam(Med_Diff ~ s(Dry_ext) + s(Dry_patt), data = Comparison_data)
  #model <- mgcv::gam(Med_Diff ~ s(Mean_STcon), data = Comparison_data)
  model <- randomForest::randomForest(Med_Diff ~ Dry_ext + Dry_patt + as.factor(Comparison_data$Pollution.x), data = Comparison_data)
  
  Dry_ext_seq <- seq(0.01, 0.9, by = 0.01)
  Dry_patt_seq <- seq(0.01, 0.9, by = 0.01)
  grid <- bind_rows(data.frame(expand.grid(Dry_ext = Dry_ext_seq, Dry_patt = Dry_patt_seq),'as.factor(Comparison_data$Pollution.x)'="Unpolluted"),
                    data.frame(expand.grid(Dry_ext = Dry_ext_seq, Dry_patt = Dry_patt_seq),'as.factor(Comparison_data$Pollution.x)'="Polluted")
  )
  
  # Predict
  grid$Med_Diff <- predict(model, newdata = grid)
  
  grid$Med_Diff_col <- ifelse(grid$Med_Diff>(-20),"blue",
                              ifelse(grid$Med_Diff>(-40),"green",
                                     ifelse(grid$Med_Diff>(-75),"gold",
                                            ifelse(grid$Med_Diff>(-85),"orange","red"))))
  
  grid$Pollut_ext.x <- unique(Data_To_Plot$Pollut_ext)[Poll_levels]
  
  
  Modeled_grid <- bind_rows(Modeled_grid,grid)
}

Modeled_grid %>% 
  rename("Pollution.x"='as.factor.Comparison_data.Pollution.x.') %>% 
  ggplot()+
  geom_raster(aes(x=Dry_ext,y=Dry_patt),fill=Modeled_grid$Med_Diff_col,interpolate = T)+
  facet_wrap(Pollut_ext.x~Pollution.x,ncol=2,strip.position = "right")+
  theme(legend.position = "none",
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        #axis.text = element_blank(),
        panel.grid = element_blank(),
        strip.background=element_blank()
  )


summary(Count_states)
quantile(Count_states)

Count_states <- Data_To_Plot %>% 
                select(Scenario, Site_ID,Pollution,Dry_ext, Pollut_ext, Dry_patt,Mean_STcon ,IBMWP) %>% 
                left_join(Big_Ref, by=c("Site_ID","Dry_ext","Dry_patt","Mean_STcon"),
                          relationship = "many-to-many") %>% 
                mutate(Med_Diff=IBMWP-Ref_IBMWP) %>%
                #group_by(Pollut_ext.x,Dry_ext,Dry_patt,Pollution.x) %>% 
                 mutate(Good_State=ifelse(Med_Diff >= quantile(Med_Diff)[3],1,0))%>% 
                 mutate(Moderate_State=ifelse(Med_Diff < quantile(Med_Diff)[3] & Med_Diff >= quantile(Med_Diff)[2],1,0))%>% 
                 mutate(Bad_State=ifelse(Med_Diff < quantile(Med_Diff)[2],1,0))%>% 
                 #mutate(Bad_State=ifelse(Med_Diff >= -85 & Med_Diff <= -66,1,0))%>% 
                 #mutate(VeryBad_State=ifelse(Med_Diff <= -86 ,1,0)) %>% 
                 summarise(G_State=sum(Good_State),
                           M_State=sum(Moderate_State),
                           B_State=sum(Bad_State)) %>% 
                           pivot_longer(5:ncol(.))

Count_states %>% filter(Pollut_ext.x==0.1) %>% filter(Pollution.x=="YES_Poll")


Count_states %>%
  mutate(name=factor(name,levels=c("G_State","M_State","B_State"))) %>% 
  ggplot(aes(x=Dry_ext,y=value,fill=name))+
  geom_col(position="fill")+
  #geom_point(,shape=21,size=2) +
  scale_fill_manual(values = c("green","gold","red"))+
  facet_wrap(Pollut_ext.x~Pollution.x,ncol=2,strip.position = "right")+
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        strip.background=element_blank()
  )
  




Comparison_data %>% # filter(Pollut_ext.x==0.1 & Dry_patt.x==0.1) %>% 
  ggplot(aes(x=Mean_STcon,y=Med_Diff,fill=as.factor(Dry_ext)))+
  geom_point(shape=21)+
  #geom_smooth(method = "lm",se=F)+
  geom_hline(yintercept = c(-40))+
  scale_fill_viridis(discrete = T)+
  facet_wrap(as.factor(Dry_ext)~Pollution.x)

Comparison_data %>% filter(Pollut_ext.x==0.5) %>% 
  mutate(Above=ifelse(Med_Diff<(-1.96),1,0)) %>% 
  group_by(Pollut_ext.x,Dry_ext.x,Dry_patt.x,Pollution.x) %>%
  summarise(Sign=sum(Above)) %>% 
  mutate(Value=Sign/ceiling(225*0.5)) %>% 
  ggplot(aes(x=as.factor(Dry_ext.x),y=Value,fill=Pollution.x))+
  geom_boxplot()

  

Comparison_data %>% filter(Pollut_ext.x==0.5) %>% 
  mutate(Above=ifelse(Med_Diff<(-1.96),1,0)) %>% 
  group_by(Pollut_ext.x,Dry_ext.x,Dry_patt.x,Pollution.x) %>%
  summarise(sum(Above))


sd(Comparison_data %>% filter(Pollut_ext.x==0.01) %>% pull(Med_Diff))



bind_rows(
data.frame("Value"=Comparison_data %>% filter(Pollut_ext.x==0.35 & Pollution.x=="Non_Poll") %>% 
             filter(Dry_ext.x==0.25,Dry_patt.x%in%c(0.1:0.35)) %>% pull(Diff),"val"="No_Poll_0.25"),
data.frame("Value"=Comparison_data %>% filter(Pollut_ext.x==0.35 & Pollution.x=="Non_Poll") %>% 
             filter(Dry_ext.x==0.75,Dry_patt.x%in%c(0.1:0.35)) %>% pull(Diff),"val"="No_Poll_0.75"),
data.frame("Value"=Comparison_data %>% filter(Pollut_ext.x==0.35 & Pollution.x=="YES_Poll") %>% 
             filter(Dry_ext.x==0.25,Dry_patt.x%in%c(0.1:0.35)) %>% pull(Diff),"val"="Yes_Poll_0.25"),
data.frame("Value"=Comparison_data %>% filter(Pollut_ext.x==0.35 & Pollution.x=="YES_Poll") %>% 
             filter(Dry_ext.x==0.75,Dry_patt.x%in%c(0.1:0.35)) %>% pull(Diff),"val"="YEs_Poll_0.75")
) %>% ggplot()+
  geom_boxplot(aes(x=val,y=Value))

prova <- Comparison_data%>% filter(Pollut_ext.x==0.75) %>% filter(Dry_ext%in%c(0.25,0.75),Dry_patt%in%c(0.1:0.35))
model <- lm(Diff~Pollution.x*as.factor(Dry_ext),data=prova)
anova(model)


multcomp::cld(lsmeans::lsmeans(model,~Pollution.x*as.factor(Dry_ext)),alpha= 0.1,Letters = letters,adjust = "sidak") ###  Tukey-adjusted











for(Dry_Pol_levels in 2:length(unique(Data_To_Plot$Pollut_ext))){
Comparison_data <- data.frame()
Comparison_data <-bind_rows(
Data_To_Plot %>% select(Scenario,Site_ID,Pollution,Dry_ext, Pollut_ext, Dry_patt,IBMWP) %>%
                 filter(Pollut_ext==0.01) %>% 
                 mutate("Reference"=paste("Ref",Pollution,sep="_")),
Data_To_Plot %>% select(Scenario,Site_ID,Pollution,Dry_ext, Pollut_ext, Dry_patt,IBMWP) %>%
                 filter(Pollut_ext!=0.01) %>% 
                 mutate("Reference"=paste("No_Ref",Pollution,sep="_")) %>%
                 filter(Pollut_ext==unique(Data_To_Plot$Pollut_ext)[Dry_Pol_levels])
)%>% 
  filter(Dry_ext==0.1,Dry_patt==0.1)

Comparison_data$Reference <- factor(Comparison_data$Reference,levels=c("Ref_Non_Poll","Ref_YES_Poll","No_Ref_YES_Poll","No_Ref_Non_Poll"))
Comparison_data <- within(Comparison_data, Reference <- relevel(Reference, ref = 1))
model <- lm(sqrt(IBMWP)  ~Reference , data=Comparison_data)
multcomp::cld(lsmeans::lsmeans(model,~ Reference),alpha= 0.05,Letters = letters,adjust = "sidak") ###  Tukey-adjusted

cat(round(summary(model)$coefficients[3:4,4],4),"\n")
}



ggplot()+
  geom_boxplot(data = Comparison_data,aes(y=sqrt(IBMWP),x=Pollution,fill=as.factor(Reference)))+
  geom_boxplot(data = Comparison_data,aes(y=sqrt(IBMWP),x=Pollution,colour=as.factor(Reference)))


Data_To_Plot 

Data_To_Plot %>% 
  dplyr::select(Scenario, Site_ID,Pollution,Dry_ext, Pollut_ext, Dry_patt ,IBMWP) %>% 
  left_join(Big_Ref, by=c("Site_ID","Dry_ext","Dry_patt"), relationship = "many-to-many")%>% 
  group_by(Dry_ext,Dry_patt) %>% 
  mutate(Mean=mean(Ref_IBMWP),SD=sd(Ref_IBMWP)) %>% 
  mutate()


Big_Ref %>% group_by(Dry_ext,Dry_patt) %>% 
            summarise(Mean=mean(Ref_IBMWP),SD=sd(Ref_IBMWP))


Comparison_data <- Data_To_Plot %>% 
  dplyr::select(Scenario, Site_ID,Pollution,Dry_ext, Pollut_ext, Dry_patt,Mean_STcon ,Mean_IBMWP) %>% 
  left_join(Big_Ref, by=c("Site_ID","Dry_ext","Dry_patt","Mean_STcon"), relationship = "many-to-many") %>%
  #mutate(Diff=abs(IBMWP-Ref_IBMWP)) %>% 
  mutate(Diff=log(Mean_IBMWP/Ref_IBMWP)) %>% 
  filter(Pollut_ext.x==0.9) %>% 
  group_by(Pollut_ext.x,Dry_ext,Dry_patt) %>% 
  mutate(Z_test=((Mean_IBMWP-mean(Ref_IBMWP))/sd(Ref_IBMWP))) %>% 
  filter(Dry_ext%in%c(0.35))
  #group_by(Pollution.x,Dry_ext, Pollut_ext.x,Dry_patt) %>% 
  
Comparison_data %>% 
ggplot(aes(y=Z_test,x=Mean_STcon,fill=Pollution.x))+
  geom_point(shape=21)+
  geom_smooth(method = "lm",se=F)  

model <-glm(Z_test ~ Pollution.x*Mean_STcon, family = gaussian(link = "identity"), data = Comparison_data)
print(summary(model))
multcomp::cld(lsmeans::lsmeans(model,~ as.factor(Pollut_ext.x)|Pollution.x),alpha= 0.01,Letters = letters,adjust = "Tukey") ###  Tukey-adjusted



ggplot()+
  #geom_point(data = Comparison_data,aes(y=Diff,x=Mean_STcon,fill=Dry_ext),shape=21,alpha=0.2)+
  geom_boxplot(data = Comparison_data,aes(y=Z_test,x=as.factor(Dry_ext),fill=Dry_ext))+
  geom_hline(yintercept = c(1.96,-1.96))+
  scale_fill_viridis()+
  facet_wrap(.~Pollution.x)






out_p_value <- data.frame()
for(Dry_Pol_levels in 1:length(unique(Data_To_Plot$Pollut_ext))){
  
  for(Dry_Ext_levels in 1:length(unique(Data_To_Plot$Dry_ext))){
    Comparison_data <- Data_To_Plot %>% 
      select(Scenario, Site_ID,Pollution,Dry_ext, Pollut_ext, Dry_patt ,IBMWP) %>% 
      left_join(Big_Ref, by=c("Site_ID","Dry_ext","Dry_patt"),
                relationship = "many-to-many") %>%
      mutate(Diff=abs(IBMWP-Ref_IBMWP)) %>% 
      group_by(Pollution.x,Dry_ext, Pollut_ext.x,Dry_patt) %>% 
      #summarise(Med_Diff=mean(Diff),sd_Diff=sd(Diff)) %>% 
      filter(Dry_ext!=0.01) %>% 
      #mutate(Diff=
      #         log(abs(
      #         IBMWP-Ref_IBMWP
      #         )+1)
      #       ) %>% 
      filter(#Dry_ext%in%c(0.1,0.25,0.35) , #unique(Data_To_Plot$Dry_ext)[Dry_Ext_levels],
        Pollut_ext.x==0.35,#unique(Data_To_Plot$Pollut_ext)[Dry_Pol_levels],
        Dry_patt%in%unique(Data_To_Plot$Dry_patt)[1:6]
      ) %>% 
      filter(Diff>30)
    
    Comparison_data %>% ggplot()+
      geom_point(aes(x=Dry_ext,y=Diff, colour=Pollution.x))+  
      geom_errorbar(aes(x=Dry_ext,y=Med_Diff,ymin = Med_Diff-sd_Diff, ymax = Med_Diff+sd_Diff))
    
    model <- lm(Diff~Pollution.x*as.factor(Dry_ext) , data=Comparison_data)
    print(summary(model))
  }
  out_p_value_temp <- data.frame("Poll_ext"=unique(Data_To_Plot$Pollut_ext)[Dry_Pol_levels],
                                 "Dry_ext"=unique(Data_To_Plot$Dry_ext)[Dry_Ext_levels],
                                 "YES_POLL_p_value"=  round(t.test(Comparison_data %>% filter(Pollution.x=="YES_Poll") %>% pull(Diff))[[3]],5),
                                 "NO_POLL_p_value"=round(t.test(Comparison_data %>% filter(Pollution.x=="Non_Poll") %>% pull(Diff))[[3]],5),
                                 "p_value"=round(anova(model)[5][1,1],5)
                                 
                                 out_p_value <- bind_rows(out_p_value,out_p_value_temp)
                                 
                                  
                                 
}

}# POLLUT










gridExtra::grid.arrange(
Data_To_Plot %>% ggplot()+
  geom_smooth(aes(x=Mean_STcon,y=Permanence,colour=Dry_ext),method = "lm",se=T)+
  scale_colour_viridis(discrete = T,option = "D",direction = 1)+
  #facet_wrap(.~Pollut_ext)+
  theme_classic(),

Data_To_Plot %>% filter(Pollut_ext==0.01) %>% 
  ggplot()+
  geom_smooth(aes(x=Mean_STcon,y=S,colour=Dry_ext),method = "lm",se=T)+
  scale_colour_viridis(discrete = T,option = "D",direction = 1)+
  #facet_wrap(.~Pollut_ext)+
  theme_classic(),

Data_To_Plot %>% filter(Pollut_ext==0.01) %>%
  ggplot()+
  geom_smooth(aes(x=STcon_AAct,y=S_AAct,colour=Dry_ext),method = "lm",se=T)+
  scale_colour_viridis(discrete = T,option = "D",direction = 1)+
  #facet_wrap(.~Pollut_ext)+
  theme_classic(),

Data_To_Plot %>% filter(Pollut_ext==0.01) %>%
  ggplot()+
  geom_smooth(aes(x=STcon_Drift,y=S_Drift,colour=Dry_ext),method = "lm",se=T)+
  scale_colour_viridis(discrete = T,option = "D",direction = 1)+
  #facet_wrap(.~Pollut_ext)+
  theme_classic(),

Data_To_Plot %>% filter(Pollut_ext==0.01) %>%
  ggplot()+
  geom_smooth(aes(x=STcon_Swim,y=S_Swim,colour=Dry_ext),method = "lm",se=T)+
  scale_colour_viridis(discrete = T,option = "D",direction = 1)+
  #facet_wrap(.~Pollut_ext)+
  theme_classic(),
ncol=5)


gridExtra::grid.arrange(
  Data_To_Plot %>% ggplot()+
    geom_smooth(aes(x=Mean_STcon,y=Permanence,colour=Dry_ext),method = "lm",se=T)+
    scale_colour_viridis(discrete = T,option = "D",direction = 1)+
    #facet_wrap(.~Pollut_ext)+
    theme_classic(),
  
  Data_To_Plot %>% filter(Pollut_ext==0.01) %>% 
    ggplot()+
    geom_smooth(aes(x=Mean_STcon,y=B,colour=Dry_ext),method = "lm",se=T)+
    scale_colour_viridis(discrete = T,option = "D",direction = 1)+
    #facet_wrap(.~Pollut_ext)+
    theme_classic()
  )


gridExtra::grid.arrange(
  Data_To_Plot %>% ggplot()+
    geom_smooth(aes(x=Mean_STcon,y=Permanence,colour=Dry_ext),method = "lm",se=T)+
    scale_colour_viridis(discrete = T,option = "D",direction = 1)+
    #facet_wrap(.~Pollut_ext)+
    theme_classic(),
  
  Data_To_Plot %>% filter(Pollut_ext==0.01) %>% 
    ggplot()+
    geom_smooth(aes(x=Mean_STcon,y=B,colour=Dry_ext),method = "lm",se=T)+
    scale_colour_viridis(discrete = T,option = "D",direction = 1)+
    #facet_wrap(.~Pollut_ext)+
    theme_classic()
)



Data_To_Plot %>% group_by(Pollut_ext,Dry_ext,Pollution ) %>% 
                 summarise(Vari=sd(IBMWP),MeanMean=sd(Mean_STcon)) %>% 
  ggplot()+
  geom_boxplot(aes(x=Pollution,y=Vari,colour=Pollution))+
  facet_wrap(.~Dry_ext)+
  scale_colour_viridis(discrete = T,option = "D",direction = 1)+
  theme_classic()+theme(legend.position = "none")

unique(Res_nodes_DaFr$Dry_ext)
Test_list <- list()
SEMU <- data.frame()
for (Driri in 1:length(unique(Res_nodes_DaFr$Dry_ext))) {
Dry_ext_to_test <- unique(Res_nodes_DaFr$Dry_ext)[Driri]

#Perm <- summary(Data_To_Plot %>% filter(Dry_ext==Dry_ext_to_test,Pollut_ext!=0.01) %>% 
#                  pull(Mean_STcon))[2]
Data_To_Plot_Ref <- Res_nodes_DaFr %>% filter(Disp==0.15,Dry_ext==0.01,Pollut_ext==0.01) #%>% 
#filter(Mean_STcon>Perm)

Data_To_Plot_Z <- Data_To_Plot %>% 
  mutate(Z_test=((IBMWP-mean(Data_To_Plot_Ref$IBMWP))/sd(Data_To_Plot_Ref$IBMWP))) %>%
  filter(Dry_ext==Dry_ext_to_test,Pollut_ext!=0.01) %>% 
  group_by(Dry_ext,Pollut_ext,Pollution) 
  
Data_To_Plot_Z %>%mutate(Z_Mean=mean(Z_test),Z_sd=sd(Z_test)) %>% 
  ggplot() +
  geom_point(aes(x=Z_test,y=Mean_STcon,fill=Pollut_ext,shape=Pollution),alpha=0.05)+
  geom_point(aes(x=Z_Mean,y=Pollut_ext,colour=Pollution),alpha=0.5)+
  geom_errorbar(aes(x=Z_Mean,y=Pollut_ext,xmin = Z_Mean-Z_sd, xmax = Z_Mean+Z_sd,colour=Pollution))+
  scale_fill_viridis(discrete = T,option = "G",direction = 1)+
  scale_colour_manual(values = c("grey","darkgreen"))+
  scale_shape_manual(values=c(21,22))+
  geom_vline(xintercept=c(0),colour="black",size=1,linetype=2)+
  scale_x_continuous(limits = c(-10,3))+
  labs(title=paste("Drying extention:",Dry_ext_to_test))+
  theme_classic()+facet_wrap(.~Pollut_ext,nrow = 2)

Test_list[[Driri]] <- Data_To_Plot_Z %>%mutate(Z_Mean=mean(Z_test),Z_sd=sd(Z_test)) %>% 
  ggplot() +
  geom_point(aes(x=Z_test,y=Mean_STcon,fill=Mean_STcon,shape=Pollution,colour=Pollution),alpha=0.5)+
  #geom_point(aes(x=Z_Mean,y=Pollut_ext,colour=Pollution),alpha=0.5)+
  #geom_errorbar(aes(x=Z_Mean,y=Pollut_ext,xmin = Z_Mean-Z_sd, xmax = Z_Mean+Z_sd,colour=Pollution))+
  scale_fill_viridis(discrete = F,option = "D",direction = 1)+
  scale_colour_manual(values = c("grey","red"))+
  scale_shape_manual(values=c(21,22))+
  geom_vline(xintercept=c(0),colour="black",size=1,linetype=2)+
  scale_x_continuous(limits = c(-10,3))+
  labs(title=paste("Drying extention:",Dry_ext_to_test))+
  theme_classic()#+facet_wrap(.~Pollut_ext,nrow = 2)

test <- Data_To_Plot_Z %>% select(Pollution,Z_test)

letssee <- c()
for (Pollut in 1:(length(unique(test$Pollut_ext))-1)) {
To_test <- test %>% filter(Pollut_ext==unique(test$Pollut_ext)[Pollut]) %>%  
                    group_by(Pollution) %>% summarise(Mean=mean(Z_test)) %>%
                    pull(Mean)
letssee[Pollut] <- To_test[1]-To_test[2]
}
SEM <- data.frame(letssee,Pollut=unique(test$Pollut_ext)[-10],Dry=unique(test$Dry_ext))

SEMU <- bind_rows(SEMU,SEM)
}

png(filename = "ScenarioS_STcon.png",width = 4000,height = 4000,units = "px",res =300)
gridExtra::grid.arrange(
  gridExtra::grid.arrange(Test_list[[1]]),gridExtra::grid.arrange(Test_list[[2]]),
  gridExtra::grid.arrange(Test_list[[3]]),gridExtra::grid.arrange(Test_list[[4]]),
  gridExtra::grid.arrange(Test_list[[5]]),gridExtra::grid.arrange(Test_list[[6]]),
  gridExtra::grid.arrange(Test_list[[7]]),gridExtra::grid.arrange(Test_list[[8]]),
  gridExtra::grid.arrange(Test_list[[9]]),gridExtra::grid.arrange(Test_list[[10]]),
  gridExtra::grid.arrange(Test_list[[11]]))
dev.off()


SEMU %>% ggplot()+
  geom_point(aes(x=as.numeric(Dry),y=letssee,colour=as.factor(Pollut)))+
  geom_smooth(aes(x=as.numeric(Dry),y=letssee,colour=as.factor(Pollut)),method = "lm",se=F)+
  scale_color_viridis(discrete = T,option = "G",direction = -1)+theme_classic()+
  geom_hline(yintercept = 1,colour="red",size=1.2,linetype=2)+
  #geom_hline(yintercept = -1,colour="red",size=1.2,linetype=2)+
  labs(title="Ratio Sensitive/Tolerant", colour="Pollution",x="Drying extent")+
  scale_y_continuous(limits = c(-0.5,7))


am <- aov(letssee~as.factor(Pollut),data=SEMU)
TukeyHSD(am)










Data_To_Plot %>% #filter(Scenario=="0.01_0.01") %>% 
                 mutate(Ref_Value=rep(Data_To_Plot_Ref$S_Swim,11*11)) %>% 
                 filter(Scenario!="0.01_0.01") %>%
                 mutate(Value_to_Plot=S_Swim/Ref_Value) %>% 
  ggplot()+
  geom_smooth(aes(x=STcon_Swim,y=Value_to_Plot,colour=Pollut_ext), method="lm",se=F)+
  scale_colour_viridis(discrete = T,option = "G",direction = -1)+
  scale_y_continuous(limits = c(-0.5,1.5))+
  facet_wrap(.~Dry_ext)+
  geom_hline(yintercept=c(0,1),colour="black")+
  theme_classic()+theme(legend.position = "none")

Data_To_Plot %>% #filter(Scenario=="0.01_0.01") %>% 
  mutate(Ref_Value=rep(Data_To_Plot_Ref$S_Drift,11*11)) %>% 
  filter(Scenario!="0.01_0.01") %>%
  mutate(Value_to_Plot=S_Drift/Ref_Value) %>% 
  ggplot()+
  geom_smooth(aes(x=STcon_Drift,y=Value_to_Plot,colour=Pollut_ext), method="lm",se=F)+
  scale_colour_viridis(discrete = T,option = "G",direction = -1)+
  scale_y_continuous(limits = c(-0.5,1.5))+
  facet_wrap(.~Dry_ext)+
  geom_hline(yintercept=c(0,1),colour="black")+
  theme_classic()+theme(legend.position = "none")

Data_To_Plot %>% #filter(Scenario=="0.01_0.01") %>% 
  mutate(Ref_Value=rep(Data_To_Plot_Ref$S_AAct,11*11)) %>% 
  filter(Scenario!="0.01_0.01") %>%
  mutate(Value_to_Plot=S_AAct/Ref_Value) %>% 
  ggplot()+
  geom_smooth(aes(x=STcon_AAct,y=Value_to_Plot,colour=Pollut_ext), method="lm",se=F)+
  scale_colour_viridis(discrete = T,option = "G",direction = -1)+
  scale_y_continuous(limits = c(-0.5,1.5))+
  facet_wrap(.~Dry_ext)+
  geom_hline(yintercept=c(0,1),colour="black")+
  theme_classic()+theme(legend.position = "none")






















asds <- rep(Res_nodes_DaFr %>% filter(Disp==0.15,Scenario=="0.01_0.75") %>% pull(IBMWP),121)
#Res_nodes_DaFr %>%
  Res_nodes_DaFr %>% filter(Disp==0.15) %>% mutate(Ref_Cond=asds) %>% 
  mutate(Ref_IBMWP=Ref_Cond-IBMWP) %>% 
  mutate(Permanence_factor=ifelse(Permanence==48,"Perm",
                                  ifelse(Permanence<48&Permanence>24,"MidDry",
                                         "ExtrDry"))) %>% 
  filter(Disp==0.15,Pollut_ext%in%c(0.75),
         Dry_ext%in%c(0.01,0.1,0.2,0.5,0.6,0.75)) %>% 
  ggplot(aes(x=Mean_STcon,y=Ref_IBMWP, colour=as.factor(Dry_ext)))+
  #geom_boxplot()+
  geom_jitter(height = 0,width = 0,alpha=0.01)+
  geom_smooth(aes(linetype=as.factor(Pollution)),method="lm",se=F)+
  scale_color_viridis(discrete = T)+
  #scale_y_continuous(limits=c(0,300))+
  #labs(title="Dispersals",subtitle = "Dry extent",
  #     colour="Poll ext.",linetype="Poll ext.")+
  facet_wrap(Pollut_ext~.,scales = "fixed",ncol=4)+
  theme_classic()

a <- Res_nodes_DaFr %>%filter(Disp==0.15,
                              Pollut_ext%in%c(0.2),
                              Dry_ext%in%c(1),Pollution=="Non_Poll") %>%
select(Mean_STcon,IBMWP) 

hist(a$Mean_STcon)

model1 <- lm(IBMWP~Mean_STcon,data=a)
newx <-data.frame(Mean_STcon=seq(0,90,0.01))
pred_a = predict(model1, newdata=newx)


b <- Res_nodes_DaFr %>%filter(Disp==0.15,
                              Pollut_ext%in%c(0.2),
                              Dry_ext%in%c(1),Pollution=="YES_Poll") %>%
select(Mean_STcon,IBMWP) 
hist(b$Mean_STcon)

model2 <- lm(IBMWP~Mean_STcon,data=b)
newx <-data.frame(Mean_STcon=seq(0,90,0.01))
pred_b= predict(model2, newdata=newx)

newx[which(round(pred_a,1)==intersect(round(pred_a,1),round(pred_b,1))[1])[1],]





Ref.value <- Res_nodes_DaFr %>% filter(Disp==0.15,Dry_ext==0.01) %>% pull(Ratio_S.T)
  

asds <- rep(Res_nodes_DaFr %>% filter(Disp==0.15,Scenario=="0.01_0.2") %>% pull(IBMWP),121)

Res_nodes_DaFr %>% filter(Disp==0.15,Scenario!="0.01_0.2") %>% mutate(Ref_Cond=asds) %>% 
  mutate(Ref_Cond-IBMWP)


Res_nodes_DaFr %>% filter(Disp==0.15,Pollut_ext==0.5,
                          Dry_ext%in%c(0.1,0.5)) %>%
  #mutate(Ref.value=rep(Meh,10)) %>% 
  ggplot(aes(x=Mean_STcon,y=S, colour=as.factor(Dry_ext)))+
  geom_jitter(height = 0,width = 0,alpha=1)+
  #geom_hline(yintercept=1)+
  #geom_text(aes(x=0.5,y=1.1),label="+ Sensitive",colour="black")+
  #geom_text(aes(x=0.5,y=0.9),label="+ Tolerant",colour="black")+
  geom_smooth(method="lm",se=F)+
  scale_color_viridis(discrete = T)+
  scale_fill_grey(guide="none")+
  #scale_y_continuous(limits=c(0,500))+
  facet_wrap(.~Pollution,scales = "fixed",ncol=2)+
  #labs(title="Ratio Sens/Tol vs Pollution ext",subtitle = "",
  #     x="Pollution extent",colour="Dry ext.",linetype="Pollution")+
  theme_classic()

Res_nodes_DaFr %>% filter(Disp==0.15,Dry_ext==0.01) %>% pull(Permanence)


model_Data <- Res_nodes_DaFr %>% filter(Pollut_ext==0.01,
                                        Dry_ext==0.25,
                                        Dry_Pattern==0.37,
                                        Disp==1)
lm(S~Permanence,data = model_Data)
  
Res_nodes_DaFr %>% filter(Disp==0.15,Pollut_ext>0.009) %>%
  ggplot()+
  geom_raster(aes(x=Pollut_ext ,y=Dry_ext,fill=S_Sen))+facet_wrap(.~Dry_Pattern)


Res_nodes_DaFr



  geom_jitter(aes(x=Permanence ,y=S_Sen,colour=Pollution), alpha=0.1)+
  #geom_smooth(aes(x=STcon_AAct ,y=S_AAct,colour=Pollution),method="loess")+
  labs(title="Dispersals",subtitle = "Dry_extent",y="")+facet_wrap(Dry_ext~Dry_Pattern)+
  theme_classic()

Res_nodes_DaFr%>% filter(Disp==0.) %>% ggplot()+
  geom_point(aes(x=))








# This is the last last part and is just ot "plot" the results as we saw for the google shit : )
plot_list <- list()
for (plottt in 1:10) {
  plot_list[[plottt]] <- Res_nodes_DaFr %>% filter(Pol_Scen==as.character(plottt)) %>% 
    select(-c(5:14)[-plottt]) %>% rename("Pollution"=colnames(Res_nodes_DaFr %>% dplyr::select(c(5:14)[plottt]))) %>% 
    ggplot()+geom_boxplot(aes(x=as.factor(Permanence),y=S,fill=Pollution))+
    facet_grid(.~as.factor(D50))
  
}
grid.arrange(
  plot_list[[1]],plot_list[[2]],plot_list[[3]],plot_list[[4]],plot_list[[5]],
  plot_list[[6]],plot_list[[7]],plot_list[[8]],plot_list[[9]],plot_list[[10]],
  ncol=2)




