
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

diff_extent <- c(0.01,0.1,0.2,0.3,0.5,0.75,0.9,1)
diff_extent <- tidyr::crossing(diff_extent, diff_extent) 

Orig_dispersal_pollution <- read.csv2("pollution_dispersal.csv") %>% drop_na()



plots_diagnosis <- list()
output_to_simulate <- list()
Flow_DB_toSTcon <- list()
for (diff_extent_value in 1:nrow(diff_extent)) {
Dry_extent <- diff_extent[diff_extent_value,1]
Poll_extent <- diff_extent[diff_extent_value,2]

nodes_DaFr <- data.frame(
  "Scenario"=paste(Dry_extent,Poll_extent,sep="_"),
  "Site_ID"=1:length(ocn_TEST$FD$X),
  "x"=ocn_TEST$FD$X,
  "y"=ocn_TEST$FD$Y,
  "weight"=ocn_TEST$FD$A)
# We plot our beloved river colored according to the weight (order)/community size
Plot_A <- ggplot()+
  geom_segment(data=edges_DaFr, aes(x=X1_coord,y=Y1_coord, xend=X2_coord, yend=Y2_coord), 
               arrow =arrow(length=unit(0.01,"cm"), ends="last"), linewidth=0.2, colour="grey50", alpha=1)+
  geom_point(data=nodes_DaFr, aes(x=x, y=y,fill=weight/120000,size=weight/120000), shape=21)+ # here weight/12000 is random decision for plotting
  scale_fill_viridis(option = "D",discrete = F)+
  scale_size(guide = "none") +
  labs(title = paste("Scenario",Dry_extent,Poll_extent,sep="_"),
       y="",x="",fill="Comm. Size")+
  theme_void()


# 2. Drying pattern  ####
# Drying patterns are incorporated following different criteria
# Temporal - years or days time span
# Duration can either be a single value or vector
## the values must range from 0 to 1 which correspond to the % of "time" that each site is drying
# Duration_constancy - responds to making the drying pattern "yearly" and repeating every year
## if FALSE it it will assign the corresponding drying days randomly through the whole time period (random drying)
# Extect - corresponds to the % of selected nodes that will be drying
# Distribution - corresponds to the number  

source("Function_to_dry.R")
Flow_DB <- function_to_dry(River_nodes = nodes_DaFr,
                           years =1,days =F,
                           duration =c(0.1,0.2,0.3,0.5,0.6,1),
                           duration_constancy = T,
                           extent =as.numeric(Dry_extent),
                           distribution =NULL,
                           skeweed_distr = rep(1,6))

# We add a factor called "Permanence to the "nodes" to plot
nodes_DaFr <- nodes_DaFr %>% mutate("Permanence"=apply(Flow_DB[,2:ncol(Flow_DB)],2,sum))

# We plot our beloved river colored according to the weight (order)/community size
Plot_B <- ggplot()+geom_segment(data=edges_DaFr, aes(x=X1_coord,y=Y1_coord, xend=X2_coord, yend=Y2_coord),
                      arrow =arrow(length=unit(0.01,"cm"), ends="last"), linewidth=0.2, colour="grey50", alpha=1)+
  geom_point(data=nodes_DaFr, aes(x=x, y=y,
                                  fill=Permanence,
                                  size=weight/120000), shape=21)+
  scale_fill_viridis(option = "D",discrete = F,direction = -1)+
  scale_size(guide = "none") +
  labs(title = paste("Scenario",Dry_extent,Poll_extent,sep="_"),y="",x="",fill="Permanence")+
  theme_void()



# 3. Pollutino assignation  ####

#Spp_tolerance <- c(rep(0.5,50),rep(0.65,50),rep(0.75,50),rep(0.8,50))
Spp_tolerance <- 1-(Orig_dispersal_pollution$IBMWP_score/10)

source("Function_to_Pollute.R")
filter_Pollution <- function_to_pollute(River_nodes =nodes_DaFr,
                                        Spp_tolerance =Spp_tolerance ,
                                        extent =as.numeric(Poll_extent),
                                        distribution =NULL)

# A way to detect polluted sites is by looking at which sites the "filter" does not meet the condition of all 0.99
Pollution <- rep("Non_Poll",nrow(nodes_DaFr))# replicate "NonPoll" as the row number of nodes
Polluted_Sites <- which(apply(filter_Pollution,2,sum)!=sum(rep(0.99,length(Spp_tolerance))))
Pollution[Polluted_Sites] <- "YES_Poll" #write "YES_Poll" to polluted sites (which were randomly selected above)

nodes_DaFr <- nodes_DaFr %>% mutate("Pollution"=Pollution)# merge scenarios and nodes
# We plot our beloved river colored according to the weight (order)/community size
Plot_C <- ggplot()+
  geom_segment(data=edges_DaFr, aes(x=X1_coord,y=Y1_coord, xend=X2_coord, yend=Y2_coord),
                      arrow =arrow(length=unit(0.01,"cm"), ends="last"), linewidth=0.2, colour="grey50", alpha=1)+
  geom_point(data=nodes_DaFr, aes(x=x, y=y,
                                  fill=Permanence,
                                  size=weight/120000,
                                  shape=Pollution,
                                  colour=Pollution))+
  scale_fill_viridis(option = "D",discrete = F,direction = -1)+
  scale_color_manual(values = c("black","red"))+
  scale_shape_manual(values=c(21,22))+
  scale_size(guide = "none") +
  labs(title = paste("Scenario",Dry_extent,Poll_extent,sep="_"),y="",x="",fill="Permanence")+
  theme_void()

# 4. Pollutino + Drying  ####
# Rescaling filters according to drying
drying_ranges <- unique(nodes_DaFr$Permanence)[order(unique(nodes_DaFr$Permanence))]
Poll_Dry_Inter <- drying_ranges/max(drying_ranges)
Poll_Dry_Inter <- ifelse(Poll_Dry_Inter==1,0.99,Poll_Dry_Inter)
Poll_Dry_Inter <- ifelse(Poll_Dry_Inter==0,0.0001,Poll_Dry_Inter)
for (dry_range in 1:length(drying_ranges)) {
filter_Pollution[,which(Pollution=="YES_Poll" & nodes_DaFr$Permanence==drying_ranges[dry_range])] <- scales::rescale(Spp_tolerance,to=c((Poll_Dry_Inter[dry_range]/10),Poll_Dry_Inter[dry_range]))
}

filter_Pollution_test <- t(filter_Pollution)
colnames(filter_Pollution_test) <- paste(seq(1:length(Spp_tolerance)),"Spe",sep="_")

plot_D <- gridExtra::arrangeGrob(
nodes_DaFr %>% 
  bind_cols(as.data.frame(filter_Pollution_test)) %>% 
  pivot_longer(cols=8:ncol(.)) %>% 
  mutate(name=factor(name,levels = paste(seq(1:length(Spp_tolerance)),"Spe",sep="_"))) %>% 
  ggplot()+
  geom_tile(aes(y=as.factor(name),x=Site_ID,fill=value))+
  scale_fill_viridis(direction = -1)+
  theme_classic()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none"),
nodes_DaFr %>% 
  bind_cols(as.data.frame(filter_Pollution_test)) %>% 
  pivot_longer(cols=8:ncol(.)) %>% 
  mutate(name=factor(name,levels = paste(seq(1:200),"Spe",sep="_"))) %>% 
  ggplot()+
  geom_tile(aes(y=as.factor(name),x=Site_ID,fill=Permanence))+
  scale_fill_viridis(option = "A",direction = -1)+
  theme_classic()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none"),
top= paste("Scenario",Dry_extent,Poll_extent,sep="_"),ncol=1)


plots_temp <- list(gridExtra::arrangeGrob(Plot_A,Plot_B,Plot_C,ncol=3),plot_D)
plots_diagnosis[[diff_extent_value]] <- plots_temp
output_to_simulate[[diff_extent_value]] <- list(filter_Pollution,nodes_DaFr)
Flow_DB_toSTcon[[diff_extent_value]] <- Flow_DB
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

# 5. STconmat calculation  ####
# Remember that STcon is able to calculate several rivers at the same time! So you just need to have a list object with the 3 elements 
# for each river scenario: 

# Intermittence database
Flow_DB_toSTcon[[length(Flow_DB_toSTcon)+1]] <- ifelse(Flow_DB_toSTcon[[1]]==0,1,Flow_DB_toSTcon[[1]])
Int_dataset <- Flow_DB_toSTcon
# Sites coordinates
Sit_coordinates <- nodes_DaFr[,c(1:4)]
Sit_coordinates <- replicate(length(Int_dataset), Sit_coordinates, simplify = FALSE)
# Network structure
Net_stru <- as.matrix(ocn_TEST$FD$W)
Net_stru <- replicate(length(Int_dataset), Net_stru, simplify = FALSE)
# Distance matrix
Dist_matr <- ((as.matrix(dist(nodes_DaFr[,3:4])))*as.matrix(ocn_TEST$FD$W))
Dist_matr <- replicate(length(Int_dataset), Dist_matr, simplify = FALSE)

tictoc::tic()
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
#save(list = "Riv_Drift",file = "Riv_Drift_STcon.RData")

Scen_Drift_STconmat <- list()
for (scen in 1:(length(Int_dataset)-1)) {
  Drift_STconmat <-Riv_Drift$STconmat[[scen]]/Riv_Drift$STconmat[[length(Int_dataset)]]
  #to construct a diagonal matrix
  diag(Drift_STconmat) <- 0
  # if Riv_STconmat is 0, write 10000, otherwise write Riv_STconmat
  Scen_Drift_STconmat[[scen]] <-ifelse(is.nan(Drift_STconmat)==T,50,Drift_STconmat)
}

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
#save(list = "Riv_Swim",file = "Riv_Swim_STcon.RData")

Scen_Swim_STconmat <- list()
for (scen in 1:(length(Int_dataset)-1)) {
  Swim_STconmat <-Riv_Swim$STconmat[[scen]]/Riv_Swim$STconmat[[length(Int_dataset)]]
  #to construct a diagonal matrix
  diag(Swim_STconmat) <- 0
  # if Riv_STconmat is 0 or NaN, write 10000, otherwise write Riv_STconmat
  Scen_Swim_STconmat[[scen]] <-ifelse(is.nan(Swim_STconmat)==T,50,Swim_STconmat)
}
tictoc::tic()
# Network structure
Distances <- as.matrix(dist(nodes_DaFr[,3:4]))
Net_stru <- as.matrix(ocn_TEST$FD$W)
Net_stru <- ifelse(Distances>0,1,0)
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
#save(list = "Riv_AerAct",file = "Riv_AerAct_STcon.RData")
tictoc::toc()
Scen_AAct_STconmat <- list()
for (scen in 1:(length(Int_dataset)-1)) {
  AAct_STconmat <-Riv_AerAct$STconmat[[scen]]/Riv_AerAct$STconmat[[length(Int_dataset)]]
  #to construct a diagonal matrix
  diag(AAct_STconmat) <- 0
  # if Riv_STconmat is 0 or NaN, write 10000, otherwise write Riv_STconmat
  Scen_AAct_STconmat[[scen]] <-ifelse(is.nan(AAct_STconmat)==T,50,AAct_STconmat)
}
tictoc::toc()


source("H2020_Lattice_expKernel_Jenv_TempMeta_DispStr.R")
###__________________________________
### WE BEGUN TO built all the data that we need to run the simulation
## Distance matrix 
# It is equivalent to the Riv_STconmat 

## Community size
# Area total is the weight. But we must convert it to not take too long computation times 
FW_area <- nodes_DaFr$weight
# We define parameters of Community area to transform weight to community size (J)
Max_Area<-max(FW_area)
Jmin <- 50 # What will be the minimum size at which we will consider "0" 
J.max<-600+Jmin # What is the maximum J that we want to assign to a community
b.ef<-0.3 # The coefficient of change. If 1 we do a direct proportion between the two but minimimum becomes "1" (only 1 indiv)
# J creation
J.freshwater<-ceiling((-Jmin+(J.max/(Max_Area^b.ef))*FW_area^b.ef))

# Species tolerances defined in the 2. script
output_to_simulate[[1]][[1]] # Filter of species per site. We will use for tolerance
# Other parameters of the model
id_NOmodule <- rep(1,nrow(nodes_DaFr)) # Modules if we want some sites to belong to the same module. 
pool_200 <- rep(1,nrow(filter_Pollution)) # Distribution of the species pool #rlnorm(n = 200,5,1) 
Meta_t0 <- matrix(nrow = length(pool_200), ncol =nrow(nodes_DaFr), 1) #Previous Metacommunity (for considering time relevance)

# Distances are related to the distance matrix. 
# Therefore we must see which values correspond to our connections to set the "D50". Dispersal distance is
# considered here as the distance at which probability of dispersal is 0.5. So "higher" or "lower" do not exclude
# other dispersal abilities. They push "overall connectivity" towards higher or lower connections.
summary(as.vector(Dist_Matrix[[1]])[-which(as.vector(Dist_Matrix[[1]])==50)]) 

dispersal_test <- c(0.5,3) # We set the dispersal abilities that we want

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
a <- NULL # We create an output object for each iteration
#b <- list()
for (it in 1:5) { # We repeat 10 times the same process
  output <- H2020_Coalescent.and.lottery.exp.Kernel.J_TempMtcom_tempIT(
    Meta.pool = pool_200, # Species pool
    m.pool = 0.001, # Regional dispersal which is always constant 
    Js = J.freshwater, # Size of the communities (AKA: number of individuals/population contained in each community)
    id.module = id_NOmodule, # id of modules if there are some - NOT used for us
    filter.env = output_to_simulate[[pollut]][[1]], # Pollution scenarios (created at 2. Pollution assignation.R)
    Disp_Strat=Disp_Str,
    M.dist =list(Scen_Drift_STconmat[[pollut]],Scen_Swim_STconmat[[pollut]],Scen_AAct_STconmat[[pollut]]), # Distance matrix which corresponds to the STconmat (created at 1. OCnet - STconmat.R)
    D50 = dispersal_test[dispersal], # Dispersal distance scenario 
    m.max = 1, # Maximum migration
    tempo_imp = 0, # Relvance of "temporal" effect
    temp_Metacom = Meta_t0, # Metacommunity at time 0 (all species are equally favored)
    temp_it = 0, # Number of temporal iterations
    id.fixed=NULL, D50.fixed=0, m.max.fixed=0, comm.fixed=pool_200, # If there are some communit. that should be fixed
    Lottery=T, 
    it=100, 
    prop.dead.by.it=0.07, # Lottery parameters, nº iterations and proportion of dead organisms  
    id.obs=1:nrow(nodes_DaFr)) # Information if we would like to keep specific results only
  a <- rbind(a,output[[1]])
  #b[[it]] <- output[[2]]
}
resume.out(a)
}
toc()

# > <
# This part below extracts the S (richness) and B (average Jaccard) from each site and "data frame" it 
Res_nodes_DaFr <- data.frame()
for (round in 1:(25*2)) {
  round_value <- round
  if (round>25 & round<51) {round_value <- round_value-25}
  if (round>50 & round<76) {round_value <- round_value-50}

  #print(Diff_scenarios[[round]][1:9])
  S_site<- Diff_scenarios[[round]][10:(nrow(output_to_simulate[[round_value]][[2]])+9)]
  B_site <- Diff_scenarios[[round]][(nrow(output_to_simulate[[round_value]][[2]])+10):((nrow(output_to_simulate[[round_value]][[2]])*2)+9)]
  #Out$Median[((nrow(nodes_DaFr)*2)+10):length(Out$Median)]
  S_sen <- Diff_scenarios[[round]][((nrow(output_to_simulate[[round_value]][[2]])*2)+13)]
  S_tol <- Diff_scenarios[[round]][((nrow(output_to_simulate[[round_value]][[2]])*2)+14)]

  S_Drift <- Diff_scenarios[[round]][(10+nrow(nodes_DaFr)+nrow(nodes_DaFr)):(10+nrow(nodes_DaFr)+nrow(nodes_DaFr))]
  S_Swim <- Diff_scenarios[[round]][(10+nrow(nodes_DaFr)+nrow(nodes_DaFr)):(10+nrow(nodes_DaFr)+nrow(nodes_DaFr))+1]
  S_AAct <- Diff_scenarios[[round]][(10+nrow(nodes_DaFr)+nrow(nodes_DaFr)):(10+nrow(nodes_DaFr)+nrow(nodes_DaFr))+2]
  #if(round<10){Pol_level <- as.character(round)}else{Pol_level <- strsplit(x = as.character(round),split = "")[[1]][2]}
  #if(Pol_level=="0"){Pol_level <- "10"}
  #
  
  STcon_Drift <- Riv_Drift$STcon[[scen]]/Riv_Drift$STcon[[length(Int_dataset)]]
  STcon_Swim <- Riv_Swim$STcon[[scen]]/Riv_Swim$STcon[[length(Int_dataset)]]
  STcon_AAct <- Riv_AerAct$STcon[[scen]]/Riv_AerAct$STcon[[length(Int_dataset)]]
  
  STcon_Drift[which(is.nan(STcon_Drift))] <- 1
  Result_df <- bind_cols(output_to_simulate[[round_value]][[2]] %>%
                         data.frame("Dry_ext"= strsplit(unique(output_to_simulate[[round_value]][[2]]$Scenario),split = "_")[[1]][1],
                                    "Pollut_ext"=strsplit(unique(output_to_simulate[[round_value]][[2]]$Scenario),split = "_")[[1]][2],
                                    "Disp"=as.numeric(Diff_scenarios[[round]][4]),
                                    "STcon_Drift"=STcon_Drift,
                                    "STcon_Swim"=STcon_Swim,
                                    "STcon_AAct"=STcon_AAct,
                                    "Mean_STcon"=apply(cbind(STcon_Drift,STcon_Swim,STcon_AAct),1,mean),
                                    "Ratio_S.T"=S_sen/S_tol,
                                    "S_Tol"=S_tol,
                                    "S_Sen"=S_sen,
                                    "S_Drift"=S_Drift,"S_Swim"=S_Swim,"S_AAct"=S_AAct,
                                    "S"=S_site,"B"=B_site))
  Res_nodes_DaFr <- bind_rows(Res_nodes_DaFr,Result_df)
}

png(filename = "RiverScenario.png",width = 7000,height = 7000,units = "px",res =300)
Res_nodes_DaFr %>% 
  ggplot()+
  geom_segment(data=edges_DaFr, aes(x=X1_coord,y=Y1_coord, xend=X2_coord, yend=Y2_coord), 
               arrow =arrow(length=unit(0.01,"cm"), ends="last"), linewidth=0.2, colour="grey50", alpha=1)+
  geom_point(aes(x=x, y=y,fill=S,size=S, shape=Pollution,colour=Pollution))+
  scale_shape_manual(values = c(21,22))+
  scale_color_manual(values=c("black","red"))+
  scale_fill_viridis(direction = -1)+
  facet_grid(as.factor(Dry_ext)~as.factor(Pollut_ext))
dev.off()

png(filename = "Mean_S.png",width = 2000,height = 2000,units = "px",res =300)
Res_nodes_DaFr %>% 
  group_by(Dry_ext,Pollut_ext,Disp) %>% summarise(Mean_S=mean(S)) %>% 
  ggplot()+
  geom_tile(aes(x=as.factor(Dry_ext),y=as.factor(Pollut_ext),fill=Mean_S))+
  scale_fill_viridis(direction = -1)+facet_grid(Disp~.)
dev.off()

png(filename = "SurfaceRatio.png",width = 2000,height = 2000,units = "px",res =300)
Res_nodes_DaFr %>% 
  group_by(Dry_ext,Pollut_ext,Disp) %>% summarise(Mean_Ratio=mean(Ratio_S.T)) %>% 
  ggplot()+
  geom_raster(aes(x=as.factor(Dry_ext),y=as.factor(Pollut_ext),fill=Mean_Ratio),interpolate = T)+
  scale_fill_viridis(direction = -1)+facet_grid(Disp~.)
dev.off()

png(filename = "Ratio_Plots.png",width = 2000,height = 2000,units = "px",res =300)
Res_nodes_DaFr %>% 
  group_by(Dry_ext,Pollut_ext,Disp) %>% summarise(Mean_Ratio=mean(Ratio_S.T))%>%ungroup() %>% 
  mutate_all(as.numeric) %>% 
  ggplot()+
  geom_line(aes(x=Dry_ext,y=Mean_Ratio,colour=as.factor(Pollut_ext)),linewidth=1)+
  geom_point(aes(x=Dry_ext,y=Mean_Ratio,fill=as.factor(Pollut_ext)),shape=21,size=3)+
  scale_fill_viridis(direction = -1,discrete = T)+
  scale_colour_viridis(direction = -1,discrete = T)+
  facet_grid(Disp~.)+geom_hline(yintercept = 1,col="black")+
  scale_y_continuous(limits=c(0,1.4))+theme_classic()
dev.off()

colnames(Res_nodes_DaFr)
Res_nodes_DaFr %>% filter(Disp==0.5) %>% 
  ggplot()+
  geom_point(aes(x=Pollut_ext ,y=S_Sen),colour="green")+
  geom_point(aes(x=Pollut_ext ,y=S_Tol),colour="red")+
  facet_wrap(.~Dry_ext)

gridExtra::grid.arrange(
Res_nodes_DaFr %>% filter(Disp==0.5) %>% 
ggplot()+geom_point(aes(x=Pollut_ext ,y=S))+
labs(title="Richness",subtitle = "Dry_extent",y="")+facet_wrap(.~Dry_ext,nrow=1),

Res_nodes_DaFr %>% filter(Disp==0.5) %>% 
ggplot()+
geom_point(aes(x=Pollut_ext ,y=S_Sen),colour="darkgreen")+geom_point(aes(x=Pollut_ext ,y=S_Tol),col="darkred")+
labs(title="Sen vs Tol",subtitle = "Dry_extent",y="")+facet_wrap(.~Dry_ext,nrow=1),

Res_nodes_DaFr %>% filter(Disp==0.5) %>%
ggplot()+
geom_point(aes(x=Pollut_ext ,y=S_Drift),colour=viridis(3)[1])+
geom_point(aes(x=Pollut_ext ,y=S_Swim),colour=viridis(3)[2])+
geom_point(aes(x=Pollut_ext ,y=S_AAct),colour=viridis(3)[3])+
labs(title="Dispersals",subtitle = "Dry_extent",y="")+facet_wrap(.~Dry_ext,nrow=1))












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




