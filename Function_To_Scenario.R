
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

diff_extent <- c(0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.75,0.9,1)
diff_extent <- tidyr::crossing(diff_extent, diff_extent,diff_extent)
diff_extent <- diff_extent %>% rename(Dry_Ext=diff_extent...1,
                                      Pollut_Ext=diff_extent...2,
                                      Dry_Pattern_Beg=diff_extent...3)

diff_extent <- diff_extent %>% filter(Dry_Pattern_Beg%in%c(0.1))

Orig_dispersal_pollution <- read.csv2("pollution_dispersal.csv") %>% drop_na()

plots_diagnosis <- list()
output_to_simulate <- list()
Flow_DB_toSTcon <- list()
for (diff_extent_value in 1:nrow(diff_extent)) {
Dry_extent <- diff_extent$Dry_Ext[diff_extent_value]
Poll_extent <- diff_extent$Pollut_Ext[diff_extent_value]

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
# Distribution - corresponds to the specific nodes that we want to submit to drying (e.g. only upstream)
# skeweed_distr is a modulator for "skewing" the distribution of durations. If all values are 1 it means 
# that there is the same probability to get any of the duration (it can be useful in case we would like to
# favor some of the values)
source("Function_to_dry.R")
#duration_for_function <- seq(from=pull(diff_extent[diff_extent_value,3]),to=1,length.out=6)
duration_for_function <- seq(diff_extent$Dry_Pattern_Beg[diff_extent_value],to=0.9,length.out=6)
Flow_DB <- function_to_dry(River_nodes = nodes_DaFr,
                           years =Years_Of_Drying,days =F,
                           duration =duration_for_function,
                           duration_constancy = F,
                           extent =as.numeric(Dry_extent),
                           distribution =NULL,
                           skeweed_distr = c(3,2,2,1,1,0.5))#rep(1,6))

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
Spp_tolerance <- 1.01-(Orig_dispersal_pollution$IBMWP_score/10)

# This function is very similar to the Dry but with less features (distribution is the same)
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

nodes_DaFr <- nodes_DaFr %>% mutate("Dry_Pattern"=round(sd(duration_for_function),2))# merge scenarios and nodes
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

# 4. Pollution + Drying  ####
# Rescaling filters according to drying
drying_ranges <- unique(nodes_DaFr$Permanence)[order(unique(nodes_DaFr$Permanence))]
Poll_Dry_Inter <- drying_ranges/max(drying_ranges)
Poll_Dry_Inter <- ifelse(Poll_Dry_Inter==1,0.99,Poll_Dry_Inter)
Poll_Dry_Inter <- ifelse(Poll_Dry_Inter==0,0.0001,Poll_Dry_Inter)
for (dry_range in 1:length(drying_ranges)) {
if (drying_ranges[dry_range]==0) {Modif_Spp_tolerance <- rep(0,length(Spp_tolerance))}else{
Modif_Spp_tolerance <- Spp_tolerance^(drying_ranges[dry_range]/max(nodes_DaFr$Permanence))}
Dry_and_Poll_Spp_tolerance <- scales::rescale(Modif_Spp_tolerance,to=c(min(Spp_tolerance),max(Spp_tolerance)))
filter_Pollution[,which(Pollution=="YES_Poll" & nodes_DaFr$Permanence==drying_ranges[dry_range])] <- Dry_and_Poll_Spp_tolerance
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
        legend.position = "right"),
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
        legend.position = "right"),
top= paste("Scenario",Dry_extent,Poll_extent,sep="_"),ncol=1)


plots_temp <- list(gridExtra::arrangeGrob(Plot_A,Plot_B,Plot_C,ncol=3),plot_D)
plots_diagnosis[[diff_extent_value]] <- plots_temp
output_to_simulate[[diff_extent_value]] <- list(filter_Pollution,nodes_DaFr)
Flow_DB_toSTcon[[diff_extent_value]] <- Flow_DB
}# Diff

# png(filename = "Scenario.png",width = 6000,height = 6500,units = "px",res =300)
# gridExtra::grid.arrange(
# plots_diagnosis[[1]][[1]],plots_diagnosis[[6]][[1]],
# plots_diagnosis[[12]][[1]],plots_diagnosis[[15]][[1]],
# plots_diagnosis[[19]][[1]],plots_diagnosis[[25]][[1]],nrow=6)
# dev.off()
# 
# png(filename = "Scenario_SppFilt.png",width = 6000,height = 2000,units = "px",res =300)
# gridExtra::grid.arrange(
#   plots_diagnosis[[1]][[2]],plots_diagnosis[[6]][[2]],
#   plots_diagnosis[[12]][[2]],plots_diagnosis[[15]][[2]],
#   plots_diagnosis[[19]][[2]],plots_diagnosis[[25]][[2]],ncol=6)
# dev.off()
# 
# grid.arrange(plots_diagnosis[[102]][[2]])
# output_to_simulate[[102]][[1]]
# output_to_simulate[[102]][[2]][c(90,139),]

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
Dist_matr <- distances(g)#((as.matrix(dist(nodes_DaFr[,3:4])))*as.matrix(ocn_TEST$FD$W))
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
save(list = "Riv_Drift",file = "Riv_Drift_STcon.RData")

Scen_Drift_STconmat <- list()
for (scen in 1:(length(Riv_Drift$STconmat)-1)) {
  Drift_STconmat <-Riv_Drift$STconmat[[scen]]#/Riv_Drift$STconmat[[length(Int_dataset)]]
  #to construct a diagonal matrix
  diag(Drift_STconmat) <- 1
  # if Riv_STconmat is 0, write 10000, otherwise write Riv_STconmat
  Scen_Drift_STconmat[[scen]] <-ifelse(Drift_STconmat==0,100,Drift_STconmat)
  #Scen_Drift_STconmat[[scen]] <-ifelse(is.nan(Drift_STconmat)==T,100,Drift_STconmat)
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
save(list = "Riv_Swim",file = "Riv_Swim_STcon.RData")

Scen_Swim_STconmat <- list()
for (scen in 1:(length(Riv_Swim$STconmat)-1)) {
  Swim_STconmat <-Riv_Swim$STconmat[[scen]]#/Riv_Swim$STconmat[[length(Int_dataset)]]
  #to construct a diagonal matrix
  diag(Swim_STconmat) <- 1
  # if Riv_STconmat is 0 or NaN, write 10000, otherwise write Riv_STconmat
  Scen_Swim_STconmat[[scen]] <-ifelse(Swim_STconmat==0,100,Swim_STconmat)
  #Scen_Swim_STconmat[[scen]] <-ifelse(is.nan(Swim_STconmat)==T,100,Swim_STconmat)
}

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
save(list = "Riv_AerAct",file = "Riv_AerAct_STcon.RData")

Scen_AAct_STconmat <- list()
for (scen in 1:(length(Riv_AerAct$STconmat)-1)) {
  AAct_STconmat <-Riv_AerAct$STconmat[[scen]]#/Riv_AerAct$STconmat[[length(Int_dataset)]]
  #to construct a diagonal matrix
  diag(AAct_STconmat) <- 1
  # if Riv_STconmat is 0 or NaN, write 10000, otherwise write Riv_STconmat
  Scen_AAct_STconmat[[scen]] <-ifelse(AAct_STconmat==0,100,AAct_STconmat)
  #Scen_AAct_STconmat[[scen]] <-ifelse(is.nan(AAct_STconmat)==T,100,AAct_STconmat)
}
tictoc::toc()
# 81057.89

summary(as.vector(Scen_AAct_STconmat[[3]]))
summary(as.vector(Scen_Swim_STconmat[[3]]))
summary(as.vector(Scen_Drift_STconmat[[3]]))

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
J.max<-300+Jmin # What is the maximum J that we want to assign to a community
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

dispersal_test <- c(0.15,1) # We set the dispersal abilities that we want

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
FW_area <- output_to_simulate[[10]][[2]]$Permanence#*output_to_simulate[[pollut]][[2]]$weight
J.freshwater<-ceiling((-Jmin+(J.max/(Max_Area^b.ef))*FW_area^b.ef))
plot(FW_area,J.freshwater)
                    
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
save(Diff_scenarios,file = "Diff_scenarios_Good.RData")
save(output_to_simulate, file="Raw_Data_To_Simul_Good.RData")
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
                                    "Mean_IBMWP"=Mean_IBMWP,
                                    "IBMWP"=IBMWP,
                                    "S"=S_site,"B"=B_site))
  Res_nodes_DaFr <- bind_rows(Res_nodes_DaFr,Result_df)
}
save(Res_nodes_DaFr,file="Results_Good_scenarios.RData")


colnames(Res_nodes_DaFr)
Res_nodes_DaFr %>% filter(Disp==0.15,Dry_ext==0.01) %>% 
  ggplot(aes(x=Mean_STcon,y=Ratio_S.T, colour=as.factor(Dry_ext)))+
  geom_jitter(height = 0,width = 0,alpha=0.01)+
  geom_smooth(aes(linetype=as.factor(Pollut_ext)),method="loess",se=F)+
  scale_color_viridis(discrete = T)+
  labs(title="Dispersals",subtitle = "Pollution extent",
       colour="Dry ext.",linetype="Poll ext.")+
  #facet_wrap(.~Pollution,scales = "free",ncol=3)+
  theme_classic()


Ref.value <- Res_nodes_DaFr %>% filter(Disp==0.15,Dry_ext==0.01) %>% pull(Ratio_S.T)
  
Res_nodes_DaFr %>% filter(Disp==0.15) %>%
  #mutate(Ref.value=rep(Meh,10)) %>% 
  ggplot(aes(x=as.numeric(Pollut_ext),y=Ratio_S.T, colour=as.factor(Dry_ext)))+
  geom_jitter(height = 0,width = 0,alpha=0.01)+
  geom_hline(yintercept=1)+
  geom_smooth(aes(x=as.numeric(Pollut_ext)),method="loess",se=F)+
  scale_color_viridis(discrete = T)+
  scale_fill_grey(guide="none")+
  labs(title="Dispersals",subtitle = "Pollution extent",
       colour="Dry ext.",linetype="Pollution")+
  theme_classic()




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




