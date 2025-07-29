
#________________________###
#________________________###
# 1. FAKE RIVER CREATION ####
#________________________###
#________________________###

# With OCNet we create a river with a determined dendritic structure
library(OCNet)

# With the following function we create a river of a determined size and charactersitics: 
# Cellsize sets the size of each cell (pixels) -- We can use this as a proxy of "community size"
# expEnergy is defining the "branching" of the river in accordance with the size
ocn_TEST <- create_OCN(dimX = 15,dimY = 15,nOutlet = 1,cellsize = 2, expEnergy=0.5)
# Easy function to draw the river
draw_simple_OCN(ocn_TEST)

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

#________________________###
#________________________###
# 2. SCENATIO DEFINITION ####
#________________________###
#________________________###

# We define the elements that we will later use for defining the major scenarios and calculate STcon values

# We define 4 years of drying as a temporal span (a total of 48 months)
Years_Of_Drying <- 4

# We define the different gradients that we will later combine to define the scenarios
# We combine 3 major elements Drying extent, Pollution extent and Drying pattern (the length and variation of drying)
diff_extent <- c(0.01,0.1,0.25,0.35,0.45,0.5,0.65,0.75,0.9)
diff_extent <- tidyr::crossing(Dry_Ext=diff_extent,
                               Pollut_Ext=diff_extent,
                               Dry_Pattern_End=diff_extent) %>% 
                filter(!Dry_Pattern_End%in%c(0.65))
nrow(diff_extent)

Flow_DB_toSTcon <- list()
Dry_nodes_DaFr <- nodes_DaFr
Dry_DataFrame_out <- list()

#________________________###
#________________________###
# 2.1 Drying extent+intensity  ####
#________________________###
#________________________###

# For consistency between scenarios, we define the sites that will be always selected to be dry as a function 
## of drying extent. 
Sites_to_Dry<- list()
Selected_Sites <- sample(nodes_DaFr$Site_ID,size = length(nodes_DaFr$Site_ID),replace = F)
for (Sit_To_Dry in 1:length(unique(diff_extent$Dry_Ext))) {
  Sites_to_Dry[[Sit_To_Dry]] <-Selected_Sites[1:ceiling(nrow(nodes_DaFr)*unique(diff_extent$Dry_Ext)[Sit_To_Dry])] 
}

# We now assign the drying properties to newtork nodes by defining the network extent (number of sites that will dry)
## and drying intensity, which corresponds to the length of drying for each nodes that can be bigger or smaller 
## as a function of "Dry_Pattern_End" (bigger drying times imply higher drying intensity).

for (diff_extent_value in 1:(length(unique(diff_extent$Dry_Ext))*length(unique(diff_extent$Dry_Pattern_End)))) {
Dry_extent <- 1 # We have to define this parameter for the "Function_to_Dry
Dry_diff_extent <- tidyr::crossing(Dry_Ext=unique(diff_extent$Dry_Ext),Dry_Pattern_End=unique(diff_extent$Dry_Pattern_End))
# Here we define how many sites will be selected to dry at each scenario, which corrresponds to the "Sites_to_Dry_Position"
Sites_to_Dry_Position <- which(unique(diff_extent$Dry_Ext)==as.numeric(Dry_diff_extent[diff_extent_value,1]))

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
source(file = paste0(getwd(),"/functions/Function_to_dry.R"))
# The "duration_for_function" defines the intensity of drying generating a sequence that ranges from 
## the Dry_Pattern_End" to 0.9 and that identifies how much variability (AKA intensity) will drying have. 
duration_for_function <- 1-seq(diff_extent$Dry_Pattern_End[diff_extent_value],to=0.9,length.out=6)

# We run the "function_to_dry"
Flow_DB <- function_to_dry(River_nodes = Dry_nodes_DaFr,
                           years =Years_Of_Drying,days =F,
                           duration =duration_for_function,
                           duration_constancy = F,
                           extent =as.numeric(Dry_extent),
                           distribution =Sites_to_Dry[[Sites_to_Dry_Position]],
                           skeweed_distr = rep(1,6))

# We add a factor called "Permanence to the "nodes" to plot which is the inverse of Drying
Dry_nodes_DaFr <- Dry_nodes_DaFr %>% mutate("Permanence"=apply(Flow_DB[,2:ncol(Flow_DB)],2,sum),
                                            "Drying"=nrow(Flow_DB)-apply(Flow_DB[,2:ncol(Flow_DB)],2,sum),
                                            "Dry_Patter"=sd(duration_for_function))

Dry_DataFrame_out[[diff_extent_value]] <- Dry_nodes_DaFr
Flow_DB_toSTcon[[diff_extent_value]] <- Flow_DB
}

#________________________###
#________________________###
# 2.2 Pollution extent   ####
#________________________###
#________________________###

# We load the database where pollution tollerances from the IBMWP
Orig_dispersal_pollution <- read.csv(file = paste0(getwd(), "/data/pollution_dispersal.csv")) %>% drop_na()

# For consistency between scenarios, we define the sites that will be always selected to be polluted as a function 
## of pollution extent. 
Sites_to_Pollut<- list()
Selected_Sites <- sample(nodes_DaFr$Site_ID,size = length(nodes_DaFr$Site_ID),replace = F)
for (Sit_To_Pol in 1:length(unique(diff_extent$Pollut_Ext))) {
  Sites_to_Pollut[[Sit_To_Pol]] <-Selected_Sites[1:ceiling(nrow(nodes_DaFr)*unique(diff_extent$Pollut_Ext)[Sit_To_Pol])] 
}

# We define a loop where we assign pollution values to all corresponding nodes as we did with Drying extent. The point here 
## is that polluted/non-polluted (refered as impactes in the ms) is a "state" and therefore does not have intensity. 

Poll_DataFrame_out <- list()
Poll_filter_Pollution <- list()

for (diff_extent_value in 1:length(unique(diff_extent$Pollut_Ext))) {
Poll_nodes_DaFr <- nodes_DaFr
Poll_extent <- 1 # We define this value that later we modify in the function (with the term "distribution")
  
Poll_diff_extent <- unique(diff_extent$Pollut_Ext)
Sites_to_Pol_Position <- which(unique(diff_extent$Pollut_Ext)==as.numeric(Poll_diff_extent[diff_extent_value]))
Spp_tolerance <- 1.01-(Orig_dispersal_pollution$IBMWP_score/10)

# This function is very similar to the Dry but with less features (distribution is the same). 
# It creates a matrix of "sites x species" where each combination of species and sites is assessed. The polluted sites will 
## have the "tolerance" values for each of the species while the non-impacted sites will feature the same value for each species. 

# This matrix is featured as a "performance" value that defines how establishment probability of one species in one site is 
## impacted by local properties of that site. If it is closer to 1, species establishment probability will only be modulated by 
## dispersal probabilities while if is closer to 0, the probability of establishment defined by dispersal will decrease correspondingly. 

# In this work, this probabilities are the corresponding Spp_tolerance values. Species that are very sensitive to pollution (Hihg IBMWP)
## will present low "performance" values at impacted sites while very tolerant species (low IBMWP) will not be punished in that sites. 
## Tolerant species will "take over" as pollution is gaining extention. 

source(file = paste0(getwd(),"/functions/Function_to_Pollute.R"))
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

# Checking lengths to make sure everythign matches
length(Dry_DataFrame_out);length(Flow_DB_toSTcon);length(Poll_DataFrame_out)

#________________________###
#________________________###
# 2.3 Combination and Pollution + Drying ####
#________________________###
#________________________###

# Finally, in this last step we combine the two variables that we have created so far. Drying (extent + intensity) and Pollution (impact)
## These elements (contained in respective list objects) are combined correspondingly for each scenario in order to create
## a final set of elements that will be used to: 
# - Calculate STconmat for each dispersal scenario (done in a cluster to optimize speed)
# - Run the coalescent models (considering STconmat values - Section 4 of this script)
# - Ensure a copy of all the elements into "Diagnostic_plots"

output_to_simulate <- list()
plots_diagnosis <- list()
for (diff_extent_value in 1:(length(Dry_DataFrame_out)*length(Poll_DataFrame_out))) {

# In the following lines we combine scenarios and make sure that each list elemnet is selected correspondingly and 
## combined with its counterpart.
## Here we combine Drying extent / Drying pattern (intensity) / Pollution extent
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

# We merge all elements into a last big dataframe
cat(Scen_Position,Sites_to_Pollut_Position, "\n") # Indicator of workflow and corresponding combination
DataFrame_out_Scen <- left_join(Dry_DataFrame_out[[Scen_Position]],Poll_DataFrame_out[[Sites_to_Pollut_Position]], 
                                by=c("Site_ID","x","y","weight")) %>% 
                      mutate(Scenario=paste(as.numeric(Merge_diff_extent[diff_extent_value,1]),
                                            as.numeric(Merge_diff_extent[diff_extent_value,3]),
                                            as.numeric(Merge_diff_extent[diff_extent_value,2]),sep="_"),.before="Site_ID")  

# We set the new filter for the pollution 
filter_Pollution <- Poll_filter_Pollution[[Sites_to_Pollut_Position]][[1]]
Pollution <- Poll_filter_Pollution[[Sites_to_Pollut_Position]][[2]]

# Re-scaling filters according to drying. Dry+Pollution increase the filter effects. Under dryign conditions, pollution effect
## will be strenghtened and therefore, sensitive species will be even more punished and tolerant species even more benefited. 
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

# Final steps and wrap-up 
# We print and save plots for each scenario  
Scenario_name <- paste(as.numeric(Merge_diff_extent[diff_extent_value,1]),
      as.numeric(Merge_diff_extent[diff_extent_value,3]),
      as.numeric(Merge_diff_extent[diff_extent_value,2]),sep="_")

# Plot A - Plot with river order 
Plot_A <- ggplot()+
  geom_segment(data=edges_DaFr, 
               aes(x=X1_coord,y=Y1_coord, xend=X2_coord, yend=Y2_coord), 
               arrow =arrow(length=unit(0.01,"cm"), ends="last"), linewidth=0.2, colour="grey50", alpha=1)+
  geom_point(data=nodes_DaFr, aes(x=x, y=y,fill=weight/120000,size=weight/120000), shape=21)+ # here weight/12000 is random decision for plotting
  scale_fill_viridis(option = "D",discrete = F)+
  scale_size(guide = "none") +
  labs(title = paste("River structure", Scenario_name),y="",x="",fill="Comm. Size")+
  theme_void()

# Plot B - Permanence indicate and alpha defined based on it 
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

# Plot C - Plot containing pollution
Plot_C <- edges_DaFr %>% left_join(Poll_DataFrame_out[[Sites_to_Pollut_Position]],by=c("from"="Site_ID")) %>% 
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

# Plot D - Site permanence and site pollutoin filter per each species 
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


# We print these images 
png(filename = "Figures/Scenario.png",width = 6000,height = 6500,units = "px",res =300)
gridExtra::grid.arrange(
plots_diagnosis[[1]][[1]],plots_diagnosis[[6]][[1]],
plots_diagnosis[[12]][[1]],plots_diagnosis[[15]][[1]],
plots_diagnosis[[19]][[1]],plots_diagnosis[[25]][[1]],nrow=6)
dev.off()

png(filename = "Figures/Scenario_SppFilt.png",width = 6000,height = 2000,units = "px",res =300)
gridExtra::grid.arrange(
 plots_diagnosis[[1]][[2]],plots_diagnosis[[6]][[2]],
 plots_diagnosis[[12]][[2]],plots_diagnosis[[15]][[2]],
 plots_diagnosis[[19]][[2]],plots_diagnosis[[25]][[2]],ncol=6)
dev.off()

save(plots_diagnosis,file = "RData_outputs/Diagnosis_Plots.RData")


#________________________###
#________________________###
# 3. STconmat calculation  ####
#________________________###
#________________________###

# We need to run STconmat in an external cluster in order to optimize the calculations (see Cluster_Code folder)

# Remember that STcon is able to calculate several rivers at the same time! So you just need to have a list object with the 3 elements 
# for each river scenario: 

# Important to save the entire image to later plot and use the simulations 
save.image(file="Cluster_Code/PreSTcon.RData")

# We save the objects to send to the SLURM running cluster. We save scenarios by packs of 15
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

# Run_STcon.R scripts contain the following lines of code to run the 3 dispersal networks (swimers, drifters and flyers) 

# After running in the server we reload the data and prepare it for the model 
# Some fast checking
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

# We substract STconmat to transform its values as a distance between sites 
Scen_Drift_STconmat <- list()
for (scen in 1:(length(Riv_Drift_Tot))) {
  Drift_STconmat <-Riv_Drift_Tot[[scen]]
  #to construct a diagonal matrix
  diag(Drift_STconmat) <- 1
  # if Riv_STconmat is 0, write 100, otherwise write Riv_STconmat
  Scen_Drift_STconmat[[scen]] <-ifelse(Drift_STconmat==0,100,Drift_STconmat)
}

Scen_Swim_STconmat <- list()
for (scen in 1:(length(Riv_Swim_Tot))) {
  Swim_STconmat <-Riv_Swim_Tot[[scen]]
  #to construct a diagonal matrix
  diag(Swim_STconmat) <- 1
  # if Riv_STconmat is 0 or NaN, write 100, otherwise write Riv_STconmat
  Scen_Swim_STconmat[[scen]] <-ifelse(Swim_STconmat==0,100,Swim_STconmat)
}

Scen_AAct_STconmat <- list()
for (scen in 1:(length(Riv_AerAct_Tot))) {
  AAct_STconmat <-Riv_AerAct_Tot[[scen]]
  #to construct a diagonal matrix
  diag(AAct_STconmat) <- 1
  # if Riv_STconmat is 0 or NaN, write 100, otherwise write Riv_STconmat
  Scen_AAct_STconmat[[scen]] <-ifelse(AAct_STconmat==0,100,AAct_STconmat)
}

# We check values distribution
summary(as.vector(Scen_AAct_STconmat[[3]]))
summary(as.vector(Scen_Swim_STconmat[[3]]))
summary(as.vector(Scen_Drift_STconmat[[3]]))

#________________________###
#________________________###
# 4. STconmat calculation  ####
#________________________###
#________________________###

# We charge the function to run coalescent models
source("H2020_Lattice_expKernel_Jenv_TempMeta_DispStr.R")

###__________________________________
### WE BEGUN TO built all the data that we need to run the simulation
## Distance matrix 
# It is equivalent to the Riv_STconmat (uploaded on step 3) 

## Community size
# Permanence time is deifned here as community space. THe more intermittent a site is the less carrying capicity it will have. 
## We must convert it to not take too long computation times. 
# We define parameters of Community area to transform permanence to community size (J)
Max_Area<-(Years_Of_Drying*12)
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

dispersal_test <- c(0.15) # We set the dispersal abilities what we want

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
FW_area <- output_to_simulate[[pollut]][[2]]$Permanence
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
save(Diff_scenarios,file = "RData_outputs/All_Simulation_Scenarios.RData")
save(output_to_simulate, file="RData_outputs/All_Scenarios_OutputToSimul.RData")
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
save(Res_nodes_DaFr,file="RData_outputs/All_Results_scenarios.RData")








