
# Charge the function to run the coalescent model
#source("H2020_Lattice_expKernel_Jenv_TempMeta.R")
#to read for reproducibility
source(file = paste0(getwd(),"/H2020_Lattice_expKernel_Jenv_TempMeta.R"))

# Intermitence database
Int_dataset <- list(Flow_DB)
# Sites coordinates
Sit_coordinates <- list(nodes_DaFr[,c(3,3,1:2)])
# Network structure
Net_stru <- list(as.matrix(ocn_TEST$FD$W))

# Main information from our habitats
edges_DaFr
nodes_DaFr

# Connectivity based on drying patterns and distance
## High values imply greater difficulty to be reached. 
## Small values imply sites easily reachable
Riv_STconmat

###__________________________________
### WE BEGUN TO built all the data that we need to run the simulation

## Distance matrix 
# It is equivalent to the Riv_STconmat 
Dist_Matrix <- Riv_STconmat

## Community size
# Area total is the weight. But we must convert it to not take too long computation times 
FW_area <- nodes_DaFr$weight
  
# We define parameters of Community area to transform weight to community size (J)
Max_Area<-max(FW_area)
Jmin <- 0 # What will be the minimum size at which we will consider "0" 
J.max<-400+Jmin # What is the maximum J that we want to assign to a community
b.ef<-0.5 # The coefficient of change. If 1 we do a direct proportion between the two but minimimum becomes "1" (only 1 indiv)

# J creation
J.freshwater<-ceiling((-Jmin+(J.max/(Max_Area^b.ef))*FW_area^b.ef))
plot(J.freshwater,FW_area) # We plot to observe the curvilinear relationship

# Species tolerances defined in the 2. script
filter_NOfilter # Filter of species per site. We will use for tolerance

# Other parameters of the model
id_NOmodule <- rep(1,nrow(nodes_DaFr)) # Modules if we want some sites to belong to the same module. 
pool_200 <- rep(1,200) # Distribution of the species pool #rlnorm(n = 200,5,1) 
Meta_t0 <- matrix(nrow = length(pool_200), ncol =nrow(nodes_DaFr), 1) #Previous Metacommunity (for considering time relevance)
  
# Distances are related to the distance matrix. 
# Therefore we must see which values correspond to our connections to set the "D50". Dispersal distance is
# considered here as the distance at which probability of dispersal is 0.5. So "higher" or "lower" do not exclude
# other dispersal abilities. They push "overall connectivity" towards higher or lower connections.
summary(as.vector(Dist_Matrix)[-which(as.vector(Dist_Matrix)==10000)]) 
#dispersal_distances <- c(0.001,0.1,0.5,1,2,3,10) # We set the dispersal abilities that we want

dispersal_test <- c(2,2,2) # We set the dispersal abilities that we want
filter_Pollution_test <- list(filter_Pollution[[5]])
#       filter_Pollution[[5]],
#       filter_Pollution[[5]])

it_test <- c(100000,200000,1000000)
dead_test <- c(0.2,0.2,0.2)

length(dispersal_test);length(it_test);length(it_test);length(filter_Pollution_test)

library(tictoc)
tic() # This is to count the time 
library(doParallel) # We activate the parallelization
registerDoParallel(cores = detectCores()-1) # We keep 1 core to be able to do something else while running
Diff_scenarios <- foreach(dispersal=1:length(dispersal_test), .combine=rbind)%:% # Parallelize for dispersal
                  foreach(pollut=1:length(filter_Pollution_test), .combine=rbind)%dopar%{ # Parallellize for pollution
a <- NULL # We create an output object for each iteration
#b <- list()
for (it in 1:5) { # We repeat 10 times the same process
output <- H2020_Coalescent.and.lottery.exp.Kernel.J_TempMtcom_tempIT(
  Meta.pool = pool_200, # Species pool
  m.pool = 0.001, # Regional dispersal which is always constant 
  Js = J.freshwater, # Size of the communities (AKA: number of individuals/population contained in each community)
  id.module = id_NOmodule, # id of modules if there are some - NOT used for us
  filter.env = filter_Pollution_test[[pollut]], # Pollution scenarios (created at 2. Pollution assignation.R)
  M.dist = Dist_Matrix, # Distance matrix which corresponds to the STconmat (created at 1. OCnet - STconmat.R)
  D50 = dispersal_test[dispersal], # Dispersal distance scenario 
  m.max = 1, # Maximum migration
  tempo_imp = 0.1, # Relvance of "temporal" effect
  temp_Metacom = Meta_t0, # Metacommunity at time 0 (all species are equally favored)
  temp_it = 0, # Number of temporal iterations
  id.fixed=NULL, D50.fixed=0, m.max.fixed=0, comm.fixed=pool_200, # If there are some communit. that should be fixed
  Lottery=T, 
  it=it_test[dispersal], 
  prop.dead.by.it=0.2, # Lottery parameters, nº iterations and proportion of dead organisms  
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
for (round in 1:((length(Diff_scenarios)/4))) {
  #Diff_scenarios[[round]][1:9]
  S_site<- Diff_scenarios[[round]][10:(nrow(nodes_DaFr)+9)]
  B_site <- Diff_scenarios[[round]][(nrow(nodes_DaFr)+10):((nrow(nodes_DaFr)*2)+9)]
  #Out$Median[((nrow(nodes_DaFr)*2)+10):length(Out$Median)]
  
  #if(round<10){Pol_level <- as.character(round)}else{Pol_level <- strsplit(x = as.character(round),split = "")[[1]][2]}
  #if(Pol_level=="0"){Pol_level <- "10"}
  #
  Result_df <- bind_cols(nodes_DaFr_Pol %>% filter(Pol_Scen==1),
                         data.frame("Dead"=rep(Diff_scenarios[[round]][7],length(S_site)),
                                    "it"=rep(Diff_scenarios[[round]][9],length(S_site)),
                                    "S"=S_site,"B"=B_site))
  Res_nodes_DaFr <- bind_rows(Res_nodes_DaFr,Result_df)
}

# Checing scenario assignation
which(
as.vector(Res_nodes_DaFr %>% mutate("Checking"=Pol_Scen-as.numeric(Pol_Scend_Simulation)) %>% select(Checking))$Checking>0
)

Res_nodes_DaFr %>% 
  ggplot()+
  geom_segment(data=edges_DaFr, aes(x=X1_coord,y=Y1_coord, xend=X2_coord, yend=Y2_coord), 
               arrow =arrow(length=unit(0.01,"cm"), ends="last"), linewidth=0.2, colour="grey50", alpha=1)+
  geom_point(aes(x=x, y=y,fill=log(S+1),size=log(S+1), shape=Pollution))+
  scale_shape_manual(values = c(21,22))+
  scale_fill_viridis()+
  facet_grid(as.factor(Dead)~as.factor(it))


Res_nodes_DaFr %>% 
  ggplot()+
  geom_segment(data=edges_DaFr, aes(x=X1_coord,y=Y1_coord, xend=X2_coord, yend=Y2_coord), 
               arrow =arrow(length=unit(0.01,"cm"), ends="last"), linewidth=0.2, colour="grey50", alpha=1)+
  geom_point(aes(x=x, y=y,fill=B, shape=Pollution),size=3)+
  scale_shape_manual(values = c(21,22))+
  scale_fill_viridis(option = "A")+
  facet_grid(as.factor(Dead)~as.factor(it))


summary(
Res_nodes_DaFr %>% filter(it==50000,Dead==0.2) %>% 
  select(S,B)
)

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



