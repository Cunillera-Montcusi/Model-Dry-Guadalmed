


# Charge the function to run the coalescent model
source("H2020_Lattice_expKernel_Jenv_TempMeta.R")

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
Jmin <- 0 # What will be the minimum size at whic we will consider "0" 
J.max<-400+Jmin # What is the maximum J that we want to assign to a community
b.ef<-0.5 # The coefficient of change. If 1 we do a direct proportion between the two but minmimum becomes "1" (only 1 indiv)

# J creation
J.freshwater<-ceiling((-Jmin+(J.max/(Max_Area^b.ef))*FW_area^b.ef))
plot(J.freshwater,FW_area) # We plot to observe the curvilinear relationship

# Other parameters of the model
id_NOmodule <- rep(1,nrow(nodes_DaFr)) # Modules if we want some sites to belong to the same module. 
filter_NOfilter <- matrix(nrow = 200, ncol=nrow(nodes_DaFr), data = 1) # Filter of species per site. We will use for tolerance
pool_200 <- rep(1,200) #rlnorm(n = 200,5,1) # Distribution of the species pool
Meta_t0 <- matrix(nrow = length(pool_200), ncol =nrow(nodes_DaFr), 1) #Previous Metacommunity (for considering time relevance)
  
# Distances are related to the distance matrix. 
# Therefore we must see which values correspond to our connections to set the "D50". Dispersal distance is
# considered here as the distance at which probability of dispersal is 0.5. So "higher" or "lower" do not exclude
# other dispersal abilities. They push "overall connectivity" towards higher or lower connections.
summary(as.vector(Dist_Matrix)[-which(as.vector(Dist_Matrix)==10000)])
dispersal_distances <- c(0.001,0.1,0.5,1,2,3,10)
  
library(doParallel)
registerDoParallel(cores = detectCores()-2)
Disp_scenarios <- foreach(loss=1:length(dispersal_distances), .combine=rbind)%dopar%{#%:%
#                  foreach(dispersal=1:length(dispersal_distances), .combine=rbind)%dopar%{
a <- NULL
#b <- list()
for (it in 1:10) {
output <- H2020_Coalescent.and.lottery.exp.Kernel.J_TempMtcom_tempIT(
  Meta.pool = pool_200,
  m.pool = 0.001,
  Js = J.freshwater,
  id.module = id_NOmodule,
  filter.env = filter_NOfilter,
  M.dist = Dist_Matrix,
  D50 = dispersal_distances[dispersal],
  m.max = 1,
  tempo_imp = 0.1,
  temp_Metacom = Meta_t0,
  temp_it = 0,
  id.fixed=NULL, D50.fixed=0, m.max.fixed=0, comm.fixed=pool_200,
  Lottery=F, it=1, prop.dead.by.it=1, id.obs=1:nrow(nodes_DaFr))
a <- rbind(a,output[[1]])
#b[[it]] <- output[[2]]
}
resume.out(a)
}


Out$Median[1:9]
S_site<- Out$Median[10:(nrow(nodes_DaFr)+9)]
B_site <- Out$Median[(nrow(nodes_DaFr)+10):((nrow(nodes_DaFr)*2)+9)]

Out$Median[((nrow(nodes_DaFr)*2)+10):length(Out$Median)]

Res_nodes_DaFr <- bind_cols(nodes_DaFr,"S"=S_site,"B"=B_site)

Res_nodes_DaFr %>% 
  ggplot()+geom_point(aes(x=Permanence, y=S,size=weight))+
  theme_classic()

Res_nodes_DaFr_2.63 <- Res_nodes_DaFr
