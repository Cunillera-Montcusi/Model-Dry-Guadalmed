
# 1. FAKE RIVER CREATION ####
# with OCNet we create a river with a determined dendritic structure
library(OCNet)

# With the following function we create a river of a determined size and charactersitics: 
# Cellsize sets the size of each cell (pixels) -- We can use this as a proxy of "community size"
# expEnergy is defining the "branching" of the river in accordance with the size
ocn_TEST <- create_OCN(dimX = 40,dimY = 30,nOutlet = 1,cellsize = 1, expEnergy=0.02)
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
  mutate(X2_coord=2)%>% mutate(Y2_coord=2)

nodes_DaFr <- data.frame("x"=ocn_TEST$FD$X,
                         "y"=ocn_TEST$FD$Y,
                         "weight"=ocn_TEST$FD$A)
  
for (nodes in 1:nrow(nodes_DaFr)) {
  edges_DaFr[which(edges_DaFr$from==nodes),3] <- nodes_DaFr[nodes,1]
  edges_DaFr[which(edges_DaFr$from==nodes),4] <- nodes_DaFr[nodes,2]
  edges_DaFr[which(edges_DaFr$to==nodes),5] <- nodes_DaFr[nodes,1]
  edges_DaFr[which(edges_DaFr$to==nodes),6] <- nodes_DaFr[nodes,2]
}

# We plot our beloved river colored according to the weight (order)/community size
ggplot()+
  geom_segment(data=edges_DaFr, aes(x=X1_coord,y=Y1_coord, xend=X2_coord, yend=Y2_coord), 
               arrow =arrow(length=unit(0.01,"cm"), ends="last"), size=0.2, colour="grey50", alpha=1)+
  geom_point(data=nodes_DaFr, aes(x=x, y=y,fill=weight/120000,size=weight/120000), shape=21)+
  scale_fill_viridis(option = "D",discrete = F)+
  scale_size(guide = "none") +
  labs(y="",x="",fill="Comm. Size")+
  theme_void()


# With all this we have our network structure that responds to a river
edges_DaFr
nodes_DaFr

# 2. DRYING DATA BASE FOR STcon

# 10 years 
# 12 months 
# 3 dry months
Flow_DB <- matrix(nrow = 12*10, ncol =nrow(nodes_DaFr)+1, data = 1)
# Month ID
Flow_DB[,1] <- seq(1:(12*10))
# Need of colnames for the matrix
colnames(Flow_DB) <- c("Site_ID",as.character(seq(1,nrow(nodes_DaFr),1)))

# In this case we assign drying according to weight (order/community size) so we take a look
summary(nodes_DaFr$weight)

# IF we are setting things in "anual" month basis, we could generate a matrix (12 cols) with all the possible drying combinations
## then we select the drying pattern randomly selecting determined rows (giving more probability to dryer/wetter ones?)

# We select those streams that will dry 3 months
THREE_month_dry <- rep(c(1,1,1,1,1,0,0,0,1,1,1,1),10) # We create an anual pattern of drying
Streams_3_month_dry <- sample(which(nodes_DaFr$weight>200 & nodes_DaFr$weight<1000),
       size = length(which(nodes_DaFr$weight>200 & nodes_DaFr$weight<1000))*0.7, # Randomly 70% of streams
       replace = F) # We randomly select which sites suffer this drying pattern

Flow_DB[,Streams_3_month_dry] <- THREE_month_dry

# We select those streams that will dry 5 months per year
FIVE_month_dry <- rep(c(1,1,1,1,0,0,0,0,0,1,1,1),10) # We create an anual pattern of drying
Streams_5_month_dry <- sample(which(nodes_DaFr$weight<101),
                       size = length(which(nodes_DaFr$weight<101))*0.7, # Randomly 70% of streams
                       replace = F) # We randomly select which sites suffer this drying pattern
Flow_DB[,Streams_5_month_dry] <- FIVE_month_dry


plot(apply(Flow_DB[,2:ncol(Flow_DB)],2,sum),nodes_DaFr$weight)
  

# 3. STconmat calculation
# Remember that STcon is able to calculate several rivers at the same time! So you just need to have a list object with the 3 elements 
# for each river scenario: 

# Intermitence database
Int_dataset <- list(Flow_DB)  
# Sites coordinates
Sit_coordinates <- list(nodes_DaFr[,c(3,3,1:2)])
# Network structure
Net_stru <- list(as.matrix(ocn_TEST$FD$W))

source("https://raw.github.com/Cunillera-Montcusi/Quantifyinig-SpaTem-connectivity/main/SpaTemp_function.R")
Riv_Und_Net <- spat_temp_index(Inermitence_dataset = Int_dataset,
                               Sites_coordinates=Sit_coordinates,
                               Network_stru =Net_stru,
                               direction="directed", sense = "out",
                               weighting=F,dist_matrices = NULL,
                               weighting_links =FALSE,link_weights = NULL,
                               legacy_effect =1, legacy_lenght = 1,
                               value_S_LINK=1,
                               value_T_LINK=1,
                               value_NO_S_link=0,
                               value_NO_T_link=0,
                               Network_variables=F,print.plots=F,print.directory="Figure/")

Riv_Und_Net$STconmat







# Small script to test configurations with differing expEnergy

out_matrix <- matrix(ncol =6 ,nrow = length(seq(0.1,50,5)))
for (ener in 1:length(seq(0.1,50,5))) {
  ocn_TEST <- create_OCN(dimX = 40,dimY = 30,nOutlet = 1,cellsize = 1, expEnergy=seq(0.1,50,5)[ener])
  draw_simple_OCN(ocn_TEST)
  out_matrix[ener,] <- c(summary(ocn_TEST$FD$A))
  colnames(out_matrix) <- names(c(summary(ocn_TEST$FD$A)))
}

