
# 1. FAKE RIVER CREATION ####
# with OCNet we create a river with a determined dendritic structure
library(OCNet)

# With the following function we create a river of a determined size and charactersitics: 
# Cellsize sets the size of each cell (pixels) -- We can use this as a proxy of "community size"
# expEnergy is defining the "branching" of the river in accordance with the size
ocn_TEST <- create_OCN(dimX = 40,dimY = 30,nOutlet = 1,cellsize = 2, expEnergy=0.5)
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

nodes_DaFr <- data.frame("x"=ocn_TEST$FD$X,
                         "y"=ocn_TEST$FD$Y,
                         "weight"=ocn_TEST$FD$A)

# Fill X and Y coordinate information for edges, using the nodes_DaFr
for (nodes in 1:nrow(nodes_DaFr)) {
  edges_DaFr[which(edges_DaFr$from==nodes),3] <- nodes_DaFr[nodes,]$x
  edges_DaFr[which(edges_DaFr$from==nodes),4] <- nodes_DaFr[nodes,]$y
  edges_DaFr[which(edges_DaFr$to==nodes),5] <- nodes_DaFr[nodes,]$x
  edges_DaFr[which(edges_DaFr$to==nodes),6] <- nodes_DaFr[nodes,]$y
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

# 2. DRYING DATA BASE FOR STcon  ####

# 10 years 
# 12 months 
# 3 dry months
# each column is one node and rows are 12 months, repeated for 10 years, filled with only 1's for now
Flow_DB <- matrix(nrow = 12*10, ncol =nrow(nodes_DaFr)+1, data = 1)
# Month ID
Flow_DB[,ncol(Flow_DB)] <- seq(1:(12*10))

# In this case we assign drying (zeros) according to weight (order/community size) so we take a look
summary(nodes_DaFr$weight)

# IF we are setting things in "anual" month basis, we could generate a matrix (12 cols) with all the possible drying combinations
## then we select the drying pattern randomly selecting determined rows (giving more probability to dryer/wetter ones?)

# We select those streams that will dry 3 months
THREE_month_dry <- rep(c(1,1,1,1,1,0,0,0,1,1,1,1),10) # We create an anual pattern of drying
# We select the nodes under a determined criteria (in the example those between half the mean and the mean)
THREE_streams <- which(nodes_DaFr$weight>(summary(nodes_DaFr$weight)[4]/2)
                        &
                       nodes_DaFr$weight<summary(nodes_DaFr$weight)[4])

#Randomly select a part of these streams 
Streams_3_month_dry <- sample(THREE_streams,
       size = length(THREE_streams)*0.7, # Randomly 70% of streams
       replace = F) # We randomly select which sites suffer this drying pattern

# We add the drying month pattern to the selected streams
Flow_DB[,Streams_3_month_dry] <- THREE_month_dry

# COMMENT_______________
### We repeat the same for another criteria

# We select those streams that will dry 5 months per year
FIVE_month_dry <- rep(c(1,1,1,1,0,0,0,0,0,1,1,1),10) # We create an anual pattern of drying
# We select the nodes under a determined criteria (in the example those below the half of the mean weight)
FIVE_streams <- which(nodes_DaFr$weight<(summary(nodes_DaFr$weight)[4]/2))
#Randomly select a part of these streams 
Streams_5_month_dry <- sample(FIVE_streams,
                       size = length(FIVE_streams)*0.7, # Randomly 70% of streams
                       replace = F) # We randomly select which sites suffer this drying pattern
# We add the drying month pattern to the selected streams
Flow_DB[,Streams_5_month_dry] <- FIVE_month_dry

# COMMENT_______________
### This can be done different times and for a more complex structure. But so far will be perfect for first try

# We add a factor called "Permanence to the "nodes" to plot
nodes_DaFr <- data.frame(nodes_DaFr,"Permanence"=apply(Flow_DB[,1:(ncol(Flow_DB)-1)],2,sum))

# We plot our beloved river colored according to the weight (order)/community size
ggplot()+
  geom_segment(data=edges_DaFr, aes(x=X1_coord,y=Y1_coord, xend=X2_coord, yend=Y2_coord), 
               arrow =arrow(length=unit(0.01,"cm"), ends="last"), size=0.2, colour="grey50", alpha=1)+
  geom_point(data=nodes_DaFr, aes(x=x, y=y,
                                  fill=Permanence,
                                  size=weight/120000), shape=21)+
  scale_fill_viridis(option = "D",discrete = F,direction = -1)+
  scale_size(guide = "none") +
  labs(y="",x="",fill="Comm. Size")+
  theme_void()

# IMPORTANT step! We switch the Flow_DB database to have the site ID in the first column
Flow_DB<- cbind(Flow_DB[,ncol(Flow_DB)],Flow_DB[,1:(ncol(Flow_DB)-1)])
# Need of colnames for the matrix
colnames(Flow_DB) <- c("Site_ID",as.character(seq(1,nrow(nodes_DaFr),1)))

# 3. STconmat calculation  ####
# Remember that STcon is able to calculate several rivers at the same time! So you just need to have a list object with the 3 elements 
# for each river scenario: 

# Intermittence database
Int_dataset <- list(Flow_DB)  
# Sites coordinates
Sit_coordinates <- list(nodes_DaFr[,c(3,3,1:2)])
# Network structure
Net_stru <- list(as.matrix(ocn_TEST$FD$W))
# Distance matrix
Dist_matr <- list((as.matrix(dist(nodes_DaFr[,1:2])))*as.matrix(ocn_TEST$FD$W))



#some library to be needed
library(shp2graph)
library(doParallel)
library(ggnetwork)
source("https://raw.github.com/Cunillera-Montcusi/Quantifyinig-SpaTem-connectivity/main/SpaTemp_function.R")
Riv_Dir <- spat_temp_index(Inermitence_dataset = Int_dataset,
                               Sites_coordinates=Sit_coordinates,
                               Network_stru =Net_stru,
                               direction="directed", sense = "out",
                               weighting=T,dist_matrices = Dist_matr,
                               weighting_links =FALSE,link_weights = NULL,
                               legacy_effect =1, legacy_lenght = 1,
                               value_S_LINK=0.2,
                               value_T_LINK=0.2,
                               value_NO_S_link=1,
                               value_NO_T_link=1,
                               Network_variables=F,print.plots=F,print.directory="Figure/")


Riv_STconmat <-Riv_Dir$STconmat[[1]]

#to construct a diagonal matrix
diag(Riv_STconmat) <- 0
# if Riv_STconmat is 0, write 10000, otherwise write Riv_STconmat
Riv_STconmat <-ifelse(Riv_STconmat==0,10000,Riv_STconmat)

Riv_STconmat[1:10,1:10]

length(which(Riv_STconmat<10000))
length(which(Riv_STconmat>0))
length(which(Dist_matr[[1]]>0))


plot(apply(Riv_STconmat,2,mean),apply(Dist_matr[[1]],2,mean))



# Small script to test configurations with differing expEnergy

out_matrix <- matrix(ncol =6 ,nrow = length(seq(0.1,50,5)))
for (ener in 1:length(seq(0.1,50,5))) {
  ocn_TEST <- create_OCN(dimX = 40,dimY = 30,nOutlet = 1,cellsize = 1, expEnergy=seq(0.1,50,5)[ener])
  draw_simple_OCN(ocn_TEST)
  out_matrix[ener,] <- c(summary(ocn_TEST$FD$A))
  colnames(out_matrix) <- names(c(summary(ocn_TEST$FD$A)))
}

