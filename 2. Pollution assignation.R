
edges_DaFr
nodes_DaFr

# Polluted sites assignation 
# POSSIBLE criteria: Downstream rivers are more polluted. We can use the "out.closeness" centrality (we could use the weight also)
Nodes_centrality <- igraph::closeness(g,mode = "out",normalized = T)
Sites_to_Pollute <- which(Nodes_centrality>quantile(Nodes_centrality,0.5,na.rm = T))

#Sites_to_Pollute <- 1:nrow(nodes_DaFr)

nodes_DaFr_Pol <- data.frame()
filter_Pollution <- list()
for (pollution_expans in 1:10) {
Polluted_Sites <- sample(Sites_to_Pollute, #randomly sample sites to pollute
                         size =length(Sites_to_Pollute)*(pollution_expans/10), #sample size =number of sites*pollution expans/10
                         replace = F)


Pollution <- rep("Non_Poll",nrow(nodes_DaFr))# replicate "NonPoll" as the row number of nodes
Pollution[Polluted_Sites] <- "YES_Poll" #write "YES_Poll" to polluted sites (which were randomly selected above)

nodes_DaFr_temp <- data.frame(nodes_DaFr,"Pol_Scen"=pollution_expans,"Pollution"=Pollution)# merge scenarios and nodes
nodes_DaFr_Pol <- bind_rows(nodes_DaFr_Pol,nodes_DaFr_temp)# bind rows after each scenario generation
# We plot our beloved river colored according to the weight (order)/community size
plt <- ggplot()+
  geom_segment(data=edges_DaFr, aes(x=X1_coord,y=Y1_coord, xend=X2_coord, yend=Y2_coord), 
               arrow =arrow(length=unit(0.01,"cm"), ends="last"), linewidth=0.2, colour="grey50", alpha=1)+
  geom_point(data=nodes_DaFr_Pol %>% filter(Pol_Scen==pollution_expans), 
             aes(x=x, y=y,fill=weight/120000,size=weight/120000, shape=Pollution))+
  scale_fill_viridis(option = "D",discrete = F)+
  scale_size(guide = "none") +
  scale_shape_manual(values=c(21,22))+
  labs(y="",x="",fill="Comm. Size")+
  theme_void()

print(plt)
# Species tolerances to pollution 
# Pollution tolerance will be a gradient from 1 to 0 with species hi
Spp_tolerance <- c(rep(0.9,50),rep(0.5,50),rep(0.2,50),rep(0.01,50))

# Pollution acts as a filter. When a site is polluted a species is being filtered according to that. 
#Therefore, for the polluted sites we need to use the "filter" 

#here this creates a matrix as per species (n=200) (rows) and site (columns), with 0.99 as tolerance values
filter_NOfilter <- matrix(nrow = 200, ncol=nrow(nodes_DaFr), data = 0.99) # Filter of species per site. We will use for tolerance
#select columns (from Polluted sites) and write spp_tolerance instead of 0.99
filter_NOfilter[,Polluted_Sites] <- Spp_tolerance# here we write these tolerances (Spp_tolerance) to the polluted sites (selected above)

filter_Pollution[[pollution_expans]] <- filter_NOfilter # write filter_NOfilter matrix into the empty filter_Pollution list into different scenarios (created in the beginning)
}

# We can just check this just to see that we have changed the values (it is just to check that we changed some values) 
plot(apply(filter_Pollution[[1]],2,mean))
plot(apply(filter_Pollution[[5]],2,mean))
plot(apply(filter_Pollution[[10]],2,mean))


