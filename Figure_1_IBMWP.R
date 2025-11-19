
library(tidyverse);library(viridis);library(ggnetwork)

# We need to load some specific values to plot determined scenarios
load("RData_outputs/All_Scenarios_OutputToSimul.RData")
load("Cluster_Code/Sc_1_15/SLURM_PreSTcon.RData") # Example of Cluste Code PreSTcon data (need to pick it from Cluster code)
load("RData_outputs/All_Results_scenarios.RData")

Data_To_Plot <- Res_nodes_DaFr %>%
  mutate(Pollution = case_when(
    str_detect(Pollution ,"YES_Poll") ~ "Polluted",
    str_detect(Pollution ,"Non_Poll") ~ "Unpolluted")) %>%
  mutate(Pollution=factor(Pollution,levels=c("Unpolluted","Polluted"))) %>% 
  filter(Disp==0.15) %>% 
  mutate(Dry_ext=as.numeric(Dry_ext),Dry_patt=as.numeric(Dry_patt),
         Pollut_ext=as.numeric(Pollut_ext)) 

Scenarios_IBMWP <- c(#"0.01_0.01_0.01","0.5_0.5_0.01",
                     "0.5_0.5_0.5"
                     #"0.1_0.1_0.01","0.5_0.1_0.01","0.9_0.1_0.01",
                     #"0.1_0.5_0.01","0.5_0.5_0.01","0.9_0.5_0.01",
                     #"0.1_0.75_0.01","0.5_0.75_0.01","0.9_0.75_0.01"
                     )

Plot_IBMWP <- list()
for (IBMWP_Scenarios in 1:length(Scenarios_IBMWP)) {
nodes_DaFr <- Data_To_Plot %>% filter(Scenario==Scenarios_IBMWP[IBMWP_Scenarios]) 

edges_DaFr <- as.data.frame(igraph::get.edgelist(g))
edges_DaFr <- edges_DaFr %>% dplyr::select("from"=V1, "to"=V2) %>% 
  mutate(X1_coord=2)%>% mutate(Y1_coord=2)%>% 
  mutate(X2_coord=2)%>% mutate(Y2_coord=2) # Create new columns for coordinates, to be filled later
# Fill X and Y coordinate information for edges, using the nodes_DaFr
for (nodes in 1:nrow(nodes_DaFr)) {
  edges_DaFr[which(edges_DaFr$from==nodes),3] <- nodes_DaFr[nodes,]$x
  edges_DaFr[which(edges_DaFr$from==nodes),4] <- nodes_DaFr[nodes,]$y
  edges_DaFr[which(edges_DaFr$to==nodes),5] <- nodes_DaFr[nodes,]$x
  edges_DaFr[which(edges_DaFr$to==nodes),6] <- nodes_DaFr[nodes,]$y
}

Plot_IBMWP[[IBMWP_Scenarios]] <- edges_DaFr %>% 
  left_join(nodes_DaFr,by=c("from"="Site_ID")) %>% 
  ggplot()+
  geom_curve(aes(x=X1_coord,y=Y1_coord, xend=X2_coord, yend=Y2_coord),alpha=0.5,
             colour="grey50",arrow =arrow(length=unit(0.01,"cm"), ends="last"),
             linewidth=1.5, 
             curvature=0.1)+
  geom_point(aes(x=x,y=y,fill=IBMWP,colour=IBMWP),shape=21,size=4,alpha=0.8)+
  #scale_fill_viridis(option = "G",direction = -1)+
  #scale_colour_viridis(option = "G",direction = -1)+
  scale_fill_distiller(palette = "Spectral",direction = 1)+
  scale_colour_distiller(palette = "Spectral",direction = 1)+
  guides(size="none",fill="none")+
  theme_void()
}
gridExtra::grid.arrange(Plot_IBMWP[[1]],Plot_IBMWP[[2]],Plot_IBMWP[[3]])
                                 
