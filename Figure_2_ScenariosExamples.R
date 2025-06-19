
library(tidyverse);library(viridis);library(ggnetwork)

load("C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/1. FEHM coses al DIA/7. DRY-GUADALMED/Model-Dry-Guadalmed/All_Scenarios_OutputToSimul.RData")
load("C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/1. FEHM coses al DIA/7. DRY-GUADALMED/Model-Dry-Guadalmed/Cluster_Code/Sc_1_15/SLURM_PreSTcon.RData")
for (Scenario_Sel in 1:length(output_to_simulate)) {
cat(Scenario_Sel,"-",unique(output_to_simulate[[Scenario_Sel]][[2]]$Scenario),"\n")  
}

Scenario_Sel <- c(74,362,578,78,366,582,79,367,583)
Scenario_plot <- list()
for (Scenario in 1:length(Scenario_Sel)) {
nodes_DaFr <- output_to_simulate[[Scenario_Sel[Scenario]]][[2]]

cat(Scenario, Scenario_Sel[Scenario],"-",unique(nodes_DaFr$Scenario),"\n")  

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


Scenario_plot[[Scenario]] <- edges_DaFr %>% 
  left_join(nodes_DaFr,by=c("from"="Site_ID")) %>% 
  mutate(Pollution=ifelse(Pollution=="Non_Poll","Unpolluted","Polluted")) %>% 
  mutate(Permanence=((Permanence/48)*100)) %>%
  #mutate(Drying=((Drying/nrow(output_to_simulate[[ScenarioS[Scenario]]][[3]]))*100)) %>% 
  ggplot()+
  geom_curve(aes(x=X1_coord,y=Y1_coord, xend=X2_coord, yend=Y2_coord,
                 colour=Permanence,alpha=Permanence),
             arrow =arrow(length=unit(0.01,"cm"), ends="last"),
             linewidth=1.5, 
             curvature=0.1)+
  geom_point(aes(x=x, y=y,fill=Permanence),size=3, shape=21,alpha=0.5)+
  scale_colour_viridis(option = "D",discrete = F,direction = -1,limits=c(1,100))+
  #scale_fill_manual(values=c("red","blue"))+
  scale_alpha(limits=c(0,1))+
  scale_fill_viridis(option = "D",discrete = F,direction = -1,limits=c(1,100))+
  labs(y="",x="",colour="Flow permanence",
        subtitle=
         paste("Dry.Ext.=",strsplit(unique(nodes_DaFr$Scenario),split = "_")[[1]][1],
                " ",
                "Dry.Int.=",1-as.numeric(strsplit(unique(nodes_DaFr$Scenario),split = "_")[[1]][2]))
       )+
  guides(alpha="none",fill="none")+
  theme_void()+theme(legend.position = "none")
}

Scenario_Sel <- c(592,622,642)
Poll_Scenario_plot <- list()
for (Scenario in 1:length(Scenario_Sel)) {
  nodes_DaFr <- output_to_simulate[[Scenario_Sel[Scenario]]][[2]]
  
  cat(Scenario, Scenario_Sel[Scenario],"-",unique(nodes_DaFr$Scenario),"\n")  
  
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
  
  
  Poll_Scenario_plot[[Scenario]] <- edges_DaFr %>% 
    left_join(nodes_DaFr,by=c("from"="Site_ID")) %>% 
    mutate(Pollution=ifelse(Pollution=="Non_Poll","Unpolluted","Polluted")) %>% 
    mutate(Permanence=((Permanence/48)*100)) %>%
    #mutate(Drying=((Drying/nrow(output_to_simulate[[ScenarioS[Scenario]]][[3]]))*100)) %>% 
    ggplot()+
    geom_curve(aes(x=X1_coord,y=Y1_coord, xend=X2_coord, yend=Y2_coord),
               arrow =arrow(length=unit(0.01,"cm"), ends="last"), colour="grey50",alpha=0.5,
               linewidth=1.5, curvature=0.1)+
    geom_point(aes(x=x, y=y,fill=Pollution),size=3, shape=21,alpha=0.5)+
    #scale_colour_viridis(option = "D",discrete = F,direction = -1,limits=c(1,100))+
    scale_fill_manual(values=c("red","blue"))+
    scale_alpha(limits=c(0,1))+
    #scale_fill_viridis(option = "D",discrete = F,direction = -1,limits=c(1,100))+
    labs(y="",x="",colour="Flow permanence",
         subtitle=paste("Impact ext.",strsplit(unique(nodes_DaFr$Scenario),split = "_")[[1]][3])
    )+
    guides(alpha="none",fill="none")+
    theme_void()+theme(legend.position = "none")
}

Synthetic <- gridExtra::arrangeGrob(Scenario_plot[[7]],Scenario_plot[[8]],Scenario_plot[[9]],ggplot()+ggnetwork::theme_blank(),Poll_Scenario_plot[[1]],
                                     Scenario_plot[[4]],Scenario_plot[[5]],Scenario_plot[[6]],ggplot()+ggnetwork::theme_blank(),Poll_Scenario_plot[[2]],
                                     Scenario_plot[[1]],Scenario_plot[[2]],Scenario_plot[[3]],ggplot()+ggnetwork::theme_blank(),Poll_Scenario_plot[[3]],
                                     widths=c(1,1,1,0.2,1),ncol=5)



png(filename = "Figure_2.png",width = 3200,height = 2500,units = "px",res =300)
gridExtra::grid.arrange(Synthetic) 
dev.off()


# Network for Figure 1
edges_DaFr %>% 
  left_join(nodes_DaFr,by=c("from"="Site_ID")) %>% 
  ggplot()+
  geom_curve(aes(x=X1_coord,y=Y1_coord, xend=X2_coord, yend=Y2_coord),
             colour="grey50",
             arrow =arrow(length=unit(0.5,"cm"), ends="last"),
             linewidth=1.5, 
             curvature=0.1)+
  geom_point(aes(x=x, y=y), colour="black",size=3, shape=16)+
  guides(alpha="none",fill="none")+
  theme_void()+theme(legend.position = "none")





load("Genal_Scenarios_OutputToSimul.RData")
DataFrame_out_Scen <- output_to_simulate[[1]][[2]]

load("Genal_ClusterCode/SLURM_GenalData.Rdata")
g <- igraph::graph_from_adjacency_matrix(as.matrix(Network_stru_Campaings_To_Run[[1]]))
edges_DaFr <- as.data.frame(igraph::get.edgelist(g))
edges_DaFr <- edges_DaFr %>% dplyr::select("from"=V1, "to"=V2) %>%
  mutate(X1_coord=2)%>% mutate(Y1_coord=2)%>% 
  mutate(X2_coord=2)%>% mutate(Y2_coord=2) %>% 
  mutate(from=as.numeric(from), to=as.numeric(to))

# Fill X and Y coordinate information for edges, using the nodes_DaFr
for (nodes in 1:nrow(DataFrame_out_Scen)) {
  edges_DaFr[which(edges_DaFr$from==nodes),3] <- DataFrame_out_Scen[nodes,]$X1
  edges_DaFr[which(edges_DaFr$from==nodes),4] <- DataFrame_out_Scen[nodes,]$X2
  edges_DaFr[which(edges_DaFr$to==nodes),5] <- DataFrame_out_Scen[nodes,]$X1
  edges_DaFr[which(edges_DaFr$to==nodes),6] <- DataFrame_out_Scen[nodes,]$X2
}

Network_Drying <- edges_DaFr %>% 
  left_join(DataFrame_out_Scen,by=c("from"="Site_ID")) %>% 
  mutate(Permanence=((Permanence/nrow(output_to_simulate[[3]][[3]]))*100)) %>%
  mutate(Drying=((Drying/nrow(output_to_simulate[[3]][[3]]))*100)) %>% 
  ggplot()+
  geom_curve(aes(x=X1_coord,y=Y1_coord, xend=X2_coord, yend=Y2_coord,colour=Permanence,alpha=Permanence),
             arrow =arrow(length=unit(0.01,"cm"), ends="last"),
             linewidth=1.5, 
             curvature=0.1)+
  geom_point(aes(x=X1, y=X2,fill=Permanence),size=3, shape=21,alpha=0.5)+
  scale_colour_viridis(option = "D",discrete = F,direction = -1)+
  scale_fill_viridis(option = "D",discrete = F,direction = -1)+
  labs(y="",x="",colour="Flow permanence",
       title="",subtitle="Network Drying")+
  guides(alpha="none",fill="none")+
  theme_void()+theme(legend.position = "none")

Genal_Scenario <- list()
ScenarioS <- c(3,12,19)
Name_ScenarioS <- c("Low Pollution","Mid Pollution","High Pollution")
for (Scenario in 1:length(ScenarioS)) {
Genal_Scenario[[Scenario]] <- edges_DaFr %>% 
  left_join(output_to_simulate[[ScenarioS[Scenario]]][[2]],by=c("from"="Site_ID")) %>% 
  mutate(Pollution=ifelse(Pollution=="Non_Poll","Unpolluted","Polluted")) %>% 
  mutate(Permanence=((Permanence/nrow(output_to_simulate[[ScenarioS[Scenario]]][[3]]))*100)) %>%
  mutate(Drying=((Drying/nrow(output_to_simulate[[ScenarioS[Scenario]]][[3]]))*100)) %>% 
  ggplot()+
  geom_curve(aes(x=X1_coord,y=Y1_coord, xend=X2_coord, yend=Y2_coord),
             arrow =arrow(length=unit(0.01,"cm"), ends="last"),colour="grey50",alpha=0.8,
             linewidth=1.5, 
             curvature=0.1)+
  geom_point(aes(x=X1, y=X2,fill=Pollution),size=3, shape=21,alpha=0.5)+
  scale_colour_viridis(option = "D",discrete = F,direction = -1)+
  scale_fill_manual(values=c("red","blue"))+
  labs(title="",subtitle=Name_ScenarioS[Scenario])+
  theme_void()+theme(legend.position = "none")
}

Legend <- cowplot::get_legend(
edges_DaFr %>% 
  left_join(DataFrame_out_Scen,by=c("from"="Site_ID")) %>% 
  mutate(Pollution=ifelse(Pollution=="Non_Poll","Unpolluted","Polluted")) %>% 
  mutate(Permanence=((Permanence/nrow(output_to_simulate[[ScenarioS[Scenario]]][[3]]))*100)) %>%
  ggplot()+
  geom_curve(aes(x=X1_coord,y=Y1_coord, xend=X2_coord, yend=Y2_coord,colour=Permanence))+
  geom_point(aes(x=X1, y=X2,fill=Pollution),size=3, shape=21)+
  scale_colour_viridis(option = "D",discrete = F,direction = -1)+
  scale_fill_manual(values=c("red","blue"))+
  theme_void()
)

edges_DaFr %>% 
  left_join(DataFrame_out_Scen,by=c("from"="Site_ID")) %>% 
  ggplot()+
  geom_curve(aes(x=X1_coord,y=Y1_coord, xend=X2_coord, yend=Y2_coord),
             colour="grey50",
             arrow =arrow(length=unit(0.5,"cm"), ends="last"),
             linewidth=1.5, 
             curvature=0.1)+
  geom_point(aes(x=X1, y=X2), colour="black",size=3, shape=16)+
  guides(alpha="none",fill="none")+
  theme_void()+theme(legend.position = "none")


Genal <- gridExtra::arrangeGrob(Network_Drying,
                                Genal_Scenario[[1]],Genal_Scenario[[2]],Genal_Scenario[[3]],
                                Legend, ncol=5, widths=c(2,2,2,2,1.2))


png(filename = "Figure_2.png",width = 3200,height = 2500,units = "px",res =300)
gridExtra::grid.arrange(
  Synthetic,Genal,
  heights=c(2,1)
) 
dev.off()


