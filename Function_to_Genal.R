
setwd("C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/1. FEHM coses al DIA/7. DRY-GUADALMED/Model-Dry-Guadalmed")
load("Genal_ClusterCode/Riv_AerAct_STcon.RData")
load("Genal_ClusterCode/Riv_Drift_STcon.RData")
load("Genal_ClusterCode/Riv_Swim_STcon.RData")
load("Genal_ClusterCode/SLURM_GenalData.Rdata")
Years_Of_Drying <- 2

library(tidyverse);library(viridis)

Scen_Drift_STconmat <- list()
for (scen in 1:1) {
  Drift_STconmat <-Riv_Drift$STconmat[[1]]#/Riv_Drift$STconmat[[length(Int_dataset)]]
  #to construct a diagonal matrix
  diag(Drift_STconmat) <- 1
  # if Riv_STconmat is 0, write 10000, otherwise write Riv_STconmat
  Scen_Drift_STconmat[[scen]] <-ifelse(Drift_STconmat==0,100,Drift_STconmat)
  #Scen_Drift_STconmat[[scen]] <-ifelse(is.nan(Drift_STconmat)==T,100,Drift_STconmat)
}

Scen_Swim_STconmat <- list()
for (scen in 1:1) {
  Swim_STconmat <-Riv_Swim$STconmat[[1]]#/Riv_Swim$STconmat[[length(Int_dataset)]]
  #to construct a diagonal matrix
  diag(Swim_STconmat) <- 1
  # if Riv_STconmat is 0 or NaN, write 10000, otherwise write Riv_STconmat
  Scen_Swim_STconmat[[scen]] <-ifelse(Swim_STconmat==0,100,Swim_STconmat)
  #Scen_Swim_STconmat[[scen]] <-ifelse(is.nan(Swim_STconmat)==T,100,Swim_STconmat)
}

Scen_AAct_STconmat <- list()
for (scen in 1:1) {
  AAct_STconmat <-Riv_AerAct$STconmat[[1]]#/Riv_Swim$STconmat[[length(Int_dataset)]]
  #to construct a diagonal matrix
  diag(AAct_STconmat) <- 1
  # if Riv_STconmat is 0 or NaN, write 10000, otherwise write Riv_STconmat
  Scen_AAct_STconmat[[scen]] <-ifelse(AAct_STconmat==0,100,AAct_STconmat)
  #Scen_Swim_STconmat[[scen]] <-ifelse(is.nan(Swim_STconmat)==T,100,Swim_STconmat)
}

summary(as.vector(Scen_AAct_STconmat[[1]]))
summary(as.vector(Scen_AAct_STconmat[[1]])[which(as.vector(Scen_AAct_STconmat[[1]])<100)])
summary(as.vector(Scen_Swim_STconmat[[1]]))
summary(as.vector(Scen_Drift_STconmat[[1]]))

Orig_dispersal_pollution <- read.csv(file = paste0(getwd(), "/data/pollution_dispersal.csv")) %>% drop_na()

nodes_DaFr <- bind_cols(
Sites_coordinates_Campaings_To_Run[[1]] %>%rename("Site_ID"="Node_ID") ,
"Permanence"=as.numeric(apply(Inermitence_dataset_Campaings_To_Run[[1]][,2:ncol(Inermitence_dataset_Campaings_To_Run[[1]])],2,sum))) %>% 
mutate("Drying"=(nrow(Inermitence_dataset_Campaings_To_Run[[1]])-Permanence))

Genal_Pollut_Ext <- c(0.01,0.1,0.25,0.35,0.45,0.5,0.65,0.75,0.9)
Genal_Pollut_Ext <- c(0.01,seq(0.05,0.9,0.05))

Sites_to_Pollut<- list()
Selected_Sites <- sample(nodes_DaFr$Site_ID,size = length(nodes_DaFr$Site_ID),replace = F)
for (Sit_To_Pol in 1:length(Genal_Pollut_Ext)) {
  Sites_to_Pollut[[Sit_To_Pol]] <-Selected_Sites[1:ceiling(nrow(nodes_DaFr)*Genal_Pollut_Ext[Sit_To_Pol])] 
}

Poll_DataFrame_out <- list()
Poll_filter_Pollution <- list()
for (diff_extent_value in 1:length(Genal_Pollut_Ext)) {
  Poll_nodes_DaFr <- nodes_DaFr
  Poll_extent <- 1
  
  Poll_diff_extent <- Genal_Pollut_Ext
  
  #Spp_tolerance <- c(rep(0.5,50),rep(0.65,50),rep(0.75,50),rep(0.8,50))
  Spp_tolerance <- 1.01-(Orig_dispersal_pollution$IBMWP_score/10)
  
  # This function is very similar to the Dry but with less features (distribution is the same)
  #source("Function_to_Pollute.R")
  source(file = paste0(getwd(),"/Function_to_Pollute.R"))
  filter_Pollution <- function_to_pollute(River_nodes =Poll_nodes_DaFr,
                                          Spp_tolerance =Spp_tolerance ,
                                          extent =Poll_extent,
                                          distribution =Sites_to_Pollut[[diff_extent_value]])
  
  # A way to detect polluted sites is by looking at which sites the "filter" does not meet the condition of all 0.99
  Pollution <- rep("Non_Poll",nrow(Poll_nodes_DaFr))# replicate "NonPoll" as the row number of nodes
  Polluted_Sites <- which(apply(filter_Pollution,2,sum)!=sum(rep(0.99,length(Spp_tolerance))))
  Pollution[Polluted_Sites] <- "YES_Poll" #write "YES_Poll" to polluted sites (which were randomly selected above)
  
  Poll_nodes_DaFr <- Poll_nodes_DaFr %>% mutate("Pollution"=Pollution)# merge scenarios and nodes
  
  Poll_DataFrame_out[[diff_extent_value]] <- Poll_nodes_DaFr
  Poll_filter_Pollution[[diff_extent_value]] <-list(filter_Pollution,Pollution) 
}



plots_diagnosis <- list()
output_to_simulate <- list()
for(Pollution_Scenario in 1:length(Poll_filter_Pollution)){
# We set the new filter for the pollution 
filter_Pollution <- Poll_filter_Pollution[[Pollution_Scenario]][[1]]
Pollution <- Poll_filter_Pollution[[Pollution_Scenario]][[2]]

# 4. Pollution + Drying  ####
# Rescaling filters according to drying
drying_ranges <- unique(nodes_DaFr$Permanence)[order(unique(nodes_DaFr$Permanence))]
Poll_Dry_Inter <- drying_ranges/max(drying_ranges)
Poll_Dry_Inter <- ifelse(Poll_Dry_Inter==1,0.99,Poll_Dry_Inter)
Poll_Dry_Inter <- ifelse(Poll_Dry_Inter==0,0.0001,Poll_Dry_Inter)
for (dry_range in 1:length(drying_ranges)) {
  if (drying_ranges[dry_range]==0) {
    Modif_Spp_tolerance <- rep(0,length(Spp_tolerance))
  }else{
    Modif_Spp_tolerance <- Spp_tolerance^(drying_ranges[dry_range]/max(nodes_DaFr$Permanence))
  }
  
  Dry_and_Poll_Spp_tolerance <- scales::rescale(Modif_Spp_tolerance,to=c(min(Spp_tolerance),max(Spp_tolerance)))
  filter_Pollution[,which(Pollution=="YES_Poll" & nodes_DaFr$Permanence==drying_ranges[dry_range])] <- Dry_and_Poll_Spp_tolerance
}
filter_Pollution_test <- t(filter_Pollution)
colnames(filter_Pollution_test) <- paste(seq(1:length(Spp_tolerance)),"Spe",sep="_")

DataFrame_out_Scen <- Poll_DataFrame_out[[Pollution_Scenario]]
Flow_DB_toSTcon <- Inermitence_dataset_Campaings_To_Run[[1]]

# We plot our beloved river colored according to the weight (order)/community size

g <- igraph::graph_from_adjacency_matrix(as.matrix(Network_stru_Campaings_To_Run[[1]]))
edges_DaFr <- as.data.frame(igraph::get.edgelist(g))
edges_DaFr <- edges_DaFr %>% dplyr::select("from"=V1, "to"=V2) %>%
  mutate(X1_coord=2)%>% mutate(Y1_coord=2)%>% 
  mutate(X2_coord=2)%>% mutate(Y2_coord=2) %>% 
  mutate(from=as.numeric(from), to=as.numeric(to))

# Fill X and Y coordinate information for edges, using the nodes_DaFr
for (nodes in 1:nrow(nodes_DaFr)) {
  edges_DaFr[which(edges_DaFr$from==nodes),3] <- nodes_DaFr[nodes,]$X1
  edges_DaFr[which(edges_DaFr$from==nodes),4] <- nodes_DaFr[nodes,]$X2
  edges_DaFr[which(edges_DaFr$to==nodes),5] <- nodes_DaFr[nodes,]$X1
  edges_DaFr[which(edges_DaFr$to==nodes),6] <- nodes_DaFr[nodes,]$X2
}


Plot_A <- ggplot()+
  geom_segment(data=edges_DaFr, 
               aes(x=X1_coord,y=Y1_coord, xend=X2_coord, yend=Y2_coord), 
               arrow =arrow(length=unit(0.01,"cm"), ends="last"), linewidth=0.2, colour="grey50", alpha=1)+
  geom_point(data=nodes_DaFr, aes(x=X1, y=X2), shape=21)+ # here weight/12000 is random decision for plotting
  scale_fill_viridis(option = "D",discrete = F)+
  scale_size(guide = "none") +
  labs(title = paste("River structure", Poll_diff_extent[Pollution_Scenario]),y="",x="",fill="Comm. Size")+
  theme_void()


# We plot our beloved river colored according to the weight (order)/community size
Alpha_plot <- (edges_DaFr %>% left_join(DataFrame_out_Scen,by=c("from"="Site_ID")) %>% pull(Permanence)/
                 (max(edges_DaFr %>% left_join(DataFrame_out_Scen,by=c("from"="Site_ID")) %>% pull(Permanence))*2))+0.5

Plot_B <- edges_DaFr %>% 
  left_join(DataFrame_out_Scen,by=c("from"="Site_ID")) %>% 
  ggplot()+
  geom_curve(aes(x=X1_coord,y=Y1_coord, xend=X2_coord, yend=Y2_coord,colour=Permanence),
             arrow =arrow(length=unit(0.01,"cm"), ends="last"),
             alpha=Alpha_plot,
             linewidth=1.5, 
             curvature=0.1)+
  geom_point(data=DataFrame_out_Scen,aes(x=X1, y=X2),size=3, colour="grey30", shape=16)+
  scale_colour_viridis(option = "D",discrete = F,direction = -1,limits=c(0,61))+
  labs(title = paste("Scenario",Poll_diff_extent[Pollution_Scenario]),y="",x="",fill="Permanence")+
  theme_void()

# We plot our beloved river colored according to the weight (order)/community size
Plot_C <- edges_DaFr %>% left_join(DataFrame_out_Scen,by=c("from"="Site_ID")) %>% 
  ggplot()+
  geom_curve(aes(x=X1_coord,y=Y1_coord, xend=X2_coord, yend=Y2_coord,colour=Pollution),
             arrow =arrow(length=unit(0.01,"cm"), ends="last"), 
             linewidth=1.5, 
             alpha=0.5,
             curvature=0.1)+
  geom_point(aes(x=X1, y=X2), size=3, colour="grey30", shape=16)+
  scale_color_manual(values = c("blue","red"))+
  scale_size(guide = "none") +
  labs(title = paste("Scenario",Poll_diff_extent[Pollution_Scenario]),y="",x="",fill="Pollution")+
  theme_void()


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
  top= paste("Scenario",Poll_diff_extent[Pollution_Scenario],sep="_"),ncol=1)


plots_temp <- list(gridExtra::arrangeGrob(Plot_A,Plot_B,Plot_C,ncol=3),plot_D)
plots_diagnosis[[Pollution_Scenario]] <- plots_temp
output_to_simulate[[Pollution_Scenario]] <- list(filter_Pollution,DataFrame_out_Scen,Flow_DB_toSTcon)
}
save(plots_diagnosis,file = "Genal_Diagnosis_Plots.RData")

# 6. Coaslescent runs ####
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
J.max<-200+Jmin # What is the maximum J that we want to assign to a community
b.ef<-(0.8) # The coefficient of change. If 1 we do a direct proportion between the two but minimimum becomes "1" (only 1 indiv)

# Species tolerances defined in the 2. script
# Filter of species per site. We will use for tolerance
# Other parameters of the model
id_NOmodule <- rep(1,nrow(nodes_DaFr)) # Modules if we want some sites to belong to the same module. 
pool_200 <- rep(1,nrow(Orig_dispersal_pollution)) # Distribution of the species pool #rlnorm(n = 200,5,1) 
Meta_t0 <- matrix(nrow = length(pool_200), ncol =nrow(nodes_DaFr), 1) #Previous Metacommunity (for considering time relevance)

# Distances are related to the distance matrix. 
# Therefore we must see which values correspond to our connections to set the "D50". Dispersal distance is
# considered here as the distance at which probability of dispersal is 0.5. So "higher" or "lower" do not exclude
# other dispersal abilities. They push "overall connectivity" towards higher or lower connections.
#summary(as.vector(Scen_Swim_STconmat[[pollut]])[-which(as.vector(Scen_Swim_STconmat[[pollut]])==100)]) 

dispersal_test <- c(0.15,0.5,1) # We set the dispersal abilities that we want

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
  foreach(pollut=1:length(Poll_filter_Pollution), .combine=rbind)%dopar%{ # Parallellize for pollution
    
    # J creation
    FW_area <- nodes_DaFr$Permanence
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
        filter.env = Poll_filter_Pollution[[pollut]][[1]], # Pollution scenarios (created at 2. Pollution assignation.R)
        Disp_Strat=Disp_Str,
        Tollerances=Spp_tolerance,
        M.dist =list(Scen_Drift_STconmat[[1]],
                     Scen_Swim_STconmat[[1]],
                     Scen_AAct_STconmat[[1]]), # Distance matrix which corresponds to the STconmat (created at 1. OCnet - STconmat.R)
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
save(Diff_scenarios,file = "Genal_Scenarios.RData")
save(output_to_simulate, file="Genal_Scenarios_OutputToSimul.RData")
# 

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
  
  
  STcon_Drift <- apply(ifelse(Scen_Drift_STconmat[[1]]==100,NA,Scen_Drift_STconmat[[1]]),1,mean, na.rm=T)
  STcon_Swim <- apply(ifelse(Scen_Swim_STconmat[[1]]==100,NA,Scen_Swim_STconmat[[1]]),1,mean, na.rm=T)
  STcon_AAct <- apply(ifelse(Scen_AAct_STconmat[[1]]==100,NA,Scen_AAct_STconmat[[1]]),1,mean, na.rm=T)
  
  STcon_Drift[which(is.nan(STcon_Drift))] <- 1
  Result_df <- bind_cols(output_to_simulate[[round_value]][[2]] %>%
                           data.frame("Pollut_ext"=Poll_diff_extent[round_value],
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
save(Res_nodes_DaFr,file="Genal_Results_scenarios.RData")

# 6. Plots and results outputs ####
library(tidyverse);library(viridis)
load("Genal_Results_scenarios.RData")

colnames(Res_nodes_DaFr)
Res_nodes_DaFr %>% 
  mutate(Pollut_ext=as.numeric(Pollut_ext)) %>% 
  ggplot()+geom_point(aes(x=Mean_STcon,y=S_Sen))+
  geom_smooth(aes(x=Mean_STcon,y=S_Sen,color=Pollut_ext,group=Pollut_ext),method = "loess")+
  scale_color_viridis()+
  facet_wrap(.~Disp)

Data_To_Plot <- Res_nodes_DaFr %>% 
  filter(Disp==0.15) %>% 
  mutate(Pollut_ext=as.numeric(Pollut_ext)) 


# Big raster plot to draw the effect of 
Big_Ref <- Data_To_Plot %>% select(Site_ID , Mean_STcon ,Pollut_ext,Pollution,
                                   "Ref_IBMWP"=IBMWP) %>% 
                            filter(Pollut_ext==0.01)



Inters_Line <- data.frame()
for (Pollution_Scenario in 2:length(Genal_Pollut_Ext)) {
  Out_test <-Data_To_Plot %>% 
    filter(Pollut_ext!=0.01) %>% 
    select(Site_ID,Pollution ,Pollut_ext, Mean_STcon ,IBMWP) %>% 
    left_join(Big_Ref, by=c("Site_ID","Mean_STcon"),relationship = "many-to-many") %>% 
    group_by(Site_ID,Pollution.x,Pollut_ext.x) %>% 
    summarise(Mean_STcon=mean(Mean_STcon),
              IBMWP=mean(IBMWP),
              Ref_IBMWP=mean(Ref_IBMWP)
    ) %>% ungroup() %>% 
    mutate(Rel_STcon=abs(Mean_STcon/max(Mean_STcon)-1)) %>%
    mutate(Diff=IBMWP-Ref_IBMWP) %>% 
    filter(Pollut_ext.x==Genal_Pollut_Ext[Pollution_Scenario])

model <- lm(Diff~Pollution.x*Mean_STcon,data=Out_test)
#print(c(summary(model)[[4]][4,4],Genal_Pollut_Ext[Pollution_Scenario]))
print(summary(model))
intersec_point <- -(coef(model)[2]/coef(model)[4])
Inters_Line_temp <-data.frame(
  "Inters"=intersec_point,
  "Pollut_ext"=Genal_Pollut_Ext[Pollution_Scenario])
Inters_Line <- bind_rows(Inters_Line,Inters_Line_temp)

}

Data_To_Plot %>% 
  filter(Pollut_ext!=0.01) %>% 
  select(Site_ID,Pollution ,Pollut_ext, Mean_STcon ,IBMWP) %>% 
  left_join(Big_Ref, by=c("Site_ID","Mean_STcon"),relationship = "many-to-many") %>%
  left_join(Inters_Line,by=c("Pollut_ext.x"="Pollut_ext")) %>% 
  group_by(Site_ID,Pollution.x,Pollut_ext.x) %>% 
  summarise(Mean_STcon=mean(Mean_STcon),
            IBMWP=mean(IBMWP),
            Ref_IBMWP=mean(Ref_IBMWP),
            Inters=mean(Inters)
  ) %>%ungroup() %>% 
  mutate(Rel_STcon=abs(Mean_STcon/max(Mean_STcon)-1)) %>% 
  #mutate(Drying=Drying/max(Drying)) %>% 
  mutate(Diff=IBMWP-Ref_IBMWP) %>% 
  ggplot()+
  geom_point(aes(x=Mean_STcon,y=Diff,colour=Pollution.x))+
  geom_smooth(aes(x=Mean_STcon,y=Diff,colour=Pollution.x),method="lm")+
  #geom_vline(aes(xintercept = Inters), linetype = "dashed", color = "black")+
  scale_colour_viridis(discrete = T)+
  facet_wrap(.~Pollut_ext.x,ncol=4)+
  #scale_x_reverse()+ #limits=c(0.75,0)
  theme_classic()

Inters_Line %>% ggplot(aes(x=Pollut_ext,y=Inters))+geom_point()

# Extract model coefficien

# Compute intersection point
intersection_connectivity <- - beta2 / beta3
print(intersection_connectivity)

# Model generation and testing
mod.RMA.S1 <-nlme::lme(Diff~ Pollution.x*Mean_STcon, random = ~1|Site_ID,data=Out_test,method="REML")
summary(mod.RMA.S1)
multcomp::cld(lsmeans::lsmeans(mod.RMA.S1,~  Pollution.x*as.factor(Pollut_ext.x)),alpha= 0.05,Letters = letters,adjust = "Bonferroni") ###  Tukey-adjusted comparisons 





