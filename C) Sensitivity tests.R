
library(tidyverse);library(viridis);library(patchwork)

# The following code develops the sensitivity analysis made to understand the relationship between intermittency, community size and richness loss.

# Parts of the following script are copied from the script A) Sim_Scenarios_Generation.R section 4. Simulation running
### PLEASE, go there for a detailed description on each action

load("RData_outputs/PreSTcon.RData")
load("RData_outputs/RData_to_Run_Sensitivity_test.RData")

## 1) COMMUNITY SIZE analysis and plotting ####

## Community size preparation for running the model 
Years_Of_Drying <- 4
Max_Area<-(Years_Of_Drying*12)
Jmin <- 50 # What will be the minimum size at which we will consider "0" 
J.max<-200+Jmin # What is the maximum J that we want to assign to a community
b.ef<-(0.8) # The coefficient of change. If 1 we do a direct proportion between the two but minimimum becomes "1" (only 1 indiv)

# Species tolerances defined in the 2. script
id_NOmodule <- rep(1,nrow(nodes_DaFr)) # Modules if we want some sites to belong to the same module. 
pool_200 <- rep(1,nrow(output_to_simulate[[1]][[1]])) # Distribution of the species pool #rlnorm(n = 200,5,1) 
Meta_t0 <- matrix(nrow = length(pool_200), ncol =nrow(nodes_DaFr), 1) #Previous Metacommunity (for considering time relevance)

# Distances are related to the distance matrix. 
Disp_Str <- Orig_dispersal_pollution%>% 
  mutate(Disp_Strateg=case_when(
    str_detect(dispersal_strategy ,"dis4") ~ "3", 
    str_detect(dispersal_strategy,"dis1") ~ "1",
    str_detect(dispersal_strategy,"dis2") ~ "2",
    TRUE ~ "MISTAKE")) %>% 
  mutate(Disp_Strateg=as.numeric(Disp_Strateg)) %>% pull(Disp_Strateg)

# Obtention of all the sizes used in simulatinos for representation and analysis. 
# Community size is equivalent to node permanence, the higher the permanence, the higher the size (max=200 individuals). 
# There is a minimum threshold of 20 individuals to ensure a sufficient number of individuals even in highly intermittent nodes.  

out_tot <- data.frame()
for (pollut in 1:length(output_to_simulate)) { #seq(1,length(output_to_simulate),by=7)
  FW_area <- output_to_simulate[[pollut]][[2]]$Permanence
  J.freshwater<-ceiling((-Jmin+(J.max/(Max_Area^b.ef))*FW_area^b.ef))
  J.freshwater <- ifelse(J.freshwater<0,20,J.freshwater)
  
  out <- data.frame(
    "Dry_ext"=as.numeric(strsplit(unique(output_to_simulate[[pollut]][[2]]$Scenario),split="_")[[1]][1]),
    "Dry_Pat"=(1-as.numeric(strsplit(unique(output_to_simulate[[pollut]][[2]]$Scenario),split="_")[[1]][2])),
    "Poll_ext"=as.numeric(strsplit(unique(output_to_simulate[[pollut]][[2]]$Scenario),split="_")[[1]][3]),
    "H_Hab"=J.freshwater)
  
  out_tot <- out_tot %>% bind_rows(out)
}

# Plot representing all potential combinations of habitat size as a funcion of Drying extent and Drying intensity
# NOTE that pollution is not modulating community size, therefore it is not considered here as a variable
out_tot %>% filter(Poll_ext%in%c(0.01)) %>%
  mutate(Dry_ext=as.factor(Dry_ext),Dry_Int=as.factor(Dry_Pat)) %>% 
  ggplot()+
  geom_jitter(aes(y=H_Hab,x=Dry_Int,colour=Dry_Int), size=0.2)+
  scale_color_viridis(option="E",discrete = T)+
  facet_grid(.~Dry_ext)+
  labs(colour="Drying intensity",x="Drying intensity",y="Community size (J)")+
  guides(colour=guide_legend(override.aes = list(size=6)))+
  theme_classic()+
  theme(legend.position = "right",
        axis.text.x = element_text(size=6),
        legend.key = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(colour="orange"),
        strip.text = element_text(colour="black"),
        strip.background =  element_rect(fill=alpha("orange",0.2)),
        panel.grid.major.x =element_line(alpha(colour="grey70",0.2))  
  )

# 2) NULL MODEL (No community size) ####
# Running a null model where community size does not change

# We charge the function to run coalescent models
source("functions/H2020_Lattice_expKernel_Jenv_TempMeta_DispStr.R")
dispersal_test <- 2

library(tictoc)
tic() # This is to count the time 
library(doParallel) # We activate the parallelization
registerDoParallel(cores = detectCores()-1) # We keep 1 core to be able to do something else while running
Diff_scenarios <- foreach(dispersal=1:length(dispersal_test), .combine=rbind)%:% # Parallelize for dispersal
  foreach(pollut=1:length(output_to_simulate), .combine=rbind)%dopar%{ # Parallellize for pollution
    
    # J creation
    # FW_area <- output_to_simulate[[pollut]][[2]]$Permanence
    # J.freshwater<-ceiling((-Jmin+(J.max/(Max_Area^b.ef))*FW_area^b.ef))
    # J.freshwater <- ifelse(J.freshwater<0,20,J.freshwater)
    J.freshwater <- rep(200, length(J.freshwater)) # WE FIX J to 200
    
    a <- NULL # We create an output object for each iteration
    #b <- list()
    for (it in 1:10) { # We repeat 10 times the same process
      output <- H2020_Coalescent.and.lottery.exp.Kernel.J_TempMtcom_tempIT(
        Meta.pool = pool_200, # Species pool
        m.pool = 0.001, # Regional dispersal which is always constant 
        Js = J.freshwater, # Size of the communities (AKA: number of individuals/population contained in each community)
        id.module = id_NOmodule, # id of modules if there are some - NOT used for us
        filter.env = output_to_simulate[[pollut]][[1]], # Pollution scenarios (created at 2. Pollution assignation.R)
        Disp_Strat= Disp_Str, #ifelse(Disp_Str==3,1,Disp_Str),
        Tollerances=Spp_tolerance,
        M.dist =list((Scen_Drift_STconmat[[pollut]]),
                     (Scen_Swim_STconmat[[pollut]]),
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

# Same code as previously to extract the values
# This part below extracts the S (richness) and B (average Jaccard) from each site and "data frame" it 
Sens_Test_nodes_DaFr <- data.frame()
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
  Sens_Test_nodes_DaFr <- bind_rows(Sens_Test_nodes_DaFr,Result_df)
}
save(Sens_Test_nodes_DaFr,file="RData_outputs/Sens_Test_Results_scenarios_Const_Size.RData")

# 3) Model COMPARISONS ####  
### Comparisons between null model without community size and model with community size 

# Original dataset used in the paper
load("RData_outputs/Sens_Test_Results_scenarios_Modif_Size.RData")
Data_To_Plot <- Sens_Test_nodes_DaFr %>%
  mutate(Pollution = case_when(
    str_detect(Pollution ,"YES_Poll") ~ "Polluted",
    str_detect(Pollution ,"Non_Poll") ~ "Unpolluted")) %>%
  mutate(Pollution=factor(Pollution,levels=c("Unpolluted","Polluted"))) %>% 
  filter(Disp==2) %>% 
  mutate(Dry_ext=as.numeric(Dry_ext),Dry_patt=as.numeric(Dry_patt),
         Pollut_ext=as.numeric(Pollut_ext)) %>% 
  group_by(Dry_ext, Dry_patt, Pollut_ext) %>% 
  mutate(Max_STcon=max(Mean_STcon)) %>% 
  mutate(Sc_STcon=Mean_STcon/Max_STcon)

# New dataset with constant 
load("RData_outputs/Sens_Test_Results_scenarios_Const_Size.RData")
SeTe_To_Plot <- Sens_Test_nodes_DaFr %>%
  mutate(Pollution = case_when(
    str_detect(Pollution ,"YES_Poll") ~ "Polluted",
    str_detect(Pollution ,"Non_Poll") ~ "Unpolluted")) %>%
  mutate(Pollution=factor(Pollution,levels=c("Unpolluted","Polluted"))) %>% 
  filter(Disp==2) %>% 
  mutate(Dry_ext=as.numeric(Dry_ext),Dry_patt=as.numeric(Dry_patt),
         Pollut_ext=as.numeric(Pollut_ext)) %>% 
  group_by(Dry_ext, Dry_patt, Pollut_ext) %>% 
  mutate(Max_STcon=max(Mean_STcon)) %>% 
  mutate(Sc_STcon=Mean_STcon/Max_STcon)

# Figure 5 comparison with respective reference data
Ref_Performance <- Data_To_Plot %>% 
  group_by(Dry_ext, Dry_patt, Pollut_ext) %>% 
  summarise(Ref_IBMWP=mean(IBMWP)) %>%  
  filter(Dry_ext==0.01,
         Dry_patt==0.9,
         Pollut_ext==0.01) %>% # We need to select this to obtain the set reference in a pristine environment
  ungroup() %>% 
  select(Dry_ext,Ref_IBMWP)

SeTe_Ref_Performance <- SeTe_To_Plot %>% 
  group_by(Dry_ext, Dry_patt, Pollut_ext) %>% 
  summarise(Ref_IBMWP=mean(IBMWP)) %>%  
  filter(Dry_ext==0.01,
         Dry_patt==0.9,
         Pollut_ext==0.01) %>% # We need to select this to obtain the set reference in a pristine environment
  ungroup() %>% 
  select(Dry_ext,Ref_IBMWP) 

# Review plot portraying the different scenarios and comparing null models ####
# Figure Supplementary to compare "drying-driven driven size" + "fixxed size"
# Find code for figures below
Fixed_J_Figure5<-ggplot()+
  geom_point(data=SeTe_To_Plot %>% 
               filter(Pollut_ext==0.1) %>% 
               group_by(Dry_ext, Dry_patt, Pollut_ext,Pollution) %>% 
               summarise(M_IBMWP=mean(IBMWP)) %>%
               mutate(Ref_IBMWP=as.numeric(SeTe_Ref_Performance[,2])) %>%
               mutate(Diff_Ref=Ref_IBMWP-M_IBMWP) %>% 
               select(-M_IBMWP) %>% 
               pivot_wider(names_from = Pollution,values_from = c(Diff_Ref)) %>%
               mutate(Performance=(1-(Unpolluted/Polluted))*100) %>%
               mutate(Pollut_ext=Pollut_ext*100) %>% 
               group_by(Dry_ext,Dry_patt) %>% 
               mutate(Dry_Int=1-Dry_patt) %>% 
               mutate(Dry_ext=Dry_ext*100),
             aes(x=Dry_ext,y=Performance,colour=as.factor(Dry_Int)),shape=17,size=5,alpha=0.6)+
  
  geom_smooth(data=SeTe_To_Plot %>% 
                filter(Pollut_ext==0.1) %>% 
                group_by(Dry_ext, Dry_patt, Pollut_ext,Pollution) %>% 
                summarise(M_IBMWP=mean(IBMWP)) %>%
                mutate(Ref_IBMWP=as.numeric(SeTe_Ref_Performance[,2])) %>%
                mutate(Diff_Ref=Ref_IBMWP-M_IBMWP) %>% 
                select(-M_IBMWP) %>% 
                pivot_wider(names_from = Pollution,values_from = c(Diff_Ref)) %>%
                mutate(Performance=(1-(Unpolluted/Polluted))*100) %>%
                mutate(Pollut_ext=Pollut_ext*100) %>% 
                group_by(Dry_ext,Dry_patt) %>% 
                mutate(Dry_Int=1-Dry_patt) %>% 
                mutate(Dry_ext=Dry_ext*100),
              aes(x=Dry_ext,y=Performance,colour=as.factor(Dry_Int),group=Dry_Int),method="loess",se=F,linetype=2)+
  scale_color_viridis(option="E",discrete = T)+
  scale_y_continuous(limits=c(0,100))+
  #geom_vline(data = DRYvER_inf, aes(xintercept =Dry_ext),linewidth=2,alpha=0.6,
  #           colour=c("#996633","#336600","#009999","#993333","#660066","#666699"))+
  labs(y="Performance (%) at 10 % of Human impact extent",
       x="Drying extent (%)",colour="Drying intensity",
       subtitle = "Fixed community size (J) performance decay as manuscript Figure 5")+
  theme_classic()+
  theme(legend.position = "none")

png(filename = "Figures/Fixed_J_Figure5.png", width = 2000,height = 1500,res = 300) 
Fixed_J_Figure5
dev.off() 

Comparison_both_Scenarios <- Data_To_Plot %>% 
  filter(Pollut_ext==0.1) %>% 
  group_by(Dry_ext, Dry_patt, Pollut_ext,Pollution) %>% 
  summarise(M_IBMWP=mean(IBMWP)) %>%
  mutate(Ref_IBMWP=as.numeric(Ref_Performance[,2])) %>%
  mutate(Diff_Ref=Ref_IBMWP-M_IBMWP) %>% 
  select(-M_IBMWP) %>% 
  pivot_wider(names_from = Pollution,values_from = c(Diff_Ref)) %>%
  mutate(Performance=(1-(Unpolluted/Polluted))*100) %>%
  mutate(Pollut_ext=Pollut_ext*100) %>% 
  group_by(Dry_ext,Dry_patt) %>% 
  mutate(Dry_Int=1-Dry_patt) %>% 
  mutate(Dry_ext=Dry_ext*100) %>% 
  select(Dry_ext,Dry_Int,Performance) %>% 
  left_join(SeTe_To_Plot %>% 
              filter(Pollut_ext==0.1) %>% 
              group_by(Dry_ext, Dry_patt, Pollut_ext,Pollution) %>% 
              summarise(M_IBMWP=mean(IBMWP)) %>%
              mutate(Ref_IBMWP=as.numeric(SeTe_Ref_Performance[,2])) %>%
              mutate(Diff_Ref=Ref_IBMWP-M_IBMWP) %>% 
              select(-M_IBMWP) %>% 
              pivot_wider(names_from = Pollution,values_from = c(Diff_Ref)) %>%
              mutate(Performance=(1-(Unpolluted/Polluted))*100) %>%
              mutate(Pollut_ext=Pollut_ext*100) %>% 
              group_by(Dry_ext,Dry_patt) %>% 
              mutate(Dry_Int=1-Dry_patt) %>% 
              mutate(Dry_ext=Dry_ext*100)%>% 
              select(Dry_ext,Dry_Int,"NoSiz_Perf"=Performance),by=c("Dry_ext","Dry_Int","Dry_patt")) %>% 
  #mutate(Diff_New=NoSiz_Perf-Performance) %>% 
  ggplot()+
  geom_point(aes(x=Dry_ext,y=NoSiz_Perf,colour=as.factor(Dry_Int)),size=3,shape=17)+
  geom_point(aes(x=Dry_ext,y=Performance,colour=as.factor(Dry_Int)),size=1.2)+
  geom_segment(aes(x=Dry_ext,yend=Performance, y=NoSiz_Perf,colour=as.factor(Dry_Int)),linetype=2,linewidth = 0.5)+
  geom_line(aes(x=Dry_ext,y=NoSiz_Perf,colour=as.factor(Dry_Int)),linewidth=1)+
  geom_line(aes(x=Dry_ext,y=Performance,colour=as.factor(Dry_Int)),linewidth=0.5,alpha=0.2)+
  scale_color_viridis(option="E",discrete=T) +
  scale_y_continuous(limits = c(0,100))+
  labs(y="Performance (%) at 10 % of Hum. imp. ext.",x="Drying extent (%)",colour="Drying intensity",
       subtitle="Comparison between Fixed (big circles) and Drying-driven (small circles) across Drying intensity categories")+
  facet_grid(.~Dry_Int)+
  theme_classic()+
  theme(legend.position = "none")

png(filename = "Figures/Fixed_J_ScenComparison.png", width = 4000,height = 1000,res = 300) 
Comparison_both_Scenarios
dev.off() 


# Review plot portraying the different scenarios and comparing null models ####
# FIgure 5 from the manuscript
A <-ggplot()+
  geom_point(data=Data_To_Plot %>% 
               filter(Pollut_ext==0.1) %>% 
               group_by(Dry_ext, Dry_patt, Pollut_ext,Pollution) %>% 
               summarise(M_IBMWP=mean(IBMWP)) %>%
               mutate(Ref_IBMWP=as.numeric(Ref_Performance[,2])) %>%
               mutate(Diff_Ref=Ref_IBMWP-M_IBMWP) %>% 
               select(-M_IBMWP) %>% 
               pivot_wider(names_from = Pollution,values_from = c(Diff_Ref)) %>%
               mutate(Performance=(1-(Unpolluted/Polluted))*100) %>%
               mutate(Pollut_ext=Pollut_ext*100) %>% 
               group_by(Dry_ext,Dry_patt) %>% 
               mutate(Dry_Int=1-Dry_patt) %>% 
               mutate(Dry_ext=Dry_ext*100),
             aes(x=Dry_ext,y=Performance,colour=as.factor(Dry_Int)),size=5,alpha=0.6)+
  geom_smooth(data=Data_To_Plot %>% 
                filter(Pollut_ext==0.1) %>% 
                group_by(Dry_ext, Dry_patt, Pollut_ext,Pollution) %>% 
                summarise(M_IBMWP=mean(IBMWP)) %>%
                mutate(Ref_IBMWP=as.numeric(Ref_Performance[,2])) %>%
                mutate(Diff_Ref=Ref_IBMWP-M_IBMWP) %>% 
                select(-M_IBMWP) %>% 
                pivot_wider(names_from = Pollution,values_from = c(Diff_Ref)) %>%
                mutate(Performance=(1-(Unpolluted/Polluted))*100) %>%
                mutate(Pollut_ext=Pollut_ext*100) %>% 
                group_by(Dry_ext,Dry_patt) %>% 
                mutate(Dry_Int=1-Dry_patt) %>% 
                mutate(Dry_ext=Dry_ext*100),
              aes(x=Dry_ext,y=Performance,colour=as.factor(Dry_Int),group=Dry_Int),method="loess",se=F)+
  
  scale_color_viridis(option="E",discrete = T)+
  scale_y_continuous(limits=c(0,100))+
  #geom_vline(data = DRYvER_inf, aes(xintercept =Dry_ext),linewidth=2,alpha=0.6,
  #           colour=c("#996633","#336600","#009999","#993333","#660066","#666699"))+
  labs(y="Performance (%) at 10 % of Human impact extent",
       x="Drying extent (%)",colour="Drying intensity",
       subtitle="A) Drying-driven community size (J) - Figure 5")+
  theme_classic()+
  theme(legend.position = "none")

# Figure 5 from the manuscript using new simulations with fixed J 
B <-ggplot()+
  geom_point(data=SeTe_To_Plot %>% 
               filter(Pollut_ext==0.1) %>% 
               group_by(Dry_ext, Dry_patt, Pollut_ext,Pollution) %>% 
               summarise(M_IBMWP=mean(IBMWP)) %>%
               mutate(Ref_IBMWP=as.numeric(SeTe_Ref_Performance[,2])) %>%
               mutate(Diff_Ref=Ref_IBMWP-M_IBMWP) %>% 
               select(-M_IBMWP) %>% 
               pivot_wider(names_from = Pollution,values_from = c(Diff_Ref)) %>%
               mutate(Performance=(1-(Unpolluted/Polluted))*100) %>%
               mutate(Pollut_ext=Pollut_ext*100) %>% 
               group_by(Dry_ext,Dry_patt) %>% 
               mutate(Dry_Int=1-Dry_patt) %>% 
               mutate(Dry_ext=Dry_ext*100),
             aes(x=Dry_ext,y=Performance,colour=as.factor(Dry_Int)),shape=17,size=5,alpha=0.6)+
  
  geom_smooth(data=SeTe_To_Plot %>% 
                filter(Pollut_ext==0.1) %>% 
                group_by(Dry_ext, Dry_patt, Pollut_ext,Pollution) %>% 
                summarise(M_IBMWP=mean(IBMWP)) %>%
                mutate(Ref_IBMWP=as.numeric(SeTe_Ref_Performance[,2])) %>%
                mutate(Diff_Ref=Ref_IBMWP-M_IBMWP) %>% 
                select(-M_IBMWP) %>% 
                pivot_wider(names_from = Pollution,values_from = c(Diff_Ref)) %>%
                mutate(Performance=(1-(Unpolluted/Polluted))*100) %>%
                mutate(Pollut_ext=Pollut_ext*100) %>% 
                group_by(Dry_ext,Dry_patt) %>% 
                mutate(Dry_Int=1-Dry_patt) %>% 
                mutate(Dry_ext=Dry_ext*100),
              aes(x=Dry_ext,y=Performance,colour=as.factor(Dry_Int),group=Dry_Int),method="loess",se=F,linetype=2)+
  scale_color_viridis(option="E",discrete = T)+
  scale_y_continuous(limits=c(0,100))+
  #geom_vline(data = DRYvER_inf, aes(xintercept =Dry_ext),linewidth=2,alpha=0.6,
  #           colour=c("#996633","#336600","#009999","#993333","#660066","#666699"))+
  labs(y="Performance (%) at 10 % of Human impact extent",
       x="Drying extent (%)",colour="Drying intensity",
       subtitle = "B) Fixed community size (J)")+
  theme_classic()+
  theme(legend.position = "none")

# Comparisons between fixed and drying-driven 
C <- Data_To_Plot %>% 
  filter(Pollut_ext==0.1) %>% 
  group_by(Dry_ext, Dry_patt, Pollut_ext,Pollution) %>% 
  summarise(M_IBMWP=mean(IBMWP)) %>%
  mutate(Ref_IBMWP=as.numeric(Ref_Performance[,2])) %>%
  mutate(Diff_Ref=Ref_IBMWP-M_IBMWP) %>% 
  select(-M_IBMWP) %>% 
  pivot_wider(names_from = Pollution,values_from = c(Diff_Ref)) %>%
  mutate(Performance=(1-(Unpolluted/Polluted))*100) %>%
  mutate(Pollut_ext=Pollut_ext*100) %>% 
  group_by(Dry_ext,Dry_patt) %>% 
  mutate(Dry_Int=1-Dry_patt) %>% 
  mutate(Dry_ext=Dry_ext*100) %>% 
  select(Dry_ext,Dry_Int,Performance) %>% 
  left_join(SeTe_To_Plot %>% 
              filter(Pollut_ext==0.1) %>% 
              group_by(Dry_ext, Dry_patt, Pollut_ext,Pollution) %>% 
              summarise(M_IBMWP=mean(IBMWP)) %>%
              mutate(Ref_IBMWP=as.numeric(SeTe_Ref_Performance[,2])) %>%
              mutate(Diff_Ref=Ref_IBMWP-M_IBMWP) %>% 
              select(-M_IBMWP) %>% 
              pivot_wider(names_from = Pollution,values_from = c(Diff_Ref)) %>%
              mutate(Performance=(1-(Unpolluted/Polluted))*100) %>%
              mutate(Pollut_ext=Pollut_ext*100) %>% 
              group_by(Dry_ext,Dry_patt) %>% 
              mutate(Dry_Int=1-Dry_patt) %>% 
              mutate(Dry_ext=Dry_ext*100)%>% 
              select(Dry_ext,Dry_Int,"NoSiz_Perf"=Performance),by=c("Dry_ext","Dry_Int","Dry_patt")) %>% 
  #mutate(Diff_New=NoSiz_Perf-Performance) %>% 
  ggplot()+
  geom_point(aes(x=Dry_ext,y=NoSiz_Perf,colour=as.factor(Dry_Int)),size=3,shape=17)+
  geom_point(aes(x=Dry_ext,y=Performance,colour=as.factor(Dry_Int)),size=1.2)+
  geom_segment(aes(x=Dry_ext,yend=Performance, y=NoSiz_Perf,colour=as.factor(Dry_Int)),linetype=2,linewidth = 0.5)+
  geom_line(aes(x=Dry_ext,y=NoSiz_Perf,colour=as.factor(Dry_Int)),linewidth=1)+
  geom_line(aes(x=Dry_ext,y=Performance,colour=as.factor(Dry_Int)),linewidth=0.5,alpha=0.2)+
  scale_color_viridis(option="E",discrete=T) +
  scale_y_continuous(limits = c(0,100))+
  labs(y="Performance (%) as in A)",x="Drying extent (%)",colour="Drying intensity",
       subtitle="D) Comparison between Fixed (big circles) and Drying-driven (small circles) across Drying intensity categories")+
  facet_grid(.~Dry_Int)+
  theme_classic()+
  theme(legend.position = "none")

# Analalysis of performance change
D <- Data_To_Plot %>% 
  filter(Pollut_ext==0.1) %>% 
  group_by(Dry_ext, Dry_patt, Pollut_ext,Pollution) %>% 
  summarise(M_IBMWP=mean(IBMWP)) %>%
  mutate(Ref_IBMWP=as.numeric(Ref_Performance[,2])) %>%
  mutate(Diff_Ref=Ref_IBMWP-M_IBMWP) %>% 
  select(-M_IBMWP) %>% 
  pivot_wider(names_from = Pollution,values_from = c(Diff_Ref)) %>%
  mutate(Performance=(1-(Unpolluted/Polluted))*100) %>%
  mutate(Pollut_ext=Pollut_ext*100) %>% 
  group_by(Dry_ext,Dry_patt) %>% 
  mutate(Dry_Int=1-Dry_patt) %>% 
  mutate(Dry_ext=Dry_ext*100) %>% 
  select(Dry_ext,Dry_Int,Performance) %>% 
  left_join(SeTe_To_Plot %>% 
              filter(Pollut_ext==0.1) %>% 
              group_by(Dry_ext, Dry_patt, Pollut_ext,Pollution) %>% 
              summarise(M_IBMWP=mean(IBMWP)) %>%
              mutate(Ref_IBMWP=as.numeric(SeTe_Ref_Performance[,2])) %>%
              mutate(Diff_Ref=Ref_IBMWP-M_IBMWP) %>% 
              select(-M_IBMWP) %>% 
              pivot_wider(names_from = Pollution,values_from = c(Diff_Ref)) %>%
              mutate(Performance=(1-(Unpolluted/Polluted))*100) %>%
              mutate(Pollut_ext=Pollut_ext*100) %>% 
              group_by(Dry_ext,Dry_patt) %>% 
              mutate(Dry_Int=1-Dry_patt) %>% 
              mutate(Dry_ext=Dry_ext*100)%>% 
              select(Dry_ext,Dry_Int,"NoSiz_Perf"=Performance),by=c("Dry_ext","Dry_Int","Dry_patt")) %>%
  mutate("Perf_Change"=Performance-NoSiz_Perf) %>% 
  group_by(Dry_ext) %>%  
  mutate(Mean_Null=mean(NoSiz_Perf),SD_Null=sd(NoSiz_Perf)) %>%
  mutate(Z_score=((Performance-Mean_Null)/SD_Null)) %>%
  mutate(Sign_Z=ifelse(Z_score>2|Z_score<(-2),"Sign","No_Sign")) %>% 
  ggplot()+
  geom_point(aes(x=Dry_ext,y=Perf_Change,colour=as.factor(Dry_Int),alpha=Sign_Z,size=Sign_Z))+
  geom_segment(aes(x=Dry_ext,y=Perf_Change,yend=0,colour=as.factor(Dry_Int),alpha=Sign_Z),size=1)+
  #geom_line(aes(x=Dry_ext,y=Perf_Change,group=as.factor(Dry_Int),colour=as.factor(Dry_Int)))+
  geom_hline(yintercept = 0,linewidth=2,colour="grey20")+
  scale_alpha_manual(values=c(0.2,1))+
  scale_size_manual(values=c(0.5,3))+
  scale_color_viridis(option="E",discrete=T) + 
  labs(y="Performance change (%) ",
       x="Drying extent (%)",colour="Drying intensity",alpha="Z Score",size="Z Score",
       subtitle="C) Performance change between fixed and drying driven comm. size (Z-score significance -> Big circles)")+
  facet_grid(.~Dry_Int)+
  theme_classic()

layout <- "
AAABBB
AAABBB
DDDDDD
CCCCCC
"
png(filename = paste("Figures/Null_ComSize_Comparison.png", sep=""),
    width =1000*3,height = 850*3,res = 300) 
print(A+B+C+D+patchwork::plot_layout(design = layout))
dev.off() 


png(filename = "Figures/Null_AllScen_Suppl.png",width = 2300*3,height = 2500*3,units = "px",res =300)
SeTe_To_Plot %>% 
  ggplot(aes(x=Sc_STcon, y=S,
             colour=as.factor(1-Dry_patt),fill=as.factor(1-Dry_patt)))+
  geom_smooth(method="lm",se=F)+
  scale_color_viridis(option="E",discrete = T)+
  scale_fill_viridis(option="E",discrete = T)+
  labs(color="Drying intensity",fill="Drying intensity",x="Dispersal resistance",y="S")+
  facet_wrap(Pollut_ext~Dry_ext, ncol=9)+
  theme_classic()+
  theme(strip.background = element_blank(),strip.text.x = element_blank(),
        legend.position = "bottom",
        panel.grid.major.y = element_line(colour="grey"),
        axis.text.x = element_text(size=8))
dev.off()
