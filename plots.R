# 6. Plots and results outputs ####
library(tidyverse);library(viridis)
load("./data/Results_Good_scenarios.RData")

colnames(Res_nodes_DaFr)
Data_To_Plot <- Res_nodes_DaFr %>% filter(Disp==0.15)
Data_To_Plot_Ref <- Res_nodes_DaFr %>% filter(Disp==0.15,Scenario=="0.01_0.01")
Data_To_Plot_Ref

unique(Data_To_Plot$Scenario)
colnames(Data_To_Plot)

# plots only filtering for pollution extent=0.01
# STcon vs S for mean and different dispersal categories
gridExtra::grid.arrange(
  Data_To_Plot %>% ggplot()+
    geom_smooth(aes(x=Mean_STcon,y=Permanence,colour=Dry_ext),method = "lm",se=T)+
    scale_colour_viridis(discrete = T,option = "D",direction = 1)+
    #facet_wrap(.~Pollut_ext)+
    theme_classic(),
  
  Data_To_Plot %>% filter(Pollut_ext==0.01) %>% 
    ggplot()+
    geom_smooth(aes(x=Mean_STcon,y=S,colour=Dry_ext),method = "lm",se=T)+
    scale_colour_viridis(discrete = T,option = "D",direction = 1)+
    #facet_wrap(.~Pollut_ext)+
    theme_classic(),
  
  Data_To_Plot %>% filter(Pollut_ext==0.01) %>%
    ggplot()+
    geom_smooth(aes(x=STcon_AAct,y=S_AAct,colour=Dry_ext),method = "lm",se=T)+
    scale_colour_viridis(discrete = T,option = "D",direction = 1)+
    #facet_wrap(.~Pollut_ext)+
    theme_classic(),
  
  Data_To_Plot %>% filter(Pollut_ext==0.01) %>%
    ggplot()+
    geom_smooth(aes(x=STcon_Drift,y=S_Drift,colour=Dry_ext),method = "lm",se=T)+
    scale_colour_viridis(discrete = T,option = "D",direction = 1)+
    #facet_wrap(.~Pollut_ext)+
    theme_classic(),
  
  Data_To_Plot %>% filter(Pollut_ext==0.01) %>%
    ggplot()+
    geom_smooth(aes(x=STcon_Swim,y=S_Swim,colour=Dry_ext),method = "lm",se=T)+
    scale_colour_viridis(discrete = T,option = "D",direction = 1)+
    #facet_wrap(.~Pollut_ext)+
    theme_classic(),
  ncol=5)

#STcon vs Permanence and Stcon vs B (beta diversity)
gridExtra::grid.arrange(
  Data_To_Plot %>% ggplot()+
    geom_smooth(aes(x=Mean_STcon,y=Permanence,colour=Dry_ext),method = "lm",se=T)+
    scale_colour_viridis(discrete = T,option = "D",direction = 1)+
    #facet_wrap(.~Pollut_ext)+
    theme_classic(),
  
  Data_To_Plot %>% filter(Pollut_ext==0.01) %>% 
    ggplot()+
    geom_smooth(aes(x=Mean_STcon,y=B,colour=Dry_ext),method = "lm",se=T)+
    scale_colour_viridis(discrete = T,option = "D",direction = 1)+
    #facet_wrap(.~Pollut_ext)+
    theme_classic()
)


# SD IBMWP at polluted vs non-polluted sites
Data_To_Plot %>% group_by(Pollut_ext,Dry_ext,Pollution) %>% 
  summarise(Vari=sd(IBMWP),MeanMean=sd(Mean_STcon)) %>% 
  ggplot()+
  geom_boxplot(aes(x=Pollution,y=Vari,colour=Pollution))+
  facet_wrap(.~Dry_ext)+
  scale_colour_viridis(discrete = T,option = "D",direction = 1)+
  theme_classic()+theme(legend.position = "none")

# Mean IBMWP at polluted vs non-polluted sites
Data_To_Plot %>% group_by(Pollut_ext,Dry_ext,Pollution) %>% 
  summarise(Vari=mean(IBMWP),MeanMean=sd(Mean_STcon)) %>% 
  ggplot()+
  geom_boxplot(aes(x=Pollution,y=Vari,colour=Pollution))+
  facet_wrap(.~Dry_ext)+
  scale_colour_viridis(discrete = T,option = "D",direction = 1)+
  theme_classic()+theme(legend.position = "none")

# test
unique(Res_nodes_DaFr$Dry_ext)
Test_list <- list()
SEMU <- data.frame()
for (Driri in 1:length(unique(Res_nodes_DaFr$Dry_ext))) {
  Dry_ext_to_test <- unique(Res_nodes_DaFr$Dry_ext)[Driri]
  
  #Perm <- summary(Data_To_Plot %>% filter(Dry_ext==Dry_ext_to_test,Pollut_ext!=0.01) %>% 
  #                  pull(Mean_STcon))[2]
  Data_To_Plot_Ref <- Res_nodes_DaFr %>% filter(Disp==0.15,Dry_ext==0.01,Pollut_ext==0.01) #%>% 
  #filter(Mean_STcon>Perm)
  
  Data_To_Plot_Z <- Data_To_Plot %>% 
    mutate(Z_test=((IBMWP-mean(Data_To_Plot_Ref$IBMWP))/sd(Data_To_Plot_Ref$IBMWP))) %>%
    filter(Dry_ext==Dry_ext_to_test,Pollut_ext!=0.01) %>% 
    group_by(Dry_ext,Pollut_ext,Pollution) 
  
  Data_To_Plot_Z %>%mutate(Z_Mean=mean(Z_test),Z_sd=sd(Z_test)) %>% 
    ggplot() +
    geom_point(aes(x=Z_test,y=Mean_STcon,fill=Pollut_ext,shape=Pollution),alpha=0.05)+
    geom_point(aes(x=Z_Mean,y=Pollut_ext,colour=Pollution),alpha=0.5)+
    geom_errorbar(aes(x=Z_Mean,y=Pollut_ext,xmin = Z_Mean-Z_sd, xmax = Z_Mean+Z_sd,colour=Pollution))+
    scale_fill_viridis(discrete = T,option = "G",direction = 1)+
    scale_colour_manual(values = c("grey","darkgreen"))+
    scale_shape_manual(values=c(21,22))+
    geom_vline(xintercept=c(0),colour="black",size=1,linetype=2)+
    scale_x_continuous(limits = c(-10,3))+
    labs(title=paste("Drying extention:",Dry_ext_to_test))+
    theme_classic()+facet_wrap(.~Pollut_ext,nrow = 2)
  
  Test_list[[Driri]] <- Data_To_Plot_Z %>%mutate(Z_Mean=mean(Z_test),Z_sd=sd(Z_test)) %>% 
    ggplot() +
    geom_point(aes(x=Z_test,y=Mean_STcon,fill=Mean_STcon,shape=Pollution,colour=Pollution),alpha=0.5)+
    #geom_point(aes(x=Z_Mean,y=Pollut_ext,colour=Pollution),alpha=0.5)+
    #geom_errorbar(aes(x=Z_Mean,y=Pollut_ext,xmin = Z_Mean-Z_sd, xmax = Z_Mean+Z_sd,colour=Pollution))+
    scale_fill_viridis(discrete = F,option = "D",direction = 1)+
    scale_colour_manual(values = c("grey","red"))+
    scale_shape_manual(values=c(21,22))+
    geom_vline(xintercept=c(0),colour="black",size=1,linetype=2)+
    scale_x_continuous(limits = c(-10,3))+
    labs(title=paste("Drying extention:",Dry_ext_to_test))+
    theme_classic()#+facet_wrap(.~Pollut_ext,nrow = 2)
  
  test <- Data_To_Plot_Z %>% select(Pollution,Z_test)
  
  letssee <- c()
  for (Pollut in 1:(length(unique(test$Pollut_ext))-1)) {
    To_test <- test %>% filter(Pollut_ext==unique(test$Pollut_ext)[Pollut]) %>%  
      group_by(Pollution) %>% summarise(Mean=mean(Z_test)) %>%
      pull(Mean)
    letssee[Pollut] <- To_test[1]-To_test[2]
  }
  SEM <- data.frame(letssee,Pollut=unique(test$Pollut_ext)[-10],Dry=unique(test$Dry_ext))
  
  SEMU <- bind_rows(SEMU,SEM)
}

png(filename = "ScenarioS_STcon.png",width = 4000,height = 4000,units = "px",res =300)
gridExtra::grid.arrange(
  gridExtra::grid.arrange(Test_list[[1]]),gridExtra::grid.arrange(Test_list[[2]]),
  gridExtra::grid.arrange(Test_list[[3]]),gridExtra::grid.arrange(Test_list[[4]]),
  gridExtra::grid.arrange(Test_list[[5]]),gridExtra::grid.arrange(Test_list[[6]]),
  gridExtra::grid.arrange(Test_list[[7]]),gridExtra::grid.arrange(Test_list[[8]]),
  gridExtra::grid.arrange(Test_list[[9]]),gridExtra::grid.arrange(Test_list[[10]]),
  gridExtra::grid.arrange(Test_list[[11]]))
dev.off()


SEMU %>% ggplot()+
  geom_point(aes(x=as.numeric(Dry),y=letssee,colour=as.factor(Pollut)))+
  geom_smooth(aes(x=as.numeric(Dry),y=letssee,colour=as.factor(Pollut)),method = "lm",se=F)+
  scale_color_viridis(discrete = T,option = "G",direction = -1)+theme_classic()+
  geom_hline(yintercept = 1,colour="red",size=1.2,linetype=2)+
  #geom_hline(yintercept = -1,colour="red",size=1.2,linetype=2)+
  labs(title="Ratio Sensitive/Tolerant", colour="Pollution",x="Drying extent")+
  scale_y_continuous(limits = c(-0.5,7))


am <- aov(letssee~as.factor(Pollut),data=SEMU)
TukeyHSD(am)










Data_To_Plot %>% #filter(Scenario=="0.01_0.01") %>% 
  mutate(Ref_Value=rep(Data_To_Plot_Ref$S_Swim,11*11)) %>% 
  filter(Scenario!="0.01_0.01") %>%
  mutate(Value_to_Plot=S_Swim/Ref_Value) %>% 
  ggplot()+
  geom_smooth(aes(x=STcon_Swim,y=Value_to_Plot,colour=Pollut_ext), method="lm",se=F)+
  scale_colour_viridis(discrete = T,option = "G",direction = -1)+
  scale_y_continuous(limits = c(-0.5,1.5))+
  facet_wrap(.~Dry_ext)+
  geom_hline(yintercept=c(0,1),colour="black")+
  theme_classic()+theme(legend.position = "none")

Data_To_Plot %>% #filter(Scenario=="0.01_0.01") %>% 
  mutate(Ref_Value=rep(Data_To_Plot_Ref$S_Drift,11*11)) %>% 
  filter(Scenario!="0.01_0.01") %>%
  mutate(Value_to_Plot=S_Drift/Ref_Value) %>% 
  ggplot()+
  geom_smooth(aes(x=STcon_Drift,y=Value_to_Plot,colour=Pollut_ext), method="lm",se=F)+
  scale_colour_viridis(discrete = T,option = "G",direction = -1)+
  scale_y_continuous(limits = c(-0.5,1.5))+
  facet_wrap(.~Dry_ext)+
  geom_hline(yintercept=c(0,1),colour="black")+
  theme_classic()+theme(legend.position = "none")

Data_To_Plot %>% #filter(Scenario=="0.01_0.01") %>% 
  mutate(Ref_Value=rep(Data_To_Plot_Ref$S_AAct,11*11)) %>% 
  filter(Scenario!="0.01_0.01") %>%
  mutate(Value_to_Plot=S_AAct/Ref_Value) %>% 
  ggplot()+
  geom_smooth(aes(x=STcon_AAct,y=Value_to_Plot,colour=Pollut_ext), method="lm",se=F)+
  scale_colour_viridis(discrete = T,option = "G",direction = -1)+
  scale_y_continuous(limits = c(-0.5,1.5))+
  facet_wrap(.~Dry_ext)+
  geom_hline(yintercept=c(0,1),colour="black")+
  theme_classic()+theme(legend.position = "none")











