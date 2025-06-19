# 6. Plots and results outputs ####
library(tidyverse);library(viridis)
load("RData_outputs/All_Results_scenarios.RData")

# Preparing the data to be ploted
colnames(Res_nodes_DaFr)
Data_To_Plot <- Res_nodes_DaFr %>%
  mutate(Pollution = case_when(
    str_detect(Pollution ,"YES_Poll") ~ "Polluted",
    str_detect(Pollution ,"Non_Poll") ~ "Unpolluted")) %>%
  mutate(Pollution=factor(Pollution,levels=c("Unpolluted","Polluted"))) %>% 
  filter(Disp==0.15) %>% 
  mutate(Dry_ext=as.numeric(Dry_ext),Dry_patt=as.numeric(Dry_patt),
         Pollut_ext=as.numeric(Pollut_ext)) %>% 
  group_by(Dry_ext, Dry_patt, Pollut_ext) %>% 
  mutate(Max_STcon=max(Mean_STcon)) %>% 
  mutate(Sc_STcon=Mean_STcon/Max_STcon)


# Figure 3 ####

png(filename = "Figures/Figure_3.png",width = 2300,height = 2500,units = "px",res =300)
Data_To_Plot %>% 
  filter(Pollut_ext%in%c(0.01,0.5,0.75), 
         Dry_ext%in%c(0.25,0.5,0.75)) %>% 
  ggplot(aes(x=Sc_STcon, y=IBMWP,
             colour=as.factor(1-Dry_patt),fill=as.factor(1-Dry_patt)))+
  geom_point(alpha=0.1,shape=16,size=0.8)+
  geom_smooth(method="lm",se=F)+
  scale_color_viridis(option="E",discrete = T)+
  scale_fill_viridis(option="E",discrete = T)+
  labs(color="Drying intensity",fill="Drying intensity",x="Dispersal resistance",y="IBMWP")+
  facet_wrap(Pollut_ext~Dry_ext, ncol=3,strip.position = "right",axis.labels = "all",axes = "all")+
  theme_classic()+
  theme(strip.background = element_blank(),strip.text.x = element_blank(),
        legend.position = "bottom")
dev.off()

# Figure 3 Supplementary all scenarios ####

png(filename = "Figures/Figure_3_Suppl.png",width = 2300*3,height = 2500*5,units = "px",res =300)
Data_To_Plot %>% 
  ggplot(aes(x=Sc_STcon, y=IBMWP,
             colour=as.factor(1-Dry_patt),fill=as.factor(1-Dry_patt)))+
  geom_point(alpha=0.1,shape=16,size=0.8)+
  geom_smooth(method="lm",se=F)+
  scale_color_viridis(option="E",discrete = T)+
  scale_fill_viridis(option="E",discrete = T)+
  labs(color="Drying intensity",fill="Drying intensity",x="Dispersal resistance",y="IBMWP")+
  facet_wrap(Pollut_ext~Dry_ext, ncol=9,strip.position = "right",axis.labels = "all",axes = "all")+
  theme_classic()+
  theme(strip.background = element_blank(),strip.text.x = element_blank(),
        legend.position = "bottom")
dev.off()


# Figure 3 version Unimpacted vs Impacted ####

Poll_plot <- Data_To_Plot %>% 
  filter(Pollut_ext%in%c(0.01,0.5,0.75), 
         Dry_ext%in%c(0.25,0.5,0.75)) %>%
  filter(Pollution=="Polluted")

UnPoll_plot <- Data_To_Plot %>% 
  filter(Pollut_ext%in%c(0.01,0.5,0.75), 
         Dry_ext%in%c(0.25,0.5,0.75)) %>%
  filter(Pollution=="Unpolluted")

colnames(Data_To_Plot) 
png(filename = "Figures/Figure_3_Unp&Pol.png",width = 3200,height = 2500,units = "px",res =300)
ggplot()+
  geom_point(data=Poll_plot,aes(x=Sc_STcon, y=IBMWP,fill=Pollution),fill="red",alpha=0.2,shape=21,size=0.8)+
  geom_point(data=UnPoll_plot,aes(x=Sc_STcon, y=IBMWP,fill=Pollution),fill="green",alpha=0.2,shape=21,size=0.8)+
  
  geom_smooth(data=Poll_plot, aes(x=Sc_STcon,y=IBMWP,colour=as.factor(1-Dry_patt),group=Dry_patt),
              method="lm",se=F,alpha=0.1,linewidth=1,linetype=1)+
  scale_colour_brewer(palette = "Reds")+  
  labs(colour="Polluted Dry. Int.")+
  ggnewscale::new_scale_colour()+
  geom_smooth(data=UnPoll_plot, aes(x=Sc_STcon,y=IBMWP,colour=as.factor(1-Dry_patt),group=Dry_patt),
              method="lm",se=F,alpha=0.1,linewidth=1,linetype=1)+
  scale_colour_brewer(palette = "Greens")+  
  
  labs(color="Unpolluted Dry. Int.",fill="Impact",
       x="Dispersal resistance",y="IBMWP")+
  facet_wrap(Pollut_ext~Dry_ext, ncol=3,strip.position = "right",axis.labels = "all",axes = "all")+
  theme(legend.position = "bottom")+
  theme_classic()+
  theme(panel.background = element_rect(fill="grey80"))
dev.off()

# Figure 4  ####

Ref_Performance <- Data_To_Plot %>% 
  group_by(Dry_ext, Dry_patt, Pollut_ext,Pollution) %>% 
  summarise(M_IBMWP=mean(IBMWP)) %>% 
  pivot_wider(names_from = Pollution,values_from = c(M_IBMWP)) %>%
  mutate(Performance=Unpolluted-Polluted) %>%
  filter(Dry_ext==0.01,Dry_patt==0.9,
         Pollut_ext==0.01) %>% # We need to select this to obtain the set reference in a pristine environment
  ungroup() %>% select(Pollut_ext,Performance) %>% 
  rename(Ref_Performance=Performance)

png(filename = "Figures/Figure_4.png",width = 1500,height = 3000,units = "px",res =300)
Data_To_Plot %>% 
  group_by(Dry_ext, Dry_patt, Pollut_ext,Pollution) %>% 
  summarise(M_IBMWP=mean(IBMWP)) %>% 
  pivot_wider(names_from = Pollution,values_from = c(M_IBMWP)) %>%
  mutate(Performance=Unpolluted-Polluted) %>% 
  mutate(Dry_ext=Dry_ext*100) %>% 
  #left_join(Ref_Performance, by="Pollut_ext") %>%
  mutate(Ref_Performance=as.numeric(Ref_Performance[,2])) %>% 
  mutate(Perf_In_Perc=(Performance/Ref_Performance)*100) %>% 
  filter(Pollut_ext%in%c(0.01,0.25,0.5,0.65,0.9)) %>% 
  ggplot()+
  #geom_hline(yintercept = c(-0,-10,-20,-30),colour="grey80")+
  geom_jitter(aes(x=Dry_ext,y=Perf_In_Perc,colour=as.factor(1-Dry_patt)), width = 0.02,height = 0,size=2,alpha=0.6)+
  geom_smooth(aes(x=Dry_ext,y=Perf_In_Perc,colour=as.factor(1-Dry_patt),group=Dry_patt),se=F,method="lm")+
  scale_color_viridis(option="E",discrete = T)+
  labs(colour="Dryint intensity", y="Performance",
       x="Drying extent (%)")+
  scale_y_continuous(breaks = c(0,25,50,75,100))+
  facet_wrap(Pollut_ext~., ncol=1,strip.position = "right",axis.labels = "all",axes = "all")+
  theme_classic()+
  theme(legend.position = "bottom")
dev.off()

# Figure 4 supplementary ####

png(filename = "Figures/Figure_4_Suppl.png",width = 3000,height = 3000,units = "px",res =300)
Data_To_Plot %>% 
  group_by(Dry_ext, Dry_patt, Pollut_ext,Pollution) %>% 
  summarise(M_IBMWP=mean(IBMWP)) %>% 
  pivot_wider(names_from = Pollution,values_from = c(M_IBMWP)) %>%
  mutate(Performance=Unpolluted-Polluted) %>% 
  mutate(Dry_ext=Dry_ext*100) %>% 
  #left_join(Ref_Performance, by="Pollut_ext") %>%
  mutate(Ref_Performance=as.numeric(Ref_Performance[,2])) %>% 
  mutate(Perf_In_Perc=(Performance/Ref_Performance)*100) %>% 
  #filter(Pollut_ext%in%c(0.01,0.25,0.5,0.65,0.9)) %>% 
  ggplot()+
  #geom_hline(yintercept = c(-0,-10,-20,-30),colour="grey80")+
  geom_jitter(aes(x=Dry_ext,y=Perf_In_Perc,colour=as.factor(1-Dry_patt)), width = 0.02,height = 0,size=2,alpha=0.6)+
  geom_smooth(aes(x=Dry_ext,y=Perf_In_Perc,colour=as.factor(1-Dry_patt),group=Dry_patt),se=F,method="lm")+
  scale_color_viridis(option="E",discrete = T)+
  labs(colour="Dryint intensity", y="Performance",
       x="Drying extent (%)")+
  scale_y_continuous(breaks = c(0,25,50,75,100))+
  facet_wrap(Pollut_ext~., ncol=3,strip.position = "top",axis.labels = "all",axes = "all")+
  theme_classic()+
  theme(legend.position = "bottom")
dev.off()
