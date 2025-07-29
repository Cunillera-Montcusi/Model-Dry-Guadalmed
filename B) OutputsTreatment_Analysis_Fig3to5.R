
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

unique(
Data_To_Plot %>%
  mutate(Dry_ext=Dry_ext*100,
         Dry_int=(1-Dry_patt),
         Permanence=(Permanence/48)*100) %>% 
  filter(Dry_ext==50,Dry_int==0.5) %>% 
  pull(Permanence)
)


Data_To_Plot %>%
  mutate(Dry_ext=Dry_ext*100,
         Dry_int=(1-Dry_patt),
         Permanence=(Permanence/48)*100) %>% 
  mutate(Dry_ext=paste("Dry ext=", Dry_ext,sep=" ")) %>% 
  ggplot(aes(x=as.factor(Dry_int),y=Mean_STcon ,fill=as.factor(Dry_int)))+
  scale_color_viridis(option="E",discrete = T)+
  scale_fill_viridis(option="E",discrete = T)+
  geom_boxplot()+
  labs(color="Drying intensity",fill="Drying intensity",x="Drying intensity",
       y="Dispersal resistance (STcon)")+
  facet_wrap(Dry_ext~., ncol=5,strip.position = "top",axis.labels = "all",axes = "all")+
  theme_classic()+
  theme(legend.position = "right",
          legend.key = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(colour="orange"),
          strip.text = element_text(colour="darkorange"),
          strip.background =  element_rect(fill=alpha("orange",0.2)),
          axis.text.x=element_text(colour=viridis(n = 1,option = "E")),
          axis.title.x=element_text(colour=viridis(n = 1,option = "E")))
  
Data_To_Plot %>%
  mutate(Dry_ext=Dry_ext*100,Dry_int=(1-Dry_patt),
         Permanence=(Permanence/48)*100) %>%
  filter(Pollut_ext==0.01) %>% 
  mutate(Dry_ext=paste("Dry ext=", Dry_ext,sep=" ")) %>% 
  group_by(Dry_ext, Dry_int, Pollut_ext,Site_ID) %>% 
  mutate(Groups=ifelse(Permanence==100, "100 %",
                ifelse(Permanence<99 & Permanence>75, ">75 %",
                ifelse(Permanence<74 & Permanence>50,">50 %",
                ifelse(Permanence<49 & Permanence>25,">25","<25"))))) %>%
  mutate(Groups=factor(Groups,levels=c("100 %",">75 %",">50 %",">25","<25"))) %>% 
  select(Groups) %>% 
  group_by(Dry_ext, Dry_int, Pollut_ext,Groups) %>% summarise(Groups_N=n()/225) %>% 
  ggplot(aes(x=as.factor(Dry_int),y=Groups_N,fill=Groups))+
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette = "BrBG",direction = -1)+
  labs(fill="Drying categories",x="Drying intensity",y="Dryin categories (%)")+
  facet_wrap(Dry_ext~., ncol=5,strip.position = "top",axis.labels = "all",axes = "all")+
  theme_classic()+
  theme(legend.position = "right",
        legend.key = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(colour="orange"),
        strip.text = element_text(colour="darkorange"),
        strip.background =  element_rect(fill=alpha("orange",0.2)),
        axis.text.x=element_text(colour=viridis(n = 1,option = "E")),
        axis.title.x=element_text(colour=viridis(n = 1,option = "E")))


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

png(filename = "Figures/Figure_3_All_S.png",width = 2800,height = 3000,units = "px",res =300)
Data_To_Plot %>% 
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


png(filename = "Figures/Figure_3_Intercepts.png",width = 4600,height = 1200,units = "px",res =300)
Data_To_Plot %>% 
  mutate(Dry_Inten=1-Dry_patt) %>% 
  group_by(Pollut_ext,Dry_ext,Dry_Inten) %>% 
  #group_by(Var1) %>% # You can add here additional grouping variables if your real data set enables it
  do(mod = lm(IBMWP ~ Sc_STcon, data = .)) %>%
  mutate(
    Intercept = summary(mod)$coeff[1],
    Slope = summary(mod)$coeff[2],
    pvalue=summary(mod)$coeff[8]) %>%
  mutate(Sign=ifelse(pvalue<0.1,"Sign","NO.sign")) %>% 
  #filter(Sign=="Sign") %>% 
  pivot_longer(5) %>% 
  mutate(Dry_ext=paste("Dry ext=", Dry_ext,sep=" ")) %>% 
  ggplot(aes(x=Dry_Inten,y=Pollut_ext*100,
             fill=as.factor(Dry_Inten)))+
  geom_point(aes(size=abs(value)),shape=21,stroke=1)+
  scale_fill_viridis(option="E",discrete = T)+
  scale_alpha_manual(values=c(0.1,1))+
  facet_grid(.~as.factor(Dry_ext))+
  labs(fill="Dry Intensity",
       x="Drying Intensity (%)",
       y="Anthropogenic impact extent (%)")+
  guides(size="none",alpha="none")+
  theme_classic()+
  theme(legend.position = "right",
      legend.key = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(colour="orange"),
      strip.text = element_text(colour="darkorange"),
      strip.background =  element_rect(fill=alpha("orange",0.2)),
      axis.text.x=element_text(colour=viridis(n = 1,option = "E")),
      axis.title.x=element_text(colour=viridis(n = 1,option = "E")))
dev.off()


png(filename = "Figures/Figure_3_Slopes.png",width = 4600,height = 1200,units = "px",res =300)
Data_To_Plot %>% 
  mutate(Dry_Inten=1-Dry_patt) %>% 
  group_by(Pollut_ext,Dry_ext,Dry_Inten) %>% 
  #group_by(Var1) %>% # You can add here additional grouping variables if your real data set enables it
  do(mod = lm(IBMWP ~ Sc_STcon, data = .)) %>%
  mutate(
    Intercept = summary(mod)$coeff[1],
    Slope = summary(mod)$coeff[2],
    pvalue=summary(mod)$coeff[8]) %>%
  mutate(Sign=ifelse(pvalue<0.1,"Sign","NO.sign")) %>% 
  #filter(Sign=="Sign") %>% 
  pivot_longer(6) %>%
  mutate(Dry_ext=paste("Dry ext=", Dry_ext,sep=" ")) %>% 
  ggplot(aes(x=Dry_Inten,y=Pollut_ext*100,
             fill=value,
             colour=as.factor(Dry_Inten)))+
  geom_point(aes(size=abs(value)),shape=21,stroke=1.2)+
  scale_color_viridis(option="E",discrete = T)+
  scale_fill_gradient2(low =RColorBrewer::brewer.pal(3,"RdBu")[1],
                       mid = RColorBrewer::brewer.pal(3,"RdBu")[2],
                       high=RColorBrewer::brewer.pal(3,"RdBu")[3],
                       midpoint = 0)+
  scale_alpha_manual(values=c(0.1,1))+
  facet_grid(.~as.factor(Dry_ext))+
  labs(colour="Dry Intensity",fill="Slope",
       x="Drying Intensity (%)",
       y="Anthropogenic impact extent (%)")+
  guides(size="none",alpha="none")+
  theme_classic()+
  theme(legend.position = "right",
        legend.key = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(colour="orange"),
        strip.text = element_text(colour="darkorange"),
        strip.background =  element_rect(fill=alpha("orange",0.2)),
        axis.text.x=element_text(colour=viridis(n = 1,option = "E")),
        axis.title.x=element_text(colour=viridis(n = 1,option = "E")))
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
  group_by(Dry_ext, Dry_patt, Pollut_ext) %>% 
  summarise(Ref_IBMWP=mean(IBMWP)) %>%  
  filter(Dry_ext==0.01,
         Dry_patt==0.9,
         Pollut_ext==0.01) %>% # We need to select this to obtain the set reference in a pristine environment
  ungroup() %>% 
  select(Dry_ext,Ref_IBMWP) 

png(filename = "Figures/Figure_4.png",width = 3000,height = 800,units = "px",res =300)
Data_To_Plot %>% 
  group_by(Dry_ext, Dry_patt, Pollut_ext,Pollution) %>% 
  summarise(M_IBMWP=mean(IBMWP)) %>%
  mutate(Ref_IBMWP=as.numeric(Ref_Performance[,2])) %>%
  #left_join(Ref_Performance, by="Dry_ext") %>%
  mutate(Diff_Ref=Ref_IBMWP-M_IBMWP) %>% 
  select(-M_IBMWP) %>% 
  pivot_wider(names_from = Pollution,values_from = c(Diff_Ref)) %>%
  mutate(Performance=100-(Unpolluted/Polluted)*100) %>%
  mutate(Pollut_ext=Pollut_ext*100) %>% 
  #mutate(Ref_Performance=as.numeric(Ref_Performance[,2])) %>% 
  mutate(Perf_In_Perc=(Performance*1)) %>% 
  filter(Dry_ext%in%c(0.1,0.25,0.5,0.75,0.9)) %>% 
  mutate(Dry_ext=Dry_ext*100) %>% 
  mutate(Dry_ext=paste("Dry ext=", Dry_ext,sep=" ")) %>% 
  ggplot()+
  #geom_hline(yintercept = c(-0,-10,-20,-30),colour="grey80")+
  geom_jitter(aes(x=Pollut_ext,y=Perf_In_Perc,colour=as.factor(1-Dry_patt)), width = 0.02,height = 0,size=2,alpha=0.6)+
  geom_smooth(aes(x=Pollut_ext,y=Perf_In_Perc,colour=as.factor(1-Dry_patt),group=Dry_patt),se=F,method="lm")+
  scale_color_viridis(option="E",discrete = T)+
  labs(colour="Drying intensity", y="Performance",x="Anthropogenic impact extent (%)")+
  scale_y_continuous(limits = c(-5,105),labels =c(0,25,50,75,100) )+
  facet_wrap(Dry_ext~., nrow = 1,strip.position = "top",scales = "fixed")+
  theme_classic()+
  theme(legend.position = "right",
        legend.key = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(colour="orange"),
        strip.text = element_text(colour="darkorange"),
        strip.background =  element_rect(fill=alpha("orange",0.2)),
        axis.text.x=element_text(colour="darkred"),
        axis.title.x=element_text(colour="darkred"))
dev.off()


# Figure 4 Based on intensity  ####
Ref_Performance <- Data_To_Plot %>% 
  group_by(Dry_ext, Dry_patt, Pollut_ext) %>% 
  summarise(Ref_IBMWP=mean(IBMWP)) %>%  
  filter(Dry_ext==0.01,
    #Dry_patt==0.9,
    Pollut_ext==0.01) %>% # We need to select this to obtain the set reference in a pristine environment
  ungroup() %>% 
  select(Dry_patt,Ref_IBMWP) 

png(filename = "Figures/Figure_4_Dry_Inten.png",width = 3000,height = 800,units = "px",res =300)
Data_To_Plot %>% 
  group_by(Dry_ext, Dry_patt, Pollut_ext,Pollution) %>% 
  summarise(M_IBMWP=mean(IBMWP)) %>%
  #mutate(Ref_IBMWP=as.numeric(Ref_Performance[,2])) %>%
  left_join(Ref_Performance, by="Dry_patt") %>%
  mutate(Diff_Ref=Ref_IBMWP-M_IBMWP) %>% 
  select(-M_IBMWP) %>% 
  pivot_wider(names_from = Pollution,values_from = c(Diff_Ref)) %>%
  mutate(Performance=100-(Unpolluted/Polluted)*100) %>%
  mutate(Pollut_ext=Pollut_ext*100) %>% 
  #mutate(Ref_Performance=as.numeric(Ref_Performance[,2])) %>% 
  mutate(Perf_In_Perc=(Performance*1)) %>% 
  filter(Dry_patt%in%c(0.1,0.25,0.5,0.75,0.9)) %>% 
  mutate(Dry_patt=1-Dry_patt) %>% 
  mutate(Dry_patt=paste("Dry Int.=", Dry_patt,sep=" ")) %>% 
  ggplot()+
  #geom_hline(yintercept = c(-0,-10,-20,-30),colour="grey80")+
  geom_jitter(aes(x=Pollut_ext,y=Perf_In_Perc,colour=as.factor(Dry_ext)), width = 0.02,height = 0,size=2,alpha=0.6)+
  geom_smooth(aes(x=Pollut_ext,y=Perf_In_Perc,colour=as.factor(Dry_ext),group=Dry_ext),se=F,method="lm")+
  scale_color_brewer(palette = "YlOrBr",direction = -1)+
  #scale_color_viridis(option="E",discrete = T)+
  labs(colour="Drying Extent", y="Performance",x="Anthropogenic impact extent (%)")+
  scale_y_continuous(limits = c(-5,105),labels =c(0,25,50,75,100) )+
  facet_wrap(Dry_patt~., nrow = 1,strip.position = "top",scales = "fixed")+
  theme_classic()+
  theme(legend.position = "right",
        legend.key = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(colour=viridis(n = 2,option = "E")[1],fill="grey70"),
        strip.text = element_text(colour=viridis(n = 2,option = "E")[2]),
        strip.background =  element_rect(fill=viridis(n = 2,option = "E")[1]),
        axis.text.x=element_text(colour="darkred"),
        axis.title.x=element_text(colour="darkred"))
dev.off()



# Figure 4 supplementary ####
Ref_Performance <- Data_To_Plot %>% 
  group_by(Dry_ext, Dry_patt, Pollut_ext) %>% 
  summarise(Ref_IBMWP=mean(IBMWP)) %>%  
  filter(Dry_ext==0.01,
         Dry_patt==0.9,
         Pollut_ext==0.01) %>% # We need to select this to obtain the set reference in a pristine environment
  ungroup() %>% 
  select(Dry_ext,Ref_IBMWP) 

png(filename = "Figures/Figure_4_Suppl.png",width = 3000,height = 3000,units = "px",res =300)
Data_To_Plot %>% 
  group_by(Dry_ext, Dry_patt, Pollut_ext,Pollution) %>% 
  summarise(M_IBMWP=mean(IBMWP)) %>%
  mutate(Ref_IBMWP=as.numeric(Ref_Performance[,2])) %>%
  #left_join(Ref_Performance, by="Dry_ext") %>%
  mutate(Diff_Ref=Ref_IBMWP-M_IBMWP) %>% 
  select(-M_IBMWP) %>% 
  pivot_wider(names_from = Pollution,values_from = c(Diff_Ref)) %>%
  mutate(Performance=100-(Unpolluted/Polluted)*100) %>%
  mutate(Pollut_ext=Pollut_ext*100) %>% 
  #mutate(Ref_Performance=as.numeric(Ref_Performance[,2])) %>% 
  mutate(Perf_In_Perc=(Performance*1)) %>% 
  mutate(Dry_ext=Dry_ext*100) %>% 
  mutate(Dry_ext=paste("Dry ext=", Dry_ext,sep=" ")) %>% 
  ggplot()+
  #geom_hline(yintercept = c(-0,-10,-20,-30),colour="grey80")+
  geom_jitter(aes(x=Pollut_ext,y=Perf_In_Perc,colour=as.factor(1-Dry_patt)), width = 0.02,height = 0,size=2,alpha=0.6)+
  geom_smooth(aes(x=Pollut_ext,y=Perf_In_Perc,colour=as.factor(1-Dry_patt),group=Dry_patt),se=F,method="lm")+
  scale_color_viridis(option="E",discrete = T)+
  labs(colour="Drying intensity", y="Performance",x="Anthropogenic impact extent (%)")+
  scale_y_continuous(limits = c(-5,105),labels =c(0,25,50,75,100) )+
  facet_wrap(Dry_ext~., nrow = 3,strip.position = "top",scales = "fixed")+
  theme_classic()+
  theme(legend.position = "right",
        legend.key = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(colour="orange"),
        strip.text = element_text(colour="darkorange"),
        strip.background =  element_rect(fill=alpha("orange",0.2)),
        axis.text.x=element_text(colour="darkred"),
        axis.title.x=element_text(colour="darkred"))
dev.off()


# Figure 5 DRYvER rivers addition ####
#load("data/Genal_Information.RData")
load("data/DRYvER_Information.RData")
DRYvER_inf <- data.frame()
for (DRN_name in 1:length(Drying_Information)) {
  Drying_Example <- Drying_Information[[DRN_name]] %>% pull(Permanence) 
  
  # Drying extent
  Extent_of_Drying <- (length(which(Drying_Example<725))/length(Drying_Example))*100
  # Drying intensity 
  Dry_Patter_from_Simulations <- sd(Drying_Example/max(Drying_Example,na.rm=T), na.rm = T)
  
  cat(names(Drying_Information)[[DRN_name]], "has", 
      round(Extent_of_Drying,2), "Drying extent and",
      round(Dry_Patter_from_Simulations,2), "of Dry patter, which is equivalent to the sd of Drying intensity. Check Dry_patt",
      "\n",sep= " ")
  
  out_temp <- data.frame(
    "DRN"=names(Drying_Information)[[DRN_name]],
    "Dry_ext"=Extent_of_Drying,
    "Dry_patter"=Dry_Patter_from_Simulations
  )
  DRYvER_inf <- bind_rows(DRYvER_inf,out_temp)
}

Data_To_Plot %>% ungroup() %>% select(Dry_Patter,Dry_patt) %>% reframe(unique(.)) %>% mutate(Dry_int=1-Dry_patt)
DRYvER_inf <- DRYvER_inf %>% mutate(Dry_int=c(0.5,0.65,0.25,0.9,0.99,0.5))  
DRYvER_inf[1,2] <- DRYvER_inf[1,2]-1
DRYvER_inf[5,2] <- DRYvER_inf[5,2]+1


Ref_Performance <- Data_To_Plot %>% 
  group_by(Dry_ext, Dry_patt, Pollut_ext) %>% 
  summarise(Ref_IBMWP=mean(IBMWP)) %>%  
  filter(Dry_ext==0.01,
         Dry_patt==0.9,
         Pollut_ext==0.01) %>% # We need to select this to obtain the set reference in a pristine environment
  ungroup() %>% 
  select(Dry_ext,Ref_IBMWP) 

png(filename = "Figures/Figure_5.png",width = 2000,height = 1500,units = "px",res =300)
Data_To_Plot %>% 
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
  do(mod = lm(Performance ~ Pollut_ext, data = .)) %>%
  mutate(
    Intercept = summary(mod)$coeff[1],
    Slope = summary(mod)$coeff[2],
    pvalue=summary(mod)$coeff[8]) %>%
  mutate(Sign=ifelse(pvalue<0.1,"Sign","NO.sign")) %>% 
  mutate(Dry_Int=1-Dry_patt) %>% 
  mutate(Dry_ext=Dry_ext*100) %>% 
  ggplot(aes(x=Dry_ext,y=Intercept,colour=as.factor(Dry_Int)))+
  geom_smooth(aes(group=Dry_Int),method="loess",se=F)+
  geom_point(size=5,alpha=0.6)+
  scale_color_viridis(option="E",discrete = T)+
  geom_vline(data = DRYvER_inf, aes(xintercept =Dry_ext),linewidth=2,alpha=0.6,
             colour=c("#996633","#336600","#009999","#993333","#660066","#666699"))+
  labs(y="Intercept (Performance under pristine conditions)",
       x="Drying extent",colour="Drying intensity")+
  theme_classic()
dev.off()








