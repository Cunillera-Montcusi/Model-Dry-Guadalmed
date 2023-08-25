
# We set the working direcory 
setwd("C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/00. URUGUAY/Ponderful/PanEcography/PanEU_fragmentation")

# Nice colours? CUNILLERA_palette is what you need
source("C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/CUNILLERA_palette.R")

# Charghe and depurate the original EU database from the 
source("BDD_PanEU/Upload_Depur_PanEU.R")

setwd("C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/00. URUGUAY/Ponderful/PanEcography/PanEU_fragmentation")
# Charge the function to run the coalescent model
source("C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/00. URUGUAY/Ponderful/PanEcography/PanEU_fragmentation/H2020_Lattice_expKernel_Jenv_TempMeta.R")

# Both datasets must have the same length
nrow(Output_GRIDvsLAKES_TvsM_dep)
nrow(Centroides_coordenadas_dep)

# Plot to check the whole dataset
ggplot(data.frame(cbind(Centroides_coordenadas_dep$CENTROID_X,
                        Centroides_coordenadas_dep$CENTROID_Y)), aes(x=X1, y=X2))+geom_point(shape=0)

EcoR_length <- c()
EcoReg_ID <- unique(Centroides_coordenadas_dep$EcoR_ID)
for (EcoRegion in 1:length(unique(Centroides_coordenadas_dep$EcoR_ID))) {
  EcoR_length[EcoRegion] <- nrow(Centroides_coordenadas_dep %>% filter(EcoR_ID==EcoReg_ID[EcoRegion]))
}


EcoR_size <- data.frame(EcoReg_ID,EcoR_length) %>%
  filter(EcoReg_ID %in%EcoReg_ID[-c((length(EcoReg_ID)-2):length(EcoReg_ID))])%>% 
  arrange(EcoR_length) 


for (ID_ecor in 1:length(EcoR_size$EcoReg_ID)) {
  
  Coord_EcoReg <- Centroides_coordenadas_dep %>% filter(EcoR_ID==EcoR_size$EcoReg_ID[ID_ecor])
  
  GRID_EcoReg <- Output_GRIDvsLAKES_TvsM_dep%>%filter(PAGENAME%in%Coord_EcoReg$PageName)
  
  # Distance matrix
  library(geosphere)
  ptxy <- cbind(Coord_EcoReg$CENTROID_X,Coord_EcoReg$CENTROID_Y)
  Dist_Matrix <- distm(ptxy, fun = distGeo)/1000
  
  # Area total
  # J calculation
  FW_area <- apply(GRID_EcoReg[,103:ncol(GRID_EcoReg)], 1, sum)
  
  # We define parameters of FW area to generate communities J
  area.max.europa<-max(FW_area)
  Jmin <- 400*0.02
  J.max<-400+Jmin
  b.ef<-0.5 
  
  id_NOmodule <- rep(1,nrow(Coord_EcoReg))
  filter_NOfilter <- matrix(nrow = 200, ncol=nrow(Coord_EcoReg), data = 1)
  pool_200 <- rlnorm(n = 200,5,1)
  Meta_t0 <- matrix(nrow = length(pool_200), ncol =nrow(Coord_EcoReg), 1)
  
  # Distances
  dispersal_distances <- c(0.001,0.1,0.5,1,2,5,10) 
  
  # Gradient of loss
  habitat_lost <- c(0,0.1,0.3,0.5,0.7,0.85,0.9,0.95,0.99)
  
  save.image(paste("RDatas_for_SLURM/","test_SLURM_", EcoR_size$EcoReg_ID[ID_ecor],".RData", sep=""))
}






library(doParallel)
registerDoParallel(cores = detectCores())

lost_scenarios <- foreach(loss=1:length(habitat_lost), .combine=rbind)%:%
  foreach(dispersal=1:length(dispersal_distances), .combine=rbind)%dopar%{
    # Scenarios of loss
    Perc_Hab_Loss <- habitat_lost[loss]
    FW_area_lost <- FW_area-(Perc_Hab_Loss*FW_area)
    J.freshwater<-ceiling((-Jmin+(J.max/(area.max.europa^b.ef))*FW_area_lost^b.ef))
    J.freshwater <- ifelse(J.freshwater<=0,0,J.freshwater)
    # 
    a <- NULL
    #b <- list()
    for (it in 1:10) {
      output <- H2020_Coalescent.and.lottery.exp.Kernel.J_TempMtcom_tempIT(
        Meta.pool = pool_200,
        m.pool = 0.01,
        Js = J.freshwater,
        id.module = id_NOmodule,
        filter.env = filter_NOfilter,
        M.dist = Dist_Matrix,
        D50 = dispersal_distances[dispersal],
        m.max = 1,
        tempo_imp = 0.1,
        temp_Metacom = Meta_t0,
        temp_it = 4,
        id.fixed=NULL, D50.fixed=0, m.max.fixed=0, comm.fixed=pool_200,
        Lottery=F, it=1, prop.dead.by.it=1, id.obs=1:nrow(Coord_EcoReg))
      a <- rbind(a,output[[1]])
      #b[[it]] <- output[[2]]
    }
    resume.out(a)
  }

out_out <- list()
for (rounds in 1:c(length(dispersal_distances)*length(habitat_lost))) {
  outer <- list(
    lost_scenarios[[rounds]],
    lost_scenarios[[c(rounds+(1*(length(dispersal_distances)*length(habitat_lost))))]],
    lost_scenarios[[c(rounds+(2*(length(dispersal_distances)*length(habitat_lost))))]],
    lost_scenarios[[c(rounds+(3*(length(dispersal_distances)*length(habitat_lost))))]]
  )
  names(outer)<-c("Median", "Standard Deviation", "out.IC.up","out.IC.inf")
  out_out[[rounds]] <- outer  
}

final_out <- list()
for (place in 1:length(habitat_lost)){
  beg <- seq(1,length(out_out),length(dispersal_distances))  
  end <- seq(7,length(out_out),length(dispersal_distances))  
  final_out[[place]] <- out_out[beg[place]:end[place]]
}

length(final_out)
length(final_out[[1]])
length(final_out[[1]][[1]])

save.image("Out_test_420.RData")


