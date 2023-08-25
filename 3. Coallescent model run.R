

library(doParallel)
registerDoParallel(cores = detectCores())

lost_scenarios <- foreach(loss=1:length(habitat_lost), .combine=rbind)%:%
  foreach(dispersal=1:length(dispersal_distances), .combine=rbind)%dopar% {
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


