
H2020_Coalescent.and.lottery.exp.Kernel.J_TempMtcom_tempIT <- function(Meta.pool, m.pool, Js, id.module, filter.env,
                                                                   M.dist, D50, m.max,
                                                                   tempo_imp,temp_Metacom,temp_it,
                                                                   id.fixed, D50.fixed, m.max.fixed, comm.fixed,
                                                                   Lottery, it, prop.dead.by.it, id.obs){

out_Metat0<- H2020_Coalescent.and.lottery.exp.Kernel.J_TempMtcom(
            Meta.pool=Meta.pool,
            m.pool=m.pool, 
            Js=Js, 
            id.module=id.module, 
            filter.env=filter.env,
            M.dist=M.dist, 
            D50=D50, 
            m.max=m.max,
            tempo_imp = 0,
            temp_Metacom = temp_Metacom,
            id.fixed=id.fixed, D50.fixed=D50.fixed, m.max.fixed=m.max.fixed, comm.fixed=comm.fixed,
            Lottery=Lottery, it=it, prop.dead.by.it=prop.dead.by.it, 
            id.obs=id.obs)

output <- out_Metat0
output

if(temp_it>0){
for (temporal_it in 1:temp_it) {
cat("coalescent iteration", temporal_it," de" ,temp_it )
    
    out_Metatn<- H2020_Coalescent.and.lottery.exp.Kernel.J_TempMtcom(
      Meta.pool=Meta.pool,
      m.pool=m.pool, 
      Js=Js, 
      id.module=id.module, 
      filter.env=filter.env,
      M.dist=M.dist, 
      D50=D50, 
      m.max=m.max,
      tempo_imp = tempo_imp,
      temp_Metacom = out_Metat0$MetaCom,
      id.fixed=id.fixed, D50.fixed=D50.fixed, m.max.fixed=m.max.fixed, comm.fixed=comm.fixed,
      Lottery=Lottery, it=it, prop.dead.by.it=prop.dead.by.it, 
      id.obs=id.obs)
    
    out_Metat0 <- out_Metatn
  }
  output <- out_Metatn
  output
} else {output}
}



######################################################################################################################
# function to resume output of simulation
resume.out<-function(out){
  out2<-list()
  out2[[1]]<-apply(out,2,quantile, 0.5, na.rm=T)    # NA originates if only a single module is involved
  out2[[2]]<-apply(out,2,sd, na.rm=T)
  out2[[3]]<-apply(out,2,quantile, 0.975, na.rm=T)
  out2[[4]]<-apply(out,2,quantile, 0.025, na.rm=T)
  names(out2)<-c("Median", "Standard Deviation", "out.IC.up","out.IC.inf")
  out2
  
}

#
#
#
#
###################################################################################################
H2020_Coalescent.and.lottery.exp.Kernel.J_TempMtcom<-function(Meta.pool, m.pool, Js, id.module, filter.env,
                                                    M.dist, D50, m.max,
                                                    tempo_imp,temp_Metacom,
                                                    id.fixed, D50.fixed, m.max.fixed, comm.fixed,
                                                    Lottery, it, prop.dead.by.it, id.obs){
  
  if(length(Meta.pool)==1)Meta.pool<-round(rlnorm(n = Meta.pool, meanlog = log(100), sdlog = log(10)))
  library(vegan)
  Meta.pool<-Meta.pool/sum(Meta.pool)
  comm.fixed<-comm.fixed/sum(comm.fixed)
  Meta<-NULL
  
  # Function to update: M.migra from Graph to a function based in distance matrix
  M.migra<-H2020_migration.matrix.kernel.all(M.dist=M.dist, m.pool=m.pool, D50=D50, m.max=m.max,             # Funciton defined above. It estimates Migration matrix
                                             id.fixed=id.fixed, D50.fixed=D50.fixed, m.max.fixed=m.max.fixed)
  
  for(i in 1:ncol(M.migra)){
    Meta<-cbind(Meta, rmultinom(1,1,Meta.pool))
  }
  
  # If J=0 there is no spp. However, Meta add the possibility of having species there leading to at least 1 species to be found. 
  col_NO_J <- which(Js==0)
  if(length(col_NO_J)==0){col_NO_J <- -(1:length(Js))}
  
  Meta[,col_NO_J] <- 0
  
  Meta_sml <- Meta[,-col_NO_J]
  M.migra_sml <- M.migra[-col_NO_J,-col_NO_J]
  filter.env_sml <- filter.env[,-col_NO_J]
  temp_Metacom_sml <- temp_Metacom[,-col_NO_J]
  
  for (ii in 2:max(Js)){
    id.j<-which(Js[-col_NO_J]>=ii)
    cat("coalescent construction in J: ", ii," de" ,max(Js),"\n")  
    if(length(id.fixed)>0)Meta_sml[,id.fixed]<-comm.fixed*(ii-1)      # scale vector of abundances in the fixed community to the abundance of all other communities
    
    Pool.neighbor<-(Meta_sml%*%M.migra_sml)         # estimates potential reclutants including immigrants for all communities weighted by local abundances
    
    Pool.neighbor<-Pool.neighbor*filter.env_sml # IMPORTANT: element by element adjustment of species abundances to local filters
    
    Pool.neighbor <- ((Pool.neighbor/max(Pool.neighbor))*(1-tempo_imp))+((temp_Metacom_sml/max(temp_Metacom_sml))*tempo_imp)

    if(length(id.j)>1){
      new<-apply(Pool.neighbor[,id.j],2,born,dead.by.it = 1, M.pool = Meta.pool, m.pool = m.pool)   # random selection of new individuals from reclutants pool 
      Meta_sml[,id.j]<-Meta_sml[,id.j]+new} else {
        Meta_sml[,id.j]<-Meta_sml[,id.j]+born(n = Pool.neighbor[,id.j],dead.by.it = 1, M.pool = Meta.pool, m.pool = m.pool) 
      }                          # upadate communities 
  }
    if(Lottery==T){  # START LOTTERY ################################################
    col_NO_J <- which(Js==0)
    if(length(col_NO_J)==0){col_NO_J <- -(1:length(Js))}
    
    dead.by.it<-round(prop.dead.by.it*Js[-col_NO_J],0)    # estiamte individual to remove in each iteration and local community
    dead.by.it<-ifelse(dead.by.it<1,1,dead.by.it)
    max.dead.by.it<-max(dead.by.it)
    
    if(length(id.fixed)>0) dead.by.it[id.fixed]<-0       # fixed communities are not updated
    if(length(id.fixed)>0)Meta_sml[,id.fixed]<-round(comm.fixed*max(Js),0)# update abundances of the fixed community to community size
    
    for(iteration in 1:it){                              # start lottery iterations  
      cat("Lottery iteration",iteration," of ", it, "\n",sep=" ")
      for(dead in 1:max.dead.by.it) {
        id.no.dead<-which(dead.by.it>=dead)               # identify communities to remove individuals
        if(length(id.no.dead)>1)Meta_sml[,id.no.dead]<-Meta_sml[,id.no.dead]-apply(Meta_sml[,id.no.dead]*(1-filter.env_sml[,id.no.dead]),2,FUN = change, change=1) # remove individuals along all communities IMPORTANT:dead is inversely proportional to filter matrix (because matrix elements are performance)
        if(length(id.no.dead)==1)Meta_sml[,id.no.dead]<-Meta_sml[,id.no.dead]-change(Meta_sml[,id.no.dead]*(1-filter.env_sml[,id.no.dead]), change=1) # remove individuals along all communities
      }
      
      Pool.neighbor<-(Meta_sml%*%M.migra_sml)         # estimates potential reclutants including immigrants for all communities weighted by local abundances
      
      Pool.neighbor<-Pool.neighbor*filter.env_sml # IMPORTANT: element by element adjustment of species abundances to local filters
      
      Pool.neighbor <- ((Pool.neighbor/max(Pool.neighbor))*(1-tempo_imp))+((temp_Metacom_sml/max(temp_Metacom_sml))*tempo_imp)
      
      id.comm<-0
      for(reclutants in dead.by.it) {          # dead.by.it is the vector of number of indiviuals to update in each iteration (a fixed fraction of J)
        id.comm<-id.comm+1
        Meta_sml[,id.comm]<-Meta_sml[,id.comm]+rmultinom(1,reclutants,
                                                 prob = (1-m.pool)*(Pool.neighbor[,id.comm]/sum(Pool.neighbor[,id.comm]))+m.pool*Meta.pool) # random selection of reclutants from neighbours, local or external pool
      }
    }
    }
  
  Meta[,-col_NO_J] <- Meta_sml
  
  MetaCom_t <- Meta
  BB<-as.matrix(vegdist(t(Meta[,id.obs]), method = "jaccard"))
  Bett.all<-apply(BB,2,mean)
  #Bett.intra<-rep(NA,length(id.module))
  #Bett.inter<-rep(NA,length(id.module))
  #for (bb in unique(id.module)){
  #  id.i<-which(id.module==bb)
  #  b.intra<-apply(BB[id.i,id.i],2,mean)
  #  Bett.intra[id.i]<-b.intra
  #  b.inter<-apply(BB[-id.i,id.i],2,mean)
  #  Bett.inter[id.i]<-b.inter
  #}
  
  Meta<-list("out"=c("m.pool"=m.pool, "Js.max"=max(Js),"Js.min"=min(Js), "D50"=D50, "m.max"=m.max,
          "D50.fixed"=D50.fixed, "prp.dead"=prop.dead.by.it,#"m.max.fixed"=m.max.fixed,
          "Lottery"=ifelse(Lottery==TRUE,1,0), "it"=it, 
          "S.loc"=apply(ifelse(Meta[,id.obs]>0,1,0),2,sum),
          "B.loc.all"=ifelse(is.na(Bett.all)==T,0,Bett.all),
          #"B.loc.intra.module"=Bett.intra,
          #"B.loc.inter.module"=Bett.inter,
          "G"=length(which(apply(ifelse(Meta[,id.obs]>0,1,0),1,sum)>1)),
          "simp"=diversity(apply(Meta[,id.obs],1,sum),"simpson"),
          "inv.simp"=diversity(apply(Meta[,id.obs],1,sum), "invsimpson")
          ),
          "MetaCom"=MetaCom_t) 
  Meta
}

####






###########################################
# Cells at contact in their edges are assumed the zero distance for migration
# m.max: migration between connected cells
# D50 is the distance at which migration decay yo half m.max

H2020_migration.matrix.kernel.all<-function(M.dist, m.pool, D50,m.max, 
                                            id.fixed, D50.fixed, m.max.fixed){
  diag(M.dist)=NA
  M.dist = M.dist-min(M.dist, na.rm=T) # min distance is the distance between neighbour cells. 
  # connected cells has distance zero and migration m.max
  b = -log(0.5)/D50         # b is estimated | m(D50)=m.max*0.5
  M.migra = m.max*exp(-b*M.dist) 
  
  if(length(id.fixed)>0){
    b.fixed= -log(0.5)/D50.fixed 
    M.migra[id.fixed,]<- m.max.fixed*exp(-b.fixed*M.dist[id.fixed,]) # migration from outlet
  }                                                          # M.migra is the potential migration between communities, 
  
  diag(M.migra)<-1                                             # selfrecruitment is considered as 1=m.intra community
  M.migra<-apply(M.migra,2,m_to_1,m.pool)                      # standirize migrations to 1: (m.intra+m.pool+m.neigh=1)
  M.migra
}

############
m_to_1<-function(m, m.pool) (1-m.pool)*m/sum(m) # standarize a vector of migration to add 1 
# also considering migration from an external pool

born<-function(n, dead.by.it, M.pool, m.pool)rmultinom(1,dead.by.it,(1-m.pool)*(n/sum(n))+m.pool*M.pool)
change<-function(n,change)rmultinom(1,change,n)
