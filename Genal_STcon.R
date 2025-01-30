# clear memory
rm(list=ls())
gc()
graphics.off()

old <- options(stringsAsFactors = FALSE)
on.exit(options(old))

MSI_Old <- "E:/"
MSI_New <- "C:/Users/David CM/"

# load libraries
library(ncdf4);library(lubridate);library(dplyr);library(ggplot2);library(reshape2)
library(sf);library(tidyverse);library(mapview)

Years_Of_Drying <- 2
TIME_WINDOW <- 365*Years_Of_Drying

# Set working directory
#working_directory <- "C:/Users/xy/Dropbox/DAVID DOC/LLAM al DIA/1. FEHM coses al DIA/10. DRYver/STcon_DRYvER"
working_directory <- paste(MSI_New,"Dropbox/DAVID DOC/LLAM al DIA/1. FEHM coses al DIA/10. DRYver/STcon_DRYvER",sep="")
setwd(working_directory)

# load functions
source("functions_flow_intermittence_indicators.R")
library(devtools)
source("https://raw.github.com/Cunillera-Montcusi/Quantifyinig-SpaTem-connectivity/main/SpaTemp_function.R")

library(tictoc)
tic()

TypeOfConnectivity <- "Normal"

  DryRiverNet <- c("Genal")#c("Albarine", "Bukkosdi", "Lepsamaanjoki", "Genal","Butiznica","Velicka")
  STcon_DRN_Total <- data.frame()
  Results_DRN <- list()
  STconmat_DRN_Total <- list()
  DRN_name=1
    catchment <- DryRiverNet[DRN_name]
    #_____________________________________________________________________________________________________________________
    # Import data following WP1 codes
    # open reaches shapefile
    # For "Butiznica" and "Velicka" we are using WP3 information (Barquin's team) and the reach_NETRACE_XX shapefile
    watershed <- import_watershed(catchment)
    reaches <- import_reaches(catchment,watershed) %>% 
      filter(!cat%in%c(5390:5442,
                       6040:6099,
                       5444:5452,
                       5454:5510,6013:6099,
                       5916:6012,
                       5512:5541
      ))
    col_plpot <- "cat"
    if (DRN_name==5) {col_plpot <- "ID"}
    mapview(reaches, zcol = col_plpot,  legend = TRUE, layer.name = "ReachID") # plot stream network
    
    # open simulation results
    nc_file <- get_results_file_present(catchment) # find filename of latest results
    nc <- nc_open(nc_file) # open netcdf file
    reachID <- ncvar_get(nc, "reachID") # get list of reaches IDs
    dates <- ncvar_get(nc, "date") # get dates of simulation period
    dates <- as.Date(dates, origin="1950-01-01") # convert dates into R date format
    # We obtain the flow intermitence data and depurate it to eliminate all the NA that can be there linked to wrong data.
    if(TypeOfConnectivity=="Normal"){
      flow_intermittence <- get_nc_var_present(nc, "isflowing", reachID, dates) # 0=dry, 1=flowing
    }
    if(TypeOfConnectivity=="Future"){
      load("ZoltanHUN_Future_DRN_values.RData")
      flow_intermittence <- Final_DRN_intermittence[[DRN_name]] %>% ungroup()%>% 
        mutate(reachID=as.factor(reachID),dates=Date) %>% 
        group_by(Date,Day,Month,Year,DRN,reachID,dates) %>% 
        summarise(value=mean(value)) %>% 
        mutate(value=ifelse(value<=0.5,0,1))
    }
    
    # We filtrate the river shapefile to eliminate the segments that correspond to a NA. This is only happening with the Finish DRN 
    ## and not in the others so it is only relevant for it. Specifically, the segment giving problems is 1039000. 
    reaches <- reaches %>% filter(!cat%in%unique(unlist(flow_intermittence %>% filter(is.na(value)==T) %>% dplyr::select(reachID))))
    Qsim <- get_nc_var_present(nc, "qsim", reachID, dates) # simulated discharge in m3/s
    # _____________________________________________________________________________________________________________________
    
    library(tidyverse); library(sp); library(shp2graph)
    # To maintain an xy value for each segment we calculate the centroids of each segment. This done just for "tracking" purposes
    Reaches_centr <- reaches %>% st_as_sf %>%st_centroid %>% st_coordinates
    # We now transform the reaches shapefile into the elements to later build an igraph object
    rtNEL1 <-readshpnw(as(reaches, "Spatial"), ELComputed = T,longlat = F)
    
    #Now we convert it into a full igraph object
    ## Notice that we add the "cat" and "centroids" coordinates to each edge in order to be able to track them after.
    ## This process together with the rtNEL1 duplicates information as for each segment there is at least 2 nodes and 
    ## consequenlty the infomration is duplicated. HOWEVER, this can later be "tracked" back after calculating the values. 
    ## As each reach will receive the same information in relation to their "flow/dry" pattern. 
    ### e.g. two consecutive reaches will have the same dry/flow values so they will end up with the same value
    igr1 <-nel2igraph(nodelist = rtNEL1[[2]],edgelist = rtNEL1[[3]],Directed = T,eadf = data.frame(reaches$cat,Reaches_centr))
    V(igr1)$Reach_ID <- seq(1,length(V(igr1)),1)
    
    igraph::components(igr1)
    
    # We need to set directionality to were the river is actually finishing. We look for the "cat" of the last segment and look 
    ## for which is the edge that coprresponds to this category. 
    ## This must be done on ArcGIS or any other software to treat with the shape file and its attribute tables. Also in the "mapview" zooming to the outlet. 
    # We obtained the following outlets for each DRN
    Genal_Outlet <-351 # Reach ID 5390 
    
    DRN_outlets <- Genal_Outlet#c(Albarine_Outlet, Bukkosdi_Outlet, Lepsamaanjoki_Outlet, Genal_Outlet,Butiznica_Outlet,Velicka_Outlet)
    DRN_outlets_ReachID <-5542# c(2408401,648600,1030801,5390,1660,40116)
    
    # The script below shows how to find the outlet value individually. However now this has been already corrected and we have each outlet for each DRN. 
    ## outlet_site <- rtNEL1[[5]][which(rtNEL1[[5]]$cat==40116),]
    
    ### With this we obtain the potential outlet edges. In this case we have to follow to see which is the last one.
    ## rtNEL1[[3]][94,]
    ## which(rtNEL1[[3]][,3]==92) #There is 1 "to" to 670 so it can be either 670 or 669
    ## which(rtNEL1[[3]][,2]==92) #There is no "from" to 670 but yes from 669 
    # We charge the riverconn pacakge to use the "set_graph_directionality" function.
    library(riverconn)
    igr1 <-set_graph_directionality(igr1, field_name = "Reach_ID", DRN_outlets[DRN_name]) # We direct the graph here
    igr2 <- igraph::simplify(igr1,remove.multiple = TRUE,remove.loops = TRUE, edge.attr.comb = "random") # We clean the graph
    
    # We merge the information coming from the Edges with the one coming from the intermittent dataset and the information 
    ## stored on each edge, which contains Reach_ID. So we set the "tracking" link between edges/vertices/reaches
    ## Note that we create a "new" edge who corresponds to the last point of the graph (the outlet).  
    ## This last point coordinates are set to the "first coordinates" of the reach corresponding to the Reach_ID of the outlet
    ## but there are more points having the same Reach_ID (because segments are composed of more than one point).  
    Edges_information <- igraph::as_data_frame(igr2, what = "edges") %>% 
      mutate(reaches.cat=as.factor(reaches.cat))%>%
      bind_rows(data.frame("from"=as.character(DRN_outlets[DRN_name]),"to"=as.character(0),
                           "reaches.cat"=as.factor(as.matrix(data.frame(reaches$cat,Reaches_centr) %>%filter(reaches.cat==DRN_outlets_ReachID[DRN_name]))[1,1]),
                           "X"=as.matrix(data.frame(reaches$cat,Reaches_centr) %>%filter(reaches.cat==DRN_outlets_ReachID[DRN_name]))[1,2],
                           "Y"=as.matrix(data.frame(reaches$cat,Reaches_centr) %>%filter(reaches.cat==DRN_outlets_ReachID[DRN_name]))[1,3]))
    # We erase some duplicated "cat" that appeared in the edges to make sure merging with flow intermitance is "clean" 
    if (length(which(duplicated(as.numeric(Edges_information[,1]))==T))>0) {
      Edges_information <- Edges_information[-which(duplicated(as.numeric(Edges_information[,1]))==T),]}
    
    
    # We filter and prepare the table of Flow intermitance to be entered in the function to calculate STcon 
    FL_intermitence <- left_join(Edges_information, flow_intermittence,by=c("reaches.cat"="reachID"),relationship = "many-to-many")%>%
      mutate(merge_ID=paste(from,to,reaches.cat,sep = "_")) %>%
      pivot_wider(id_cols = dates,names_from = merge_ID,values_from = value) %>% 
      mutate(dates=as.factor(dates)) %>%
      filter(is.na(dates)==F) 
    NAsss <- c()
    for (NA_find in 1:ncol(FL_intermitence)){ 
      NAsss[NA_find] <- length(which(is.na(as.matrix(FL_intermitence[,NA_find]))==TRUE))
    }
    cat("We have found",length(which(NAsss>0)), "columns with  NAs. There should be none as they are depurated.","\n")
    
    ## Some flow intermitences datasets had some "hidden" NA inside (this was an error from old archives)
    ### We correct this mistake and rebuild the dataset
    Corr_FL_intermitence<-FL_intermitence %>%dplyr::select(-dates) %>%replace(is.na(.),0) #%>% replace(.==0,1)
    FL_intermitence <- bind_cols(dates=FL_intermitence$dates,Corr_FL_intermitence)
    
    # We charge the file with the corresponding dates of the sampling
    #Orig_Samp_DATES <- read.csv2("DRYvER_samp_dates.csv") %>% filter(Catchment==catchment)
    Orig_Samp_DATES <- read.csv2("DRYvER_samp_dates.csv") %>% filter(Catchment==catchment)
    
    # Due to format differences we need to "prepare" the dates to fit the ones in the FL_intermitence 
    Orig_DATES <- matrix(unlist(lapply(strsplit(Orig_Samp_DATES$date, split='.', fixed=TRUE), `[`, 1:3)),ncol=3, byrow=TRUE)
    
    if(TypeOfConnectivity=="Future"){
      # Monica values
      #Orig_DATES[,3][which(Orig_DATES[,3]=="2021")] <- 2060
      #Orig_DATES[which(Orig_DATES[,3]=="2022"),1] <- 31
      #Orig_DATES[which(Orig_DATES[,3]=="2022"),2] <- 12
      #Orig_DATES[which(Orig_DATES[,3]=="2022"),3] <- 2061
      
      # Zoltan values
      Orig_Samp_DATES <- read.csv2("ZoltanHUN_Future_Dates_Samples.csv") %>% rename(date=Date,Site=code)
      Orig_DATES <- matrix(unlist(lapply(strsplit(Orig_Samp_DATES$date, split='/', fixed=TRUE), `[`, 1:3)),ncol=3, byrow=TRUE)
    }
    
    Orig_Samp_DATES <- bind_cols(Orig_Samp_DATES, data.frame(day=Orig_DATES[,1],month=Orig_DATES[,2]))
    
    Dates_SamplingSites_ID <- list()
    Check_dates <- c()
    for (dat_month in 1:length(unique(Orig_Samp_DATES$Campaign))) {
      # We select the month
      Smap_month <- as.numeric(unique(Orig_Samp_DATES$Campaign)[dat_month])
      # We select the first date of the sampling (they are separated between 2-5 days, so we chose the first one)
      first_date <- as.matrix(Orig_Samp_DATES %>% arrange(month,day) %>% filter(Campaign==Smap_month) %>% select(date))[1]
      
      if(TypeOfConnectivity=="Normal"){
        # We add zeros and finish format polishing to match the format of FL_intermitence
        split_date <- strsplit(first_date, split='.', fixed=TRUE)
        if (nchar(split_date[[1]][2])<2) {split_date[[1]][2] <- paste("0",split_date[[1]][2],sep="")}# Add zero to day
        if (nchar(split_date[[1]][1])<2) {split_date[[1]][1] <- paste("0",split_date[[1]][1],sep="")}# Add zero to month
        
        first_date <- paste(split_date[[1]][3],split_date[[1]][2],split_date[[1]][1], sep="-")# Paste in the good format
      }
      
      if(TypeOfConnectivity=="Future"){
        split_date <- Orig_DATES[which(Orig_Samp_DATES$date==first_date)[1],]
        if (nchar(split_date[2])<2) {split_date[2] <- paste("0",split_date[2],sep="")}# Add zero to day
        if (nchar(split_date[1])<2) {split_date[1] <- paste("0",split_date[1],sep="")}# Add zero to month
        first_date <- paste(split_date[3],split_date[2],split_date[1], sep="-")# Paste in the good format
      }
      
      site_names <- c(Orig_Samp_DATES %>% arrange(day) %>% filter(Campaign==Smap_month) %>% select(Site))# Arrange all the names
      out <- list(first_date, site_names)# Create a list with the names of all the sampling sites of that date
      Dates_SamplingSites_ID[[dat_month]] <- out
    }
    length(Dates_SamplingSites_ID) # Check that sampling campaings is 6
    
    #_________________________________________________________________________________________________
    # From here we run the whole process for each sampling dates
    ## We will use a directed river
    Inermitence_dataset_Campaings_To_Run<- list()
    Sites_coordinates=Sites_coordinates_Campaings_To_Run<- list()
    Network_stru = Network_stru_Campaings_To_Run<- list()
    Dist_matrix <- list()
    FULL_output_FIN <- list()
    for (Samp_Dates in 1:1) { 
      cat("We are at", Samp_Dates, "campaign of", length(Dates_SamplingSites_ID),"\n")
      
      # Row_Date_End will correspond to the "date" where we want to finish 
      RowS_Date_End <- as.numeric(unlist(FL_intermitence %>% mutate(row_id=row_number(), .before=dates) %>%
                                           mutate(dates=as.character(dates)) %>% 
                                           filter(dates%in%Dates_SamplingSites_ID[[Samp_Dates]][[1]]) %>% select(row_id)))
      
      Row_Date_End <- RowS_Date_End
      cat("The end is at", Row_Date_End, "\n")
      # Row_Date_Begin is the date that we want to consider as the begining (now set at 50 days before)
      # DATES DEFINITION ####
      Row_Date_Begin <- RowS_Date_End-TIME_WINDOW
      cat("The begin is at", Row_Date_Begin, "\n")
      
      # FIRST element of STcon --- The water permanence database
      # We "cut" the FL_intermitence dataset to adapt the database to what interest us.  
      FL_intermitence_cut <- FL_intermitence[Row_Date_Begin:Row_Date_End,]
      nrow(FL_intermitence_cut) # We chech the number of rows should be always the desired +1
      # Flow values
      #Q_values_cut <- Q_values[Row_Date_Begin:Row_Date_End,]
      #nrow(Q_values_cut) # We chekch the number of rows should be always the desired +1
      
      # SECOND element of STcon --- SITES_ID (the coordinates)
      # We prepare the coordinates of each edges using the information from rtNEL1 object. THIS is crucial as the nodes can
      ## only be references through these coordinates. Otherwise the graph will not be ploted properly.
      site_cord <- matrix(unlist(rtNEL1[[2]][,2]), ncol=2, byrow=TRUE)
      Sites_ID <- data.frame(Node_ID=seq(1,nrow(site_cord),1),Node_ID_2=seq(1,nrow(site_cord),1),site_cord)
      
      # THIRD element of STcon --- The network as a matrix (matrix indicating where are the connections)
      Network_structure <- as.matrix(as_adjacency_matrix(igr2))
      
      # FOURTH we calculate the distance matrix
      Distance_matrix <- as.matrix(dist(site_cord)/1000)
      
      # Last comprobation 
      cat("There are",nrow(Sites_ID),"sites.",ncol(FL_intermitence_cut),"columns in the intermitence dataset &",dim(Network_structure),"network rows and columns.")
      
      ncol(FL_intermitence_cut) # Should always be nrow(Sites_ID)+1
      dim(Network_structure) # Should be the same as nrow(Sites_ID)
      
      Inermitence_dataset_Campaings_To_Run[[Samp_Dates]] <- as.data.frame(FL_intermitence_cut)
      Sites_coordinates_Campaings_To_Run[[Samp_Dates]] <- as.data.frame(Sites_ID)
      Network_stru_Campaings_To_Run[[Samp_Dates]] <- as.data.frame(Network_structure)
      Dist_matrix[[Samp_Dates]] <- Distance_matrix
      Check_dates[Samp_Dates] <- Dates_SamplingSites_ID[[Samp_Dates]][[1]]
    }# Samp_Dates
    
    # We run a reference value to obtain the MAXIMUM connectivity values that can be attained by each node.
    ## To do this we will use the last generated FL_intermitence (using 50 days) and substitute all the 0 value by 1. In that way we 
    ## In this way, we obtain a "fully" connected river where the obtained values can be used as reference to set a reference value
    ## of STcon and STconmat and calculate them betewen 1 and 0.
    # REF_FL_intermitence<-FL_intermitence_cut %>% replace(.==0,1)
    # # We add the seventh "reference river into the list
    # Inermitence_dataset_Campaings_To_Run[[2]] <- as.data.frame(REF_FL_intermitence)
    # Sites_coordinates_Campaings_To_Run[[2]] <- as.data.frame(Sites_ID)
    # Network_stru_Campaings_To_Run[[2]] <- as.data.frame(Network_structure)
    # Dist_matrix[[2]] <- Dist_matrix[[1]]
    
    save( 
    Inermitence_dataset_Campaings_To_Run,
    Sites_coordinates_Campaings_To_Run,
    Network_stru_Campaings_To_Run,
    Dist_matrix,
    file="C:/Users/David CM/Dropbox/DAVID DOC/LLAM al DIA/1. FEHM coses al DIA/7. DRY-GUADALMED/Model-Dry-Guadalmed/Genal_ClusterCode/SLURM_GenalData.RData")
    
    
    