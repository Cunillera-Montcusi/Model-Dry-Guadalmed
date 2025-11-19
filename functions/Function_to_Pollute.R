
## Function to dry rivers according to 
# extent - Number of sites that dry 
# distribution - where the drying is ocurring
function_to_pollute <- function(River_nodes,
         Spp_tolerance,
         extent,
         distribution){
  
if (is.null(distribution)==TRUE) {
  #Randomly select a part of these streams 
  Streams_to_Polut <- sample(1:nrow(River_nodes),
                           size = nrow(River_nodes)*extent, # Random % of streams
                           replace = F) # We randomly select which sites suffer this drying pattern
}else{
  Streams_to_Polut <- sample(distribution,
                           size = length(distribution)*extent, # Random % of streams
                           replace = F) # We randomly select which sites suffer this drying pattern
}


# Pollution acts as a filter. When a site is polluted a species is being filtered according to that. 
#Therefore, for the polluted sites we need to use the "filter" 
#here this creates a matrix as per species (n=200) (rows) and site (columns), with 0.99 as tolerance values
filter_NOfilter <- matrix(nrow = length(Spp_tolerance), ncol=nrow(River_nodes), data = 0.99) # Filter of species per site. We will use for tolerance
#select columns (from Polluted sites) and write spp_tolerance instead of 0.99
filter_NOfilter[,Streams_to_Polut] <- Spp_tolerance# here we write these tolerances (Spp_tolerance) to the polluted sites (selected above)

filter_NOfilter
}


