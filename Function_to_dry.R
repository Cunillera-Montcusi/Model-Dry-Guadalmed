

## Function to dry rivers according to 
# extent - Number of sites that dry 
# duration - lenght of the drying period
# distribution - where the drying is ocurring

function_to_dry <- function(River_nodes,
                            years,
                            days,
                            duration,
                            duration_constancy=T,
                            extent,
                            distribution,
                            skeweed_distr){
# DRYING DATABASE
# We set the basic matrix structure conceptualized as 
# rows = a time unit 
# columns = unique sites in our river network 
if (days==FALSE) {
Flow_DB <- matrix(nrow = 12*years, ncol =nrow(River_nodes)+1, data = 1)
# Month ID
Flow_DB[,ncol(Flow_DB)] <- seq(1:(12*years))
}else{
Flow_DB <- matrix(nrow = 12*years*30, ncol =nrow(River_nodes)+1, data = 1)
# Month ID
Flow_DB[,ncol(Flow_DB)] <- seq(1:(12*years*30))}

# DRYING PATTERN
# We elaborate the drying pattern, which is the number of days that a site will be dry or not
duration_patterns <- list()
for (dur_level in 1:length(duration)) {
if (days==FALSE) {
if (duration_constancy==T) {
DryPat <- rep(1,12)
DryPat[round(length(DryPat)*(1-duration[dur_level])):length(DryPat)] <- 0
DryPat <- rep(DryPat,years)}else{
DryPat <- rep(1,12*years)
DryPat[sample(seq(1:length(DryPat)), length(DryPat)*duration[dur_level],replace = F)] <- 0}
}else{
if (duration_constancy==T) {
DryPat <- rep(1,12*30)
DryPat[round(length(DryPat)*(1-duration[dur_level])):length(DryPat)] <- 0
DryPat <- rep(DryPat,years)}else{
DryPat <- rep(1,12*30*years)
DryPat[sample(seq(1:length(DryPat)), length(DryPat)*duration[dur_level],replace = F)] <- 0}
}
duration_patterns[[dur_level]] <- DryPat
}# dur_level

  
if (is.null(distribution)==TRUE) {
#Randomly select a part of these streams 
Streams_to_dry <- sample(1:nrow(River_nodes),
                         size = nrow(River_nodes)*extent, # Random % of streams
                         replace = F) # We randomly select which sites suffer this drying pattern
}else{
Streams_to_dry <- sample(distribution,
                         size = length(distribution)*extent, # Random % of streams
                         replace = F) # We randomly select which sites suffer this drying pattern
}

# We create a cummulative sums vector so select the randomly selected nodes and distribute the duration patterns
skeweed_Stream <- cumsum(c(1,floor(length(Streams_to_dry)*(skeweed_distr/sum(skeweed_distr)))))
# We add the drying month pattern to the selected streams
for (dur_level in 1:length(duration)) {
Flow_DB[,Streams_to_dry[skeweed_Stream[dur_level]:(skeweed_Stream[dur_level+1]-1)]] <- duration_patterns[[dur_level]]
}

# IMPORTANT step! We switch the Flow_DB database to have the site ID in the first column
Flow_DB<- cbind(Flow_DB[,ncol(Flow_DB)],Flow_DB[,1:(ncol(Flow_DB)-1)])
# Need of colnames for the matrix
colnames(Flow_DB) <- c("Site_ID",as.character(seq(1,nrow(River_nodes),1)))

Flow_DB
}




