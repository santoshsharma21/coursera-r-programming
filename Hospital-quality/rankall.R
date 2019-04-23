# Ranking hospitals in all states
rankall <- function(outcome, num = 'best'){
    # read data
    data = read.csv("A:/coursera-r-programming/Hospital-quality/outcome-of-care-measures.csv", 
                    na.strings = "Not Available", stringsAsFactors = FALSE)
    # Filter unnecessary data 
    data <- data[,c(2,7,11,17,23)]
    
    # rename columns
    names(data) <- c('hospital_name','State','heart attack','heart failure','pneumonia')
    
    # check validity of the input
    if(! outcome %in% c('heart attack','heart failure','pneumonia')){
        stop('invalid outcome')
    }
    
    # Initialize empty data frame
    rankall_df = data.frame(hospital = vector(), state = vector())
    
    # get list of unique states
    States = sort(unique(data[,"State"]))
    
    # loop over all states, find the hospital of the given rank
    for(i in 1:length(States)){
        subset_data = data[data[,"State"] == States[i],]
        subset_data = subset_data[!is.na(subset_data[,outcome]),]
        sorted_data = subset_data[order(subset_data[,outcome], subset_data[,"hospital_name"]),]
        
        if(num == 'best'){
            rankall_df[i,'hospital'] = sorted_data[,"hospital_name"][which.min(sorted_data[,outcome])]
            rankall_df[i,'state'] = States[i]
        } else if(num == 'worst'){
            rankall_df[i,'hospital'] = sorted_data[,"hospital_name"][which.max(sorted_data[,outcome])]
            rankall_df[i,'state'] = States[i]
        } else {
            rankall_df[i,'hospital'] = sorted_data[,"hospital_name"][num]
            rankall_df[i,'state'] = States[i]
        }
    }
    # add row names & return data frame
    row.names(rankall_df) = States
    return(rankall_df)
}
