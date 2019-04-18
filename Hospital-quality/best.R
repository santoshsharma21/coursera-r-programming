best <- function(state, outcome){
    # read data
    data = read.csv("A:/coursera-r-programming/Hospital-quality/outcome-of-care-measures.csv", 
                    na.strings = "Not Available", stringsAsFactors = FALSE)
    
    # Filter unnecessary data 
    data <- data[,c(2,7,11,17,23)]
    
    # rename columns
    names(data) <- c('hospital_name','State','heart attack','heart failure','pneumonia') 
    
    # Check the state and the outcome are valid
    if(!state %in% unique(data[,'State'])){
        stop('invalid state')
    }
    
    if(!outcome  %in% c('heart attack','heart failure','pneumonia')){
        stop('invalid outcome')
    }
    
    # return
    best_hospital = NULL
    
    # subset data on state
    df = data[data[,'State'] == state & !is.na(data[,outcome]),]
    
    # Return hospital name in the state with lowest 30-day mortality rate
    if(outcome == 'heart attack'){
        sorted_df = df[order(df[,'heart attack'],df[,'hospital_name']),]
        best_hospital = sorted_df[,'hospital_name'][1]
    
    } else if(outcome == 'heart failure'){
        sorted_df = df[order(df[,'heart failure'],df[,'hospital_name']),]
        best_hospital = sorted_df[,'hospital_name'][1]
    
    } else if(outcome == 'pneumonia'){
        sorted_df = df[order(df[,'pneumonia'],df[,'hospital_name']),]
        best_hospital = sorted_df[,'hospital_name'][1]
    }
    
    return(best_hospital)
}
