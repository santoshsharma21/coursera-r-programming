# Ranking hospitals by outcome in a state
rankhospital <- function(state, outcome, num = 'best'){
    # read data
    data = read.csv("A:/coursera-r-programming/Hospital-quality/outcome-of-care-measures.csv", 
                    na.strings = "Not Available", stringsAsFactors = FALSE)
    # Filter unnecessary data 
    data <- data[,c(2,7,11,17,23)]
    
    # rename columns
    names(data) <- c('hospital_name','State','heart attack','heart failure','pneumonia')
    
    # check validity of the input: 'state', 'outcome'
    if(! outcome %in% c('heart attack','heart failure','pneumonia')){
        stop('invalid outcome')
    }
    
    if(! state %in% unique(data[,"State"])){
        stop('invalid state')
    }
    
    # return hospital name 
    hospital = NULL
    
    # subset data on state
    data = data[data[,'State'] == state & !is.na(data[,outcome]),]
    
    # sort data
    sorted_df = data[order(data[,outcome], data[,"hospital_name"]),]
    
    # rank hospital by outcome in state
    if(num %in% 1:nrow(data)){
        hospital = sorted_df[,"hospital_name"][num]
    } else if(num == 'best'){
        hospital = sorted_df[,"hospital_name"][which.min(sorted_df[,outcome])]
    } else if(num == 'worst'){
        hospital = sorted_df[,"hospital_name"][which.max(sorted_df[,outcome])]
    }
    
    # return 'NA' if num is larger than the number of hospitals in state
    if(!num %in% 1:nrow(data) & !num %in% c("best","worst")){
        hospital = NA
    }
    return(hospital)
}

