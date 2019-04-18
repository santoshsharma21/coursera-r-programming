# Part-3
# calculate the correlation between sulfate and nitrate for monitor locations,
# where the number of completely observed cases (on all variables) is greater than the threshold

# Source complete.R function
source("complete.R")

corr <- function(directory, threshold = 0){
    # list all the files in a directory,prepend with path
    files = list.files(path = directory, pattern = "*.csv", full.names = TRUE)
    
    # return a numeric vector containing correlation value b/w pollutant
    cor_value = NULL
    
    # retrive complete cases dataframe
    complete_df = complete('specdata')
    
    # loop
    for(i in seq_len(nrow(complete_df))){
        if(complete_df[,'nobs'][i] > threshold){
            data = read.csv(files[i])
            data = data[complete.cases(data),]
            cor_value = append(cor_value, cor(data[,'sulfate'], data[,'nitrate']))
        }
    }
    return(cor_value)
}

