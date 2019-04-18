# Part-2
# Find the number of completely observed cases in each data file

complete <- function(directory, id = 1:332){
    # list all the files in a directory,prepend with path
    files = list.files(path = directory, pattern = "*.csv", full.names = TRUE)
    
    #intialize variables
    monitor_id = NULL
    comp_obs = NULL
    
    # loop over monitor's
    for(i in id){
        monitor_id = append(monitor_id,i)
        comp_obs = append(comp_obs,sum(complete.cases(read.csv(files[i]))))
    }
    # return data frame with id and count of complete cases
    complete_df = data.frame(id = monitor_id, nobs = comp_obs)
    return(complete_df)
}

