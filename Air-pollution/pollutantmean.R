# PART - 1
# calculate the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. 

pollutantmean <- function(directory, pollutant, id = 1:332){
    # list all the files in a directory,prepend with path
    files = list.files(path = directory, pattern = "*.csv", full.names = TRUE)
    
    # initialize empty data frame
    data = data.frame()
    
    # loop over id's:
    for(i in id){
        data = rbind(data,read.csv(files[i]))
    }
    # calculate mean of pollutant:
    avg = mean(data[,pollutant], na.rm = TRUE)
    
    # return mean
    return(avg)
}

