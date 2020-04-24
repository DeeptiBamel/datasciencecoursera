pollutantmean <- function(directory, pollutant, id=1:322){

  files_list <- list.files(directory)
  files_list

  data <- NA
  
  for(i in id){
    
    file_directory <- paste(directory, files_list[i], sep="")
    file_data <- read.csv(file_directory)
    
    data <- rbind(data, file_data)
  }
  
  mean_data <- mean(data[[pollutant]], na.rm = TRUE)
  return(mean_data)
}

getwd()
directory <- paste(getwd(),"/","specdata","/",sep="")
directory

#Assignment - Test Scenarios
test_data1 <- pollutantmean(directory,"sulfate",id=1:10) 
test_data1

test_data2 <- pollutantmean(directory,"nitrate",id=70:72) 
test_data2

test_data3 <- pollutantmean(directory,"nitrate",id=23) 
test_data3

#Quiz - Questions
test_data4 <- pollutantmean(directory,"sulfate",34) 
test_data4

test_data5 <-pollutantmean(directory, "nitrate")
test_data5
