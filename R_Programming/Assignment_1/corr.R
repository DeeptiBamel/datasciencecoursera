corr <- function(directory, threshold = 0) {
  
  observations <- complete(directory)
  filtered_observations = subset(observations,observations$nobs > threshold)
  
  file_list <- list.files(directory)
  correlation <- vector()
  
  for (i in filtered_observations$id) {
    
    file_dir <- paste(directory,file_list[i],sep="")
    file_data <- read.csv(file_dir)
   
    file_data <- subset(file_data,complete.cases(file_data))        
   
    correlation <- c(correlation,cor(file_data$nitrate,file_data$sulfate))    
  }
  
  correlation
}

getwd()
directory <- paste(getwd(),"/","specdata","/",sep="")
directory

#Assignment - Test Scenarios
#Test Scenario 1
cr <- corr(directory, 150)
head(cr)

summary(cr)

#Test Scenario 2
cr <- corr(directory, 400)
head(cr)

summary(cr)

#Test Scenario 3
cr <- corr(directory, 5000)
summary(cr)

length(cr)

#Test Scenario 4
cr <- corr(directory)
summary(cr)

length(cr)

#Quiz - Questions
cr <- corr(directory)                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)


cr <- corr(directory, 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)


cr <- corr(directory, 2000)                
n <- length(cr)                
cr <- corr(directory, 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
