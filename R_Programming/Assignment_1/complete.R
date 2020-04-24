complete <- function(directory, id = 1:332) {
  
  file_list <- list.files(directory)
  ids <- vector()
  nobs <- vector()
  
  for (i in id) {

    file_dir <- paste(directory,file_list[i],sep="")
    file_data <- read.csv(file_dir)
    
    ids = c(ids,i)
    nobs = c(nobs,sum(complete.cases(file_data)))        
  }

  data.frame(id = ids, nobs = nobs)
}

getwd()
directory <- paste(getwd(),"/","specdata","/",sep="")
directory

#Assignment - Test Scenarios
test_data1 <- complete(directory, 1)
test_data1

test_data2 <- complete(directory,  c(2, 4, 8, 10, 12))
test_data2

test_data3 <- complete(directory, 30:25)
test_data3

test_data4 <- complete(directory, 3)
test_data4

#Quiz - Questions
cc <- complete(directory,  c(6, 10, 20, 34, 100, 200, 310))
cc$nobs

cc <- complete(directory, 54)
cc$nobs

RNGversion("3.5.1")  
set.seed(42)
cc <- complete(directory, 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])
