## read data
options(scipen = 999)
data <- readRDS("~/data.rds")
colnames(data) <- c("Sex", "Length", "Diameter", "Height", "Whole weight", 
                    "Shuckedweight", "Visceraweight", "Shellweight", "Label")

#
naivebayes <- function(data, Ntrain) {
  
  # split data
  trainlines <- sample(1:nrow(data), Ntrain)
  traindata <- data[trainlines, ]
  testdata <- data[-trainlines, ]
  # classes
  dataclassmap <- data.frame()
  for (i in 1:(ncol(traindata) - 1)) {
    dataclassmap <- rbind(dataclassmap, data.frame(col = i, class = class(traindata[, i])))
  }
  
  summaries <- list()
  
  for (j in dataclassmap[dataclassmap$class == "numeric", "col"]) {
    probs <- cbind(aggregate(. ~ Label, data = data.frame(traindata[,j], Label = traindata$Label), FUN = base::mean), 
                   aggregate(. ~ Label, data = data.frame(traindata[,j], Label = traindata$Label), sd))[, c(1,2,4)]
    
    colnames(probs) <- c("Label", "mean", "sd")
    summaries[[j]] <- probs
  }
  
  for (j in dataclassmap[dataclassmap$class == "factor", "col"]) {
    probs <- data.frame(table(data.frame(traindata[, j], Label = traindata$Label)))
    probs$Freq <- probs$Freq/nrow(traindata)
    summaries[[j]] <- probs
  }
  
  # Calculate Gaussian Probability Density
  calculateProbability <- function(x, mean, stdev) {
    # exponent = exp(-(((x-mean)^2)/(2*(stdev^2)))) return (1 / (sqrt(2*pi) * stdev) * exponent)
    return(dnorm(x, mean, stdev))
  }
  
  # Calculate Class Probabilities
  calculateClassProbabilities <- function(summaries, inputVector) {
    probss <- data.frame()
    
    for (k in dataclassmap[dataclassmap$class == "factor","col"]) {
      index <- summaries[k][[1]]
      probss <- rbind(probss, index[index[, 1] == as.vector(inputVector[1,k]), c(2, 3)])
      
    }
    
    
    
    
    for (l in dataclassmap[dataclassmap$class == "numeric", "col"]) {
      index <- summaries[l][[1]]
      
      for (s in index$Label) {
        probss <- rbind(probss, data.frame(Label = s, Freq = calculateProbability(inputVector[1,l], index[s, 2], index[s, 3])))
      }
    }
    
    return(which.max(aggregate(probss$Freq, by = list(probss$Label), prod)[, 2]))
  }
  
  
  
  
  #Prediction
  
  resulttest <- data.frame()
  
  for (z in 1:nrow(testdata)) {
    resulttest <- rbind(resulttest, data.frame(line = z, pred = calculateClassProbabilities(summaries, testdata[z, ]), actual = as.numeric(testdata[z, "Label"])))
  }
  
  resulttrain <- data.frame()
  
  for (y in 1:nrow(traindata)) {
    resulttrain <- rbind(resulttrain, data.frame(line = y, pred = calculateClassProbabilities(summaries, traindata[y, ]), actual = as.numeric(traindata[y, "Label"])))
  }
  
  
  results <- list()
  
  results[["test Conf. Matrix"]] <- table(resulttest[, 2:3])
  
  results[["test Misclass. Results"]] <- table(resulttest$pred == resulttest$actual)
  
  
  results[["train Conf. Matrix"]] <- table(resulttrain[, 2:3])
  
  results[["train Misclass. Results"]] <- table(resulttrain$pred == resulttrain$actual)
  
  
  return(results)
}

# Testing for different cases
#1) Using only the first 3 features (sex, length, and diameter) as input, and
#
#1.1) 100 samples for training, and rest for validation set
#1.2) 1000 samples for training, and rest for validation set
#1.3) 2000 samples for training, and rest for validation set
#
#2) Using all features as input, and
#
#2.1) 100 samples for training, and rest for validation set
#2.2) 1000 samples for training, and rest for validation set
#2.3) 2000 samples for training, and rest for validation set





data1 <- data[, c(1, 2, 3, 9)]



## Prediction
a11 <- naivebayes(data1, 100)
a12 <- naivebayes(data1, 1000)
a13 <- naivebayes(data1, 2000)

b21 <- naivebayes(data, 100)
b22 <- naivebayes(data, 1000)
b23 <- naivebayes(data, 2000)
