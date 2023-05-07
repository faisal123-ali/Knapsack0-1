#0-1 Knapsack's Problem
library(GA)


item=c('Sofas ','couches','Bookshelves','armchairs','Desks','Dressers','wardrobes','Entertainment centers','Dinning Tables','Beds ')
weight = c(70,73,77,80,82,87,90,94,98,106)
survival = c(135,139,149,150,156,163,173,184,192,201) 
data=data.frame(item,weight,survival)
max_weight=750


#1 means that we bring the item, while 0 means that we left the item
chromosomes=c(0,1,0,0,1,0,0,1,0,1)
data[chromosomes==1,]

#create the function that we want to optimize
fitness=function(x)
{

  current_survpoint=x%*%data$survival
  current_weight=x%*%data$weight
  if(current_weight>max_weight)
  {
    return(0)
  }
  else
  {
    return(current_survpoint)
  }
}
num_runs <- 30
result1 <- list()
result2 <- list()
result3 <- list()

for (i in 1:num_runs) {
  print(i)
GA <- ga(type = 'binary', fitness = fitness, nBits = nrow(data), maxiter = 30, popSize = 20, seed = 1234, keepBest = TRUE)

  result1[[i]] <- fitness

}
for (i in 1:num_runs) {
  print(i)
  GA2 <- ga(type = 'binary', fitness = fitness, nBits = nrow(data), maxiter = 30, popSize = 50, seed = 1234, keepBest = TRUE)
  
  result2[i] <- list(GA2 = GA)
  
}
for (i in 1:num_runs) {
  print(i)
  GA3 <- ga(type = 'binary', fitness = fitness, nBits = nrow(data), maxiter = 30, popSize = 200, seed = 1234, keepBest = TRUE)
  
  result3[[i]] <- list(GA3 = GA3)
  
}
plot(GA)
plot(GA2)
plot(GA3)


