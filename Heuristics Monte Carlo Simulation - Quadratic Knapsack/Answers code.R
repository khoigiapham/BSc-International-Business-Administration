##Q1----
#Convert revenue to matrix
value_matrix <- as.matrix(value)
#Get the revenue of single item
ai <- diag(value_matrix)
#Remove value below diagonal revenue_matrix
for (i in 1:200){
  for (j in 1:200){
    if (i> j){
      value_matrix[i,j] = 0
    }
  }
}
#Generate X matrices
num_item <- nrow(value)
x <- rep(0, num_item)
x_matrix <- matrix(0,num_item,num_item)
index <- c(1:num_item)
#Random get the 40 initial solutions
set.seed(15)
initial_sol <- sample(index,size = 40)
#Set the corresponding initial solution's x to 1
x[initial_sol] = 1
#Calculate initial revenue
i<-1
j<-1
for (i in 1:num_item){
  for(j in 1: num_item){
    x_matrix[i,j] <- x[i]*x[j]
  }
}
initial_revenue = sum(x_matrix * value_matrix)

#1a----
iterations <- 7000
k <- 1
best_revenue_1 <- initial_revenue
initial_sol_1 <- initial_sol
best_sol_1 <- initial_sol
set.seed(15)
while (k < iterations){
  h <- c(match((sample(initial_sol_1,1)),index),match((sample(index[-initial_sol_1],1)),index))
  x <- rep(0, num_item)
  x[initial_sol_1] = 1
  x_swap <- replace(x, h, x[rev(h)])
  x_matrix <- matrix(0,num_item,num_item)
  i<-1
  j<-1
  for (i in 1:num_item){
    for(j in 1: num_item){
      x_matrix[i,j] <- x_swap[i]*x_swap[j]
    }
  }
  revenue_swap <- sum(x_matrix * value_matrix)
  initial_sol_1 <- which(x_swap == 1)
  if(revenue_swap > best_revenue_1){
    best_revenue_1 <- revenue_swap
    best_sol_1 <- initial_sol_1
    }
  k <- k+1
}
best_revenue_1
best_sol_1

#1b----
iterations <- 7000
k <- 1
best_revenue_2 <- initial_revenue
initial_sol_2 <- initial_sol
best_sol_2 <- initial_sol
set.seed(15)
while (k < iterations){
  item_add <- match(sample(index[-initial_sol_2],1),index)
  lowest_value_item <- match(min(ai[initial_sol_2]),ai)
  x <- rep(0, num_item)
  x[initial_sol_2] = 1
  h <- c(lowest_value_item,item_add)
  x_swap <- replace(x, h, x[rev(h)])
  x_matrix <- matrix(0,num_item,num_item)
  i<-1
  j<-1
  for (i in 1:num_item){
    for(j in 1: num_item){
      x_matrix[i,j] <- x_swap[i]*x_swap[j]
    }
  }
  revenue_swap <- sum(x_matrix * value_matrix)
  initial_sol_2 <- which(x_swap == 1)
  if(revenue_swap > best_revenue_2){
    best_revenue_2 <- revenue_swap
    best_sol_2 <- initial_sol_2
  }
  k <- k+1
}
best_revenue_2
best_sol_2

#1c----
#Swaping the highest item outside with random chosen item.
iterations <- 7000
k <- 1
best_revenue_3 <- initial_revenue
initial_sol_3 <- initial_sol
best_sol_3 <- initial_sol
set.seed(15)
while (k < iterations){
  item_remove <- match(sample(index[initial_sol_3],1),index)
  highest_value_item <- match(max(ai[-initial_sol_3]),ai)
  x <- rep(0, num_item)
  x[initial_sol_3] = 1
  h <- c(item_remove,highest_value_item)
  x_swap <- replace(x, h, x[rev(h)])
  x_matrix <- matrix(0,num_item,num_item)
  i<-1
  j<-1
  for (i in 1:num_item){
    for(j in 1: num_item){
      x_matrix[i,j] <- x_swap[i]*x_swap[j]
    }
  }
  revenue_swap <- sum(x_matrix * value_matrix)
  initial_sol_3 <- which(x_swap == 1)
  if(revenue_swap > best_revenue_3){
    best_revenue_3 <- revenue_swap
    best_sol_3 <- initial_sol_3
  }
  k <- k+1
}
best_revenue_3
best_sol_3

#1d----
#Duplicate the "index" 2 times
index_1d <- rep(index, times = 2)
#Random get the 40 initial solutions
set.seed(15)
initial_sol_1d <- sample(index_1d,size = 40, replace = FALSE)
#Items that are on 2 pages in the initial solutions
initial_sol_1d[duplicated(initial_sol_1d)]
#Re-generate x matrix
x <- rep(0, num_item)
x_matrix <- matrix(0,num_item,num_item)
#Set the corresponding initial solution's x to 1 and increase to 2 if there is already a '1' value
for (i in 1:length(initial_sol_1d)){
  d <- initial_sol_1d[i]
  x[d] <- x[d]+1
}


#Calculate initial revenue
i<-1
j<-1
for (i in 1:num_item){
  for(j in 1: num_item){
    if(i==j){
      x_matrix[i,j] = x[i]
    }else{
      x_matrix[i,j] <- x[i]*x[j]
    }
  }
}
initial_revenue_1d = sum(x_matrix * value_matrix)

#Adapted heuristic search from 1c
iterations <- 7000
k <- 1
best_revenue_4 <- initial_revenue_1d
initial_sol_4 <- initial_sol_1d
best_sol_4 <- initial_sol_1d
set.seed(15)
while (k < iterations){
  item_remove <- sample(initial_sol_4,1)
  highest_value_item <- match(max(ai[-initial_sol_4]),ai)
  x <- rep(0, num_item)
  for (i in 1:length(initial_sol_1d)){
    d <- initial_sol_1d[i]
    x[d] <- x[d]+1
  }
  h <- c(item_remove,highest_value_item)
  x_swap <- replace(x, h, x[rev(h)])
  x_matrix <- matrix(0,num_item,num_item)
  i<-1
  j<-1
  for (i in 1:num_item){
    for(j in 1: num_item){
      if(i==j){
        x_matrix[i,j] = x_swap[i]
      }else{
        x_matrix[i,j] <- x_swap[i]*x_swap[j]
      }
    }
  }
  revenue_swap <- sum(x_matrix * value_matrix)
  initial_sol_4 <- vector()
  for (i in 1:length(x_swap)){
    if (x_swap[i] == 2){
      initial_sol_4 <- c(initial_sol_4, i)
      initial_sol_4 <- c(initial_sol_4, i)
    } else if(x_swap[i] == 1){
      initial_sol_4 <- c(initial_sol_4, i)
    }
  }
  if(revenue_swap > best_revenue_4){
    best_revenue_4 <- revenue_swap
    best_sol_4 <- initial_sol_4
  }
  k <- k+1
}
best_revenue_4
best_sol_4


#Q2----
#install.packages("EnvStats")
library(EnvStats)

#Step1a: Create a Monte Carlo simulation function that generate a value_matrix based on 3 tables: value_min; value_max; value_mode
mc_simulation <- function(anymatrix) {
  anymatrix <- matrix(0,num_item,num_item)
  for(i in 1:num_item){
    for(j in 1:num_item){
      anymatrix[i,j] <- rtri(1,min = value_min[i,j],max =value_max[i,j],mode = value_mode[i,j])
    }
  }
  #Remove value below diagonal line in anymatrix
  for (i in 1:200){
    for (j in 1:200){
      if (i> j){
        anymatrix[i,j] = 0
      }
    }
  }
  return(anymatrix)
}

#Step 1b: Create 100MC simulated dataframes
relizations <- 100
MC100 <- list()
pb <- txtProgressBar(min = 0, max = relizations,style = 3, width = 50, char = "=")
set.seed(15)
for (i in 1:100){
  MC100[[i]] <- mc_simulation(i)
  setTxtProgressBar(pb, i)
}
close(pb)

#Step 2a:Create a function that calculate the initial revenue on any Monte Carlo simulated matrix
cal_initial_sol <- function(anymatrix) {
  x <- rep(0, num_item)
  x_matrix <- matrix(0,num_item,num_item)
  x[initial_sol] = 1
  i<-1
  j<-1
  for (i in 1: num_item){
    for(j in 1: num_item){
      x_matrix[i,j] <- x[i]*x[j]
    }
  }
  initial_revenue_q2 = sum(x_matrix * anymatrix)
  return(initial_revenue_q2)
}

#Step 2b: Initial revenue of 100 MC simulated dataframes
MC100_initial_revenue <- vector()
for (i in 1:100){
  MC100_initial_revenue <- c(MC100_initial_revenue, cal_initial_sol(MC100[[i]]))
  setTxtProgressBar(pb, i)
}
close(pb)

#Step 3: Heuristic search
#vector to store objective value
mc_obj <- vector()
#Store the objective values to mc_obj
l <- 1
relizations <- 100 # Number of iterations of the loop
# Initializes the progress bar to keep track of the loop progress
pb <- txtProgressBar(min = 0, max = relizations,style = 3, width = 50, char = "=")
set.seed(15)
for(l in 1:relizations) {
  mc_value <- MC100[[l]]
  initial_revenue_q2 <- MC100_initial_revenue[l]
  best_revenue_q2 <- initial_revenue_q2
  #intial_sol remains unchanged from question 1
  initial_sol_q2 <- initial_sol
  best_sol_q2 <- initial_sol
  #run heuristic search 200 times
  iterations <- 200
  k <- 1
  while (k < iterations){
    h <- c(match((sample(initial_sol_q2,1)),index),match((sample(index[-initial_sol_q2],1)),index))
    x <- rep(0, num_item)
    x[initial_sol_q2] = 1
    x_swap <- replace(x, h, x[rev(h)])
    x_matrix <- matrix(0,num_item,num_item)
    i<-1
    j<-1
    for (i in 1:num_item){
      for(j in 1: num_item){
        x_matrix[i,j] <- x_swap[i]*x_swap[j]
      }
    }
    revenue_swap <- sum(x_matrix * mc_value)
    initial_sol_q2 <- which(x_swap == 1)
    if(revenue_swap > best_revenue_q2){
      best_revenue_q2 <- revenue_swap
      best_sol_q2 <- initial_sol_q2
    }
    k <- k+1
  }
  mc_obj <- c(mc_obj, best_revenue_q2)
  setTxtProgressBar(pb, l)
}
close(pb) #disconnect the progress bar
mc_obj
#average objective value
avg_mc_obj <- mean(mc_obj)
#Step4: compare to objective value 1a
avg_mc_obj
best_revenue_1
#END----
# Thank you Professor and team for such an intensive course, were so glad that we are now able to graduate <3!!

