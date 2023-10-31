library(lpSolve)
library(readr)
library(writexl)
#Q1----
#Load dataset
project <- read_delim("Project_Management_data.csv", 
                                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

#objective: minimize: 250 + sigma(ai*yi) + 70x8
#n=6 activities
#xi = start time of activity i
#yi = number of crashing day for activity i
#ai = cost of crashing 1 day

f.obj <- rep(0,16)
normaldur <- project$`Normal Duration`
crashingcost <- project$`a(i)`
variable_coef <- vector()

for (i in 1:8) {
  if(i == 8) {
    variable_coef[i] <- 70
  } else {
    variable_coef[i] <- 0
  }
  f.obj <- c(variable_coef, crashingcost)
}
f.obj

#contraint 1, precedence relations:
#1: x2 - x1 + y1 >= r1 = 0  <=>    -x1 + x2 + y1 >= r1
#2: x3 - x2 + y2 >= r2 = 4  <=>    -x2 + x3 + y2 >= r2
#3: x4 - x2 + y2 >= r2 = 4  <=>    -x2 + x4 + y2 >= r2
#4: x5 - x2 + y2 >= r2 = 4  <=>    -x2 + x5 + y2 >= r2
#5: x6 - x3 + y3 >= r3 = 2  <=>    -x3 + x6 + y3 >= r3
#6: x7 - x4 + y4 >= r4 = 2  <=>    -x4 + x7 + y4 >= r4
#7: x7 - x5 + y5 >= r5 = 4  <=>    -x5 + x7 + y5 >= r5
#8: x7 - x6 + y6 >= r6 = 3  <=>    -x6 + x7 + y6 >= r6
#9: x8 - x7 + y7 >= r7 = 2  <=>    -x7 + x8 + y7 >= r7
#ri: normal duration
#ri - yi = actual duration

#contraint 1:
constraint1_lhs <- matrix(c(-1,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,
                     0,-1,1,0,0,0,0,0,0,1,0,0,0,0,0,0,
                     0,-1,0,1,0,0,0,0,0,1,0,0,0,0,0,0,
                     0,-1,0,0,1,0,0,0,0,1,0,0,0,0,0,0,
                     0,0,-1,0,0,1,0,0,0,0,1,0,0,0,0,0,
                     0,0,0,-1,0,0,1,0,0,0,0,1,0,0,0,0,
                     0,0,0,0,-1,0,1,0,0,0,0,0,1,0,0,0,
                     0,0,0,0,0,-1,1,0,0,0,0,0,0,1,0,0,
                     0,0,0,0,0,0,-1,1,0,0,0,0,0,0,1,0), nrow = 9, ncol = 16, byrow = TRUE)

#contraint 2: maximize yi <= di (crashing cost)
#di = amount of days activity i can be crashed, i in 1:8
constraint2_lhs <- matrix(rep(0,16), nrow = 8, ncol = 16, byrow = TRUE)
for (i in 1:8) {
  constraint2_lhs[i, i+8] <- 1
}

#contraint 3: domains: x1 = 0; xi is integer (i in 2:8); yi is integer (i in 1:8)
constraint3_lhs <- matrix(rep(0,16), ncol = 16)
for (i in 1:16) {
  if(i == 1) {
    constraint3_lhs[i] <- 1
  }
}

#combine three contraints
f.con <- as.matrix(rbind(constraint1_lhs, constraint2_lhs, constraint3_lhs))

#direction
f.dir <- rep(0,18)
for (i in 1:nrow(f.con)) {
  if(i <= 9) {
    f.dir[i] <- ">="
  } else {
    if(i == 18) {
      f.dir[i] <- "=="
    } else {
      f.dir[i] <- "<="
    }
  }
}
f.dir

#RHS
f.rhs <- vector()
crashdays <- project$`d(i)`
for (i in 1:18) {
  for (j in 1:8) {
    if(i == 18) {
      f.rhs[i] <- 0
    } else {
      if(i <= 9) {
        if(constraint1_lhs[i,j] == -1) {
          f.rhs[i] <- normaldur[j]
        } 
      } else {
        f.rhs[i] <- crashdays[i-9]
      }
    }
  }
}
f.rhs

#1.optimization
project <- lp("min", f.obj, f.con, f.dir, f.rhs, all.int = TRUE)
project
project$solution
project_optimize <- 250 + project$objval

#2.continuous variables
proj_sol_cont <- lp("min", f.obj, f.con, f.dir, f.rhs, all.int = FALSE)
proj_sol_cont 
proj_sol_cont $solution

#3.sensitivity
proj_sol_sens <- lp("min", f.obj, f.con, f.dir, f.rhs, compute.sens = TRUE)
proj_sol_sens $sens.coef.from
proj_sol_sens $sens.coef.to

#Q2----
#Load dataset
duration <- read_delim("Duration_Matrix.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)
#Subset duration matrix
duration_matrix <- duration[1:30,2:31]
n <- nrow(duration_matrix)

#dynamic programming 
for (k in 1:n){
  for (i in 1:n){
    for (j in 1:n){
      if (duration_matrix[i,j] >= duration_matrix[i,k]+duration_matrix[k,j]){
        duration_matrix[i,j] <- duration_matrix[i,k]+duration_matrix[k,j]
      }
    }
  }
}

#approach 2 using min function instead of if
duration_matrix <- duration[1:30,2:31]

for (k in 1:n){
  for (i in 1:n){
    for (j in 1:n){
      duration_matrix[i,j] = min(duration_matrix[i,j], duration_matrix[i,k]+duration_matrix[k,j])
    }
  }
}

#re create column id
duration_matrix['ID'] <- c(1:30)
#reorder by column index
duration_matrix <- duration_matrix[, c(31,1:30)]
#my final answer is table duration_matrix
write.table(duration_matrix, "question2.txt", row.names = F, sep = ",")

