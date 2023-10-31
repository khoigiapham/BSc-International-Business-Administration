deliveries <- read.csv("DistanceMatrixAss2.csv", sep = ",", header = TRUE)
#Question 1a ----
#Step 1 Eopt
INF = 9999999
# number of nodes 
num_node = dim(deliveries)[1]
# create a array to track selected node
# K will become true otherwise false
K = rep(0,num_node)
# set number of edge to 0
num_edge = 0
# Minimumspanning tree: Eopt to track path
Eopt = data.frame(node = rep(0, num_node - 1), to = rep(0,num_node - 1), distance = rep(0,num_node - 1))
# the number of egde in minimum spanning tree will be
# always less than(num_node - 1), where num_node is number of vertices in
# choose random vertex and make it true
set.seed(5)
random <- sample(1:num_node,size = 1)
K[random] = TRUE
k <- 1
# print for edge and weight
while (num_edge < (num_node - 1)) {
  minimum = INF
  x = 0
  y = 0
  for (i in seq(num_node)) {
    if (K[i]) {
      for (j in seq(num_node)) {
        if ((!K[j]) && deliveries[i,j]){
          if (minimum > deliveries[i,j]){
            minimum = deliveries[i,j]
            x = i
            y = j
          }
        }
      }
    }
  }
  Eopt[k,1] <- x
  Eopt[k,2] <- y
  Eopt[k,3] <- deliveries[x,y]
  k <- k+1
  K[y] = TRUE
  num_edge <- num_edge + 1
}
Eopt

##Step 2:
#double the edges
E <- as.data.frame(rbind(as.matrix(Eopt[,1:3]),as.matrix(Eopt[,c(2:1,3)])))
Eout <- E[,1:2]
T <- data.frame(node = rep(0, (num_node - 1)*2), to = rep(0,(num_node - 1)*2))
V <- (c(1:num_node))
# Starting point
set.seed(5)
i = sample(V,size = 1)
V <- V[-(which(i==V))]
k=1
#Approximation
while(length(Eout[which(i==Eout$node),])!=0) {
  selected_edge <- Eout[which(i==Eout$node),][1,]
  for(l in 1:nrow(Eout[which(i==Eout$node),])){
    if (Eout[which(i==Eout$node),][l,2] %in% V){
      selected_edge <- Eout[which(i==Eout$node),][l,]
      index <- l
    }
  }
  T[k,] <- selected_edge
  Eout <- Eout[-which(Eout$node == selected_edge$node & Eout$to==selected_edge$to),]
  j <- selected_edge[,2]
  i <- j
  V <- V[-(which(V==i))]
  k=k+1
}
#Step 3: Remove second visit city
Tour <- T$node
Tour <- append(unique(Tour), Tour[1])
dist_path_a <- 0
#calculate distance
for(i in 1:num_node){
  dist <- as.numeric(deliveries[Tour[i], Tour[i+1]])
  dist_path_a <- dist_path_a + dist
}
dist_path_a


#Question 1b----
#algorithm b1 goes from 1 to 50 then go back to 1
path_b1 <- c(1:50,1)
path_b1

dist_path_b1 <- 0
dist <- 0
for(i in 1:num_node){
  dist <- as.numeric(deliveries[path_b1[i], path_b1[i+1]])
  dist_path_b1 <- dist_path_b1 + dist
}
dist_path_b1

#algorithm b2 from 1 3 5 ... to 49 to 2 4 6 ... to 50 then back to 1
path_b2 <- rep(0,50)
path_b2_even <- rep(0,25)
path_b2_odd <- rep(0,25)
for (i in 1:50) {
  if(i %% 2 == 0) {
    path_b2_even[i] <- i
  } else {
    path_b2_odd[i] <- i
  }
}
path_b2 <- c(path_b2_odd, path_b2_even, 1)
path_b2 <- path_b2[!is.na(path_b2)]
path_b2 <- path_b2[path_b2!=0]
path_b2

dist_path_b2 <- 0
dist <- 0
for(i in 1:num_node){
  dist <- as.numeric(deliveries[path_b2[i], path_b2[i+1]])
  dist_path_b2 <- dist_path_b2 + dist
}
dist_path_b2

#algorithm b3 from 25 to 50 to 24 to 1
path_b3 <- c(25:50,24:1,25)
path_b3

dist_path_b3 <- 0
dist <- 0
for(i in 1:num_node){
  dist <- as.numeric(deliveries[path_b3[i], path_b3[i+1]])
  dist_path_b3 <- dist_path_b3 + dist
}
dist_path_b3

#Question 2----
#4 start solutions
path_a <- Tour
path_b1
path_b2
path_b3
#4 corresponding objective function value
dist_path_a
dist_path_b1
dist_path_b2
dist_path_b3
#dist

#Question 2a----
iterations <- 2000

#1: path_a
j<-1
s1 <- path_a
set.seed(5)
while(j<iterations){
  g <- match((sample.int(num_node,1)),s1)
  path_a_swap <- replace(s1, c(g,g+1), s1[c(g+1, g)])
  if (g==1) {
    path_a_swap <- replace(path_a_swap, length(s1), path_a_swap[1])
  }
  if (g==num_node) {
    path_a_swap <- replace(path_a_swap, 1, path_a_swap[g+1])
  }
  dist_path_a_swap <- 0
  dist <- 0
  for(i in 1:num_node){
    dist <- as.numeric(deliveries[path_a_swap[i], path_a_swap[i+1]])
    dist_path_a_swap <- dist_path_a_swap + dist
  }
  if(dist_path_a_swap < dist_path_a){
    s1 <- path_a_swap
  }
  j <- j+1
}

dist_s1 <- 0
dist <- 0
for(i in 1:num_node){
  dist <- as.numeric(deliveries[s1[i], s1[i+1]])
  dist_s1<- dist_s1 + dist
}

s1
dist_s1


#2: path_b1
j<-1
s2 <- path_b1
set.seed(5)
while(j<iterations){
  g <- match((sample.int(num_node,1)),s2)
  path_b1_swap <- replace(s2, c(g,g+1), s2[c(g+1, g)])
  if (g==1) {
    path_b1_swap <- replace(path_b1_swap, length(s2), path_b1_swap[1])
  }
  if (g==num_node) {
    path_b1_swap <- replace(path_b1_swap, 1, path_b1_swap[g+1])
  }
  dist_path_b1_swap <- 0
  dist <- 0
  for(i in 1:num_node){
    dist <- as.numeric(deliveries[path_b1_swap[i], path_b1_swap[i+1]])
    dist_path_b1_swap <- dist_path_b1_swap + dist
  }
  if(dist_path_b1_swap < dist_path_b1){
    s2 <- path_b1_swap
  }
  j <- j+1
}

dist_s2 <- 0
dist <- 0
for(i in 1:num_node){
  dist <- as.numeric(deliveries[s2[i], s2[i+1]])
  dist_s2 <- dist_s2 + dist
}

s2
dist_s2


#3: path b2
j<-1
s3 <- path_b2
set.seed(5)
while(j<iterations){
  g <- match((sample.int(num_node,1)),s3)
  path_b2_swap <- replace(s3, c(g,g+1), s3[c(g+1, g)])
  if (g==1) {
    path_b2_swap <- replace(path_b2_swap, length(s3), path_b2_swap[1])
  }
  if (g==num_node) {
    path_b2_swap <- replace(path_b2_swap, 1, path_b2_swap[g+1])
  }
  dist_path_b2_swap <- 0
  dist <- 0
  for(i in 1:num_node){
    dist <- as.numeric(deliveries[path_b2_swap[i], path_b2_swap[i+1]])
    dist_path_b2_swap <- dist_path_b2_swap + dist
  }
  if(dist_path_b2_swap < dist_path_b2){
    s3 <- path_b2_swap
  }
  j <- j+1
}

dist_s3 <- 0
dist <- 0
for(i in 1:num_node){
  dist <- as.numeric(deliveries[s3[i], s3[i+1]])
  dist_s3 <- dist_s3 + dist
}

s3
dist_s3


#4: path b3
j<-1
s4 <- path_b3
set.seed(5)
while(j<iterations){
  g <- match((sample.int(num_node,1)),s4)
  path_b3_swap <- replace(s4, c(g,g+1), s4[c(g+1, g)])
  if (g==1) {
    path_b3_swap <- replace(path_b3_swap, length(s4), path_b3_swap[1])
  }
  if (g==num_node) {
    path_b3_swap <- replace(path_b3_swap, 1, path_b3_swap[g+1])
  }
  dist_path_b3_swap <- 0
  dist <- 0
  for(i in 1:num_node){
    dist <- as.numeric(deliveries[path_b3_swap[i], path_b3_swap[i+1]])
    dist_path_b3_swap <- dist_path_b3_swap + dist
  }
  if(dist_path_b3_swap < dist_path_b3){
    s4 <- path_b3_swap
  }
  j <- j+1
}

dist_s4 <- 0
dist <- 0
for(i in 1:num_node){
  dist <- as.numeric(deliveries[s4[i], s4[i+1]])
  dist_s4 <- dist_s4 + dist
}

s4
dist_s4


#Question 2b----
#1: path_a
j<-1
ss1 <- path_a
set.seed(5)
while(j<iterations){
  h <- c(match((sample.int(num_node,1)),ss1),match((sample.int(num_node,1)),ss1))
  path_a_swap <- replace(ss1, h, ss1[rev(h)])
  if (1 %in% h) {
    path_a_swap <- replace(path_a_swap, length(ss1), path_a_swap[1])
  }
  dist_path_a_swap <- 0
  dist <- 0
  for(i in 1:num_node){
    dist <- as.numeric(deliveries[path_a_swap[i], path_a_swap[i+1]])
    dist_path_a_swap <- dist_path_a_swap + dist
  }
  if(dist_path_a_swap < dist_path_a){
    ss1 <- path_a_swap
  }
  j <- j+1
}

dist_ss1 <- 0
dist <- 0
for(i in 1:num_node){
  dist <- as.numeric(deliveries[ss1[i], ss1[i+1]])
  dist_ss1<- dist_ss1 + dist
}

ss1
dist_ss1


#2: path_b1
j<-1
ss2 <- path_b1
set.seed(5)
while(j<iterations){
  h <- c(match((sample.int(num_node,1)),ss2),match((sample.int(num_node,1)),ss2))
  path_b1_swap <- replace(ss2, h, ss2[rev(h)])
  if (1 %in% h) {
    path_b1_swap <- replace(path_b1_swap, length(ss2), path_b1_swap[1])
  }
  dist_path_b1_swap <- 0
  dist <- 0
  for(i in 1:num_node){
    dist <- as.numeric(deliveries[path_b1_swap[i], path_b1_swap[i+1]])
    dist_path_b1_swap <- dist_path_b1_swap + dist
  }
  if(dist_path_b1_swap < dist_path_b1){
    ss2 <- path_b1_swap
  }
  j <- j+1
}

dist_ss2 <- 0
dist <- 0
for(i in 1:num_node){
  dist <- as.numeric(deliveries[ss2[i], ss2[i+1]])
  dist_ss2<- dist_ss2 + dist
}

ss2
dist_ss2


#3: path b2
j<-1
ss3 <- path_b2
set.seed(5)
while(j<iterations){
  h <- c(match((sample.int(num_node,1)),ss3),match((sample.int(num_node,1)),ss3))
  path_b2_swap <- replace(ss3, h, ss3[rev(h)])
  if (1 %in% h) {
    path_b2_swap <- replace(path_b2_swap, length(ss3), path_b2_swap[1])
  }
  dist_path_b2_swap <- 0
  dist <- 0
  for(i in 1:num_node){
    dist <- as.numeric(deliveries[path_b2_swap[i], path_b2_swap[i+1]])
    dist_path_b2_swap <- dist_path_b2_swap + dist
  }
  if(dist_path_b2_swap < dist_path_b2){
    ss3 <- path_b2_swap
  }
  j <- j+1
}

dist_ss3 <- 0
dist <- 0
for(i in 1:num_node){
  dist <- as.numeric(deliveries[ss3[i], ss3[i+1]])
  dist_ss3<- dist_ss3 + dist
}

ss3
dist_ss3


#4: path b3
j<-1
ss4 <- path_b3
set.seed(5)
while(j<iterations){
  h <- c(match((sample.int(num_node,1)),ss4),match((sample.int(num_node,1)),ss4))
  path_b3_swap <- replace(ss4, h, ss4[rev(h)])
  if (1 %in% h) {
    path_b3_swap <- replace(path_b3_swap, length(ss4), path_b3_swap[1])
  }
  dist_path_b3_swap <- 0
  dist <- 0
  for(i in 1:num_node){
    dist <- as.numeric(deliveries[path_b3_swap[i], path_b3_swap[i+1]])
    dist_path_b3_swap <- dist_path_b3_swap + dist
  }
  if(dist_path_b3_swap < dist_path_b3){
    ss4 <- path_b3_swap
  }
  j <- j+1
}

dist_ss4 <- 0
dist <- 0
for(i in 1:num_node){
  dist <- as.numeric(deliveries[ss4[i], ss4[i+1]])
  dist_ss4<- dist_ss4 + dist
}

ss4
dist_ss4


#Question 2c----
#1: path_a
j<-1
sss1 <- path_a
set.seed(5)
while(j<iterations){
  k <- c(1:5)
  k <- c(match((sample.int(num_node,1)),sss1):match((sample.int(num_node,1)),sss1))
  path_a_swap <- replace(sss1, k, sss1[rev(k)])
  if (1 %in% k) {
    path_a_swap <- replace(path_a_swap, length(sss1), path_a_swap[1])
  }
  dist_path_a_swap <- 0
  dist <- 0
  for(i in 1:num_node){
    dist <- as.numeric(deliveries[path_a_swap[i], path_a_swap[i+1]])
    dist_path_a_swap <- dist_path_a_swap + dist
  }
  if(dist_path_a_swap < dist_path_a){
    sss1 <- path_a_swap
  }
  j <- j+1
}

dist_sss1 <- 0
dist <- 0
for(i in 1:num_node){
  dist <- as.numeric(deliveries[sss1[i], sss1[i+1]])
  dist_sss1<- dist_sss1 + dist
}

sss1
dist_sss1


#2: path_b1
j<-1
sss2 <- path_b1
set.seed(5)
while(j<iterations){
  k <- c(1:5)
  k <- c(match((sample.int(num_node,1)),sss2):match((sample.int(num_node,1)),sss2))
  path_b1_swap <- replace(sss2, k, sss2[rev(k)])
  if (1 %in% k) {
    path_b1_swap <- replace(path_b1_swap, length(sss2), path_b1_swap[1])
  }
  dist_path_b1_swap <- 0
  dist <- 0
  for(i in 1:num_node){
    dist <- as.numeric(deliveries[path_b1_swap[i], path_b1_swap[i+1]])
    dist_path_b1_swap <- dist_path_b1_swap + dist
  }
  if(dist_path_b1_swap < dist_path_b1){
    sss2 <- path_b1_swap
  }
  j <- j+1
}

dist_sss2 <- 0
dist <- 0
for(i in 1:num_node){
  dist <- as.numeric(deliveries[sss2[i], sss2[i+1]])
  dist_sss2 <- dist_sss2 + dist
}

sss2
dist_sss2


#3: path b2
j<-1
sss3 <- path_b2
set.seed(5)
while(j<iterations){
  k <- c(1:5)
  k <- c(match((sample.int(num_node,1)),sss3):match((sample.int(num_node,1)),sss3))
  path_b2_swap <- replace(sss3, k, sss3[rev(k)])
  if (1 %in% k) {
    path_b2_swap <- replace(path_b2_swap, length(sss3), path_b2_swap[1])
  }
  dist_path_b2_swap <- 0
  dist <- 0
  for(i in 1:num_node){
    dist <- as.numeric(deliveries[path_b2_swap[i], path_b2_swap[i+1]])
    dist_path_b2_swap <- dist_path_b2_swap + dist
  }
  if(dist_path_b2_swap < dist_path_b2){
    sss3 <- path_b2_swap
  }
  j <- j+1
}

dist_sss3 <- 0
dist <- 0
for(i in 1:num_node){
  dist <- as.numeric(deliveries[sss3[i], sss3[i+1]])
  dist_sss3 <- dist_sss3 + dist
}

sss3
dist_sss3


#4: path b3
j<-1
sss4 <- path_b3
set.seed(5)
while(j<iterations){
  k <- c(1:5)
  k <- c(match((sample.int(num_node,1)),sss4):match((sample.int(num_node,1)),sss4))
  path_b3_swap <- replace(sss4, k, sss4[rev(k)])
  if (1 %in% k) {
    path_b3_swap <- replace(path_b3_swap, length(sss4), path_b3_swap[1])
  }
  dist_path_b3_swap <- 0
  dist <- 0
  for(i in 1:num_node){
    dist <- as.numeric(deliveries[path_b3_swap[i], path_b3_swap[i+1]])
    dist_path_b3_swap <- dist_path_b3_swap + dist
  }
  if(dist_path_b3_swap < dist_path_b3){
    sss4 <- path_b3_swap
  }
  j <- j+1
}

dist_sss4 <- 0
dist <- 0
for(i in 1:num_node){
  dist <- as.numeric(deliveries[sss4[i], sss4[i+1]])
  dist_sss4 <- dist_sss4 + dist
}

sss4
dist_sss4


#Question 2d----
#1: path_a
j<-1
#start solutions w1
w1 <- path_a
#best found solution
w_hat <- w1
dist_path_w_hat <- 0
dist <- 0
for(i in 1:num_node){
  dist <- as.numeric(deliveries[w_hat[i], w_hat[i+1]])
  dist_path_w_hat <- dist_path_w_hat + dist
}
dist_path_w_hat
#while loop
set.seed(5)
while(j<(iterations*2)){
  g <- match((sample(2:49,1)),w1)
  path_a_swap <- replace(w1, c(g-1,g+1), s1[c(g+1, g-1)])
  if (g==2) {
    path_a_swap <- replace(path_a_swap, length(s1), path_a_swap[1])
  }
  dist_path_a_swap <- 0
  dist <- 0
  for(i in 1:num_node){
    dist <- as.numeric(deliveries[path_a_swap[i], path_a_swap[i+1]])
    dist_path_a_swap <- dist_path_a_swap + dist
  }
  if(dist_path_a_swap < dist_path_w_hat){
    w_hat <- path_a_swap
  }
  j <- j+1
  if (dist_path_a_swap<dist_path_a){
    w1 <- path_a_swap
    break
  }
  else{
    x<-sample(1:100,1)
    if (x==1){
      w1 <- path_a_swap
    }
  }
}

dist_w1 <- 0
dist <- 0
for(i in 1:num_node){
  dist <- as.numeric(deliveries[w1[i], w1[i+1]])
  dist_w1<- dist_w1 + dist
}

w1
dist_w1


#2: path_b1
j<-1
#start solutions w2
w2 <- path_b1
#best found solution
w_hat2 <- w2
dist_path_w_hat2 <- 0
dist <- 0
for(i in 1:num_node){
  dist <- as.numeric(deliveries[w_hat2[i], w_hat2[i+1]])
  dist_path_w_hat2 <- dist_path_w_hat2 + dist
}
dist_path_w_hat2
#while loop
set.seed(5)
while(j<(iterations*2)){
  g <- match((sample(2:49,1)),w2)
  path_b1_swap <- replace(w2, c(g-1,g+1), s2[c(g+1, g-1)])
  if (g==2) {
    path_b1_swap <- replace(path_b1_swap, length(s2), path_b1_swap[1])
  }
  dist_path_b1_swap <- 0
  dist <- 0
  for(i in 1:num_node){
    dist <- as.numeric(deliveries[path_b1_swap[i], path_b1_swap[i+1]])
    dist_path_b1_swap <- dist_path_b1_swap + dist
  }
  if(dist_path_b1_swap < dist_path_w_hat2){
    w_hat2 <- path_b1_swap
  }
  j <- j+1
  if (dist_path_b1_swap<dist_path_b1){
    w2 <- path_b1_swap
    break
  }
  else{
    x<-sample(1:100,1)
    if (x==1){
      w2 <- path_b1_swap
    }
  }
}

dist_w2 <- 0
dist <- 0
for(i in 1:num_node){
  dist <- as.numeric(deliveries[w2[i], w2[i+1]])
  dist_w2<- dist_w2 + dist
}

w2
dist_w2


#3: path b2
j<-1
#start solutions w3
w3 <- path_b2
#best found solution
w_hat3 <- w3
dist_path_w_hat3 <- 0
dist <- 0
for(i in 1:num_node){
  dist <- as.numeric(deliveries[w_hat3[i], w_hat3[i+1]])
  dist_path_w_hat3 <- dist_path_w_hat3 + dist
}
dist_path_w_hat3
#while loop
set.seed(5)
while(j<(iterations*2)){
  g <- match((sample(2:49,1)),w3)
  path_b2_swap <- replace(w3, c(g-1,g+1), s3[c(g+1, g-1)])
  if (g==2) {
    path_b2_swap <- replace(path_b2_swap, length(s3), path_b2_swap[1])
  }
  dist_path_b2_swap <- 0
  dist <- 0
  for(i in 1:num_node){
    dist <- as.numeric(deliveries[path_b2_swap[i], path_b2_swap[i+1]])
    dist_path_b2_swap <- dist_path_b2_swap + dist
  }
  if(dist_path_b2_swap < dist_path_w_hat3){
    w_hat3 <- path_b2_swap
  }
  j <- j+1
  if (dist_path_b2_swap<dist_path_b2){
    w3 <- path_b2_swap
    break
  }
  else{
    x<-sample(1:100,1)
    if (x==1){
      w3 <- path_b2_swap
    }
  }
}

dist_w3 <- 0
dist <- 0
for(i in 1:num_node){
  dist <- as.numeric(deliveries[w3[i], w3[i+1]])
  dist_w3<- dist_w3 + dist
}

w3
dist_w3


#4: path b3
j<-1
#start solutions w4
w4 <- path_b3
#best found solution
w_hat4 <- w4
dist_path_w_hat4 <- 0
dist <- 0
for(i in 1:num_node){
  dist <- as.numeric(deliveries[w_hat4[i], w_hat4[i+1]])
  dist_path_w_hat4 <- dist_path_w_hat4 + dist
}
dist_path_w_hat4
#while loop
set.seed(5)
while(j<(iterations*2)){
  g <- match((sample(2:49,1)),w4)
  path_b3_swap <- replace(w4, c(g-1,g+1), s4[c(g+1, g-1)])
  if (g==2) {
    path_b3_swap <- replace(path_b3_swap, length(s4), path_b3_swap[1])
  }
  dist_path_b3_swap <- 0
  dist <- 0
  for(i in 1:num_node){
    dist <- as.numeric(deliveries[path_b3_swap[i], path_b3_swap[i+1]])
    dist_path_b3_swap <- dist_path_b3_swap + dist
  }
  if(dist_path_b3_swap < dist_path_w_hat4){
    w_hat4 <- path_b3_swap
  }
  j <- j+1
  if (dist_path_b3_swap<dist_path_b3){
    w4 <- path_b3_swap
    break
  }
  else{
    x<-sample(1:100,1)
    if (x==1){
      w4 <- path_b3_swap
    }
  }
}

dist_w4 <- 0
dist <- 0
for(i in 1:num_node){
  dist <- as.numeric(deliveries[w4[i], w4[i+1]])
  dist_w4<- dist_w4 + dist
}

w4
dist_w4


