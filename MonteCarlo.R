#Function to calculate distance between passenger and car 
library(lpSolve)
calculate_distance <- function(pass.location,car.location) {
  distance <-sum(abs(pass.location - car.location))
 
  return(distance)
}
car.location  = c(3,7)
pass.location = c(6,3)
calculate_distance(car.location,pass.location)

# Function to create a new car s
newcar <- function(CID = paste(sample(c(LETTERS,0:9),10),sep="",collapse=""),
                       Status = 'Waiting',
                       Location = c(sample(1:18, 1),sample(1:18, 1)),
                       Destination = NULL,
                       PID= NULL ) {
   
   new.car <- list('CID' = CID,"Status" = Status,"Location" = Location,"Destination" = Destination,"PID" = PID)
   return (new.car)
  
}
#Function to create a new passenger 
newpass <- function(PID = paste(sample(c(LETTERS,0:9),10),sep="",collapse=""),
                   WaitTime = 0,
                   Location = c(sample(1:18, 1),sample(1:18, 1)),
                   Destination = c(sample(1:18, 1),sample(1:18, 1)))
                   {
  new.pass <- list('PID' = PID,"WaitTime" = WaitTime,"Location" = Location,"Destination" = Destination)
  return (new.pass)
  
}

set.seed(19930427)


passenger.list<-replicate(12,list(newpass()))
cars.list<-replicate(12,list(newcar()))
passenger_PIDs <- unlist(lapply(passenger.list, function(x) (x$PID)))
car_CIDs <- unlist(lapply(cars.list, function(x) (x$CID)))

create_distance_matrix <- function(row_number,col_number,car_CIDS,pasenger_PIDs,passenger_list,cars_list) {
  dist.matrix <-matrix( nrow = row_number, ncol = col_number) # create matrix
  colnames(dist.matrix) <-car_CIDs # name columns 
  rownames(dist.matrix) <- passenger_PIDs # name rows
  
  for(row in 1:nrow(dist.matrix)) {
    for(col in 1:ncol(dist.matrix)) {
      car_id <- colnames(dist.matrix)[col] # get name of current column 
      passenger_id <- rownames(dist.matrix)[row] # get name of current row
      current_passenger <- Filter(function(x){ x$PID == passenger_id},passenger_list) # get current passenger
      current_car <- Filter(function(x){ x$CID == car_id},cars_list) # get current car 
      current_distance <- calculate_distance(current_passenger[[1]]$Location,current_car[[1]]$Location)
      dist.matrix[row,col] <-current_distance
    }
  }
  return(dist.matrix)
}



dist.matrix <- create_distance_matrix(12,12,car_CIDs,passenger_PIDs,passenger.list,cars.list)
# Get the closest car with the distance to each paasenger
calculate_shortest_car <- function(row_number) {
  j <- which.min(dist.matrix[row_number,]) # get the lowest distance o
  return(c(paste(rownames(dist.matrix)[row_number], colnames(dist.matrix)[j], sep=' '), dist.matrix[row_number,j]))
}
shortest_distance_vector <- t(sapply(seq(nrow(dist.matrix)), function(x) calculate_shortest_car(x)))

# Get the closest car and distance to each passenger 
car_passenger1 <-unlist(lapply(shortest_distance_vector[1,1], (strsplit), " "))
print(paste("For passenger 1, the CID of shortest car is ",car_passenger1[2],
            ".The distance is ",shortest_distance_vector[1,2]))

fm <- lp.assign(dist.matrix)
solution <- fm$solution

# MONTE CARLO SIMULATION PART 1 
num.its <- 10000 # set the number of iterations
results.container <-
  rep(NA,num.its)# A container to store the results of each iteration
for (i in 1:num.its) { #Start the simulation
  total_distance <- 0 
  print(paste("We are on iteration number",i))
  p.list<-replicate(12,list(newpass())) # create a new passenger list
  c.list<-replicate(12,list(newcar())) # create a new car list
  passenger_PIDs <- unlist(lapply(p.list, function(x) (x$PID))) # get Passenger ID's for row names of our matrix 
  car_CIDs <- unlist(lapply(c.list, function(x) (x$CID)))# get Car ID's for column names of our matrix 
  distance.matrix <- create_distance_matrix(12,12,car_CIDs,passenger_PIDs,p.list,c.list) # Create our distance matrix 
  fm <- lp.assign(distance.matrix)
  matrix_positions <- which(round(fm$solution) == 1,arr.ind =TRUE) # get the matrix positions of (Passenger,Car) pairs. 
  for(row in 1:nrow(matrix_positions)) { # for each position
    position<- matrix_positions[row,]
    r <- position[1] # get row of matrix position in fm$solution
    col <- position[2] # get column of matrix position in fm$solution
    c_id <- colnames(distance.matrix)[col] # get the car id corresponding to that column in distance.matrix
    p_id <- rownames(distance.matrix)[r]# get the passenger id corresponding to that row in distance.matrix
    distance <- distance.matrix[r,col] # get the distance from that 
    total_distance <- total_distance + distance # Accumulate distances between passenger and car
    
  }
  results.container <- (total_distance / 12) # record the average distance (waiting time)
}
print(paste("The average waiting time for 12 passengers was",mean(results.container),'.'))

# MONTE CARLO SIMULATION PART 2
num.its <- 10000 # set the number of iterations
results.container <-
  rep(NA,num.its)# A container to store the results of each iteration
for (i in 1:num.its) { #Start the simulation
  print(paste("We are on iteration number",i))
  p.list<-replicate(12,list(newpass())) # create a new passenger list
  c.list<-replicate(12,list(newcar())) # create a new car list
  passenger_PIDs <- unlist(lapply(p.list, function(x) (x$PID))) # get Passenger ID's for row names of our matrix 
  car_CIDs <- unlist(lapply(c.list, function(x) (x$CID)))# get Car ID's for column names of our matrix 
  distance.matrix <- create_distance_matrix(12,12,car_CIDs,passenger_PIDs,p.list,c.list) # Create our distance matrix 
  fm <- lp.assign(distance.matrix)
  matrix_positions <- which(round(fm$solution) == 1,arr.ind =TRUE) # get the matrix positions of (Passenger,Car) pairs. 
  for(row in 1:nrow(matrix_positions)) { # for each position
    position<- matrix_positions[row,]
    r <- position[1] # get row of matrix position in fm$solution
    col <- position[2] # get column of matrix position in fm$solution
    c_id <- colnames(distance.matrix)[col] # get the car id corresponding to that column in distance.matrix
    p_id <- rownames(distance.matrix)[r]# get the passenger id corresponding to that row in distance.matrix
    distance <- distance.matrix[r,col] # get the distance from that 
    p.list[[r]]$WaitTime <- distance # Update waiting time of the passenger
    
  }
  # Now we want to check if any passenger waits for a car and gets dropped of before Passenger 12 is picked up 
  
    # calculate waiting time + distance between location and destination for each of the passengers. 
    total_times <- unlist(lapply(p.list,function(x) {x$WaitTime + calculate_distance(x$Location,x$Destination)}))
    total_times <- total_times[-12] # We have to exclude the time for the last passenger
    waiting_time_of_12th_passenger <- p.list[[12]]$WaitTime # get waiting time of the 12th passenger
    print("ali")
    greater_than_1 <- sum(total_times <= waiting_time_of_12th_passenger)
    if(greater_than_1 == 0 ) {
      results.container[i] = 0
    } else{
      results.container[i] = 1
    }
}
prob <- sum(results.container == 1) / length(results.container)
print(paste("Probability that at least one of the 12 passengers arrives at his or
her destination before the last of the 12 passengers is first picked up is",prob))
