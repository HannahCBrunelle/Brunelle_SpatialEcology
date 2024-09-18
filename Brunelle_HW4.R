#Spatial Ecology - Homework #4 
#Date: September 24, 2024
#Hannah C Brunelle
#Contact: hannahb7@umbc.edu
#Resources used: ChatGPT, GitHub Copilot, 
#Class activity on Sept. 19, 2024 

#Install packages
install.packages("spatstat")

#Read in needed packages 
library(spatstat)
########Number 1########
#1 - Complete Spatial Randomness (CRS)
#The Poisson distribution is a discrete probability distribution that expresses 
#the probability of a given number of events occurring in a fixed interval of time or space. 
#The Poisson distribution is characterized by a single parameter, λ (lambda), 
#which is the average number of events in the interval.

#Generate a Poisson distribution with lambda = 2
set.seed(123)
poisson_data <- rpois(1000, lambda = 5)

#Check the first few values
head(poisson_data)

#Complete spatial randomness (CSR) or homogeneous Poisson process
set.seed(698) 
dims <- 100 # window size
pts <- 500 # number of points
(lambda <- pts/dims/dims)

###Generate a point pattern with csr using the rpoispp function
csr <- rpoispp(lambda, 
               win=owin(xrange=c(0,dims), yrange=c(0,dims)),
               nsim=1) 

#Examining the csr object
summary(csr)
summary(csr)$intensity

#Check the class of csr
class(csr)  #returns "ppp"

# Plot the csr point pattern
plot(csr, pch = 20) #title will be crs

###Generate a clustered point pattern - CSR has to be done before this
set.seed(978)
# kappa = intensity of cluster centers
thom <- rThomas(kappa=30, 
                scale=0.02, 
                mu=10)

#Check the class of thom
class(thom)  #returns "ppp"

# plot the Thomas process
plot(thom, pch=20,  main="Clustered Point Pattern")

###Generate a segregated point pattern 
#Create an observation window
win <- owin(xrange = c(0, dims), yrange = c(0, dims))

#Define the Strauss process model
strauss_model <- Strauss(r = 10)  # r = interaction range (distance of repulsion)

#Create the parameters for the point process model, using the same parameters as the csr model
segregated_model <- rmhmodel(cif = "strauss", 
                             par = list(beta = lambda, gamma = 0.5, r = 10),  # Use 'r' for interaction range
                             w = win)

#Simulating the segregated point pattern using rmh() 
set.seed(123)
segregated <- rmh(segregated_model)

#Check the class of segregated
class(segregated)  #returns "ppp"

#Plotting the segregated point pattern
plot(segregated, pch = 20, main = "Segregated Point Pattern (Strauss Process)")

###Answer to "For the Point-pattern CSR: what is the value of lambda and what is the definition of this parameter?" 
#The value of lamba is 0.05. As described in class, Lambda is the average number of events within an interval. Lambda is also
#the only parameter in the Poisson distribution.

##########Number 2##########
### G-, K-, and F- tests

##Run a G test - measures the distribution of distances between pairs of points
#CSR Point Pattern
# Compute raw G-function (no correction)
g_csr <- Gest(csr, correction="none")
plot(g_csr, main="G-Function for CSR Point Pattern")
# Perform Monte Carlo simulation for CSR
g_csr_mc <- envelope(csr, Gest, correction="none", nsim=99)
plot(g_csr_mc, main="G-Function Envelope for CSR Point Pattern")

# Clustered Point Pattern
g_thom <- Gest(thom, correction="none")
plot(g_thom, main="G-Function for Clustered Point Pattern")
# Perform Monte Carlo simulation for clustered pattern
g_thom_mc <- envelope(thom, Gest,correction="none", nsim=99)
plot(g_thom_mc, main="G-Function Envelope for Clustered Point Pattern")

# Segregated Point Pattern
g_segregated <- Gest(segregated, correction="none")
plot(g_segregated, main="G-Function for Segregated Point Pattern")
# Perform Monte Carlo simulation for segregated pattern
g_segregated_mc <- envelope(segregated, Gest,correction="none", nsim=99)
plot(g_segregated_mc, main="G-Function Envelope for Segregated Point Pattern")

##Run a K test - is a generalization of the G-function that accounts for edge effects.
# CSR Point Pattern
k_csr <- Kest(csr, correction="none")
plot(k_csr, main="K-Function for CSR Point Pattern")
# Perform Monte Carlo simulation for CSR
k_csr_mc <- envelope(csr, Kest,correction="none", nsim=99)
plot(k_csr_mc, main="K-Function Envelope for CSR Point Pattern")

# Clustered Point Pattern
k_thom <- Kest(thom, correction="none")
plot(k_thom, main="K-Function for Clustered Point Pattern")
# Perform Monte Carlo simulation for clustered pattern
k_thom_mc <- envelope(thom, Kest,correction="none", nsim=99)
plot(k_thom_mc, main="K-Function Envelope for Clustered Point Pattern")

# Segregated Point Pattern
k_segregated <- Kest(segregated, correction="none")
plot(k_segregated, main="K-Function for Segregated Point Pattern")
# Perform Monte Carlo simulation for segregated pattern
k_segregated_mc <- envelope(segregated, correction="none", Kest, nsim=99)
plot(k_segregated_mc, main="K-Function Envelope for Segregated Point Pattern")

##Run a F test - measures the distribution of distances from each point to its nearest neighbor
# CSR Point Pattern
f_csr <- Fest(csr, correction="none")
plot(f_csr, main="F-Function for CSR Point Pattern")
# Perform Monte Carlo simulation for CSR
f_csr_mc <- envelope(csr, Fest, correction="none", nsim=99)
plot(f_csr_mc, main="F-Function Envelope for CSR Point Pattern")

# Clustered Point Pattern
f_thom <- Fest(thom, correction="none")
plot(f_thom, main="F-Function for Clustered Point Pattern")
# Perform Monte Carlo simulation for clustered pattern
f_thom_mc <- envelope(thom, Fest,correction="none", nsim=99)
plot(f_thom_mc, main="F-Function Envelope for Clustered Point Pattern")

# Segregated Point Pattern
f_segregated <- Fest(segregated, correction="none")
plot(f_segregated, main="F-Function for Segregated Point Pattern")
# Perform Monte Carlo simulation for segregated pattern
f_segregated_mc <- envelope(segregated, Fest,correction="none", nsim=99)
plot(f_segregated_mc, main="F-Function Envelope for Segregated Point Pattern")

###Interpretations of the G-, K-, and F- tests are in a seperate document 

##########Number 3##########
#Read in the data
library(readr)
stems2014 <- read_csv("stems2014.csv")
View(stems2014)

#Check the data
head(stems2014)
class(stems2014)
View(stems2014)
str(stems2014)
names(stems2014)

