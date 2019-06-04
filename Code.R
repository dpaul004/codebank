data<-read.csv("Hospital.csv",header=T)

########## Part 1 ################
control<-factor(data$Control,labels = c("G_NFed","NG_NP","Profit","F_GOV"))
region<-factor(data$Geog..Region,labels = c("So","NE","MW",'SW',"RM","CA","NW"))
control_region<-table(control,region)
control_region

m_c_r<-addmargins(control_region)
m_c_r

#Black Pg 140 Prob 2
## Probability that a randomly selected hospital is in the modwest, if it is known to be Profit
m_c_r[3,3]/m_c_r[3,8]

# Prob that (if the hospital is known o be in south) the hospital is a govt. non fed hospital
m_c_r[1,1]/m_c_r[5,1]

# Prob that it is a either RM or a NP_NG hospital
(m_c_r[5,5]+m_c_r[2,8]-m_c_r[2,5])/m_c_r[5,8]

# Prob for a randomy picked hospital to be a for profit in CA
m_c_r[3,6]/m_c_r[5,8]

######### Part 2 ################
# Chapter 5 page 180 problem 2
# Provide a breakdown among the different type of hospitals.
service<-factor(data$Service,labels=c('General','Psychiatric'))
addmargins(table(service))

# prob of selecting exactly 9 Psychiatric hospitals if 16 hospitals are selected.
# c(168,7) * c(32,9)/c(200,16)
dhyper(9,32,168,16)

# Number of hospitals that are for profit
m_c_r[3,8]
#Proportion of Hospitals that are for profit
m_c_r[3,8]/m_c_r[5,8]
#Prob of picking exctly 10 for-profit hospital from a sample of 30 hospital
dbinom(10,30,prob=m_c_r[3,8]/m_c_r[5,8])

#Pg 220 Prob 3
lambda = 50/12

#prob that 30 min passes before the nex patient
pexp(.5,lambda,lower.tail=F)

#prob that 10 min passes before the nex patient
pexp(1/6,lambda,lower.tail=F)

########### Part 3 #############
# Prop. of the NG_NP Hospitals
m_c_r[2,8]/m_c_r[5,8]
#Prob that a min of 45% of the hospitals in 500 belong to this category
pbinom(5*45,500,.43,lower.tail=F)
#Prob that a max of 45% of the hospitals in 500 belong to this category
pbinom(40,100,.43,lower.tail=T)

########### Part 4 #############

p<-.5
sample_size <- numeric(0)
tail_p <- numeric(0)
for (i in (1:80)) {
  n <- 5*i
  x <- n*p + 1.644854*sqrt(p*(1-p)*n)
  sample_size[i] <- n
  tail_p[i] <- pbinom(x,n,p,lower.tail=F)
}

N_size1 <- 5/p
N_size2 <- 9*(1-p)/p
N_size3 <- 15/(p*(1-p))

print(c(N_size1,N_size2,N_size3))

lower<-0.025
upper<-0.075
plot(sample_size,tail_p,type="b",col="blue",ylim=c(0,0.125),main="Exact")
abline(h=.05)
abline(h=c(lower,upper),col="red")
abline(v=N_size1)
abline(v=N_size2)
abline(v=N_size3)







  