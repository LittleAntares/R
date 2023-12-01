
X = scan(file="assignment4_x_2021.txt", what=double())
Y = scan(file="assignment4_y_2021.txt", what=double())

length(X)
mean.x = mean(X)

length(Y)
mean.y = mean(Y)

sum.x = sum(X)
sum.y = sum(Y)


###################################################################################
log.posterier <- function(mu1,mu2){
  n.prob <- -0.5*(103*mu1^2+4*mu2*mu1-2*sum.x*mu1+(82/2)*mu2^2-0.5*sum.y*mu2)
  return(n.prob)
}


MH_Algo <- function(mu.1,mu.2,num.it){
  Chain.sample = matrix(nrow = num.it+2,ncol = 2)
  Chain.sample[1,] = c(mu.1,mu.2) 
  accepted <- 0
  sd.cur <- 0.1
  for (i in 1:num.it){
    p.mu1 <- rnorm(1, mean = Chain.sample[i,1],sd=sd.cur)
    p.mu2 <- rnorm(1, mean = Chain.sample[i,2],sd=sd.cur)
    prob <- exp(log.posterier(p.mu1,p.mu2)-log.posterier(Chain.sample[i,1],Chain.sample[i,2]))
    if (runif(1) < prob) {
      Chain.sample[i+1,] = c(p.mu1,p.mu2)
      accepted <- accepted+1
    } else{
      Chain.sample[i+1,] <- Chain.sample[i,]
    }
  }
  Chain.sample[(num.it+2),]= accepted/num.it
  return(Chain.sample)
}  


num.it = 2000   

Sample.1 = MH_Algo(0,0,num.it)
MH_A1 <- Sample.1[1:(num.it+1),]
Accepted_Rate1 <- Sample.1[(num.it+2),1]
Accepted_Rate1

Sample.2 = MH_Algo(2,-1,num.it)
MH_A2 = Sample.2[1:(num.it+1),]
Accepted_Rate2 <- Sample.2[(num.it+2),1]
Accepted_Rate2


burnIn = 1000
Combined = c(MH_A1[-(1:burnIn),2],MH_A2[-(1:burnIn),2])
B = sd(Combined)
W = mean(sd(MH_A1[-(1:burnIn),2]),sd(MH_A2[-(1:burnIn),2]))
B/W




par(mfrow=c(3,1), mar=c(4,4,1,1)) 
plot(1:500, MH_A1[,1], type="l", col="red", 
     ylim = c(0,max(MH_A1[,1],MH_A2[,1])), xlab = "iteration", ylab ="mu.1")
points(1:500, MH_A2[,1], type="l", col="blue")

par(mfrow=c(3,1), mar=c(4,4,1,1)) 
plot(1:500, MH_A1[,2], type="l", col="red",
     ylim = c(min(MH_A1[,2],MH_A2[,2]),0), xlab = "iteration", ylab ="mu.2")
points(1:500, MH_A2[,2], type="l", col="blue")













###############################################################################

CAVI_A <- function(X,Y,mu10,mu20,num.it=100, epsilon=1e-5){
  
  #Redefine all the initial value and Sample used
  mu1.vi = mu10
  mu2.vi = mu20
  sigma1.star2 = 1/103
  sigma2.star2 = 1/78
  
  #Storing ELBO, mu1 and mu2 value for each iteration
  elbo = c()
  mu1.list = c()
  mu2.list = c()
  
  #Computing ELBO Value
  Elogq.mu1 = -log(sigma1.star2)/2
  Elogq.mu2 = -log(sigma2.star2)/2
  Emu1.2 = sigma1.star2+mu1.vi^2
  Emu2.2 = sigma2.star2+mu2.vi^2
  #Let A=sum(E(xi-mu1)^2) and B=sum(E(yj-mu2)^2) and define below
  A=sum(Emu1.2-2*X*mu1.vi+X*X)
  B=sum(Emu2.2-2*Y*mu2.vi+Y*Y)
  
  Elogp.mu1.mu2 = -0.5*(3*Emu1.2+4*mu1.vi*mu2.vi+3*Emu2.2+A+0.5*B)
  
  #Storing ELBO, mu1, mu2 value
  elbo= c(elbo ,Elogp.mu1.mu2-Elogq.mu1-Elogq.mu2)
  mu1.list = c(mu1.list, mu1.vi)
  mu2.list= c(mu2.list,mu2.vi)
  
  #Set initial iteration value to 1
  curr.int = 1
  
  #Set change in the ELBO as 1
  delta.elbo = 1
  
  #Using a loop where if change ELBO is less than epsilon or the max number of iteration reach it will stop CAVI algorithm
  while ((delta.elbo > epsilon) & (curr.int <= num.it)){
    
    #Update mu1 and mu2
    mu1.vi = (1/103)*(sum(X)-2*mu2.vi)
    mu2.vi = (1/156)*(sum(Y)-4*mu1.vi)
    
    
    
    #Computing ELBO using the new mu1 and mu2
    Emu1.2 = sigma1.star2+mu1.vi^2
    Emu2.2 = sigma2.star2+mu2.vi^2
    
    #Let A=sum(E(xi-mu1)^2) and B=sum(E(yj-mu2)^2) and define below
    A=sum(Emu1.2-2*X*mu1.vi+X*X)
    B=sum(Emu2.2-2*Y*mu2.vi+Y*Y)
    Elogp.mu1.mu2 = -0.5*(3*Emu1.2+4*mu1.vi*mu2.vi+3*Emu2.2+A+0.5*B)
    
    #Storing the new value
    elbo= c(elbo,Elogp.mu1.mu2-Elogq.mu1-Elogq.mu2)
    mu1.list = c(mu1.list, mu1.vi)
    mu2.list= c(mu2.list,mu2.vi)
    
    #Computing Change in ELBO
    delta.elbo = elbo[length(elbo)]-elbo[length(elbo)-1]
    
    #increase number of iteration
    curr.int = curr.int+1
  }
  return(list(elbo=elbo,mu1.list=mu1.list,mu2.list=mu2.list))
}


Trial1 = CAVI_A(X,Y,0,0)
Trial2 = CAVI_A(X,Y,100,6)
Trial2$elbo

Trial1$elbo
Trial1$mu1.list
Trial1$mu2.list







