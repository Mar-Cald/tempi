RDM_pdf = function(x,drift_pdf,threshold){
  pdf = (threshold ) / sqrt(2*pi*(x^3)) * exp(-0.5 * ((drift_pdf*(x) - threshold)^2) / (x))
  return(pdf)}

probs_1 =  RDM_pdf(seq(0.1,3,0.01),.5,1)
probs_2=  RDM_pdf(seq(0.1,3,0.01),1,1)
probs_3 =  RDM_pdf(seq(0.1,3,0.01),1.5,1)
probs_4=  RDM_pdf(seq(0.1,3,0.01),2,1)
probs_5 =  RDM_pdf(seq(0.1,3,0.01),2.5,1)
probs_6=  RDM_pdf(seq(0.1,3,0.01),3,1)

P1 = sample(x = seq(0.1,3,0.01), 1000,probs_1,replace = TRUE)
P2 = sample(x = seq(0.1,3,0.01), 1000,probs_2,replace = TRUE)

P3 = sample(x = seq(0.1,3,0.01), 1000,probs_3,replace = TRUE)
P4 = sample(x = seq(0.1,3,0.01), 1000,probs_4,replace = TRUE)


P5 = sample(x = seq(0.1,3,0.01), 1000,probs_5,replace = TRUE)
P6 = sample(x = seq(0.1,3,0.01), 1000,probs_6,replace = TRUE)


meanrt_rdm = c(mean(P1),mean(P2),mean(P3),mean(P4),mean(P5),mean(P6))
sdrt_rdm = c(sd(P1),sd(P2),sd(P3),sd(P4),sd(P5),sd(P6))
cor(meanrt_rdm,sdrt_rdm)


hist(P1)
hist(P2)
median(P1);sd(P1)    
median(P2);sd(P2)  
cor()