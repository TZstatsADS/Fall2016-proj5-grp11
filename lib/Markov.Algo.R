Prob.Match.Win<-function(pa,pb){
Win<-function(pa,pb){
qa = 1 - pa
qb = 1- pb
pag = pa^4*(1+4*qa+10*(qa)^2) + 20*(pa*qa)^3*pa^2*(1-2*pa*qa)^(-1)
qag = 1-pag
pbg = pb^4*(1+4*qb+10*(qb)^2) + 20*(pb*qb)^3*pb^2*(1-2*pb*qb)^(-1)
qbg = 1-pbg
p<-matrix(0,8,8)
p[1,1] = 1
for(i1 in 1:8){
  for(j1 in 1:8){
    if(i1*j1==1) next
    i = i1-1
    j = j1-1
    k = i+j-1
    if(k%%4 %in%c(3,0)){#1
      if(i ==0) p[i1,j1] = p[i1,(j1-1)]*qa
      if(j == 0) p[i1,j1] = p[(i1-1),j1]*pa
      if(i*j!=0) p[i1,j1] = p[(i1-1),j1]*pa + p[i1,(j1-1)]*qa
      if(i==7 & j <=6) p[i1,j1] = p[(i1-1),j1]*pa
      if(j==7 & i<=6) p[i1,j1] = p[i1,(j1-1)]*qa
    }#1
    if(k%%4 %in%c(1,2)){#1
      if(i ==0) p[i1,j1] = p[i1,(j1-1)]*pb
      if(j == 0) p[i1,j1] = p[(i1-1),j1]*qb
      if(i*j!=0) p[i1,j1] = p[(i1-1),j1]*qb + p[i1,(j1-1)]*pb
      if(i==7 & j <=6) p[i1,j1] = p[(i1-1),j1]*qb
      if(j==7 & i<=6) p[i1,j1] = p[i1,(j1-1)]*pb
    }#1
  }
}

Pat = sum(p[8,1:6]) + p[7,7]*pa*qb*(1-pa*pb-qa*qb)^-1


S = matrix(0,8,8)
S[1,1] =1 
for(i1 in 1:7){
  for(j1 in 1:7){
    if((i1*j1)==1) next
    i = i1-1
    j = j1-1
    k = i-1+j
    if(k%%2 == 0){
      if(i==0) S[i1,j1] = S[i1,(j1-1)]*qag
      if(j==0) S[i1,j1] = S[(i1-1),j1]*pag
      if(i*j!=0){
        S[i1,j1] = S[i1,(j1-1)]*qag + S[(i1-1),j1]*pag
      }
      if(j==6 & i<=5) S[i1,j1] = S[i1,(j1-1)]*qag
      if(i==6 & j<=5) S[i1,j1] = S[(i1-1),j1]*pag
    }
    if(k%%2 == 1){
      if(i==0) S[i1,j1] = S[i1,(j1-1)]*pbg
      if(j==0) S[i1,j1] = S[(i1-1),j1]*qbg
      if(i*j!=0){
        S[i1,j1] = S[i1,(j1-1)]*pbg + S[(i1-1),j1]*qbg
      }
      if(j==6 & i<=5) S[i1,j1] = S[i1,(j1-1)]*pbg
      if(i==6 & j<=5) S[i1,j1] = S[(i1-1),j1]*qbg
    }
  }
}
S[8,6] = S[7,6]*qbg
S[6,8] = S[6,7]*pbg
Pas = sum(S[7,1:5]) + S[8,6] + S[7,7]*Pat
return(Pas)
}
Pas = Win(pa,pb)
Pbs = Win(pb,pa)
Pam = Pas^3 + 3*Pas^3*Pbs + 6*Pas^3*Pbs^2
return(Pam)
}


# K = NULL
# i = 1
# for(i in 1:30000){
#   print(i)
#   pa = rtruncnorm(1,a=0,b=1,mean=0.67,sd=0.02)
#   pb = rtruncnorm(1,a=0,b=1,mean=0.7,sd=0.02)
#   K[i] = Prob.Match.Win(pa,pb)
# }

