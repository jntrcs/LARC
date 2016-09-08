library(optimx)

sample <- data.frame(c(1,1),c(1,0),c(0,1),c(1,0))
names(sample) <- c("str","wins","n1","n2")

sample2 <- data.frame(c(1,1),c(2,0),c(0,2),c(2,0))
names(sample2) <- c("str","wins","n1","n2")

sample3 <- data.frame(c(1,1),c(1,1),c(0,2),c(2,0))
names(sample3) <- c("str","wins","n1","n2")

sample4 <- data.frame(c(1,1,1),c(1,1,0),c(0,1,0),c(1,0,1),c(0,1,0))
names(sample4) <- c("str","wins","n1","n2","n3")

POST.LARC <- function(str,w,n){
  exp(-str[1]-str[2])*str[1]^(w[1]+1)*str[2]^(w[2]+1)*(1/(str[1]+str[2])^n[1])
}

POST.LARC2 <- function(str,w,n1,n2){
  exp(-str[1]-str[2]-str[3])*str[1]^(w[1]+1)*str[2]^(w[2]+1)*str[3]^(w[3]+1)*
    (1/(str[1]+str[2])^n1[2])*(1/(str[1]+str[3])^n1[3])*(1/(str[2]+str[3])^n2[3])
}

POST.LARC(sample$str,sample$wins,sample$n2)
POST.LARC2(sample4$str,sample4$wins,sample4$n1,sample4$n2)

optimx(sample$str,POST.LARC,w=sample$wins,n=sample$n2,method="L-BFGS-B",lower=0,upper=Inf
      ,control=list(trace=6,fnscale=-1,maxit=99999,REPORT=1))

optimx(sample$str,POST.LARC,w=sample$wins,n=sample$n2,
      control=list(fnscale=-1,maxit=99999,REPORT=1))

.5*exp(-2)
4/3
2/3

optimx(sample2$str,POST.LARC,w=sample2$wins,n=sample2$n2,
       control=list(fnscale=-1,maxit=99999,REPORT=1))

optimx(sample3$str,POST.LARC,w=sample3$wins,n=sample3$n2,
       control=list(fnscale=-1,maxit=99999,REPORT=1))

optimx(sample4$str,POST.LARC2,w=sample4$wins,n1=sample4$n1,n2=sample4$n2,
       control=list(fnscale=-1,maxit=99999,REPORT=1))
