library(latex2exp)
y <- c(1.64, 1.70 , 1.72 , 1.74 , 1.82 , 1.82 , 1.82 , 1.90 , 2.08)
mu0<-1.9
t0<-0.95
v0<-1
sig02<-0.01
n<-9
N.grid<-100
theta.grid<-seq(from=1.505,to=2,length.out=N.grid)
sig2.grid<-seq(from=1.75,to=175,length.out = N.grid)
posterior.grid<-matrix(0,nrow=100,ncol=100)
for (i in 1:N.grid){
  for (j in 1:N.grid){
    th<-theta.grid[i]
    sig<-sqrt(sig.grid[j])
    posterior.grid[i,j]<-dnorm(th,mu0,t0)*dgamma(sig^2,v0/2,v0*sig02/2)*
      prod(apply(as.matrix(y),1,function(x){dnorm(x,th,1/sig)}))
  }
}
posterior.grid<-posterior.grid/sum(posterior.grid)
image(theta.grid,sig2.grid,posterior.grid,xlab=TeX('$\\theta$'),ylab=TeX('$\\tilde{\\sigma^2}$'))
