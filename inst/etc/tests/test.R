library(ggplot2)
library(FLCore)

data(ple4)
t.<-t(apply(I(m(ple4))@.Data,1,function(x) {x[is.na(x)]<-0; cumsum(x)}))
t.[is.na(I(m(ple4)))]<-NA
t.<-FLQuant(t.,dimnames=dimnames(I(m(ple4))))

