gge_cov = function(df.y,ids = c(1:51),
                   return=F,type = F, y = F,
                   centering = "tester", 
                   scaling = "none",scale=F,
                   SVP = "column"){
  
  if(isFALSE(y)){ygge = gge.m(df.y,return=return,type=type,
                             scaling = scaling,SVP = SVP,
                             centering = centering)} 
  if(isFALSE(scale)){
    w = data.frame(df.y[,1],
                   scale(unique(df.y[,ids])[,-1],center = T,scale = T))
  }
  if(!isFALSE(scale)){
    w = data.frame(df.y[,ids])}
  
  w = w[match(rownames(ygge),w[,1]),]
  
  return(list(ygge=ygge,w=w))
}
