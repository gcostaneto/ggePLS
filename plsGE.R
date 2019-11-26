pls.GE = function(df.cov, df.y,ncomps=5,scale=F){
  
  require(plsdepot)
  # df.cov = df.cov[match(rownames(df.y),df.cov[,"env.id"]),]
  if(isFALSE(scale)){
    pls = plsreg2(predictors = df.cov[,-1],responses = df.y,crosval = F,comps=ncomps)
  }
  if(!isFALSE(scale)){
    pls = plsreg2(predictors = scale(df.cov[,-1],center = T,scale = T),
                  responses = df.y,crosval = F,comps=ncomps)
  }
  
  
  return(list(expvar = pls$expvar, # [[1]]
              x.loads=pls$x.loads, # [[2]]
              mod.wgs=pls$mod.wgs, # [[3]]
              cor.xt=pls$cor.xt,   # [[4]]
              cor.yt=pls$cor.myt,  # [[5]]
              reg.coefs = melt(pls$reg.coefs), # [[6]]
              std.coefs = melt(pls$std.coefs), # [[7]]
              y.pred= melt(pls$y.pred),# [[8]]
              pls,# [[9]]
              df.y)) # [[10]]
}
