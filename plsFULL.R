PLS.full = function(X,return=F,type = F,ncomps=7,
                    centering = "tester", scale=F,
                    scaling = "none",
                    SVP = "column"){
  Y = gge_cov(df.y = X,return=return,type=type,
              scaling = scaling,SVP = SVP,scale=scale,
              centering = centering)
  W = Y[[2]]
  GGE = Y[[1]]
  model = pls.GE(df.cov = W,df.y = GGE,scale = scale,ncomps = ncomps)
  return(model)
}
