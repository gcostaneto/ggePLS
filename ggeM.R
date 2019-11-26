gge.m = function(df.y,gid="ntrat", env="Trial",return=F){
  df.y = data.frame(ntrat=df.y[,gid],Trial=df.y[,env]);
  if(isFALSE(return)){
    return(t(gge(acast(df.y, ntrat~Trial, value.var="emmean"))$x))
  }
  if(!isFALSE(return)){
    return(gge(acast(df.y, ntrat~Trial, value.var="emmean")))}
  }
