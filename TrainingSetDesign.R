TrainingSetDesign = function(df,p,env,gid,boots,seed=6172){
  
  .df     = data.frame(gid=df[,gid],env=df[,env])
  .df$env = as.factor(.df$env)
  .names  = levels(.df$env)
  .N      = round(nlevels(.df$env)*p)
  .out    = list()
  set.seed(seed);
  for(K in 1:boots){.out[[K]]=which(.df$env %in% sort(sample(.names,size = .N)))
  seed = seed+10}
  return(.out)
}
