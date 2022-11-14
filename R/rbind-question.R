# Question from Catarina:
#  I wanted to extract and rbind all the “um” data frames in this nested list.

library(parallel)

result<-mclapply(mc.cores=ncores,
  X=1:5,
  FUN=function(a){
    out<-mclapply(mc.cores=ncores,
      X=1:20,
      FUN=function(u){
        um<-data.frame(oa=paste0("o",1:a),a=a)
        dois<-data.frame(dois=paste0("i",u))
        return(structure(list(um=um,dois=dois)))
    })
    return(list(out))
})
