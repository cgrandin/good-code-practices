# Question from Catarina:
#  I wanted to extract and rbind all the “um” data frames in this nested list.
# First, improve formatting of code.
#  in Emacs)
library(parallel)

result <- mclapply(mc.cores = 1,
                   X = 1:5,
                   FUN = function(a){
                     out <- mclapply(mc.cores = 1,
                                     X = 1:20,
                                     FUN = function(u){
                                       um <- data.frame(oa = paste0("o", 1:a),
                                                        a = a)
                                       dois <- data.frame(dois = paste0("i", u))
                                       structure(list(um = um,
                                                      dois = dois))
                                     })
                     list(out)
                   })

# Same code simplified, using purrr::map()
result_map <- map(1:5,
                  function(a){
                    out <- map(1:20,
                               function(u){
                                 um <- data.frame(oa = paste0("o", 1:a),
                                                  a = a)
                                 dois <- data.frame(dois = paste0("i", u))
                                 structure(list(um = um,
                                                dois = dois))
                               })
                    list(out)
                  })
