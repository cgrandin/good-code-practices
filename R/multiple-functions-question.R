# Made-up example regarding arguments in multiple functions.

# Plot the values of a vector with a choice of colour
func_1 <- function(vec,
                   my_col = "red",
                   ...){
  plot(vec,
       col = my_col,
       ...)
}

# Call func_1 but with a new choice of colour, and changes the line
#  type if the vector is longer than 5 elements.
func_2 <- function(vec,
                   prefer_col = "blue",
                   my_type = "p",
                   ...){
  if(length(vec) > 5){
    my_type = "o"
  }

  func_1(vec,
         my_col = prefer_col,
         type = my_type,
         ...)
}

# Code that runs and calls functions
vec <- 1:10
par(mfrow = c(2,2))
func_1(vec)
func_2(vec)
func_2(vec,
       prefer_col = "green")
func_2(1:4,
       main = "This shows that ... are working")

# Q1: I've used `vec` in the functions, but also in the main code. What's the best
#  way to name such an object - is it okay to repeat it like here?
# Q2: The `my_col` and `prefer_col` arguments both refer to colour. Any advice
#  on whether you should use the same names.



# Ignore this:

## # Code that runs (normally in separate file)

## plot_pbsEDM_Evec <- function(E_res,
##                              ...){
##   # The data are the same for each value of E, and so use one for the first five panels:
##   plot.pbsEDM(E_res[[1]],
##               ...)

##   plot_pred_obs(E_res,
##                 ...)
##   invisible()
## }
##  # What to do when want to change some of the args in the ... in an intermediate
##                 function. Okay to add fake unused ones in one later function
##                 (that are used in another), to allow the other function to
##                 use. e.g. pbsEDM::plot_pred_obs(portrait) is a dummy argument.
##     Or have 'type' used throughout, use something like 'type_val' within a function?
