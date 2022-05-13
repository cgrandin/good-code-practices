bubble_sort <- function(x){
  # calculate the length of array
  n <- length(x)
  # run loop n-1 times
  for (i in 1 : (n - 1)) {
    # run loop (n-i) times
    for (j in 1 : (n - i)) {
      # compare elements
      if (x[j] > x[j + 1]) {
        temp <- x[j]
        x[j] <- x[j + 1]
        x[j + 1] <- temp
      }
    }
  }
  x
}

insertion_sort <- function(x)
{
  # calculate the length of array
  n <- length(x)
  # outer loop
  for (i in 2 : (n))
  {
    # store first element as key
    key = x[i]
    j   = i - 1
    # compare key with elements for
    # its correct position
    while (j > 0 && x[j] > key)
    {
      x[j + 1] = x[j]
      j = j - 1
    }
    # Place key at its correct position
    x[j + 1] = key
  }
  # return sorted array
  x
}

selection_sort <- function(x){
  # length of array
  n <- length(x)
  for (i in 1 : (n - 1))
  {
    # assume element at i is minimum
    min_index <- i
    for (j in (i + 1) : (n))
    {
      # check if element at j is smaller
      # than element at min_index
      if (x[j] < x[min_index]) {
        # if yes, update min_index
        min_index = j
      }
    }
    # swap element at i with element at min_index
    temp <- x[i]
    x[i] <- x[min_index]
    x[min_index] <- temp
  }
  x
}

merge <- function(a, b){
  # create temporary array
  temp <- numeric(length(a) + length(b))

  # take two variables which initially points to
  # starting of the sorted sub arrays
  # and j which points to starting of starting
  # of temporary array
  astart <- 1
  bstart <- 1
  j <- 1
  for(j in 1 : length(temp)) {
    # if a[astart] < b[bstart]
    if((astart <= length(a) &&
        a[astart] < b[bstart]) ||
       bstart > length(b)) {
      # insert a[astart] in temp and increment
      # astart pointer to next
      temp[j] <- a[astart]
      astart <- astart + 1
    }
    else {
      temp[j] <- b[bstart]
      bstart <- bstart + 1
    }
  }
  temp
}

merge_sort <- function(arr) {

  # if length of array is greater than 1,
  # then perform sorting
  if(length(arr) > 1) {

    # find mid point through which
    # array need to be divided
    mid <- ceiling(length(arr)/2)

    # first part of array will be from 1 to mid
    a <- merge_sort(arr[1:mid])

    # second part of array will be
    # from (mid+1) to length(arr)
    b <- merge_sort(arr[(mid+1):length(arr)])

    # merge above sorted arrays
    merge(a, b)
  }
  # else just return arr with single element
  else {
    arr
  }
}

quick_sort <- function(arr) {

  # Pick a number at random
  random_index <- sample(seq_along(arr), 1);
  pivot <- arr[random_index]
  arr <- arr[-random_index]

  # Create array for left and right values.
  left <- c()
  right <- c()

  # Move all smaller and equal values to the
  # left and bigger values to the right.
  # compare element with pivot
  left<-arr[which(arr <= pivot)]
  right<-arr[which(arr > pivot)]

  if (length(left) > 1)
  {
    left <- quick_sort(left)
  }
  if (length(right) > 1)
  {
    right <- quick_sort(right)
  }

  # Return the sorted values.
  return(c(left, pivot, right))
}

vec <- as.integer(floor(abs(rnorm(1000) * 100)))
vec <- abs(rnorm(1000) * 100)

microbenchmark(
  quick_sort(vec)
)

microbenchmark(
  merge_sort(vec)
)

microbenchmark(
  selection_sort(vec)
)
microbenchmark(
  insertion_sort(vec)
)
microbenchmark(
  bubble_sort(vec)
)
microbenchmark(
  sort(vec)
)
