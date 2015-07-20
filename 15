# function used to build a cache location for the (inverted) matrix entered and sets the 
# cache vector with the initial value of NULL
CacheMatrix=function(re=matrix()) {
  do=NULL
  set=function(me){
    re<<-me
    do<<-NULL
  }
  get=function() re
  setinverse=function(inverse) do<<-inverse
  getinverse=function() do
  # using the <list> function to combine tags to their values
  list(set=set, get=get, setinverse=setinverse, 
       getinverse=getinverse) 
}
# Checking to see if the <do> vector contains a value other than NULL
# If NULL is TRUE then the function skips to 2nd half and computes 
# inverse of the matrix entered into the function
CacheSolve=function(re, ...){
  do=re$getinverse()
  if(!is.null(do)){
    message("getting cached matrix")
    return(do)
  }
  data=re$get()
  # <solve> is the function that inverts the matrix entered, it's part of the R{base} pkg
  do=solve(data, ...)
  re$setinverse(do)
  # prints out the value of <do>
  do
}
