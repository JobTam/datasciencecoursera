## The r functions below cache potentially time-consuming computations.
## Specifically, they cache the value of the mean. 

## The first function, makeCacheMatrix creates a special
## "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
}

get<-function() x
setmatrix<-function(solve) m<<- solve
getmatrix<-function() m
list(set=set, get=get,
     setmatrix=setmatrix,
     getmatrix=getmatrix)
}

## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data") ## Confirmation that data is obtained from cache
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}

## below is a sample run of the code
x = rbind(c(1, -1/4), c(-1/4, 1))
m = makeCacheMatrix(x)
m$get()

## runs below confirm cache code retrieval
cacheSolve(m)
cacheSolve(m) ## second run should be from cache

