## Put comments here that give an overall description of what your
## functions do

## makeMatrixCache creates a special "matrix"-object,
## which is really a list containing a function to

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  } ## 1. set the values of the matrix
  get<-function() x ## 2. get the values of the matrix
  setmatrix<-function(solve) m<<-solve ## 3. set the values of the inversed matrix
  getmatrix<-function() m ## 4. get the values of the inversed matrix
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## The cacheSolve function first gets the matrix, checks if it is not NULL and returns the inverse of 'x'

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}