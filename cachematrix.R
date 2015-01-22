## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that solves the inverse of a matrix and saves it as a list variable.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set <- function(y){
    x<<-y
    m<<-NULL
    
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get, 
       setmatrix = setmatrix,
       getmatrix = getmatrix)
  
}

## CacheSolve first checks if the matrix has been entered before and if yes, loads the inverse matrix from the one in cache
##otherwise, calculates inverse matrix

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){message("loading cached data")
                  return(m)
  }
  matrix<-x$get()
  m<-solve(matrix,...)
  x$setmatrix(m)
  m
  ## Return a matrix that is the inverse of 'x'
}