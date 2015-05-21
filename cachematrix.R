## A pair of functions that cache the inverse of a matrix.
## 1) MakeCacheMatrix
## 2) cacheSolve

## MakeCacheMatrix makes a special list of 4 component functions
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the matrix inverse
## 4.get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  r<- NULL
  set<- function(y)
  {
    x<<-y
    r <<- NULL
  }
  get<- function() x
  setinverse<- function(invm) 
  {
    r<<-invm
  }
  getinverse<- function () 
  {
    r
  }
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}
## cacheSolve returns the matrix inverse from the special list created by MakeCacheMatrix
##  If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  r <- x$getinverse()
  if(!is.null(r)) {
    message("getting cached inverse")
    return(r)
  }
  ## else the inverse is computed and set in the cache
  matrix_data <- x$get()
  r <- solve(matrix_data)
  x$setinverse(r)
  r
}
