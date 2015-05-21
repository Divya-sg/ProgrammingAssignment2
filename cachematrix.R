## Put comments here that give an overall description of what your
## functions do
## MakeCacheMatrix makes a special list of 4 component functions



## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  r <- x$getinverse()
  if(!is.null(r)) {
    message("getting cached data")
    return(r)
  }
  matrix_data <- x$get()
  r <- solve(matrix_data)
  x$setinverse(r)
  r
}
