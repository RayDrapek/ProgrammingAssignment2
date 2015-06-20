## This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
      }
      get <- function() x
      setinverse <- function(newinv) inv <<- newinv
      getinverse <- function() inv
      list(set=set, get=get, 
           setinverse=setinverse,
           getinverse=getinverse)
}


## This function first checks to see if the inverse matrix
## is in the cache. If it is then it returns the cached 
## value. If it is not then it solves for the inverse
## and returns the solution.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv<-x$getinverse()
     if(!is.null(inv)) {
          message("getting cahced data")
          return(inv)
     }
     data<-x$get()
     inv<-solve(data)
     x$setinverse(inv)
     inv
}
