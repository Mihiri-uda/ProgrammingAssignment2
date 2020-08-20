## This is a R function that allows you to cache the inverse of a matrix.

## In the first function, makeCacheMarix, I created a function to create 
## a matrix and then set values of it, set it's inverse and get the values  of the inverse of the matrix.

makeCacheMarix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) (inv <<- inverse)
  getinverse <- function () inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## cacheSolve function first looks to if the inverse of the matrix x is
## already has been calculated and if so it retrieves the values of inverse of 
##the matrix from cache. if the inverse is not calculated, the function calculates
##the values of the inverse of the matrix and returns a matrix that is the inverse of matrix "x". 

cacheSolve <- function(x, ...){
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}


        
