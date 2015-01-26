## Two functions to find the inverse of a matrix or get the inverse from cache

## Makes a cache of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  I <- matrix()
  set <- function(y){
    x <<- y
    I <<- matrix(data = NA)
  }
  get <- function(){
    x
  }
  setinverse <- function(inverse){
    I <- inverse
  }
  getinverse <- function(){
    I
  }
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Solves the inverse or get it from the previously computed at cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  I <- x$getinverse()
  if (!is.na(I)){
    message("Getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data)
  x$setinverse(I)
  I
}
