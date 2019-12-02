## This is a pair of funct;ons that cache the inverse of a matrix which helps
## The input matrix is assumed to be always invertible

## The makeCacheMatrix function is used to create a special "Matrix" object that can cache its inverse matrix. 


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<-NULL 
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function will generate/computes the inverse of the matrix returned by makeCacheMatrix above.
## It the inverse of the matrix has already been calculated (Matrix not changed) , 
##then the cachesolve will retrieve the inverse from the cache else it will compute the inverse of the cache


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  input <- x$get()
  inv <- solve(input, ...)
  x$setInv(inv)
  inv
  
}

