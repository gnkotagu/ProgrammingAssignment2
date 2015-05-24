## This workspace contains two functions (one that caches and the other computes the inverse of a matrix)

## The first function creates a matrix and caches its inverse

makeCacheMatrix <- function(thematrix = matrix())
{
  inverse <- NULL
  set <- function(x)
  {
    thematrix <<- x;
    inverse <<- NULL;
  }
  get <- function() thematrix
  setinv <- function(inv) inverse <<- inv
  getinv <- function() inverse
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The following function computes the inverse of the special matrix.
## If the inverse has already been calculated,'cacheSolve' retrieves the inverse from the cache.

cacheSolve <- function(thematrix, ...) 
{
  inverse <- thematrix$getinv()
  if(!is.null(inverse)) 
  {
    message("Here is the cached data:")
    return(inverse)
  }
  data <- thematrix$get()
  inverse <- solve(data)
  thematrix$setinv(inverse)
  inverse
}