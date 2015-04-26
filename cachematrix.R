## The following functions act in unison and take advantage of lexical scoping
## to cache the inverse of a matrix. This can be used in looping processes
## such tht the inverse of a large matrix does not need to be repeatedly
## calculated

makeCacheMatrix <- function(x = matrix()) {
  ## This function creates an environment object "cachedInverseM"
  ## and returns a list of functions X which take a matrix as a parameter
  ## The functions are fairly simple get and set methods for the 
  ## base matrix object x, and functions to set and get the given
  ## matrix's inverse
  
  cachedInverseM <- NULL

  ## Asssuming new matrix on Set(), set current object in memory to null 
  ## and assign new object 
  set <- function(y) {
    x <<- y
    cachedInverseM <<- NULL
  }
  
  ## Return original matrix from environment variable
  get <- function() x

  ## usE solve() function to return the inverse
  ## in the process set "cachedInverseM" to result, effectively caching
  ## into the "cachedInverseM" object for later reuse
  setInverse <- function(solve) cachedInverseM <<- solve
  
  ## Return value stored to cachedInverseM, assumes the set Inverse() has
  ## been called after the orginal matrix has been set
  ## otherwise, cachedInverseM will still be null
  getInverse <- function() cachedInverseM
  
  ## Set up the list of functions
  ## Will be accessed by the resultant list object as <object>$<function>()
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

cacheSolve <- function(x, ...) {
  ## This function takes a list of functions on a matrix 
  ## and either casches and returns an inverse, or retrieves the inverse
  ## from the enviroment object "cachedInverseM"
  
  
  ## calls the getInverse function within the list then checks
  cachedInverseM <- x$getInverse()
  ## the environment to see if resulatnt object "cachedInverseM" is not null
  ## if it is not null, simply return the object
  if(!is.null(cachedInverseM)) {
    message("getting cached data")
    return(cachedInverseM)
  }
  ## if it is null, call the solve function to store the inverse into
  ## the cachedInverseM object
  newM <- x$get()
  cachedInverseM <- solve(newM)
  x$setInverse(cachedInverseM)
  cachedInverseM
}
