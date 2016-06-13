# Matrix inversion is time consuming so we save time by caching it rather than
# computing it over and over. The following two functions cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
     # Initially set to NULL, but changes when the user sets the value
    inv <- NULL
    
     # the set function sets the matrix itself but not the inverse
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    
    # the get function gets the matrix itself but not the inverse
    get <- function() x
    
     # Manually set the inverse
    setinverse <- function(inverse) inv <<- inverse
    
     # Get the inverse
    getinverse <- function() inv
    
     # Encapsulate into a list
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# cacheSolve gives the inverse of the matrix. It first checks
# the inverse is already computed, gets the result and skips the
# computation. If not, it computes the inverse.

cacheSolve <- function(x, ...) {
      # Get the current state of the inverse and see if it has been computed yet
      inv <- x$getinverse()
      
      # If it has...
      if (!is.null(inv)) {
        # return the computed inverse		
        message("getting cached matrix")
        return(inv)
      }
      
      # If it hasn't... get the matrix itself
      data <- x$get()
      
      # Find the inverse
      inv <- solve(data, ...)
      
      # Cache this result in the object
      x$setinverse(inv)
      
      # Return this new result
      inv
}

