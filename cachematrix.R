## These functions are designed to keep a cache of previously solved matrix inverses
  ## if the inverse of a matix has been solved before, that solution is stored in 
  ## the parent environment
## If the inverse matrix was solved, and is stored in the cache, that result will be
  ## returned along with a message saying that the result is from the cache


## First we initialize the NULL m Matrix - to be used in both functions.
## Then we create the set function, assign the matrix input x from the 
  ## original function input and assign m as NULL to clear the prior cache
## Next we retrieve the x value from the parent environment of makeCacheMatrix
## set_in sets the inverse matrix for m, defined in the parent environment
  ## scoping forces R to search for m within the defined function
## get_in determines where to look to get the inverse matrix
## Finally, we create a new list containing all the above set, get, set_in, 
## get_in values -- these are returned to the parent environment

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_in <- function(solve) m <<- solve
  get_in <- function() m
  list(set = set, 
       get = get,
       set_in = set_in,
       get_in = get_in)
  
}


## We set m to the get_in() value from the makeCacheMatrix fn
  ## this allows us to determine if the inverse has been previously cached
## If m is NULL, then we have already found the inverse of this matrix, 
  ## then we want to return that inverse matrix 
  ## along with the message 'getting cached data'
## If m is not nULL, or if the above if statement is FALSE, then we will 
  ## calculate the inverse of the matrix and store that in the 
  ## parent environment

cacheSolve <- function(x, ...) {
  m <- x$get_in()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
  data <- x$get()
  m <- solve(data, ...)
  x$set_in(m)
  ## Return a matrix that is the inverse of 'x'
    m
  }
