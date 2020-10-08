## The following functions are written to cache the inverse of a square invertible matrix.
##Therefore, two functions are needed:
 ##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
 ##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
 

##makeCacheMatrix: This function creates an R object that stores a matrix and its inverse. 
makeCacheMatrix <- function(x = matrix()) {
    i = NULL   ## Initializing Objects: x is an empty matrix, i is an object that represents the inverse
    ## defining the behaviours of the functions set, get, setinverse, get inverse:
    ## set is used to assign the input argument to the parent environment and to clear any value of i that has been cached 
    set <- function(y){    
      x <<- y
      i <<- NULL
    }
    get <- function() x  ## getter for the matrix x
    setinverse <- function(inverse) i <<- inverse ## it assigns the input argument to the value of i in the parent environment
    getinverse <- function() i ## getter for the matrix i 
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    ##it returns a fully formed object of type makeCacheMatrix; naming the list elements allows to use $ to access function names
}


## cacheSolve is used to calculate the inverse of the matrix and is therefore necessary to complete the makeCacheMatrix function.
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() ## attemps to retrive an inverse from the object passed in as the argument
  if(!is.null(i)){ ## checks if the result is null 
    message("getting cached data") ## if the value is not null, there is a valid, cached inverse 
    return(i) ##which can be returned into the parent environment
  }
  data <- x$get() ##otherwise it gets the matrix from the input 
  i <- solve(data, ...) ##and calculates the inverse
  x$setinverse(i) ## and then sets the inverse in the input object
  i ## and returns the inverse to the parent environment by printing i 
}

