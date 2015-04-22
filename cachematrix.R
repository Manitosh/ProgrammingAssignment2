## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
   
    l_inv <- NULL
    set <- function(y) {
        x <<- y
        # Initilally set the l_inv to null to check that inverse hasn not run yet
        l_inv <<- NULL
    }
    
    # Return the matrix x
    get <- function() x
   
    # set the value of l_inv via setinverse function
    setinverse <- function(inverse) l_inv <<- inverse
   
    #Get the value of l_inv using getinverse function
    getinverse <- function() l_inv

    # Last but not least create list for all 4 functions
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



# The following function returns the inverse of the matrix and completes the following steps
# 1. Checks if the inverse has already been computed. 
# 2. If computed then gets the result and does not compute again
# 3. If computaion hasnot been completed yet then it computes the inverse
# 4. it sets value in the cache by calling setinverse function.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

