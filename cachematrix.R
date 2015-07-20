##The 2 functions work together to take a matrix, find its inverse and caches the result in the special matrix object
## that was created. 

##It takes a matrix object and encapsulates it and adds additional functions to allow access to it 

makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL
    
    #changes the value of x to y
    set <- function (y){
        x <<- y
        inv <<- NULL
    }
    
    get <- function () x
    setinv <- function (inverse) inv <<- inverse
    getinv <- function () inv
    
    list (set = set, get = get, setinv = setinv, getinv = getinv) #ensures that the object has access to the functions

}


## Takes a special matrix object and optional arguments and calculates the inverse matrix 

cacheSolve <- function(x, ...) {
  
    ## First gets the cached inverse of the matrix
    inv <- x$getinv()
    
    #then ensures that the inverse exists. 
    #If it doesn't it calculates and caches it. If it does, it returns the value.
  
    if (is.null(inv))
    {
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setinv(inv)
    }
    else message("getting cached inversed matrix")
    
    inv
}
