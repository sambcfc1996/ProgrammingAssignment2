## The two functions below find the inverse of an invertible matrix,
## if the inverse of the matrix has already been calculated it retrieves this 
## value from the parent environment. The functions must be used together as the 
## input to the cachesolve function must also be an input to the makeCacheMatrix
## function.  

## The first function creates an object that stores a matrix and its calculated
## inverse. Four functions are created, within this function and they are stored
## as varibles in a list. 

makeCacheMatrix <- function(x = matrix()) {  ## defines initial function and sets the variable s 
    s <- NULL 
    create <- function(y) { ## Gives the x input the value y in the parent environment
    x <<- y                 ## and the s varible the value NULL, to effectively clear the environment 
    s <<- NULL              ## and stop the wrong value being returned. 
    }
    find <- function() x ## using scoping rules returns x from parent environment 
    setinverse <- function(solve) s <<- solve ## sets s equal to solve 
    getinverse <- function() s
    list(create = create, find = find, setinverse = setinverse, getinverse = getinverse)
} ## stores each function in a list and returns it to the parent environment. 


## The second function checks the parent environment for the inverse of the matrix
## and returns it, if it is found. If the inverse is not found the function 
## calcualtes it. 

cacheSolve <- function(x, ...) {
     s <- x$getinverse() ## check parent environment for stored value of inverse
     if(!is.null(s)) {
          message("retrieving cached data")
          return(s) ## if value found print it 
     }
     data <- x$find()
     s <- solve(data, ...) ## if value is not found in parent environment calculate it
     x$setinverse(s)
     s
}
        ## Return a matrix that is the inverse of 'x'


