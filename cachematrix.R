## Put comments here that give an overall description of what your
## functions do

## This makeCacheMatrix takes the inverse (solve()) of the supplied matrix and stores it in cache
## cacheSolve does the same thing, only checks whether the inverse is stored in cache before performing 
## the operation.  If the solution is stored, it retrieves the stored solution and returns it rather 
## recalculating 
makeCacheMatrix <- function(x = matrix()) {
        invmtrx <- NULL ##create blank solution
        set <- function(y) {
                x<<-y
                invmtrx <<-NULL
                
        }
        get <- function() x
        setinv <- function(solve) invmtrx <<-solve ## create inverse and cache
        getinv <- function() invmtrx 
        list( set=set, get = get,
              setinv = setinv,
              getinv = getinv)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmtrx <- x$getinv()
        if(!is.null(invmtrx)){ ## check to see if solution exists
                message("getting cached inverse matrix")
                return(invmtrx)
        }
        data <-x$get() ## If solution doesn't exist,load argument into function
        invmtrx <- solve(data,...) #create the solution
        x$setinv(invmtrx)
        invmtrx ## return the solution
}
