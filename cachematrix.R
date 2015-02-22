## Combining the two functions described bellow allows the user to compute the 
## inversion of a given square matrix and cache it in the global environment or 
## call an inverted matrix already computed and cached without repeting the 
## calculations.
## Using this approach, the user will avoid computing the inversion of the same
## matrix more than once and therefore save time.



## The makeCacheMatrix() takes a matrix as an argument and creates a special 
## "matrix" object that can cache its inverse by assign it to an object in an 
## environment that is different from the current environment. The function 
## consists of a list of sub-functions to define and get the variable matrix 
## (set() and get()) and to define and get the inverse of that input 
## matrix (setinv() ad getinv()). This function will define the variable inv as  
## a NULL matrix object or as a matrix object if it that has been previously 
## cached in the global environment. To use this function to create an input for 
## cacheSolve() the matrix taken as an argument must be a square matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}

## The function bellow takes the matrix object defined by makeCacheMatrix() as 
## an argument and computes its inverse. The variable inv is set in the current 
## environment as the inverted matrix defined by getinv() of the
## makeCacheMatrix(). If the inverted matrix has not been calculated before, inv 
## will be NULL and the function will show the message "calculating inverse",
## compute the inverted matrix using the solve() and cache it by calling the 
## setinv() defined in the makeCacheMatrix(). If the inversion of the matrix 
## used as an argument for makeCacheMatrix() has already been computed and 
## cached in the global enviroment, inv will be different than NULL and the 
## function will show the message "getting cached inverse" and return the matrix 
## in cache without computing the inversion.



cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        message("calculating inverse")
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
