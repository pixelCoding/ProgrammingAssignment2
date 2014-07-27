## INTRO: These are a pair of functions that compute and cache
## the inverse of a matrix.
## I make many coments because I understood it's a part of assignment,
## normally I would make less comments
## because I belive this code is explaining itself in many parts

## USAGE: create "special matrix object" with: makeCacheMatrix(matrix)
## i.e. a <- makeCacheMatrix(matrix)
## where matrix is a proper matrix object eg. matrix(1:4,2,2)
## then call cacheSolve on it eg. cacheSolve(a)
## It's possible to change matrix in special matrix object
## using set func eg. a$set(matrix(1:9,3,3))

## makeCacheMatrix: This function creates a special "matrix"
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL # sets s (which holds inverse to null)
        # it's essential because without it, it would use
        # s previously cached (if existing)
        set <- function(y) { # func to change object data
                x <<- y # sets new matrix
                s <<- NULL # sets cache inverse to null
        }
        get <- function() x # returns original matrix
        setsolve <- function(solve) s <<- solve # caches inverse
        getsolve <- function() s ## returns inverse
        list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
        # availabe calling above subfunctions in easy way
}


## cacheSolve: Function returns the inverse of the special "matrix"
## object created by makeCacheMatrix function.
## It computes inverse of matrix for the first time run and caches it
## If run again (and spec. matrix object is not changed)
## it gets inversion from cache (without computing)


cacheSolve <- function(x, ...) {        
        s <- x$getsolve() # gets inverted matrix (if availabe)
        if(!is.null(s)) { # checks if inverted matrix is really cached
                message("getting cached data")
                return(s) # if so returns it
                          # and ends function
        }
        data <- x$get() # otherwise gets original matrix
        s <- solve(data, ...) # and computes inverse
        x$setsolve(s) # caches inverse
        s # prints it on the screen
}
