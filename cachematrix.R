## written by KellyMHays
## for Coursera "R Programming", Data Science Specialization, July 2016
## 
## Programming assignment #2, week 3


##  "makeCacheMatrix" takes, as input, a matrix.  Since the program assignment
##  said we can assume the matrix is invertable, we do not need to check for 
##  squareness or other properties
##  
##  the function sets up a list which stores the matrix and creates space for
##  for the inverse.  
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


##  "cacheSOlve" takes, as input, the output from makeCacheMatrix.
##  Since the program assignment said we can assume the matrix is invertable, we
##  do not need to check for squareness or other properties
##  
##  the function returns the inverse if it has already been calculated
##  if it has not already been calcuated, it calculates it and stores it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m        
}
