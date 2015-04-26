## The "makeCacheMatrix" function makes a list of 4 functions that set and get a matrix and its inverse in the parent environment of the function
## The "cacheSolve" function is passed on the list from the "makeCacheMatrix" and attempts to calculate the inverse of the matrix returned by "makeCacheMatrix".  If the inverse already exists, the cached value is returned (without the need to recalculate it)

## "makeCacheMatrix" creates a matrix x, and 3 functions to set/get x and its inverse

makeCacheMatrix <- function(x = matrix()) {
        cached <- NULL ## initiate an empty vector to store inverse
        
        ## substitute x in parent environment (i.e. the env of function makeCacheMatrix) with the desired matrix object (y), if inverse is already set, restore to null the value of cached
        set <- function(y = matrix()) {
                x <<- y
                cached <<- NULL
        }
        
        get <- function() x ## returns the matrix x stored in the main function
        
        ## store the inverse of y to the desired value (cached) in parent environment and return the value of cached
        setInverse <- function(invy) {
                cached <<- invy
                return(cached)
        }
        
        getInverse  <- function() cached ## return the value of cached
        
        ## store the 4 functions in the function "makeCacheMatrix", so that when we assign makeCacheMatrix to an object, the object has all the 4 functions
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Given the list from the "makeCacheMatrix function", the "cacheSolve" checks to see if there is already a cached inverse and that it is not NULL. 
## If it exists in the memory, it returns a message and the value of cached; else it computes its inverse and set/return it.

cacheSolve <- function(x=makeCacheMatrix(), ...) { 
        
        ## check if the inverse of x already exists and assign it "Inverse"
        Inverse <- x$getInverse() 
        
        ## check if cached data is not NULL and if it is a matrix
        ## if so return a message and its value
        if(!is.null(Inverse) & is.matrix(Inverse)) { 
                message("Found and getting cached data ")
                return(Inverse)
        }
        
        ## if no cached matrix found, get the matrix stored with "makeCacheMatrix"
        matrix <- x$get()  
        
        ## compute the inverse of the above matrix 
        Inverse <- solve(matrix)
        
        ## set the value of the inverse and print inverse
        x$setInverse(Inverse)
        Inverse
}