### Cache the inverse of a matrix, calculate inverse only if the matrix was changed

## Create a list of follwing functions to manage matrix and it's inverse:   
# get()         - return matrix
# set(y)        - set new matrix
# getInverse()  - return inverse matrix
# setInverse(y) - set inverse matrix 

makeCacheMatrix <- function(x = matrix())
{      
    # Inverse matrix
    I <- NULL
    
    # Get matrix
    get <- function() x
    
    # Set new matrix
    set <- function(y)
    {
        x <<- y
        
        # Matrix was change, so and inverse of this matrix
        I <<- NULL
    }
    
    # Get inverse matrix
    getInverse <- function() I
    
    # Set inverse matrix
    setInverse <- function(y) I <<- y
    
    # Return list of functions
    list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}


## Retrun and set inverse matrix created with makeCacheMatrix().
## If inverse matrix was calculated, then function return it, don't calculate it again

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    
    # Get inverion matrix
    I <- x $ getInverse()
    
    # Is inversion was calculated?
    if (!is.null(I))
    {
        #message("getting cached data")
        return(I)
    }
    
    # If no, then calculate it...
    I  <- solve(x $ get())
    
    # ...save...
    x $ setInverse(I)

    # ...and return.
    I
}
