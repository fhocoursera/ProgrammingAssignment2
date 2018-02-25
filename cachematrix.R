## Caching the Inverse of a Matrix
## Together these functions will compute the inverse of a matrix,
## and store the resulting inverse matrix within local memory - 
## stored within a list.  The second function will look inside 
## the list to get the inverse matrix from memory (if already 
## calculated earlier); if the inverse is not in memory, the 
## second function will calculate the inverse.


## makeCacheMatrix () creates a special "vector", which is a list 
## containing a function to:
## 1. Set the value of the vector
## 2. Get the value of the vector
## 3. Set the value of the inverse of the 'input' matrix
## 4. Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL
    set <- function(y) { ## sets the value of the vector
        x <<- y
        inv_matrix <<- NULL
    }
    get <- function() x ## get the value of the vector
    ## setinv sets the value of the inverse of the 'input' matrix
    setinv <- function(solve) inv_matrix <<- solve
    ## gets the value of the inverse matrix
    getinv <- function() inv_matrix
    ## assigns the values within a list to store in memory
    list(set = set, 
         get = get, 
         setinv = setinv, 
         getinv = getinv)
}


## cacheSolve () looks within a stored list to find the 
## inverse matrix - if the 'input' matrix was alread calculated
## if the 'input' matrix is new - then this function will
## calculate the inverse matrix within the function and 
## display the inverse matrix

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    inv_matrix <- x$getinv()
    ## the if clause looks to see if the inv_matrix is
    ## stored within the list stored in the
    ## makeCacheMatrix function
    if(!is.null(inv_matrix)){ 
        message("getting cached data")
        return(inv_matrix)
    }
    ## if not already calculated - this will calculate
    ## the inverse of the provided matrix
    data <- x$get
    inv_matrix <- solve(data, ...)
    x$setinv(inv_matrix)
    inv_matrix
}
