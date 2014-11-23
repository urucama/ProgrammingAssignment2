## Functions solving the inversion of a matrix, reducing processing time using cached results.

## Can be tested using the makeCacheMatrix with a invertible matrix like this one: matrix(c(1,0,5,2,1,6,3,4,0),3,3)
## the list returned must be stored in a value that will used as an argument for the cacheSolve function.
## The cachesolve function will then display the inverted matrix and specify if the result is cached.


## MakeCacheMatrix - takes a matrix x as argument // returns a list of getters and setters functions that allows to get or set the current
## cached matrix x and the inverted one invmat.

makeCacheMatrix <- function(x) {
    
    invmat <- NULL
    
    set <- function(y){
        x <<- y
        invmat <<- NULL
    }
    
    get <- function() x
    
    set_im <- function(inverted_matrix){
        invmat <<- inverted_matrix
    }
    
    get_im <- function() invmat
    
    list(get = get, set = set, set_im = set_im, get_im = get_im)
}


## Cachsolve takes a matrix x as argument // Returns the inverse of a matrix whenever its exists one. It details wether
## the result is cached or not. If not cached, it calculates it and store it in the object created by the MakeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    invmat <- x$get_im()
    
    if (!is.null(invmat)){
        
        print("Getting cached result")
        return (invmat)
    }
    
    mat <- x$get()
    invmat <- solve(mat) 
    x$set_im(invmat)
    invmat
}
