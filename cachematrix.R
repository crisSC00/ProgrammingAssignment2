## For this assignment I wrote a pair of functions that cache the inverse of a 
## matrix. 


## The makeCacheMatrix function creates a special "matrix" object that can cache 
## its inverse. It returns an object that can be used by downstream R code.

## The x argument here is the matrix which inverse we want to cache. It's 
## important to set a default value for x because otherwise data <- x$get()
## (in the cacheSolve function) generates an error message.
## The i object is the inverse of x, but it's initialized as NULL to be used
## later in this function.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    ## This is the set function. It is a "setter" because it mutates the data
    ## values within an object. This function allows the user to use a different
    ## matrix as the input (reset x) without the need to call the makeCacheMatrix 
    ## function again.
    ## The y argument takes the value of the x argument defined in the parent 
    ## environment, hence the <<- operator.
    ## The next line clears any value of i that had been cached by a prior 
    ## execution of cacheSolve(). If there is already a valid matrix cached in i, 
    ## whenever x is reset, i is cleared and next time cacheSolve() is called, it
    ## recalculates the inverse of the new matrix (the new x).
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## The get function is the "getter" for the matrix x. Since x is not
    ## defined in the (), R retrieves x from the parent environment thanks to
    ## lexical scoping.
    ## The setinverse function sets the inverse of x by using the <<- operator
    ## to assign the value of "inverse" to i (defined in the parent environment).
    ## The getinverse function gets i from the parent environment using lexical
    ## scoping, just like in the get function.
    
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    
    ## This creates a list with all the functions within makeCacheMatrix and 
    ## assigns a name to each of them. This allows the use of $ to call these
    ## functions by name.
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache. Without this fucntion, makeCacheMatrix is incomplete because it's
## here that the actual inverse matrix is calculated.

## The z argument is the object returned by the makeCacheMatrix function.
## The next line retrieves an inverse matrix from the object passed as the 
## argument (z) using the getinverse() and assigns it to i.


cacheSolve <- function(z, ...) {
        i <- z$getinverse()
        
        ## If i is not NULL (!is.null(m) is TRUE), that means there is a valid 
        ## matrix that has already been calculated previously. In this case, you'll
        ## get the message "getting cached data" and the cached matrix, which is 
        ## stored in i, as the output.
        
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        
        ## If i is NULL (!is.null(i) is FALSE), cacheSolve gets the matrix (x) 
        ## from the input object (z) and assigns it to "data".
        ## Then solve() calculates the inverse of "data" and stores it is i.
        ## The next step is to use setinverse() to set i (that has just been 
        ## calculated) as the inverse matrix in z.
        ## The output is i, which is the inverse of the matrix used as input to 
        ## the parent environment. It might be x (argument in makeCacheMatrix) or
        ## it might be a new matrix used as input by z$set().
        
        data <- z$get()
        i <- solve(data)
        z$setinverse(i)
        i
}
