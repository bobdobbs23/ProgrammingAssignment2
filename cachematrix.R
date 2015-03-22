## Together these functions (makeCacheMatrix and cacheSolve) allow the user to create and cache
## the inverse of a matrix.  


## examples of how to call and output:

##  mat <- matrix(c(4, 3, 3, 2), nrow = 2, ncol =2, byrow=TRUE)
# > makeCacheMatrix(mat)
# $set
# function (y) 
# {
#         x <<- y
#         i <<- NULL
# }
# <environment: 0x7f9bf1e59ab0>
#         
#         $get
# function () 
#         x
# <environment: 0x7f9bf1e59ab0>
#         
#         $setinverse
# function (inverse) 
#         i <<- inverse
# <environment: 0x7f9bf1e59ab0>
#         
#         $getinverse
# function () 
#         i
# <environment: 0x7f9bf1e59ab0>
# z <- makeCacheMatrix(mat)
# > cacheSolve(z)
# [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4




## This function creates a special "matrix" object that can cache its inverse.
## 
makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL

        # set the value of the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        # get the value of the matrix
        get <- function() x

        # set the value of the inverse
        setinverse <- function(inverse) i <<- inverse
        
        # get the value of the inverse
        getinverse <- function() i
        
        # return a list containing set, get, setinverse, and getinverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated (and the matrix has not changed), then 
# the cachesolve should retrieve the inverse from the cache. It uses the solve() function 
# to calculate the inverse of matrix x. 

cacheSolve <- function(x, ...) {
        
        # sets i to the stored inverse of x
        i <- x$getinverse() 
        
        # checks to see if i exists
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
        
}
