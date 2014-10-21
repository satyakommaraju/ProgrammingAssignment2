## This function creates "matrix" object that can cache its inverse.
##Basically it uses setters and getters for access functions
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse of matrix
##get the value of the inverse of matrix
##

makeCacheMatrix <- function(inp = matrix()) {
   Iinv <- NULL 
       set <- function(y) {
             inp <<- y
             Iinv <<- NULL
         }
       get <- function() inp
       setinvMat <- function(inverse) Iinv <<- inverse
       getinvMat <- function() Iinv
       list(set=set, get=get, setinvMat=setinvMat, getinvMat=getinvMat)

}

## This function computes the inverse of the special "matrix"
##returned by makeCacheMatrix. If the inverse has already been 
##calculated (and the matrix has not changed),
##then cacheSolve  retrieves the inverse from the cache.

cacheSolve <- function(cx, ...) {

  inv <- cx$getinvMat()
       if(!is.null(inv)) {
  
            return(inv)
         }
       dataMat <- cx$get()
       inv <- solve(dataMat)
       cx$setinvMat(inv)
       inv
  
}

###TEST-RUN
##amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
##amatrix$get()
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
##---------------
##cacheSolve(amatrix)
## [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##-----------------
##amatrix$getinvMat()
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##-----------------
##amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2))
##cacheSolve(amatrix) 
##           [,1] [,2]
##[1,] -0.13333333  0.2
##[2,]  0.01010101  0.0
##---------------------
##> amatrix$get()
##[,1] [,2]
##[1,]    0   99
##[2,]    5   66
##---------------------
##> amatrix$getinvMat()
##[,1] [,2]
##[1,] -0.13333333  0.2
##[2,]  0.01010101  0.0
##
##
