## This function aims to cache the inverse of a given matrix,
## it contains two sets of functions (makeCacheMatrix and cacheSolve)
## Examples for usage:
### example 1
#  v <- matrix(1:4,2,2)
#  v2 <- makeCacheMatrix(v)
# cacheinverse(v2)

#       [,1] [,2]
#[ 1,]   -2  1.5
# [2,]    1 -0.5


### example 2
# m <- matrix(c(4,5,3,6),2,2)
# m2 <- makeCacheMatrix(v)
# cacheinverse(m2)
#       [,1]       [,2]
# [1,]  0.6666667 -0.3333333
# [2,] -0.5555556  0.4444444


## The first function aims to create a "special" matrix 
## that caches the inverse of the given matrix

makeCacheMatrix <- function(x = matrix()) {
           m_inv <- NULL
          set <- function(y){
            x <<- y
             m_inv <<- NULL
          }
  
  get <- function() x
  setinverse <- function(inverse)  m_inv <<- inverse
  getinverse <- function()  m_inv
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse =getinverse)

}


## The second function aims to extract the cache of the matrix inverse
## after checking that it exists, if it doesn't exist, it calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
         m_inv <- x$getinverse()
        #check if the  m_inv exists or not
         if(!is.null(m_inv)){
              message("getting cached data")
              return(m_inv)
         }
         data <- x$get()
         m_inv <- solve(data, ...)
        #set the computed inverse value in the cache using the setinverse()
         x$setinverse(m_inv)
         m_inv
}
