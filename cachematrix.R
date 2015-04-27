
# makeCacheMatrix receives a matrix variable, and sets variables and functions in memory, 
# and returns a list of functions nested within makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
        local_m <- NULL                                         
        set <- function(y) {                                    
                cache_x <<- y                                   
                cache_m <<- NULL                                         
        }
        get <- function() cache_x                               
        set_cache_m <- function(local_m) cache_m <<- local_m         
        get_cache_m <- function() cache_m                      
        list(set = set, get = get,
             set_cache_m = set_cache_m,
             get_cache_m = get_cache_m)

}
# cacheSolve function receives a variable that is a matrix that is expected to have been defined as makeCacheMatrix(),
# as in m <- makeCacheMatrix(), and then populated with an invertible matrix using the m$set() function that is nested 
# in makeCacheMatrix(). In this syntax, the variable "m" can be any letter. j See validation instruction, above.

# cacheSolve returns the inverted form of the submitted matrix.
 
# When cacheSolve is called, cacheSolve checks to see if there already exists a non-NULL value for m in cache. 

# If cacheSolve finds a non-NULL value for m existing in cache already, it returns that value.  

# If cacheSolve does not find an existing non-NULL value for m in cache, cacheSolve gets the commandline values for m, inverts the matrix 
# in m, and sets the value of m in the cache environment to the just-computed inverted matrix.

# cacheSolve then evaluates the ending matrix so as to return it.

cacheSolve <- function(x) {                     
        local_m<- x$get_cache_m()               
        if(!is.null(local_m)) {                  
                message("getting cached data")  
                return(local_m)
        }                             
        startingmatrix <- x$get()                                        
        endingmatrix <- solve(startingmatrix)   
        x$set_cache_m(endingmatrix)            
        endingmatrix                            
}

#
# Test Results:
# 
# > m <- makeCacheMatrix()
# > m$set(matrix(c(0,2,2,0),2,2))
# > m$get()
# [,1] [,2]
# [1,]    0    2
# [2,]    2    0
# > cacheSolve(m)
#      [,1] [,2]
# [1,]  0.0  0.5
# [2,]  0.5  0.0
# > cacheSolve(m)
# getting cached data
#      [,1] [,2]
# [1,]  0.0  0.5
# [2,]  0.5  0.0
# > 
# 
# Change to matrix triggers fresh inversion solution:
#
# > m$set(matrix(c(100,2,2,100),2,2))
# > m$get()
# [,1] [,2]
# [1,]  100    2
# [2,]    2  100
# > cacheSolve(m)
# [,1]        [,2]
# [1,]  0.01000400 -0.00020008
# [2,] -0.00020008  0.01000400
# > cacheSolve(m)
# getting cached data
# [,1]        [,2]
# [1,]  0.01000400 -0.00020008
# [2,] -0.00020008  0.01000400
# 
# Change to matrix triggers fresh inversion solution:
#
# > m$set(matrix(c(0,2,2,0),2,2))
# > m$get()
# [,1] [,2]
# [1,]    0    2
# [2,]    2    0
# > cacheSolve(m)
# [,1] [,2]
# [1,]  0.0  0.5
# [2,]  0.5  0.0
# > cacheSolve(m)
# getting cached data
# [,1] [,2]
# [1,]  0.0  0.5
# [2,]  0.5  0.0
# > 
