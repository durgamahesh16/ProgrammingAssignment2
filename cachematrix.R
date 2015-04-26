## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
## Write a short comment describing this function

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
