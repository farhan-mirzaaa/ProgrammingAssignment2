## Author : Muhammad Farhan Mirza, Dated : 6 Sep, 2017
## My functions will help to take the inverse of a matrix given in the arguments of cacheSolve function
## These functions will allow to cache the matrix inverse if its not changed because inverse of a matricx
## will always be same if matrix does not change

## makeCacheMatrix function will take an inversible matrix as input and stores its inverse
## return list a functions to set, get matrix and set, get inverse

makeCacheMatrix <- function(mat = matrix()) 
{
        inverse <- NULL
        set <- function( m )
        {
                mat <<- m
                inverse <<- NULL
        }
        get <- function() mat
        setinverse <- function(inv) inverse <- inv
        getinverse <- function() inverse
        list( set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse )
}


## cacheSolve takes a cacheMatrix and calculates its inverse with the help of its function
## if not calculated and cached earlier
## if cached, then this function will return that inverse matrix result

cacheSolve <- function(cacheMat, ...) 
{
        inverse <- cacheMat$getinverse()
        if ( !is.null(inverse) )
        {
                message( "Getting cache inverse ..." )
                return( inverse )
        }
        mat <- cacheMat$get()
        inverse <- solve( mat )
        cacheMat$setinverse( inverse )
        ## Return a matrix that is the inverse of 'x'
        inverse
}
