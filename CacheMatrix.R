## These 2 functions will cache a matrix, and, if unchanged, retrieve 
## the already calculated inverse of that matrix quickly.

## This first function creates a matrix that can "cache" its inverse
makeCacheMatrix <- function(x = matrix()) {
m<-NULL  
evn <- environment()  
y<-NULL 

setmatrix<-function(y){  
	x<<-y  
	m<<-NULL 
	}
  
getmatrix<-function() x  
setinverse<-function(solve) m<<- solve  
getinverse<-function() m 
getenv<- function() environment()

list (setmatrix=setmatrix, getmatrix = getmatrix, # creates list to house the four functions  
setinverse = setinverse,
getinverse = getinverse,
getenv = getenv)

}

## If above matrix is calculated and unchanged, this function should
## retrieve the inverse of the matrix.

cacheSolve <- function(xMat= m(), ...) {
	m <- xMat$getinverse() 
	if(!is.null(m)){ 
		if(xMat$setmatrix() == xMat$getmatrix()) { 
    	message("getting cached data")
    	matrix<-xMat$get()
    	m<-solve(matrix, ...)
    	xMat$setmatrix(m)
    	return(m) 
    	}
   
    	y <- xMat$getmatrix() 
    	xMat$setmatrix(y) 
    	m <- solve(y, ...) 
    	xMat$setinverse(m) 
    	m 
    	}
    	
}
