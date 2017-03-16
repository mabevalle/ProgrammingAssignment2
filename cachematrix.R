## These functions are meant to calculate the inverse a matrix object, in case the inverse
## has already being calculated, it just return it from the cache.

## This function creates an object which is a list with functions to set and get a matrix
## and functions to set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set<-function(y){
		x<<-y
		inv<<-NULL
	}
	get<-function() x
	setInverse<-function(i) inv<<-i
	getInverse<-function() inv

	list(set=set,get=get,setInverse=setInverse, getInverse=getInverse)
}


## This function takes a matrix object and check if the inverse is in cache 
## if it doesn't then it calculates it and return the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv<-x$getInverse()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	mtx<-x$get()
	inv<-solve(mtx,...)
	x$setInverse(inv)
	inv
}
