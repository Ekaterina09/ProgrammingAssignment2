## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	S<-NULL	#assign zero value to inverse matrix in environment 
			#"makeCacheMatrix"

	set<-function(y) {
		x<<-y		#assign value of y to x in the parent environment 
				#"makeCacheMatrix". This allows to set new matrix. 
		S<<-NULL	#reset the value of S in the environment 
				#"makeCacheMatrix"
	}

	get<-function() x	#returns the value of x

	setinv<-function(solve) S<<-solve	#reset the value of S in the 
							#environment "makeCacheMatrix"; 
							#S computes inverse of the matrix "x"

	getinv<-function() S			#returns the value of S

	list(set=set,get=get,setinv=setinv,getinv=getinv)	#makes the list of 
										#functions

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	S<-x$getinv()	#S takes the value of the inverse matrix returned by 
				#subfunction "getinv" in function "makeCacheMatrix"
	if(!is.null(S)) {	#if S is non-null then its' value is searched in cache 
				#and returned
		message("getting cached data")
		return(S)
	}
	
	data<-x$get()	#if S is null then 'data' is assigned the value of 
				#matrix returned by subfunction 'get'
	S<-solve(data,...)#compute the inverse matrix of the 'data'
	x$setinv(S)		#store the inverted matrix to 'setinv' of 'makeCacheMatrix'
	S			#return the value of S
}
