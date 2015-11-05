## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y=matrix()){
        X<<-y
        m<<-NULL
    }
    get<-function() x
    setinv<-function(inverse) m<<-inverse
    getinv<-function() m
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}


##Computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    m<-x$getinv()
    if(!is.null(m)){
        message("getting cached inverse of a matrix")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix,...)
    x$setinv(m)
    m
}
