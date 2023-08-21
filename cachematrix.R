
makeCacheMatrix<-function (m=matrix()) {
    inv<-NULL
    set<-function(y) {
        m<<-y
        inv<<-NULL
    }
    get<-function() m
    setinv<-function(solve) inv<<-solve
    getinv<-function() inv
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}


cacheSolve<-function(x,...) {
    inv<-x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data,...)
    x$setinv(inv)
    inv
}
