# CREACIÓN DE MATRIZ INVERSA UTILIZANDO CACHE // CREATING INVERSE MATRIX USING CACHE
# 

#Estableciendo la matriz

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solveinv) i <<- solveinv
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#Calculando la matriz Inversa

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("obteniendo datos de cache")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
