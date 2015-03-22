## The next two function will be usefull to avoid time consuming in elaborate
## the inverse of a single matrix many times, e.g. in a for-loop.
##
## More detalied:
##        the first function is able to store a matrix and its inverse in a
##        suitable mode;
##        the second function can return the inverse of a matrix in two ways:
##                1 - computing it, if the first one don't "know" that result
##                    (and next the second function teach it to the first one);
##                2 - getting it from the first function, if it has just
##                    learned it.




## First function: "makeCacheMatrix". This function accept in input a matrix
## (the 1x1 empty matrix by default) and return a list of four function:
## SET: which store the matrix given in memory and restore the inverse stored
##      to the empty one;
## GET: which simply return the stored matrix (mayby the empty one if no one
##      is stored);
## SETINVERSE: which accept a given computed inverse of a matrix and store it;
## GETINVERSE: whic simply return the stored matrix (maybe the empty one if no
## one is stored)

makeCacheMatrix <- function(x = matrix()) {       # the defoult matrix is empty
    inv <- NULL                              # set the inverse to NULL (localy)
    SET <- function(y){
      x <<- y            # change the stored matrix to the given one (globally)
      inv <<- NULL              # restore the stored inverse to NULL (globally)
    }
    GET <- function() x                              # return the stored matrix
    SETINVERSE <- function(INVERSE) inv <<- INVERSE
                                             # store a given inverse (globally)
    GETINVERSE <- function() inv                    # return the stored inverse
    list(#SET = SET, 
      GET = GET, SETINVERSE = SETINVERSE,
         GETINVERSE = GETINVERSE)       # return the list of the four functions
}


## Second function: "cacheSolve". This function accept in input the first
## function (maybe provided with a given matrix as input of that one). Firstly
## it check if a stored inverse is provide by the "memory" of the first
## function. Next, if it is, return the inverse, or, if it isn't, compute it,
## store it in the "memory" of the first function and than return the invers
## just computed. (It can admit more optional arguments)

cacheSolve <- function(x, ...) {         # note: x have to seams something like
                                                  # makeCheckMatrix(<a matrix>)
        inv <- x$GETINVERSE()                          # get the inverse stored
        if (!is.null(inv)) {              # if there is a "real" stored inverse
                message("Getting the stored inverse")
                return(inv)        # then return return it (and send a message)
        }
        matrix <- x$GET()                      # else, take the original matrix
        inv <- solve(matrix, ...)                         # compute the inverse
        x$SETINVERSE(inv)                            # store it in the "memory"
        inv                                                        # return it.
        }
