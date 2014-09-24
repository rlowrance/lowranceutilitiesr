Cache <- function() {
    # return a new cache and access functions for it:
    # cache <- Cache()
    # cache$Reset()        : empty the cache
    # cache$Set(key, value): mutate the cache
    # cache$Get(key)       : return value at key
    # cache$Haskey(key)   : return TRUE or FALSE
    # ref: github.com/hadley/memoise
    cache <- NULL
    
    Reset <- function() {
        cache <<- new.env(TRUE, emptyenv())
    }

    Set <- function(key, value) {
        assign(key, value, envir = cache)
    }

    Get <- function(key) {
        get(key, envir = cache, inherits = FALSE)
    }

    Haskey <- function(key) {
        exists(key, envir = cache, inherits = FALSE)
    }
    
    Keys <- function() {
        ls(envir = cache)
    }

    Reset()

    list( Reset   = Reset
         ,Set     = Set
         ,Get     = Get
         ,Haskey  = Haskey
         ,Keys    = Keys
         )
}

Cache.test <- function() {
    c <- Cache()
    c$Set('a', 10)
    stopifnot(c$Get('a') == 10)
    stopifnot(c$Haskey('a'))
    stopifnot(!c$Haskey('b'))
    stopifnot(length(c$Keys()) == 1)
}

Cache.test()
