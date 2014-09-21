WithRestoredRandomSeed <- function(f) {
    # execute f() then restore the current random seed
    oldseed <- NULL
    if (exists('.Random.seed')) oldseed <- get('.Random.seed', .GlobalEnv)
    f()
    if (exists('.Random.seed')) assign('.Random.seed', oldseed, .GlobalEnv)
}
