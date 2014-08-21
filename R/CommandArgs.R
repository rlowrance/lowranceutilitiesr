#' Return the command line argumes after \code{--arg}
#'
#' \code{CommandArgs} returns either the arguments from the command line or
#' the default arguments supplied in the function call.
#'
#' @param defaultArgs optional; if present, returned instead of the actual command line arguments
#' @param verbose optional default \code{TRUE}; whether to print the returned args
#' @export
CommandArgs <- function(defaultArgs, verbose = TRUE) {
    # return command args if present, otherwise return defaultArg
    command.args <- commandArgs()
    for (command.arg in command.args) {
        if (command.arg == '--args') {
            # this happens if started with Rscript and arguments are supplied
            if (verbose) {
                print('CommandArgs returning actual args, which are')
                print(command.args)
            }
            return(command.args)
        }
    }

    if (verbose) {
        print('CommandArgs returning defaultArgs, which are')
        print(defaultArgs)
    }
    return(defaultArgs)
}
