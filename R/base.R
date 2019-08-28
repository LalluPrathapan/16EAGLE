## DOC_TAGS 
#doc_name: base.R
#title: base
#type: function_coll
#description: Set of easy-life R functions
#date_of_creation: 2017-01-01
#status: ud
#author: Jakob Schwalb-Willmann
#copyright: ask author
#bugs: unkown
#use: source


## out()
#type: function
#description: three-level prompting function for prompting messages, warnings and errors
#date_of_creation: 2017-03-01
#author: Jakob Schwalb-Willmann
#arguments:
#input: string, prompt message string
#type: numeric, 1 = LOG, 2 = WARNING, 3 = ERROR
#ll: global numeric, 1 = all, 2 = WARNINGS+ERRORS, 3 = ONLY ERRORS
#msg: logical, change from cat to message
#sign: string, output string prefix
#return: none
#bugs: none
#use: SOURCE
#depens: base
out <- function(input, type = 1, msg = FALSE, t = T, sign = " - "){
  if(isTRUE(t)) sign <- paste0(toString(Sys.time()), " ", sign) 
  if(type == 2){ warning(paste0(sign,input), call. = FALSE, immediate. = TRUE)} else{
    if(type == 3){ stop(input,call. = FALSE)}else{
      if(msg == FALSE) cat(paste0(sign,input),sep="\n") else message(paste0(sign, input)) }}
}


## r_load()
#type: function
#description: R package manager, loads or installs and loads packages from CRAN
#date_of_creation: 2017-03-01
#author: Jakob Schwalb-Willmann
#arguments:
#...: package names
#silent: logical, turns off prompts
#return: none
#bugs: none
#use: SOURCE
#depends: base
r_load <- function(..., silent = FALSE) {
  pkg <- unlist(list(...))
  if(class(pkg) != "character"){out("'pkg' has to be a 'character' vector.", type=3)}
  no_return <- lapply(pkg, function(x, s = silent){
    if(!s){out(paste0("Loading package '",x,"'..."),type=1)}
    pkg.try <- try(library(x, character.only = TRUE),silent = TRUE)
    pkg.status <- FALSE
    if(class(pkg.try) == "try-error"){
      if(!s){out(paste0("Could not find the package '",x,"'."),type=2)}
      pkg.status <- TRUE
    }
    if(pkg.status == TRUE){
      install.packages(x)
      library(x,character.only = TRUE)
    }
  })
}


## py_load()
#type: function
#description: Python library manager, loads or installs and loads libraries via cmd pip
#date_of_creation: 2017-11-22
#author: Jakob Schwalb-Willmann
#arguments:
#lib: string, vector containing library names
#get.auto: logical, define, if packages should be installed via pip
#install.only: logical, define, if packages should be installed only (TRUE) or also be loaded (FALSE)
#return: module or list of modules
#bugs: none
#use: SOURCE
#depends: reticulate
py_load <- function(lib, get.auto = TRUE, install.only = FALSE){ #returns list of imports
  if(class(lib) != "character"){out("'lib' has to be a 'character' vector.", type=3)}
  r_load("reticulate", silent = TRUE)
  imports <- lapply(lib, function(x){
    out(paste0("Loading package '",x,"'..."),type=1)
    from <- F
    if(length(grep("[$]",x)) == 1){
      y <- unlist(strsplit(x, "[$]"))[2]
      x <- unlist(strsplit(x, "[$]"))[1]
      from <- T
    }
    lib.try <- try(reticulate::import(x), silent = TRUE)
    if(class(lib.try)[1] == "try-error"){
      if(get.auto == TRUE){
        system(paste0("pip install ",x))
        re <- reticulate::import(x)
      }else{
        out(paste0("Module '",x,"' is not installed. Auto-install is not available. Please install modules."),type=3)
      }
    }else{re <- lib.try}
    if(from){
      g <- parse(text = paste0("re$",y))
      g <- list(eval(g)); names(g) <- y
    }else{
      g <- list(re); names(g) <- x
    }
    return(g)
  })
  if(length(imports) == 1){imports <- imports[[1]]
  }else{imports <- unlist(imports)}
  if(install.only == TRUE){return(TRUE)}else{return(imports)}
}



## simplifiy
is.FALSE <- function(evaluate){if(evaluate == FALSE){return(TRUE)}else{return(FALSE)}}
is.TRUE <- function(evaluate){if(evaluate == TRUE){return(TRUE)}else{return(FALSE)}}

check.cmd <- function(cmd, dirs = NULL){
  sc <- try(devtools::system_check(cmd, quiet = TRUE),silent = TRUE)
  if(class(sc) == "try-error"){return(FALSE)}else{return(TRUE)}
}
  
quiet <- function(expr){
  return(suppressWarnings(suppressMessages(expr)))
}

search_in_files <- function(term, files){
  unlist(lapply(files, function(x){
    l <- readLines(x, warn = F)
    grepl(tolower(term), tolower(paste0(l, collapse = " ")))
  }))
}
