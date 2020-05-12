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

## three-level prompting function for prompting messages, warnings and errors
out <- function(input, type = 1, msg = FALSE, t = T, sign = " - "){
  if(isTRUE(t)) sign <- paste0(toString(Sys.time()), " ", sign) 
  if(type == 2){ warning(paste0(sign,input), call. = FALSE, immediate. = TRUE)} else{
    if(type == 3){ stop(input,call. = FALSE)}else{
      if(msg == FALSE) cat(paste0(sign,input),sep="\n") else message(paste0(sign, input)) }}
}


## loads or installs and loads packages from CRAN
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


## loads or installs and loads libraries via cmd pip
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


## build and check moveVis
buildcheck <- function(pkgdir = getwd(), check_pkg = TRUE, test_pkg = TRUE,
                       test_dir = tempdir(), test_maps = FALSE, preview_site = T,
                       check_esri = FALSE,
                       map_token = ""){
  
  cat("Building and checking moveVis in '", pkgdir, "'...\n", sep = "")
  
  Sys.setenv("moveVis_map_token" = map_token)
  Sys.setenv("moveVis_test_dir" = test_dir)
  Sys.setenv("moveVis_n_cores" = 1) #parallel::detectCores()-1)
  Sys.setenv("moveVis_test_maps" = as.character(test_maps))
  
  # functions
  get_esri_urls <- function(base = "https://services.arcgisonline.com", ep = "/arcgis/rest/"){
    
    .get <- function(url){
      con <- curl::curl(url)
      return(readLines(con))
      close(con)
    }
    .grep_urls <- function(x) grep("MapServer", x, value = T)
    .clean_urls <- function(x) as.list(paste0(base, sapply(strsplit(x, '"'), function(y) grep("arcgis", y, value =T))))
    
    # get urls
    x <- .get(paste0(base, ep))
    
    # grep sub directories
    sub <- c()
    i <- grep("Folders", x)+2
    end <- FALSE
    while(isFALSE(end)){
      if(x[i] == "</ul>") end <- T else{
        sub <- c(sub, x[i])
        i <- i+1 
      }
    }
    
    # extract urls and names
    urls <- as.list(paste0(.clean_urls(.grep_urls(unlist(c(x, lapply(.clean_urls(sub), .get))))), "/tile/"))
    keys <- tolower(gsub("/MapServer/tile/", "", gsub(paste0(base, ep, "services/"), "", urls)))
    names(urls) <- sapply(strsplit(keys, "/"), function(x) x[length(x)], USE.NAMES = F)
    return(urls)
  }
  
  
  add_esri_urls <- function(path = "R/moveVis-internal.R"){
    file_int <- file(path)
    code_int <- readLines(file_int, warn = F)
    i <- grep("esri = c", code_int)
    code_int <- code_int[-c(i:length(code_int))]
    
    esri_urls <- get_esri_urls()
    cat("Found", length(esri_urls), "ESRI base map URLs:\n")
    cat(paste0(esri_urls, collapse = "\n"))
    x <- paste0("                                 esri = ",
                paste0("c(", paste0(mapply(x = esri_urls, y = names(esri_urls), function(x, y) paste0(y, ' = "', x, '"'), USE.NAMES = F), 
                                    collapse = paste0(",\n", paste0(rep(" ", 42), collapse = ""))), ")))"),
                "\n}")
    if(readline("Update 'R/moveVis-internal.R' with displayed URLs? [y/n]") == "y") {
      writeLines(c(code_int, x), file_int) 
      cat("ESRI base map URLs have been added.\n")
    } else{
      cat("Code remains untouched.\n")
    }
    close(file_int)
  }
  
  # add esri URLs
  if(isTRUE(check_esri)){
    cat("Adapting internal code to update ESRI REST API URLs...\n")
    add_esri_urls(paste0(pkgdir, "/R/moveVis-internal.R"))
  }
  
  # build package and site
  cat("Roxygenizing...\n")
  devtools::document(pkgdir)
  cat("Building package...\n")
  devtools::build(pkgdir, manual = T)
  cat("Installing package...\n")
  devtools::install(pkgdir, build = T)
  cat("Building web page...\n")
  pkgdown::build_site(pkgdir, examples = F, preview = preview_site)
  
  # check and test package
  if(isTRUE(check_pkg)){
    cat("Checking...\n")
    devtools::check(args = "--no-tests")
  }
  
  if(isTRUE(test_pkg)){
    cat("Testing...\n")
    devtools::test(pkgdir)
  }
  
  cat("Done.\n")
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
  files[unlist(lapply(files, function(x){
    l <- readLines(x, warn = F)
    grepl(tolower(term), tolower(paste0(l, collapse = " ")))
  }))]
}
