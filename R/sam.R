# SAM, a little service availability tool for live monitoring services
# non-function design, as it is intended to be executed on the command line with "Rscript sam.R"

source("https://raw.githubusercontent.com/16EAGLE/16EAGLE/master/R/base.R")
r_load(c("getSpatialData", "httr", "cli"), silent = T)
urls <- getOption("gSD.api")
urls <- urls[names(urls) != "aws.l8.sl"]

while(T){
  x <- lapply(urls, GET)
  
  df <- sapply(x, function(y) y$status_code)
  df <- cbind.data.frame(c("ESA operational (DHUS)", "ESA pre-operational (S3)", "USGS ESPA", "USGS EarthExplorer", "AWS Landsat 8", "NASA DAAC LAADS"),
                         "available", df, names(df), "green", stringsAsFactors=F)
  rownames(df) <- NULL
  colnames(df) <- c("service", "status", "code", "id",  "colour")
  
  if(!any(c(df[df$id == "aws.l8",]$code == 404,  df[df$id == "aws.l8",]$code == 200))){
    df[df$id == "aws.l8",c(2,5)] <- c("unavailable", "red")
  }
  
  df[df$code == 301,c(2,5)] <- c("maintenance", "orange")
  df[df$code == 503,c(2,5)] <- c("unavailable", "red")
  df[df$code == 400,c(2,5)] <- c("retry", "blue")
  
  system("tput reset")
  cat_boxx(c("SAM | Service Availability Monitor", "getSpatialData"), align = "center", padding = c(2,2,2,2))
  catch <- apply(df, MARGIN = 1, function(x, nc = max(nchar(df$service))) cat_bullet(paste0(x[1], ": ", paste0(rep(" ", times = nc-nchar(x[1])), collapse = ""), "  ", x[2], " "), bullet_col = x[5]))
  sp <- get_spinner("simpleDots")
  interval <- sp$interval/1000
  frames <- sp$frames
  cat("\n")
  for (i in 1:(length(frames) * 120) - 1) {
    fr <- unclass(frames[i%%length(frames) + 1])
    cat("\rMonitoring", cli:::rpad(fr, width = 30), sep = "")
    Sys.sleep(interval)
  }
}

