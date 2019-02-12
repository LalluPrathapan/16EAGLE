#Title: fake_move
#Type: function
#Description: Creates a fake move class dataset from a shapefile and manual inputs
#Date of creation: 2017-07-07
#Author: Jakob Schwalb-Willmann
#Arguments:
    #shp:         shape file, including your points and projection (MANDATORY)
    #shp_start:   string, starting datetime of your fake data, using format "YYYY-MM-DD hh:mm:ss",
    #default is "2018-03-03 12:00:00"
    #shp_res:     string, setting the temporal resolution of your input data (depending on your point distances)
    #with a number followed by a space and then either "sec", "min", "hour", "day", "DSTday", "week",
    #"month", "quarter" or "year", default is "5 min"
    #indi_name:   string, naming of the individual path, default is "fake_bird"
    #interpolate: logical, if TRUE, your data points and datetime are interpolated to a finer (or coarser)
    #resolution compared to your shp_res input resolution, default is TRUE
    #out_res:     integer, output resolution in minutes, if interpolate = TRUE. If shp_res = "5 min",
    #out_res of 1 would create a 1 min resolution, default is 1.

fake_move <- function(shp, shp_start = "2018-03-03 12:00:00", shp_res = "5 min", indi_name = "fake_bird", interpolate = TRUE, out_res = 1,
                      skips = 0, noise = T){
  if(suppressWarnings(require(move,quietly = TRUE)) == FALSE){
    install.packages("move"); require(move)
  }
  
  #Create sequences
  dt_seq <- seq.POSIXt(from = as.POSIXct(strptime(shp_start, "%Y-%m-%d %H:%M:%S", tz = "UTC")), by= shp_res, length.out = length(dr_shp@coords[,1]) + skips)
  dt_seq <- dt_seq[sort(sample(1:(length(dr_shp@coords[,1]) + skips), size = length(dr_shp@coords[,1])))]
  if(isTRUE(noise)) dt_seq <- sort(dt_seq+rnorm(length(dt_seq)))
  
  indi_seq <- rep(indi_name,length(dr_shp@coords[,1]))
  proj = proj4string(dr_shp)
  
  #Create data.frame
  dr <- data.frame(dr_shp@coords[,1]); dr <- cbind(dr,dr_shp@coords[,2],dt_seq,indi_seq)
  colnames(dr) <- c("x","y","dt","individual")
  
  #Interpolate coordiantes & time
  if(interpolate == TRUE){
    out_n <- round((as.integer(difftime(dr$dt[length(dr$dt)],dr$dt[1],units="mins"))+1)/out_res)
    coords <- matrix(c(dr$x,dr$y),nrow=length(dr$x))
    x_inter <- approx(coords[,1], n = out_n); y_inter <- approx(coords[,2], n = out_n)
    dt_inter <- seq(dr$dt[1], dr$dt[length(dr$dt)], length.out = out_n)
    indi_inter <- rep(dr$individual[1],out_n)
    
    dr <- data.frame(x_inter$y); dr <- cbind(dr,y_inter$y,dt_inter,indi_inter)
    colnames(dr) <- c("x","y","dt","individual")
  }
  return(move(x=dr$x,y=dr$y,time = dr$dt,animal=dr$individual,proj = proj))
}