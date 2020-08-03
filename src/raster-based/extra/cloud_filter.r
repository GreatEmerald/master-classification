cloud_filter <- function(x, probav_sm_dir, cf_bands, pattern, tiles, thresholds=c(-50, Inf) , span=0.3, minrows=1, mc.cores=1, logfile, overwrite=FALSE, filename="") {

  thresholds <- matrix(thresholds, nrow=2)
  
  df_probav_sm <- getProbaVinfo(probav_sm_dir, pattern = pattern, tiles = tiles)
  s_info <- df_probav_sm
  #s_info <- getProbaVinfo(names(x))
  bands <- s_info[s_info$date == s_info$date[1], 'band']
  dates <- s_info[s_info$band == bands[1], 'date']
  ydays <- s_info[s_info$band == bands[1], 'yday']
  
  
  
  cf <- function(x){
    # smooth loess and getHarmMetrics
    m <- matrix(x, nrow= length(bands), ncol=length(dates))
    #qcb <- smoothLoess(m, dates = dates, thresholds=NULL, res_type = "QC", span=0.3)
    if (!all(is.na(m[1,]))) {
      res <- try({
        # smooth loess on all cf bands, then combine
        qc <- foreach(bn = 1:length(cf_bands), .combine='&') %do% {
          qcb <-   smoothLoess(m[cf_bands[bn],], dates = dates, threshold = thresholds[,bn],
                               res_type = "QC", span=span)
        }
      })  
      
      if(class(res) == 'try-error') {
        res <- rep(NA_integer_, length(dates))
      }
    } else {
      res <- rep(NA_integer_, length(dates))
    }
    
    return(res)
  }
  
  
  out <- mcCalc(x=x, fun = cf, minrows = minrows, mc.cores = mc.cores, logfile=logfile, out_name = filename, overwrite = overwrite, mc.preschedule = FALSE, datatype = "INT1U", of="GTiff")

  return(out)
}


