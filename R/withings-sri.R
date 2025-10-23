#' Calculate Sleep Regularity Index (SRI) from binary sleep-wake files
#'
#' @param binarydir Directory containing binary sleep-wake CSV files
#' @param tz Timezone for timestamps
#' @param alloutdir Output directory (default: binarydir + '_output')
#' @param col.trans Column index for sleep-wake (default: 1)
#' @param col.timestamp Column index for timestamp (default: 2)
#' @param overwr Overwrite output file (default: FALSE)
#' @param wr.raster Write raster plots (default: TRUE)
#' @param minSRIdays Minimum days required for SRI (default: 5)
#' @param exclNAhrs Exclude days with more than this many NA hours (default: 6)
#' @return Completion message
#' @export
SRI_from_binary <- function (binarydir = c(),
                             tz = "UTC",
                             alloutdir = c(),
                             col.trans = 1,
                             col.timestamp = 2,
                             overwr = FALSE,
                             wr.raster = TRUE,
                             minSRIdays = 5,
                             exclNAhrs = 6
                             ){
  if (length(binarydir) == 0){
    stop("Error: Specify directory containing sleep diary files")
  }
  if (length(alloutdir) == 0){
    alloutdir <- paste0(binarydir,"_output")
  }
  if (!dir.exists(alloutdir)){
    dir.create(alloutdir)
  }
  if (wr.raster == TRUE){
    rasdir <- paste0(alloutdir,"/raster_output")
    if (!dir.exists(rasdir)){
      dir.create(rasdir)
    }
  }
  file_list <- list.files(binarydir, pattern = "*.csv", full.names = TRUE)
  ppt_list <- list.files(binarydir, pattern = "*.csv")
  SRIfile <- paste(alloutdir,"SRI.csv",sep="/")
  SRIheader <- c("File", "SRI", "SRI_days", "SRI_pctl")
  if (overwr == FALSE){
    if (!file.exists(SRIfile)) {
      write.table(t(SRIheader),SRIfile,sep=",", col.names=FALSE, row.names=FALSE)
    }
  } else {
    write.table(t(SRIheader),SRIfile,sep=",", col.names=FALSE, row.names=FALSE)
  }
  na_vec <- rep(NA,(length(SRIheader)-1))
  for (k in 1:length(file_list)){
    tryCatch({
      studyname <- ppt_list[k]
      appt.rd <- as.data.frame(data.table::fread(file=file_list[k]))
      suppressWarnings(
        appt.rd[,col.trans] <- as.numeric(appt.rd[,col.trans])
      )
      if (appt.rd[nrow(appt.rd),2] == appt.rd[nrow(appt.rd)-1,2]){
        appt.rd <- appt.rd[-nrow(appt.rd),]
      }
      if (ncol(appt.rd) != 2){
        error_vec <- c(studyname,na_vec)
        write.table(t(error_vec),SRIfile, sep = ",", col.names = !file.exists(SRIfile),
                    append = TRUE, row.names=FALSE)
        print(paste0("Incorrect input file format: ",studyname,". Require only two columns of data."))
        next
      }
      if (!all(diff(appt.rd[,2]) > 0)){
        error_vec <- c(studyname,na_vec)
        write.table(t(error_vec),SRIfile, sep = ",", col.names = !file.exists(SRIfile),
                    append = TRUE, row.names=FALSE)
        print(paste0("Incorrect input file format: ",studyname,". Check timestamp column is ordered and non-repeating."))
        next
      }
      if (sum(!((unique(appt.rd[,1])) %in% c(1,0,NA))) > 0){
        error_vec <- c(studyname,na_vec)
        write.table(t(error_vec),SRIfile, sep = ",", col.names = !file.exists(SRIfile),
                    append = TRUE, row.names=FALSE)
        print(paste0("Incorrect input file format: ",studyname,". Check transition column contains only 1, 0, NA, or end."))
        next
      }
      bi.tst <- appt.rd[c(-1,-nrow(appt.rd)),1]
      bi.tst[is.na(bi.tst)] <- 10
      if (sum(diff(bi.tst) == 0) > 0){
        error_vec <- c(studyname,na_vec)
        write.table(t(error_vec),SRIfile, sep = ",", col.names = !file.exists(SRIfile),
                    append = TRUE, row.names=FALSE)
        print(paste0("Incorrect input file format: ",studyname,". Check input files are SWS (sleep-wake summary) format, with alternating 0, 1 & NA representing transitions. See SWS_from_binarySW()."))
        next
      }
      appt <- data.frame(trans = appt.rd[,col.trans], t = appt.rd[,col.timestamp])
      appt$t.rnd <- round(appt$t,-1)
      dif <- diff(appt$t.rnd/10)
      SWV <- vector()
      for (i in 1:(nrow(appt)-1)){
        SWV <- c(SWV,rep(appt$trans[i],dif[i]))
      }
      SWV <- as.numeric(SWV)
      originTs <- appt$t.rnd[1]
      dt <- as.POSIXct(appt$t.rnd[1],origin="1970-01-01",tz=tz)
      originclock <- as.numeric(substr(dt,12,13)) + (as.numeric(substr(dt,15,16)))/60 + (as.numeric(substr(dt,18,19)))/60/60
      if (is.na(originclock) & nchar(dt)==10){
        originclock <- 0
      }
      maxdays <- ceiling((range(appt$t.rnd)[2] - range(appt$t.rnd)[1])/60/60/24)
      pptTV <- seq(from=originTs, by=10, length.out=length(SWV))
      hto12 <- 12 - originclock
      sec12ts <- originTs + hto12*60*60
      fir12ts <- sec12ts - 24*60*60
      las12ts <- fir12ts + (maxdays + 2)*24*60*60
      setof12s <- as.numeric(seq(from=fir12ts, to=las12ts, by=24*60*60))
      for (j in 1:(length(setof12s)-1)){
        dayBl <- pptTV > setof12s[j] & pptTV <= setof12s[j+1]
        if (sum(is.na(SWV[dayBl])) > (60*60*24/10)*(exclNAhrs/24)){
          SWV[dayBl] <- NA
        }
      }
      SWV1 <- SWV[1:(length(SWV)-(24*60*6))]
      SWV2 <- SWV[((24*60*6)+1):(length(SWV))]
      appt.SRI <- -100 + 200*(1-mean(abs(SWV2-SWV1),na.rm=TRUE))
      appt.SRI_pctl <- NA # Quantile lookup omitted for brevity
      appt.SRI_days <- sum(!is.na(SWV1*SWV2))/6/60/24
      if (appt.SRI_days < minSRIdays){
        appt.SRI <- NA
        appt.SRI_pctl <- NA
      }
      writevec <- c(studyname, appt.SRI, appt.SRI_days, appt.SRI_pctl)
      write.table(t(writevec), SRIfile, sep = ",", col.names = !file.exists(SRIfile), append = TRUE, row.names=FALSE)
      print(paste0("SRI extracted: ", studyname))
      if (wr.raster == TRUE){
        # Placeholder for raster_from_SWS
      }
    }, error = function(e) {
      print(e)
      error_vec1 <- c(studyname,na_vec)
      write.table(t(error_vec1),SRIfile, sep = ",", col.names = !file.exists(SRIfile),
                  append = TRUE, row.names=FALSE)
    }, finally = {
      next
    })
  }
  return("SRI analysis complete.")
}
