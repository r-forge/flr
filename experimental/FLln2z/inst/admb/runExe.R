cd /home/lkell/flr/experimental/FLSeine/admb
export ADMB_HOME='/usr/local/admb/'
sudo ln -s /usr/local/admb/bin/adlink /usr/local/bin/adlink

readADMB=function (file) {
    dat <- scan(file, what = "", sep = "\n", skip = 1)
    vals <- lapply(strsplit(dat[grep("#", dat, invert = TRUE)], " "), function(x) as.numeric(x[nchar(x) > 0]))
    names(vals) <- lapply(grep("#", dat, value = T), function(x) substr(x, 3, nchar(x)))
    
    return(vals)}
    

