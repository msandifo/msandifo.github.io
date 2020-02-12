`%ni%` = Negate(`%in%`) 


tik_do <-function( my.p, file="test", width=8, height=5, standalone=T, close=T, bg="white" , sanitize=T, console=F){
  
  tikzDevice::tikz(paste0(file,'.tex'), standAlone = standalone,
                   width=width, height=height, bg=bg, 
                   sanitize = sanitize,
                   verbose=T, console=console)
  print(my.p)
  if (close) dev.off()
}


pdf2png <- function(file, open=TRUE, clean=TRUE){
  if ( tools::file_ext(file) =="tex") {
    message(paste("processing",file,"file using tools::texi2dvi" ))
    tools::texi2dvi(file,pdf=T, clean = clean)  
    file.remove( basename(stringr::str_replace(file, ".tex", ".aux") ) )
    file.remove( basename(stringr::str_replace(file, ".tex", ".log")))
    outfile = stringr::str_replace(file, ".tex", ".pdf") %>% str_split("/")  
    file = outfile[[1]][length(outfile[[1]])] 
  }
  outfile = stringr::str_replace(file, ".pdf", ".png")
  system(paste("sips -s format png --padColor FFFFFF --optimizeColorForSharing -s formatOptions 300 ", file,"  --out ", outfile))
  system(paste("convert", outfile," -fill white -opaque none ", outfile))
  if (open==T) system(paste("open ", outfile ))
}
