pkgname <- "trappingmotion"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('trappingmotion')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("trappingmotion-package")
### * trappingmotion-package

flush(stderr()); flush(stdout())

### Name: trappingmotion-package
### Title: prueba a las 11:45
### Aliases: trappingmotion-package trappingmotion
### Keywords: package

### ** Examples

~~ simple examples of the most important functions ~~



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
