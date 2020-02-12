## slab directory

slab.model <-"slab2"

slab.dir ="/Volumes/data/data/global/slabs/"

slab.grd.dir <- paste0( slab.dir, slab.model, "/allslabs" )

slab.dep.files <- list.files(slab.grd.dir, pattern="_dep_", full.names=T)
slab.str.files <- list.files(slab.grd.dir, pattern="_str_", full.names=T)
slab.dip.files <- list.files(slab.grd.dir, pattern="_dip_", full.names=T)
slab.unc.files <- list.files(slab.grd.dir, pattern="_unc_", full.names=T)
slab.thk.files <- list.files(slab.grd.dir, pattern="_thk_", full.names=T)

raster::raster(slab.dep.files[12])
