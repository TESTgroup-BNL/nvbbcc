####################################################################################################
#   BNL UAS COGS example. Sourcing data from the LBNL ESS-DIVE Tier2 Storage
#
#   Example data source:
#   URL: https://download.ess-dive.lbl.gov/doi-1234-test/
#
#
#  	--- Last updated:  11.07.2022 by Shawn P. Serbin <sserbin@bnl.gov>
####################################################################################################


#--------------------------------------------------------------------------------------------------#
#### Load R libraries
list.of.packages <- c("rgdal","raster", "terra", "sp", "here","RColorBrewer")
invisible(lapply(list.of.packages, library, character.only = TRUE))
`%notin%` <- Negate(`%in%`)
here::here()
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### output location
output_dir <- file.path(here::here("R_Output/ESS-Dive_example"))
if (! file.exists(output_dir)) dir.create(output_dir,recursive=TRUE)
outdir <- file.path(path.expand(output_dir))
setwd(outdir) # set working directory
getwd()  # check wd
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
# setup datasets. Link to COG as a virtual raster

### Kougarok_20180725_Flight6
essdive_url <- "https://download.ess-dive.lbl.gov/doi-1234-test/Yang_et_al_UAS_Manuscript_2_Data/Kougarok_20180725_Flight6/Level_2/"
essdive_vr_url <- paste0("/vsicurl/",essdive_url)

# RGB
# by appending /vsicurl/ we create a virtual raster
rgb_raster_name <- "NGEEArctic_UAS_Kougarok_20180725_Flight6_RGB_cog.tif"

# CHM
# by appending /vsicurl/ we create a virtual raster
chm_raster_name <- "NGEEArctic_UAS_Kougarok_20180725_Flight6_CHM_cog.tif"

# TIR
tir_raster_name <- "NGEEArctic_UAS_Kougarok_20180725_Flight6_TIR_cog.tif"

# grab some example locations
example_kg_points <- rgdal::readOGR(file.path(here::here(),
                                              "shapefiles","ngeearctic"),"kg_example_points")
plot(example_kg_points)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
# view raster info / plot raster

# RGB
rgb_ras <- rast(paste0(essdive_vr_url,rgb_raster_name))
RGB(rgb_ras) <- c(1,2,3)
rgb_ras

pdf(file.path(outdir,gsub(".tif",".pdf",rgb_raster_name)), height=9, width=11)
raster::plotRGB(rgb_ras, r=1, g=2, b=3, stretch="lin")
box(lwd=2.2)
dev.off()

# CHM
chm_ras <- rast(paste0(essdive_vr_url,chm_raster_name))
names(chm_ras) <- "CHM_meters"
chm_ras

pdf(file.path(outdir,gsub(".tif",".pdf",chm_raster_name)), height=9, width=11)
terra::plot(chm_ras, legend=TRUE, axes=TRUE, smooth=FALSE,range=c(0,315),
            col=topo.colors(35), plg=list(x="topright",cex=1,title="CHM (m) x 100"),
            pax=list(sides=c(1,2), cex.axis=1.2),
            mar=c(2,2,3,5.5)) # b, l, t, r
box(lwd=2.2)
dev.off()

# TIR
tir_ras <- rast(paste0(essdive_vr_url,tir_raster_name))
names(tir_ras) <- "Tsurf_degCx10"
tir_ras

pdf(file.path(outdir,gsub(".tif",".pdf",tir_raster_name)), height=9, width=11)
terra::plot(tir_ras, legend=TRUE, axes=TRUE, smooth=TRUE,range=c(170,300),
            col=rev(brewer.pal(11,"RdYlBu")), 
            plg=list(x="topright",cex=1,title="Tsurf (deg C) x 10"),
            pax=list(sides=c(1,2), cex.axis=1.2),
            mar=c(2,2,3,5.5)) # b, l, t, r
box(lwd=2.2)
dev.off()
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
# extract data example
pdf(file.path(outdir,paste0(gsub(".tif","",rgb_raster_name),"_points.pdf")), height=9, width=11)
raster::plotRGB(rgb_ras, r=1, g=2, b=3, stretch="lin")
box(lwd=2.2)
points(example_kg_points, pch=20, col="blue", cex=2)
dev.off()

pdf(file.path(outdir,paste0(gsub(".tif","",chm_raster_name),"_points.pdf")), height=9, width=11)
terra::plot(chm_ras, legend=TRUE, axes=TRUE, smooth=FALSE,range=c(0,315),
            col=topo.colors(35), plg=list(x="topright",cex=1,title="CHM (m) x 100"),
            pax=list(sides=c(1,2), cex.axis=1.2),
            mar=c(2,2,3,5.5)) # b, l, t, r
box(lwd=2.2)
points(example_kg_points, pch=20, col="black", cex=2)
dev.off()


# extract data
chm.data <- terra::extract(chm_ras, terra::vect(example_kg_points))
chm.data[,2] <- chm.data[,2]*0.01
tir.data <- terra::extract(tir_ras, terra::vect(example_kg_points))
tir.data[,2] <- tir.data[,2]*0.1
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
# plot point data
hist(chm.data[,2],freq=T, xlab="CHM (meters)",main="")

pdf(file.path(outdir,"CHM_hist.pdf"), height=6, width=12)
par(mar=c(5,5,1,1)) #b, l, t, r
hist(chm.data[,2],freq=T, xlab="CHM (meters)",main="", cex.axis=2,
     cex.lab=1.5)
box(lwd=2.2)
dev.off()

hist(tir.data[,2],freq=T, xlab="Tsurf (degC)",main="")

pdf(file.path(outdir,"CHM_vs_TIR.pdf"), height=9, width=11)
par(mar=c(5,5,1,1)) #b, l, t, r
plot(chm.data[,2],tir.data[,2], ylab="Tsurf (deg C)", xlab="CHM (meters)",
     cex.axis=1.4,cex.lab=1.8, pch=21, bg="grey50", cex=2)
box(lwd=2.2)
dev.off()

#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
# example subset
site.area <- cbind(lon = c(508210, 508260), 
                   lat = c(7226820.0,7226750.0))
site.area <- terra::vect(site.area, type="polygon", crs="+init=epsg:32603") 
site.area

# this may take awhile
sub.ras <- terra::crop(rgb_ras, site.area)

pdf(file.path(outdir,paste0(gsub(".tif","",rgb_raster_name),"_subset.pdf")), height=9, width=11)
raster::plotRGB(sub.ras, r=1, g=2, b=3, stretch="lin")
box(lwd=2.2)
dev.off()
#--------------------------------------------------------------------------------------------------#

### EOF