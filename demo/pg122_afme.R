#----------------------------------------------------------
#3. FUNDAMENTALS OF SPATIAL RANDOM PROCESSES
#---------------------------------------------------------

#----------------------------------------------------------
#3.5. SPECTRAL REPRESENTATION OF TEMPORAL PROCESSES
#---------------------------------------------------------

#----------------------------------------------------------
#3 Accountig for measurement error
#Figure 4.1
#pg 122
#----------------------------------------------------------
countriesNames <- c("Argentina", "Brazil", "Bolivia", "Canada", "Chile", "Colombia", "Costa Rica", "Cuba", "Dominican Republic", "Ecuador", "French Guiana", "Guatemala", "Guyana", "Haiti", "Honduras", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru", "Puerto Rico", "Suriname", "Uruguay", "USA", "Venezuela")
countries.map <- map("world", countriesNames, fill = TRUE, col="transparent", plot = FALSE)
IDs <- sapply(strsplit(countries.map$names, ":"), function(x) x[1])
countries.sp <- map2SpatialPolygons(countries.map, IDs=IDs,proj4string=CRS("+proj=longlat +datum=WGS84"))
countriesNames.df <- data.frame(sapply(1:length(countries.sp), function(n){slot(slot(countries.sp[n], "polygons")[[1]], "ID")}, simplify = TRUE))
countries.spdf <- SpatialPolygonsDataFrame(countries.sp, countriesNames.df, match.ID = FALSE)
lat <- seq(from = (2.8 * 3)/2, to = 40, by = 2.8 * 3)
lat <- sort(c(lat, -1 * lat))
lon <- lat - 85
coords <- cbind(rep(lon, each = 10), rep(lat, times = 10))
samplepoints.spdf <- SpatialPointsDataFrame(coords = coords, data = as.data.frame(1:100), proj4string = slot(countries.spdf, "proj4string"))
spplot(countries.spdf, xlim = c(-130, -40), ylim = c(-40, 40), sp.layout = list(sp.points, samplepoints.spdf), fill = FALSE, main = "Figure 4.1", xlab = "Longitude", ylab = "Latitude", scales = list(draw = TRUE))

