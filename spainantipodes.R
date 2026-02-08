# Spain and its antipodes in New Zealand
# www.overfitting.net
# https://www.overfitting.net/2026/02/nueva-zelanda-en-nuestras-antipodas-con.html

library(ggmap)  # map_data() provides (long, lat) pairs forming all countries borders
library(data.table)
library(Cairo)  # output antialiasing


# Polygon plotting function provided by ChatGPT
# Draws a series of points based on coordinates as solid polygons
plot_subregion_polygons <- function(df, x = "long", y = "lat", subregion = "subregion",
                                    ord = "order", border = "black", col = NA) {
    
    df <- as.data.frame(df)   # in case df is a data.table
    subs <- unique(df[[subregion]])
    
    for (s in subs) {
        if (is.na(s)) {  # usually main lands (not islands) of each country have subregion=NA
            d <- df[is.na(df[[subregion]]), ]
        } else {
            d <- df[df[[subregion]] == s, ]
        }
        
        d <- d[order(d[[ord]]), ]  # order by column "order" in case they are not
        polygon(d[[x]], d[[y]],          # to properly plot the polygon
                border = border,
                col = col)
    }
}


################################################################################


# READ WORLD AND CAPITALS COORDINATES
DT=data.table(map_data("world"))  # (long, lat) pairs for all countries
DT$long[DT$long>180]=DT$long[DT$long>180]-360  # offset out of range points

SP=DT[DT$region=='Spain']
NZ=DT[DT$region=='New Zealand' & DT$long>0 & DT$lat> -48]  # ignore NZ tiniest far away islands

# Capitals
cities=data.frame(
    city = c("Madrid", "Wellington"),
    long = c(-3.7038, 174.7762),
    lat  = c(40.4168, -41.2865)
)


# MAPS BEFORE REPROJECTION

# Output image dimensions
DIMX=1080
DIMY=1080

CairoPNG("mapreal.png", width=DIMX*1.6, height=DIMY, antialias="subpixel")
    plot(SP$long, SP$lat, pch=".", col='darkred',
         main='Spain and New Zealand',
         xlab="Longitude (º)", ylab="Latitude (º)",
         xlim=c(-25,180), ylim=c(-50,50),
         cex.main = 2, cex.lab = 2, cex.axis = 2,
         asp=1.3)
    points(NZ$long, NZ$lat, pch=".", col='darkgreen')
    abline(h=0, v=0, lty="dotted")
dev.off()


# MAPS WITH NEW ZEALAND REPROJECTION

# Project New Zealand
NZ$long = NZ$long - 180
NZ$lat = -NZ$lat

# Project Wellington too
cities$long[cities$city == "Wellington"] = cities$long[cities$city == "Wellington"] - 180
cities$lat[cities$city == "Wellington"] = -cities$lat[cities$city == "Wellington"]

CairoPNG("mapantipodes.png", width=DIMX, height=DIMY, antialias="subpixel")
    # Empty basic plotting parameters
    plot(NA,
         main = "Spain and its antipodes (New Zealand)",,
         xlab = "Longitude (º)", ylab = "Latitude (º)",
         xlim = range(c(SP$long, NZ$long), na.rm = TRUE),
         ylim = range(c(SP$lat,  NZ$lat),  na.rm = TRUE),
         cex.main = 2, cex.lab = 1.5, cex.axis = 1.5,
         asp = 1.3)
    
    # Solid country maps
    plot_subregion_polygons(SP, border='darkred', col=rgb(1,0,0,0.4))
    plot_subregion_polygons(NZ, border='darkgreen', col=rgb(0,1,0,0.4))
    
    # Plot cities
    points(cities$long, cities$lat, 
           pch = 19,       # solid circle
           cex = 1.5, col = "black")
    text(cities$long, cities$lat,
         labels = cities$city,
         pos = c(4, 4),    # 4 = right of the point
         cex = 1.5, col = "black")
    
    abline(h=0, v=0, lty="dotted")
dev.off()


# NEW ZEALAND AND ITALY COMPARISON

IT=DT[DT$region=='Italy' & is.na(subregion)]  # plot only the Italian boot
NZ$long = NZ$long + 10  # offset NZ to set it closer to Italy

CairoPNG("newzealandvsitaly.png", width=DIMX*1.2, height=DIMY, antialias="subpixel")
    # Empty basic plotting parameters
    plot(NA,
         main = "New Zealand vs Italy (boots competition)",
         xlab = "Longitude (º)", ylab = "Latitude (º)",
         xlim = range(c(IT$long, NZ$long), na.rm = TRUE),
         ylim = range(c(IT$lat,  NZ$lat),  na.rm = TRUE),
         cex.main = 2, cex.lab = 1.5, cex.axis = 1.5,
         asp = 1.3)
    
    # Solid country maps
    plot_subregion_polygons(IT, border='darkred', col=rgb(1,0,0,0.4))
    plot_subregion_polygons(NZ, border='darkgreen', col=rgb(0,1,0,0.4))
dev.off()
