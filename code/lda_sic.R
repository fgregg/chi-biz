library(slam)
library(topicmodels)
library(sp)
library(rgdal)

EPSG.102671 <- "+proj=tmerc +lat_0=36.66666666666666 +lon_0=-88.33333333333333 +k=0.9999749999999999 +x_0=300000 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"

spatialize <- function(businesses) {
    businesses <- businesses[!is.na(businesses$x_coord),]
    businesses <- sp::SpatialPointsDataFrame(businesses[, c("x_coord",
                                                            "y_coord")],
                                             businesses)
    proj4string(businesses) <- CRS("+proj=longlat")
    businesses <- sp::spTransform(businesses, sp::CRS(EPSG.102671))
    return(businesses)
}


blockIDs <- function(addresses) {
    addresses <- strsplit(as.character(addressses), " ")

    block <- sapply(addresses,
                    FUN=function(x) {
                        paste(c(substr(x[1],
                                       1,
                                       nchar(x[1]) -1),
                                x[-1]),
                              collapse=' ')
                    })
    return(block)
}

fishnet <- function(points, resolution=1000) {
    coords = coordinates(points)
    
    grid_origin <- apply(coords, FUN=min, MARGIN=2)
    grid_count <- ceiling(apply(coords,
                                FUN=max, MARGIN=2)
                          - grid_origin)/resolution

    grid <- sp::SpatialGrid(sp::GridTopology(grid_origin,
                                             c(resolution, resolution),
                                             grid_count))
    proj4string(grid) <- sp::CRS(EPSG.102671)

    covered_cells <- na.omit(unique(sp::over(points, grid)))
    
    grid <- sp::SpatialPixels(sp::SpatialPoints(grid))

    grid <- grid[covered_cells]

    proj4string(grid) <- sp::CRS(EPSG.102671)

    return(grid)
}

businesses <- read.csv("../data/geocoded_businesses.csv")
sic <- read.csv("../data/sic.csv")

businesses <- spatialize(businesses)

chi_grid <- fishnet(businesses, 2000)

businesses$grid <- sp::over(businesses, chi_grid)

businesses$sic <- sic[match(businesses$SIC, sic$code), "name"]

grid_sic_M <- slam::as.simple_triplet_matrix(table(businesses$grid,
                                                   businesses$sic))

model_1 <- topicmodels::LDA(grid_sic_M, 10)

topicmodels::terms(model_1, 10)

chi_grid <- sp::SpatialPixelsDataFrame(chi_grid,
                                       data.frame(topic=topicmodels::topics(model_1)))

image(chi_grid['topic'], col=palette())

