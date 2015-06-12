library(slam)
library(topicmodels)
library(sp)
library(rgdal)
library(rgeos)

businesses <- read.csv("../data/geocoded_businesses.csv")
sic <- read.csv("../data/sic.csv")

businesses <- businesses[!is.na(businesses$x_coord),]

addresses <- strsplit(as.character(businesses$Address), " ")

businesses$block <- sapply(addresses,
                           FUN=function(x) {
                               paste(c(substr(x[1],
                                              1,
                                              nchar(x[1]) -1),
                                       x[-1]),
                                     collapse=' ')
                           })

businesses$sic <- sic[match(businesses$SIC, sic$code), "name"]

businesses <- sp::SpatialPointsDataFrame(businesses[, c("x_coord",
                                                        "y_coord")],
                                         businesses)
proj4string(businesses) <- CRS("+proj=longlat")

EPSG.102671 <- "+proj=tmerc +lat_0=36.66666666666666 +lon_0=-88.33333333333333 +k=0.9999749999999999 +x_0=300000 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"

businesses <- sp::spTransform(businesses, sp::CRS(EPSG.102671))

resolution <- 1000
grid_origin <- apply(sp::coordinates(businesses), FUN=min, MARGIN=2)
grid_count <- ceiling(apply(sp::coordinates(businesses),
                            FUN=max, MARGIN=2)
               - grid_origin)/resolution

grid <- SpatialGrid(GridTopology(grid_origin,
                                 c(resolution, resolution),
                                 grid_count))

# Use rgeos to calcualte the smaller clusters
                              

zip_sic_M <- slam::as.simple_triplet_matrix(table(businesses$block,
                                                  businesses$sic))

model_1 <- topicmodels::LDA(zip_sic_M, 10)

topicmodels::terms(model_1, 10)

