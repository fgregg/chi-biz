library(slam)
library(topicmodels)

businesses <- read.csv("../data/geocoded_businesses.csv")
sic <- read.csv("../data/sic.csv")

businesses$zip5 <- factor(substr(businesses$ZIP, 1, 5))
businesses$sic <- sic[na.omit(match(businesses$SIC, sic$code)), "name"]

zip_sic_M <- slam::as.simple_triplet_matrix(table(businesses$zip5,
                                                  businesses$sic))

model_1 <- topicmodels::LDA(zip_sic_M, 10)

topicmodels::terms(model_1, 10)

