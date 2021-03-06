# Plot Ravelry knitting patterns category 
# as a tree (nested rectangles)

library(treemap)
library(reshape2)

# Nested list representing the knitting pattern breakdown by category
# Coresponds to by craft=knitting and has-photo=True: 360856 hits in August 2015
# Data entered by hand from Ravelry public information

patternCatTree <- list("Clothing"=list("Coat/Jacket"=10707,
                                         "Dress"=5232,
                                         "Intimate Apparel"=list("Bra"=35,
                                                                 "Pasties"=10,
                                                                 "Underwear"=152,
                                                                 "Other"=69),
                                         "Leggings"=332,
                                         "Onesies"=905,
                                         "Pants"=1107,
                                         "Robe"=104,
                                         "Shorts"=330,
                                         "Shrug/Bolero"=3939,
                                         "Skirt"=1660,
                                         "Sleepwear"=354,
                                         "Soakers"=470,
                                         "Sweater"=list("Cardigan"=31580,
                                                        "Pullover"=41795,
                                                        "Other"=1088),
                                         "Swimwear"=135,
                                         "Tops"=list("Sleeveles Top"=7195,
                                                     "Strapless Top"=130,
                                                     "Tee"=3604,
                                                     "Other"=550),
                                         "Vest"=9256,
                                         "Other"=767),
                         "Accesories"=list("All Bag"=8883,
                                           "Belt"=271,
                                           "Feet/Legs"=list("Booties"=3501,
                                                            "Legwarmers"=1801,
                                                            "Slippers"=2188,
                                                            "All Socks"=23631,
                                                            "Spats"=89,
                                                            "Other"=540),
                                           "All Hands"=23733,
                                           "All Hat"=45386,
                                           "All Jewelry"=1642,
                                           "Neck/Torso"=list("Bib"=364,
                                                             "Cape"=1591,
                                                             "Collar"=921,
                                                             "Cowl"=17798,
                                                             "Necktie"=362,
                                                             "Poncho"=2604,
                                                             "Scarf"=26600,
                                                             "Shawl/Wrap"=26210,
                                                             "Other"=691),
                                           "All Other Headwear"=3989,
                                           "Other"=1277),
                         "All Home"=36478,
                         "All Toys and Hobbies"=21023,
                         "All Pet"=1551,
                         "All Components"=7073)

# Make a data frame from the nested list
patternCatTree <- melt(patternCatTree)
# Warning: tree depth will increase for more complete breakdown, may be more than 3 levels
names(patternCatTree) <- c("patterns_count", "cat3", "cat2", "cat1")

# R default tree colors are ugly 
customPalette = c("#FAF0E6", "#7FFFD4", "#008080", "#FF6347", "#FA8072", "#B0C4DE")
# Plot tree as nested rectangles
treemap(patternCatTree, index=c("cat1", "cat2", "cat3"), vSize="patterns_count",
        title="Number of knitting patterns per category",
        palette=customPalette, border.col="White", border.lwds=c(6,3,0.5),
        bg.labels=0, fontsize.labels=c(16,16,11),
        align.labels=list(c("left", "top"), c("center", "center"), c("center", "center")))

