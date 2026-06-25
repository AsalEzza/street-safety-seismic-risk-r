# Read data
streets = vect('data/raw data/StreetCenterlines.shp')
shore   = vect('data/raw data/Shoreline.shp')
hz      = vect('data/raw data/SeismicHazardZones.shp')

trees_df = read.csv('data/raw data/Street_Tree_Map.csv')
trees = vect(trees_df, geom=c('Longitude','Latitude'), crs='EPSG:4326')

usa = ne_countries(country='United States of America', returnclass='sv')

# Merge BEFORE projection
hz_data = read.csv('data/raw data/Seismic_Hazard_Zones_Data.csv')
hz = merge(hz, hz_data, by.x='id', by.y='GEOID')

# Projection
streets_ca = project(streets, "EPSG:3310")
shore_ca   = project(shore, "EPSG:3310")
hz_ca      = project(hz, "EPSG:3310")
trees_ca   = project(trees, "EPSG:3310")

# Crop
streets_ext = ext(streets_ca)
shore_ca = crop(shore_ca, streets_ext)
trees_ca = crop(trees_ca, streets_ext)

# Filter trees
trees_big = trees_ca[!is.na(trees_ca$DBH) &
                       trees_ca$DBH > 48 &
                       trees_ca$DBH < 384, ]

# Filter hazard
hz_ca$Zone_Type = trimws(hz_ca$Zone_Type)
liquid = hz_ca[hz_ca$Zone_Type == "Liquefaction", ]

# Intersect + buffer
trees_liquid = intersect(trees_big, liquid)
trees_100 = buffer(trees_liquid, width=66)

danger_zone = aggregate(trees_100, dissolve=TRUE)

# Streets danger
streets_ca$in_danger = lengths(intersect(streets_ca, danger_zone)) > 0

streets_ca$status = ifelse(streets_ca$in_danger,
                           'Danger! Avoid!',
                           'Have a nice walk :)')

streets_ca$status = factor(streets_ca$status)

plot(streets_ca, "status",
     col=c('#ba001e','grey50'),
     main='Street Safety Status Under Seismic Hazards',
     axes=FALSE)



