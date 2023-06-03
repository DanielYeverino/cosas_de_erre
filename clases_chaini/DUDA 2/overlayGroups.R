library(raster)
library(leaflet)

#load in shapefiles, trim excess data
gtm <- getData('GADM', country = 'GTM', level = 0)
gtm <- gtm[, -c(2:68)]

mex <- getData('GADM', country = 'MEX', level = 0)
mex <- mex[, -c(2:68)]


leaflet() %>%
  addTiles() %>% 
  addLayersControl(overlayGroups  = c("gtm", "mex"), 
                   options = layersControlOptions(collapsed = F)) %>% 
  addPolygons(data = gtm, 
              fillColor = 'red', 
              group = "gtm") %>% 
  addLegend(color = "red", 
            labels = "a", 
            group = "gtm", 
            position = "bottomleft") %>%   
  addPolygons(data = mex, 
              fillColor = 'blue', 
              group = "mex") %>% 
  addLegend(color = "blue", 
            labels = "b", 
            group = "mex", 
            position = "bottomright") 
