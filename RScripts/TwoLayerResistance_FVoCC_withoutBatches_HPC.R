##Load packages
library("sp", lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/") #error when loading raster
library("iterators",lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/") # Otherwise error when loading doParallel
library("codetools",lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/") # Otherwise error when loading raster
library("sf",lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/")# Otherwise error when loading gdalUtilities
library("dplyr", lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/")
library('raster',lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/")
library('igraph',lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/")
library('Matrix',lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/")
library('rgdal',lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/")#Otherwise rgeos and gdistance package cannot be loaded
library('gdistance',lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/")
library('rgeos',lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/")
library('foreach',lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/")
library('doParallel',lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/")
library('snow',lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/")
library("snowfall", lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/")
library('data.table',lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/")

##Set climate bin width.
bin.width <- 0.25   
## +- 0.25 degrees C 
## total bin diameter = 0.5 degrees C 

cost.penalty <- 2 ## Two penalty units per degree C dissimilarity from temperature of interest

## Get current and future climate and resistance raster
pre1 <- raster('/vsc-hard-mounts/leuven-data/348/vsc34871/VoCC/Input/EU_CHELSAbio1_EPSG3035_1981-2010.tif'); names(pre1) <- 'pre1' ## Mean annual temperature (1995); units are degrees C 
fut30 <- raster('/vsc-hard-mounts/leuven-data/348/vsc34871/VoCC/Input/ssp126_EU_CHELSAbio1_EPSG3035.tif'); names(fut30) <- 'fut30' ## Mean annual temperature (2085); units are degrees C 
#resistance.mask <- raster('/vsc-hard-mounts/leuven-data/348/vsc34871/VoCC/Input/resistance.mask_EU_CHELSAbio1_ssp126.tif')

####Create a stack for only pre and fut data, so 2 layers.
the.stack_ssp126 <-  stack(pre1,fut30)
plot(the.stack_ssp126)

####Create initial climate data frame.####
clim_ssp126 <- the.stack_ssp126 %>%
  getValues() %>%
  data.frame(cid = 1:ncell(the.stack_ssp126)) %>%
  na.omit()


clim_ssp126[,c("x","y")] <- xyFromCell(the.stack_ssp126, clim_ssp126$cid)

##Round the pre1 and fut30 to keep 1 decimal.
colnames(clim_ssp126)[1] <- "pre1" #Change the column name of data frame
colnames(clim_ssp126)[2] <- "fut30"
clim_ssp126$pre1 <- round(clim_ssp126$pre1,1) 
clim_ssp126$fut30 <- round(clim_ssp126$fut30, 1)
head(clim_ssp126)

##Create a data table summarize all the different current temperature.
clim_ssp126 <- as.data.table(clim_ssp126)
#preT <- clim_ssp126[, list(cid = length(unique(cid))), 
#                    by = list(pre1)]
#setnames(preT, "pre1", "MAT_pre");setnames(preT, "cid", "number of cells")
#preT <- preT[order(preT$MAT_pre, decreasing = TRUE), ]
#sum(preT$'number of cells')#Check if the total number is correct
#preT <- preT[preT$MAT_pre + bin.width >= minValue(fut30)]
#head(preT)

####Create resistance mask.
#resistance.mask <- pre1 # initial set up of the resistance mask
#resistance.mask[resistance.mask < 5000] <- 1 # convert all pixels to a value = 1
#resistance.mask <- distance(resistance.mask) # calcualte distance from pixels that are not NA
#resistance.mask[resistance.mask > 10000] <- NA # convert all pixels > 10,000 m from study area boundary to NA
#resistance.mask[resistance.mask >= 0] <- 1 # convert all values = 1 for simplicity

##Define input indices
n=1
geoTol = 180000
tdiff = 90
methods = "Single"

##ForLoop
dat <- na.omit(data.table(clim_ssp126))
# matrix with the future climatic values for all cells
fut <- dat[, seq(2, (2*n), by = 2), with=FALSE]

# set things up for parallel processing
cores = detectCores()
ncores = cores[1]-1
cuts <- cut(1:nrow(dat), ncores, labels = FALSE)
cl <- makeCluster(ncores)
registerDoParallel(cl)

result <- foreach(x = 1:ncores, .combine = rbind, 
                  .multicombine = TRUE) %dopar% { #Change the raster package to terra.
                    library("sp", lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/") #error when loading raster
                    library("iterators",lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/") # Otherwise error when loading doParallel
                    library("codetools",lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/") # Otherwise error when loading raster
                    library('raster',lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/")
                    library('igraph',lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/")
                    library('Matrix',lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/")
                    library('rgdal',lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/")
                    library('gdistance',lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/")
                    library('rgeos',lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/")
                    library('foreach',lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/")
                    library('doParallel',lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/")
                    library('snow',lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/")
                    library("snowfall", lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/")
                    library('data.table',lib.loc = "/vsc-hard-mounts/leuven-data/348/vsc34871/Rlib/4.0.2-foss-2018a/")
                    a <- x
                    Dat <- dat[cuts == a,] #Cut the data table into different parts to do parallel calculation in different cores.
                    
                    resu <- data.table(focal = Dat$cid, from.x = Dat$x, from.y = Dat$y, target = as.integer(NA), to.x = as.double(NA), to.y = as.double(NA), climDis = as.double(NA), geoDis_m = as.double(NA), ang = as.double(NA), vel_km_yr = as.double(NA))
                    i <- 0
                    while(i <= nrow(Dat)){
                      i <- i+1
                      # for each focal cell subset target cell analogues (within ClimTol)
                      pres <- as.numeric(Dat[i, seq(1, (2*n), by = 2), with=FALSE])
                      dif <- data.table(sweep(fut, 2, pres, "-"))
                      # Identify future analogue cells
                      # Ohlemuller et al 2006 / Hamann et al 2015
                      upper = colnames(dif)
                      l <- lapply(upper, function(x) call("<", call("abs", as.name(x)), bin.width[grep(x, colnames(dif))]))
                      ii = Reduce(function(c1, c2) substitute(.c1 & .c2, list(.c1=c1, .c2=c2)), l)
                      anacid <- dat$cid[dif[eval(ii), which=TRUE]]  # cids analogue cells
                      
                      # LOCATE CLOSEST ANALOGUE
                      if(length(anacid)>0){
                        # check which of those are within distance and get the analogue at minimum distance
                        #Create trans layer
                        # temp.index <- 1 #Trial test
                        #temp <- Dat$pre1[i]
                        
                        #for (k in 1:2) { assign(paste('t', k, '.rr', sep=''), (abs(the.stack_ssp126[[k]] - (temp)) * cost.penalty) + 1) }
                        
                        ## The below block of code ensures that any pixel identified as a climate analog (+- 0.25 degrees C)
                        ## is given a resistance value = 1. By definition, climate analogs should have the least resistance.
                        #for (k in 1:2) { 
                        #  rr.raster <- get(paste('t', k, '.rr', sep=''))
                        #  rr.raster[the.stack_ssp126[[k]] >= (temp - bin.width) & the.stack_ssp126[[k]] <= (temp + bin.width)] <- 1
                        #  assign(paste('t', k, '.rr', sep=''), rr.raster)
                        #}
                        
                        ##################################################
                        ## End: create intermediate resistance rasters  ##
                        ##################################################
                        
                        ###############################################################################
                        ## Create final resistance raster for temperature increment of interest  ##
                        ###############################################################################
                        
                        #resistance.raster <- (t1.rr+t2.rr)/2
                        
                        ####################################################################################
                        ## End: Create final resistance raster for for temperature increment of interest  ##
                        ####################################################################################
                        
                        ###################################################################################################
                        ## Using the previously created resistance mask, assign all pixels > 10,000 m from study area a  ##
                        ## value 5000, which is a very high resistance value intented to prevent the LCP alogrithm from  ##
                        ## selecting those pixels as the optimal path.                                                   ## 
                        ###################################################################################################
                        #resistance.raster[is.na(resistance.raster)] <- 5000 
                        #resistance.raster <- mask(resistance.raster, resistance.mask)
                        #writeRaster(resistance.raster, filename = "C:/Users/u0142858/OneDrive - KU Leuven/KUL/PhD/My Project/WP1_Mapping_CB/OutputsVoCC/OutputTIF/OutputSSP126/resistanceRaster10_4.tif", overwrite=TRUE)
                        ##plot(resistance.raster) gives a buffering zone around the study area. Does the width of the buffering zone is 10 km?
                        ##################################################################################
                        ## Create a transition object (see 'gdistance' package) from resistance raster  ##
                        ##################################################################################
                        #f <- function(x) 1/mean(x)
                        #trans <- transition(resistance.raster, transitionFunction=f, directions=8) 
                        #trans <- geoCorrection(trans) 
                        ####load raster layers####
                        trans <- readRDS(paste0('/lustre1/scratch/348/vsc34871/output/TransitionLayers/ssp126/RDS/Trans_ssp126_EU_',pres,'.rds'))
                        
                        ####Calculate cost distance####
                        
                        #######################################################################################
                        ## Create dataframe identifying all pixels with the current temperature of interest  ##
                        #######################################################################################
                        
                        from.xy <- Dat[,cbind(Dat$x[i],Dat$y[i])]
                        from.xy <- as.matrix(from.xy)
                        
                        
                        ###############################################################################
                        ## Create dataframe identifying all pixels with the matching climate analog  ##
                        ###############################################################################
                        
                        
                        to.xy <- dat[,cbind(dat$x[dat$cid %in% anacid], dat$y[dat$cid %in% anacid])]
                        to.xy <- as.matrix(to.xy)
                        
                        costdist <- round(costDistance(trans, cbind(Dat$x[i],Dat$y[i]), cbind(dat$x[dat$cid %in% anacid], dat$y[dat$cid %in% anacid])))
                        #cost.distance <- do.call('rbind', cost.distance)
                        cost.distance1 <- as.data.frame(costdist) 
                        out.lcd <- as.data.frame(apply(cost.distance1, 1, min))
                        names(out.lcd) <- 'cost.dist'
                        #Create if else condition for analogue pixels that are in another fragmented land requiring organisms to cross sea.
                        if(out.lcd$cost.dist == 0) {
                          resu[i, target := NA]   # cid of geographically closest climate analogue
                          resu[i, climDis := NA]  # mean clim difference for the closest analogue
                          resu[i, geoDis_m := NA]
                          #resu[i, ang := geosphere::bearing(Dat[i, c("x","y")], dat[cid == resu[i, target], c("x","y")])] #If projection is in degree, you can calculate the angle.
                          resu[i, vel_km_yr := NA]
                        } else {
                        from.index <- which(out.lcd[1,] == cost.distance1[1,])	
                        out <- data.frame(to.x = as.double(NA), to.y = as.double(NA))
                        if (length(from.index) > 1) {
                          from.index[1]
                          out$to.x <- to.xy[from.index[1],][1]
                          out$to.y <- to.xy[from.index[1],][2]
                          #return(c(to.x, to.y))
                        } else {
                          out$to.x <- to.xy[from.index,][1]
                          out$to.y <- to.xy[from.index,][2]
                          #return(c(to.x, to.y))
                        }
                        resu$to.x[i] <- out[1,1]
                        resu$to.y[i] <- out[1,2]
                        #SL <- gdistance::shortestPath(trans, cbind(Dat$x[i],Dat$y[i]), cbind(dat$x[dat$cid %in% anacid], dat$y[dat$cid %in% anacid]), output="SpatialLines")
                        SL1 <- gdistance::shortestPath(trans, cbind(resu$from.x[i],resu$from.y[i]), cbind(resu$to.x[i], resu$to.y[i]), output="SpatialLines")
                        d <- SpatialLinesLengths(SL1)    # in km for longlat TRUE
                        # correct for analogues that are within search distance but have no directed path with the focal cell (i.e. conductivity = 0)
                        d[which(d == 0 & anacid != Dat$cid[i])] <- NA
                        
                        an <- anacid[d < geoTol]       # cids analogue cells within search radius
                        dis <- d[d < geoTol]      # distance to candidate analogues
                        if (length(an) > 0){
                          resu[i, target := an[which.min(dis)]]   # cid of geographically closest climate analogue
                          resu[i, climDis := mean(as.numeric(dif[which(anacid == resu[i, target]),]))]  # mean clim difference for the closest analogue
                          resu[i, geoDis_m := min(dis)]
                          #resu[i, ang := geosphere::bearing(Dat[i, c("x","y")], dat[cid == resu[i, target], c("x","y")])]
                          resu[i, vel_km_yr := (resu$geoDis_m[i]/1000)/tdiff]
                        }}
                      }
                    }
                    return(resu)
                  }
stopCluster(cl)



#Plot the results of avocc1.
r1 <- raster(the.stack_ssp126)
r1[result$focal] <-  result$vel_km_yr
plot(r1)

write.csv(result, paste('/vsc-hard-mounts/leuven-data/348/vsc34871/VoCC/Output/', 'FVoCC_TwolayersResist_geoTol180km_EU_CHELSA_EPSG3035_', '.csv', sep=''), row.names=F)
writeRaster(r1, filename = "/vsc-hard-mounts/leuven-data/348/vsc34871/VoCC/Output/FVoCC_TwolayersResist_geoTol180km_EU_CHELSA_EPSG3035_ssp126_.tif", format="GTiff", overwrite=TRUE)
