x <- c('raster', 'sf','dplyr','ggplot2')
lapply(x, require, character.only = TRUE)
setwd('C:\\Users\\basti\\Documents\\GitHub\\landloss')
dir1 <- "C:/Users/basti/Box/LandLossLouisiana/"
main_folder1 <- "C:\\Users\\basti\\Documents\\GitHub\\landloss\\Data\\LCMap\\LC\\"
main_folder2 <- "C:\\Users\\basti\\Documents\\GitHub\\landloss\\Data\\LCMap\\LCChange\\"




## Section 1 Read Land Cover Files (start)
    data_path<- list.dirs(main_folder1,recursive=TRUE)

    # Find all .tif files in the folder
    file_list <- list.files(data_path, pattern = "\\.tiff$", full.names = TRUE)

    # Create an empty list to store the raster data
    raster_list <- list()

    # Loop through the file list and read each raster file and associated files
    for (i in seq_along(file_list)) {
        raster_data <- raster(file_list[i],RAT=TRUE)
        #1 Developed 2 Cropland 3 Grass/Shrub 4 Tree Cover 5 Water 6 Wetland 7 Ice/Snow 8 Barren
        #plot(raster_data)
        #values(raster_data) <- as.numeric(raster::getValues(raster_data ))

        raster_list[[i]] <- raster_data
        if(i==1){
            raster_layers <- raster_data
            water_layers <- raster_data
            values(water_layers)[values(water_layers) != 5 ] = NA
        }else{
            raster_layers <- stack(raster_layers,raster_data)
            raster_data_water <- raster_data
            values(raster_data_water )[values(raster_data_water ) != 5 ] = NA
            water_layers <- stack(water_layers,raster_data_water)
        }
        print(i)
    }
    stackSave(water_layers, "WaterLayers1985_2021")
    writeRaster(water_layers, "Data/WaterLayers1985_2021.grd", , bandorder='BIL', overwrite=TRUE)

## Section 1 Read Land Cover Files

## Section 2 Read Land Cover Change Files (start)
    data_path<- list.dirs(main_folder2,recursive=TRUE)

    # Find all .tif files in the folder
    file_list <- list.files(data_path, pattern = "\\.tiff$", full.names = TRUE)

    # Create an empty list to store the raster data
    raster_list_change <- list()

    # Loop through the file list and read each raster file and associated files
    for (i in seq_along(file_list)) {
        raster_data <- raster(file_list[i],RAT=TRUE)
        #1 Developed 2 Cropland 3 Grass/Shrub 4 Tree Cover 5 Water 6 Wetland 7 Ice/Snow 8 Barren
        #plot(raster_data)
        #values(raster_data) <- as.numeric(raster::getValues(raster_data ))

        raster_list_change[[i]] <- raster_data
        # if(i==1){
        #     raster_layers <- raster_data
        #     water_layers <- raster_data
        #     values(water_layers)[values(water_layers) != 5 ] = NA
        # }else{
        #     raster_layers <- stack(raster_layers,raster_data)
        #     raster_data_water <- raster_data
        #     values(raster_data_water )[values(raster_data_water ) != 5 ] = NA
        #     water_layers <- stack(water_layers,raster_data_water)
        # }
        print(i)
    }
    #stackSave(water_layers, "WaterLayers1985_2021")
    #writeRaster(water_layers, "Data/WaterLayers1985_2021.grd", , bandorder='BIL', overwrite=TRUE)

## Section 2 Read Land Cover Change Files


## Section 3 Extract Land Cover by Parish (start)


    library(tidycensus)
    #census API: 99d02cc7192f34a7eb0ad14e97cca22bd456d3ab
    # census_api_key("99d02cc7192f34a7eb0ad14e97cca22bd456d3ab", install=TRUE,overwrite=TRUE)
    # totpop_county <- get_decennial(geography = "county", 
    #                    variables = "P001001",
    #           state = "LA",  year = 2010,geometry=TRUE)
    # glimpse(totpop_county)
    # glimpse(df_land2)
    # totpop_county <- df_land2[which(df_land2$year==1990),c(1,20,21)]
    # names(totpop_county)[1]<-"GEOID"
    #save(totpop_county,file="Data/totpop_county.Rda")
    
    load("Data/totpop_county.Rda")




    pop <- read.csv("Data/PopulationDataset.csv")
    geoids_pop <- levels(factor(pop$FIPS))
    
    geoids <- levels(factor(totpop_county$GEOID))
    geoids <- geoids[geoids %in% geoids_pop]
    years <- 1984+seq(1985:2021)
    
    first <- 1
    for (j in 1:length(geoids)){
        #for (j in 1:2){
                #Read geoID
                geom_iso <- totpop_county$geometry[totpop_county$GEOID==geoids[j]]
                geom_iso <- st_cast(geom_iso, "POLYGON")
                geom_iso <-as_Spatial(geom_iso)
                
                if(length(geom_iso)>1){
                    geom_iso <- rbind(unlist(geom_iso))
                }
                
                
                for (i in 1:length(raster_list_change)){

                        
                        # Reproject raster to same CRS as geom_iso
                        geom_iso <- spTransform(geom_iso, crs(raster_list_change[[2]]))

                        landloss <- raster::extract(raster_list_change[[i]],geom_iso, na.rm=TRUE)
                        #glimpse(landloss)
                        
                        
                        # plot(raster_list[[2]])
                        # plot(geom_iso_transformed,add=TRUE)
                        # class(geom_iso)
                        # class(raster_list[[1]])
                        # plot(geom_iso)
                        #raster::extract(raster_list[[i]],geom_iso,na.rm=TRUE)
                    
                        if(class(landloss)=="list"){
                            landloss <- unlist(landloss)

                        }
                        
                        area <- area(geom_iso)
                        area <- sum(area)

                        landloss_str <- as.character(landloss)
                        #landloss[endsWith(landloss_str, '5') & nchar(landloss_str)>1]
                        
                        if(sum(is.na(landloss))==length(landloss)){
                               

                                
                                
                                Total_pixels <- length(landloss)
                                
                                d2w <- NA
                                c2w <-  NA
                                g2w <-  NA
                                t2w <-  NA
                                we2w <-  NA
                                s2w <-  NA
                                b2w <-  NA
                                w2d <- NA
                                w2c <-  NA
                                w2g <-  NA
                                w2t <- NA
                                w2we <-  NA
                                w2s <- NA
                                w2b<- NA
                            }else{
                                
                                

                                
                                
                                Total_pixels <- length(landloss)
                                
                                d2w <- area*length(landloss[startsWith(landloss_str, '1') & endsWith(landloss_str, '5')])/(Total_pixels)
                                c2w <-  area*length(landloss[startsWith(landloss_str, '2') & endsWith(landloss_str, '5')])/(Total_pixels)
                                g2w <-  area*length(landloss[startsWith(landloss_str, '3') & endsWith(landloss_str, '5')])/(Total_pixels)
                                t2w <-  area*length(landloss[startsWith(landloss_str, '4') & endsWith(landloss_str, '5')])/(Total_pixels)
                                we2w <-  area*length(landloss[startsWith(landloss_str, '6') & endsWith(landloss_str, '5')])/(Total_pixels)
                                s2w <-  area*length(landloss[startsWith(landloss_str, '7') & endsWith(landloss_str, '5')])/(Total_pixels)
                                b2w <-  area*length(landloss[startsWith(landloss_str, '8') & endsWith(landloss_str, '5')])/(Total_pixels)

                                w2d <- area*length(landloss[startsWith(landloss_str, '5') & endsWith(landloss_str, '1')])/(Total_pixels)
                                w2c <-  area*length(landloss[startsWith(landloss_str, '5') & endsWith(landloss_str, '2')])/(Total_pixels)
                                w2g <-  area*length(landloss[startsWith(landloss_str, '5') & endsWith(landloss_str, '3')])/(Total_pixels)
                                w2t <-  area*length(landloss[startsWith(landloss_str, '5') & endsWith(landloss_str, '4')])/(Total_pixels)
                                w2we <-  area*length(landloss[startsWith(landloss_str, '5') & endsWith(landloss_str, '6')])/(Total_pixels)
                                w2s <-  area*length(landloss[startsWith(landloss_str, '5') & endsWith(landloss_str, '7')])/(Total_pixels)
                                w2b <-  area*length(landloss[startsWith(landloss_str, '5') & endsWith(landloss_str, '8')])/(Total_pixels)


                               

                        }
                    y <- years[i]
                    
                    newdf <- data.frame(geoid = geoids[j], 
                                        #popgeoid = popgeoid,
                                        
                                        d2w = d2w,
                                        c2w = c2w,
                                        g2w =g2w,
                                        t2w=t2w,
                                        we2w=we2w,
                                        s2w=s2w,
                                        b2w=b2w,
                                        w2d=w2d, 
                                        w2c=w2c, 
                                        w2g=w2g, 
                                        w2t=w2t, 
                                        w2we=w2we, 
                                        w2s=w2s, 
                                        w2b=w2b,
                                        #gdp_pc=gdp_pc,pop_estimation = pop_estimation,
                                        year=y,area=area)
                
                    if(first == 1){
                        df_land <- newdf
                        first <- 0
                    }else{
                    df_land <- rbind(df_land,newdf)}
                    #print(i)
                
                
                }
                print(j)
        }

        glimpse(df_land)
        #write.csv(df_land,"Data/changeland.csv")
        df_landchange<-read.csv("Data/changeland.csv")
        glimpse(df_landchange)

        ggplot(df_landchange)+
        geom_point(aes(x=year,y=d2w,color=geoid))


## Section 3 Extract Land Cover by Parish 

## Section 4 Read Population in Parishes (start)
    #load("Data/LandLoss_coastalparishes_geom.Rda")
    
    pop <- read.csv("Data/PopulationDataset.csv")
    pop <- pop[,c(1:5)]
    
    pop$Population.Data <- as.double(gsub(",", "", pop$Population.Data, fixed = TRUE))
    df_land <- read.csv("Data/LandLoss_coastalparishes_March4.csv")
    df_landchange<-read.csv("Data/changeland.csv")
    df_land3 <- merge(pop[,c(1,4,5)],df_landchange,by.x=c("FIPS","Year"),by.y=c("geoid","year"))
    df_land3$year <- df_land3$Year

    df_land3 <- df_land3 %>%
    group_by(FIPS) %>%
    arrange(Year) %>%
    mutate(pop_g = Population.Data / dplyr::lag(Population.Data) - 1)

     
    pop_land <- merge(df_land3,df_land,by.x=c("FIPS","year"),by.y=c("geoid","year"))
    

    pop_land <- pop_land %>%
    filter(year == 1990) %>%
    select(FIPS, Water_area) %>%
    rename(Water_area_1990 = Water_area) %>%
    inner_join(pop_land, by = "FIPS")

    pop_land <- pop_land %>%
    filter(year == 1990) %>%
    select(FIPS, Population.Data) %>%
    rename(Pop_1990 = Population.Data) %>%
    inner_join(pop_land, by = "FIPS")

    pop_land <- pop_land %>%
    group_by(FIPS) %>%
    arrange(year) %>%
    mutate(water_change_perc =  Water_area/ dplyr::lag(Water_area) - 1)

    pop_land <- pop_land %>%
    group_by(FIPS) %>%
    arrange(year) %>%
    mutate(water_change =  Water_area - dplyr::lag(Water_area))

    
    econ <- read.csv("Data/EconomicData.csv")
    
    df_land3 <- merge(econ,pop_land,by.x=c("year"),by.y=c("Year"))
    


    df_land3 <- df_land3 %>%
    group_by(FIPS) %>%
    arrange(year) %>%
    mutate(water_change = Water_area -  dplyr::lag(Water_area))
    glimpse(df_land3)

    
    df_land3$Population.Data[which(df_land3$year==2020)]

    pop_land <- df_land3

    
    pop_land$area <- pop_land$area.y
    
    pop_land$pop_g_perc <- pop_land$pop_g*100
    pop_land$Developed_area <- pop_land$Developed_pixels*pop_land$Water_area/pop_land$Water_pixels
   
    

   


## Section 4 Read Population in Parishes

## Section 5 Regression models (start)

        library(lfe)
        
        model_h0_1 <- felm(pop_g_perc~I((100*d2w/Land_area))+I(100*we2w/Land_area)+
        I(Land_area*0.0001)|year+FIPS|0|FIPS,data=pop_land)
        summary(model_h0_1)

   
        
        model_h0_1_l1 <- felm(pop_g_perc~ I(100*we2w/Land_area)+I(100*lag(we2w,1)/Land_area)+I(100*d2w/Land_area)+
        I(Land_area/1000)|year+FIPS|0|FIPS,data=pop_land)
        model_h0_1 <- felm(pop_g_perc~I(100*d2w/Land_area)+I(100*we2w/Land_area)+
        I(Land_area/1000)|year+FIPS|0|FIPS,data=pop_land)
        summary(model_h0_1)
      
          # Calculating sum of coefficients (start)
            # Extract the coefficients and standard errors for we2w and its lags
            coefficients <- coef(model_h0_1_l1)
            we2w_coef <- coefficients[c("I(100 * we2w/Land_area)", "I(100 * lag(we2w, 1)/Land_area)")]
            we2w_se <- sqrt(diag(vcov(model_h0_1_l1)))[c("I(100 * we2w/Land_area)", "I(100 * lag(we2w, 1)/Land_area)")]

            # Calculate the sum of coefficients and standard error of the sum
            sum_coef <- sum(we2w_coef)
            sum_se <- sqrt(sum(we2w_se^2))

            # Calculate the p-value for the sum using a t-distribution with degrees of freedom equal to the number of coefficients
            p_value <- pt(abs(sum_coef/sum_se), df = df.residual(model_h0_1_l1), lower.tail = FALSE) * 2


            # Print the results
            cat("Sum of coefficients using we2w:", sum_coef, "\n")
            cat("Standard error of the sum:", sum_se, "\n")
            cat("t-value:", sum_coef/sum_se, "\n")
            cat("p-value:", p_value, "\n")

            pop_land$coef_persistent_wew <- sum_coef
            pop_land$coef_persistent_wew_se <- sum_se
            pop_land$coef_persistent_wew_pval <- p_value



            # Extract the coefficients and standard errors for we2w and its lags
            coefficients <- coef(model_h0_1_l1)
            we2w_coef <- coefficients[c("I(100 * we2w/Land_area)", "I(100 * lag(we2w, 1)/Land_area)")]
            we2w_se <- sqrt(diag(vcov(model_h0_1_l1)))[c("I(100 * we2w/Land_area)", "I(100 * lag(we2w, 1)/Land_area)")]

            # Calculate the sum of coefficients and standard error of the sum
            sum_coef <- sum(we2w_coef)
            sum_se <- sqrt(sum(we2w_se^2))

            # Calculate the p-value for the sum using a t-distribution with degrees of freedom equal to the number of coefficients
            p_value <- pt(abs(sum_coef/sum_se), df = df.residual(model_h0_1_l1), lower.tail = FALSE) * 2


            # Print the results
            cat("Sum of coefficients using we2w:", sum_coef, "\n")
            cat("Standard error of the sum:", sum_se, "\n")
            cat("t-value:", sum_coef/sum_se, "\n")
            cat("p-value:", p_value, "\n")

            
            pop_land$coef_persistent_wew <- sum_coef
            pop_land$coef_persistent_wew_se <- sum_se
            pop_land$coef_persistent_wew_pval <- p_value

            
            coefficients <- coef(model_h0_1_l1)
            d2w_coef <- coefficients[c("I((100 * d2w/Land_area))", "I((100 * lag(d2w, 1)/Land_area))")]
            d2w_se <- sqrt(diag(vcov(model_h0_1_l2)))[c("I((100 * d2w/Land_area))", "I((100 * lag(d2w, 1)/Land_area))")]

            # Calculate the sum of coefficients and standard error of the sum
            sum_coef <- sum(d2w_coef)
            sum_se <- sqrt(sum(d2w_se^2))

            # Calculate the p-value for the sum using a t-distribution with degrees of freedom equal to the number of coefficients
            p_value <- pt(abs(sum_coef/sum_se), df = df.residual(model_h0_1_l2), lower.tail = FALSE) * 2

            # Print the results
            cat("Sum of coefficients using d2w:", sum_coef, "\n")
            cat("Standard error of the sum:", sum_se, "\n")
            cat("t-value:", sum_coef/sum_se, "\n")
            cat("p-value:", p_value, "\n")
            
            
            pop_land$coef_persistent_dw <- sum_coef
            pop_land$coef_persistent_dw_se <- sum_se
            pop_land$coef_persistent_dw_pval <- p_value
            
            

    # Calculating Sum of ciefficients (end)


    
## Section 5 Regression models (end)
    
    

## Section 6 Counterfactual population (start)
    
    for (i in 1:length(unique(pop_land$FIPS))) {
   
    df_g <- pop_land %>% filter(FIPS == unique(pop_land$FIPS)[i])
    
    
    growth_rate <-  (100+df_g$pop_g_perc)/100
    growth_rate_h0 <- rep(NA,length(df_g$pop_g_perc))
    growth_rate_h1 <- 100+df_g$pop_g_perc - df_g$coef_persistent_dw[1]*(100*df_g$d2w/df_g$Land_area) - df_g$coef_persistent_wew[1]*(100*df_g$we2w/df_g$Land_area)
    growth_rate_h1_onlyDeveloped <- 100+df_g$pop_g_perc - df_g$coef_persistent_dw[1]*(100*df_g$d2w/df_g$Land_area) 
    growth_rate_h1_onlyWetland <- 100+df_g$pop_g_perc - df_g$coef_persistent_wew[1]*(100*df_g$we2w/df_g$Land_area)
    
    growth_rate_h1_se_p <- 100+df_g$pop_g_perc - (df_g$coef_persistent_dw[1]+df_g$coef_persistent_dw_se[1]*1.96)*(100*df_g$d2w/df_g$Land_area) - (df_g$coef_persistent_wew[1]+df_g$coef_persistent_wew_se[1])*(100*df_g$we2w/df_g$Land_area)
    growth_rate_h1_onlyDeveloped_se_p <- 100+df_g$pop_g_perc - (df_g$coef_persistent_dw[1]+df_g$coef_persistent_dw_se[1]*1.96)*(100*df_g$d2w/df_g$Land_area) 
    growth_rate_h1_onlyWetland_se_p <- 100+df_g$pop_g_perc - (df_g$coef_persistent_wew[1]+df_g$coef_persistent_wew_se[1]*1.96)*(100*df_g$we2w/df_g$Land_area)

    growth_rate_h1_se_m <- 100+df_g$pop_g_perc - (df_g$coef_persistent_dw[1]-df_g$coef_persistent_dw_se[1]*1.96)*(100*df_g$d2w/df_g$Land_area) - (df_g$coef_persistent_wew[1]-df_g$coef_persistent_wew_se[1])*(100*df_g$we2w/df_g$Land_area)
    growth_rate_h1_onlyDeveloped_se_m <- 100+df_g$pop_g_perc - (df_g$coef_persistent_dw[1]-df_g$coef_persistent_dw_se[1]*1.96)*(100*df_g$d2w/df_g$Land_area) 
    growth_rate_h1_onlyWetland_se_m <- 100+df_g$pop_g_perc - (df_g$coef_persistent_wew[1]-df_g$coef_persistent_wew_se[1]*1.96)*(100*df_g$we2w/df_g$Land_area)

    growth_rate_h1_se_p90 <- 100+df_g$pop_g_perc - (df_g$coef_persistent_dw[1]+df_g$coef_persistent_dw_se[1]*1.64)*(100*df_g$d2w/df_g$Land_area) - (df_g$coef_persistent_wew[1]+df_g$coef_persistent_wew_se[1])*(100*df_g$we2w/df_g$Land_area)
    growth_rate_h1_onlyDeveloped_se_p90 <- 100+df_g$pop_g_perc - (df_g$coef_persistent_dw[1]+df_g$coef_persistent_dw_se[1]*1.64)*(100*df_g$d2w/df_g$Land_area) 
    growth_rate_h1_onlyWetland_se_p90 <- 100+df_g$pop_g_perc - (df_g$coef_persistent_wew[1]+df_g$coef_persistent_wew_se[1]*1.64)*(100*df_g$we2w/df_g$Land_area)

    growth_rate_h1_se_m90 <- 100+df_g$pop_g_perc - (df_g$coef_persistent_dw[1]-df_g$coef_persistent_dw_se[1]*1.64)*(100*df_g$d2w/df_g$Land_area) - (df_g$coef_persistent_wew[1]-df_g$coef_persistent_wew_se[1])*(100*df_g$we2w/df_g$Land_area)
    growth_rate_h1_onlyDeveloped_se_m90 <- 100+df_g$pop_g_perc - (df_g$coef_persistent_dw[1]-df_g$coef_persistent_dw_se[1]*1.64)*(100*df_g$d2w/df_g$Land_area) 
    growth_rate_h1_onlyWetland_se_m90 <- 100+df_g$pop_g_perc - (df_g$coef_persistent_wew[1]-df_g$coef_persistent_wew_se[1]*1.64)*(100*df_g$we2w/df_g$Land_area)
    
    if(sum(df_g$water_change,na.rm=TRUE)==0){
        print(paste0("water0",i))
        next
    }

    if(sum(is.na(growth_rate))==length(growth_rate)){
        print(paste0("nodata",i))
        next
    }
    # Calculate the counterfactual growth using the for loop
    pop_cf <- rep(NA, length(df_g$year))
    pop_cf_h0 <- rep(NA, length(df_g$year))
    pop_cf_h1 <- rep(NA, length(df_g$year))
    pop_cf_h1_onlyDeveloped <- rep(NA, length(df_g$year))
    pop_cf_h1_onlyWetland <- rep(NA, length(df_g$year))
    pop_cf_h1_se_p <- rep(NA, length(df_g$year))
    pop_cf_h1_onlyDeveloped_se_p <- rep(NA, length(df_g$year))
    pop_cf_h1_onlyWetland_se_p <- rep(NA, length(df_g$year))
    pop_cf_h1_se_m <- rep(NA, length(df_g$year))
    pop_cf_h1_onlyDeveloped_se_m <- rep(NA, length(df_g$year))
    pop_cf_h1_onlyWetland_se_m <- rep(NA, length(df_g$year))
    pop_cf_h1_se_p90 <- rep(NA, length(df_g$year))
    pop_cf_h1_onlyDeveloped_se_p90 <- rep(NA, length(df_g$year))
    pop_cf_h1_onlyWetland_se_p90 <- rep(NA, length(df_g$year))
    pop_cf_h1_se_m90 <- rep(NA, length(df_g$year))
    pop_cf_h1_onlyDeveloped_se_m90 <- rep(NA, length(df_g$year))
    pop_cf_h1_onlyWetland_se_m90 <- rep(NA, length(df_g$year))
    for(jj in 1:length(pop_cf)){
        
        pop_cf[1:jj] <-df_g$Population.Data[1:jj]
        pop_cf_h0[1:jj] <-df_g$Population.Data[1:jj]
        pop_cf_h1[1:jj] <-df_g$Population.Data[1:jj]
        pop_cf_h1_onlyDeveloped[1:jj] <-df_g$Population.Data[1:jj]
        pop_cf_h1_onlyWetland[1:jj] <-df_g$Population.Data[1:jj]
        pop_cf_h1_se_p[1:jj] <-df_g$Population.Data[1:jj]
        pop_cf_h1_onlyDeveloped_se_p[1:jj] <-df_g$Population.Data[1:jj]
        pop_cf_h1_onlyWetland_se_p[1:jj] <-df_g$Population.Data[1:jj]
        pop_cf_h1_se_m[1:jj] <-df_g$Population.Data[1:jj]
        pop_cf_h1_onlyDeveloped_se_m[1:jj] <-df_g$Population.Data[1:jj]
        pop_cf_h1_onlyWetland_se_m[1:jj] <-df_g$Population.Data[1:jj]
        pop_cf_h1_se_p90[1:jj] <-df_g$Population.Data[1:jj]
        pop_cf_h1_onlyDeveloped_se_p90[1:jj] <-df_g$Population.Data[1:jj]
        pop_cf_h1_onlyWetland_se_p90[1:jj] <-df_g$Population.Data[1:jj]
        pop_cf_h1_se_m90[1:jj] <-df_g$Population.Data[1:jj]
        pop_cf_h1_onlyDeveloped_se_m90[1:jj] <-df_g$Population.Data[1:jj]
        pop_cf_h1_onlyWetland_se_m90[1:jj] <-df_g$Population.Data[1:jj]
        year_data <- 1
        if(!is.na(pop_cf[jj])){
            year_data <- jj
            break
        }

    }

    for (j in (year_data+1):length(pop_cf)) {
        pop_cf[j]<- pop_cf[j-1] * growth_rate[j]
        pop_cf_h0[j]<- pop_cf_h0[j-1] * growth_rate_h0[j]/100
        pop_cf_h1[j]<- pop_cf_h1[j-1] * growth_rate_h1[j]/100
        pop_cf_h1_onlyDeveloped[j] <-pop_cf_h1_onlyDeveloped[j-1]  * growth_rate_h1_onlyDeveloped[j]/100 
        pop_cf_h1_onlyWetland[j] <- pop_cf_h1_onlyWetland[j-1] * growth_rate_h1_onlyWetland[j]/100
        
        pop_cf_h1_se_p[j]<- pop_cf_h1_se_p[j-1] * growth_rate_h1_se_p[j]/100
        pop_cf_h1_onlyDeveloped_se_p[j] <-pop_cf_h1_onlyDeveloped_se_p[j-1]  * growth_rate_h1_onlyDeveloped_se_p[j]/100 
        pop_cf_h1_onlyWetland_se_p[j] <- pop_cf_h1_onlyWetland_se_p[j-1] * growth_rate_h1_onlyWetland_se_p[j]/100

        pop_cf_h1_se_m[j]<- pop_cf_h1_se_m[j-1] * growth_rate_h1_se_m[j]/100
        pop_cf_h1_onlyDeveloped_se_m[j] <-pop_cf_h1_onlyDeveloped_se_m[j-1]  * growth_rate_h1_onlyDeveloped_se_m[j]/100 
        pop_cf_h1_onlyWetland_se_m[j] <- pop_cf_h1_onlyWetland_se_m[j-1] * growth_rate_h1_onlyWetland_se_m[j]/100

        pop_cf_h1_se_p90[j]<- pop_cf_h1_se_p90[j-1] * growth_rate_h1_se_p90[j]/100
        pop_cf_h1_onlyDeveloped_se_p90[j] <-pop_cf_h1_onlyDeveloped_se_p90[j-1]  * growth_rate_h1_onlyDeveloped_se_p90[j]/100 
        pop_cf_h1_onlyWetland_se_p90[j] <- pop_cf_h1_onlyWetland_se_p90[j-1] * growth_rate_h1_onlyWetland_se_p90[j]/100

        pop_cf_h1_se_m90[j]<- pop_cf_h1_se_m90[j-1] * growth_rate_h1_se_m90[j]/100
        pop_cf_h1_onlyDeveloped_se_m90[j] <-pop_cf_h1_onlyDeveloped_se_m90[j-1]  * growth_rate_h1_onlyDeveloped_se_m90[j]/100 
        pop_cf_h1_onlyWetland_se_m90[j] <- pop_cf_h1_onlyWetland_se_m90[j-1] * growth_rate_h1_onlyWetland_se_m90[j]/100
    }
    if(i==1){
        df_pcf <- data.frame(GEOID=unique(pop_land$FIPS)[i],year=df_g$year,pop_cf=pop_cf,
            pop_cf_h0=pop_cf_h0,pop_cf_h1=pop_cf_h1,
            pop_cf_h1_onlyDeveloped=pop_cf_h1_onlyDeveloped,pop_cf_h1_onlyWetland=pop_cf_h1_onlyWetland,pop_cf_h1_se_p=pop_cf_h1_se_p,
            pop_cf_h1_onlyDeveloped_se_p=pop_cf_h1_onlyDeveloped_se_p,pop_cf_h1_onlyWetland_se_p=pop_cf_h1_onlyWetland_se_p,pop_cf_h1_se_m=pop_cf_h1_se_m,
            pop_cf_h1_onlyDeveloped_se_m=pop_cf_h1_onlyDeveloped_se_m,pop_cf_h1_onlyWetland_se_m=pop_cf_h1_onlyWetland_se_m,
            growth_rate_h1_onlyWetland= growth_rate_h1_onlyWetland/100)

        df_pcf2 <- data.frame(GEOID=unique(pop_land$FIPS)[i],year=df_g$year,pop_observed=pop_cf,
            pop_cf= c(pop_cf_h1,pop_cf_h1_onlyDeveloped,pop_cf_h1_onlyWetland),
            pop_cf_sep=c(pop_cf_h1_se_p,pop_cf_h1_onlyDeveloped_se_p,pop_cf_h1_onlyWetland_se_p),
            pop_cf_sem=c(pop_cf_h1_se_m,pop_cf_h1_onlyDeveloped_se_m,pop_cf_h1_onlyWetland_se_m),
            pop_cf_sep90=c(pop_cf_h1_se_p90,pop_cf_h1_onlyDeveloped_se_p90,pop_cf_h1_onlyWetland_se_p90),
            pop_cf_sem90=c(pop_cf_h1_se_m90,pop_cf_h1_onlyDeveloped_se_m90,pop_cf_h1_onlyWetland_se_m90),
            effect=rep(c("Total","Developed","Wetland"),each=length(pop_cf_h1_se_m)))
    } else{
        df_pcf <- rbind(df_pcf,data.frame(GEOID=unique(pop_land$FIPS)[i],year=df_g$year,pop_cf=pop_cf,
            pop_cf_h0=pop_cf_h0,pop_cf_h1=pop_cf_h1,
            pop_cf_h1_onlyDeveloped=pop_cf_h1_onlyDeveloped,pop_cf_h1_onlyWetland=pop_cf_h1_onlyWetland,pop_cf_h1_se_p=pop_cf_h1_se_p,
            pop_cf_h1_onlyDeveloped_se_p=pop_cf_h1_onlyDeveloped_se_p,pop_cf_h1_onlyWetland_se_p=pop_cf_h1_onlyWetland_se_p,pop_cf_h1_se_m=pop_cf_h1_se_m,
            pop_cf_h1_onlyDeveloped_se_m=pop_cf_h1_onlyDeveloped_se_m,pop_cf_h1_onlyWetland_se_m=pop_cf_h1_onlyWetland_se_m,
            growth_rate_h1_onlyWetland= growth_rate_h1_onlyWetland/100))

        df_pcf2 <- rbind(df_pcf2,data.frame(GEOID=unique(pop_land$FIPS)[i],year=df_g$year,pop_observed=pop_cf,
            pop_cf= c(pop_cf_h1,pop_cf_h1_onlyDeveloped,pop_cf_h1_onlyWetland),
            pop_cf_sep=c(pop_cf_h1_se_p,pop_cf_h1_onlyDeveloped_se_p,pop_cf_h1_onlyWetland_se_p),
            pop_cf_sem=c(pop_cf_h1_se_m,pop_cf_h1_onlyDeveloped_se_m,pop_cf_h1_onlyWetland_se_m),
            pop_cf_sep90=c(pop_cf_h1_se_p90,pop_cf_h1_onlyDeveloped_se_p90,pop_cf_h1_onlyWetland_se_p90),
            pop_cf_sem90=c(pop_cf_h1_se_m90,pop_cf_h1_onlyDeveloped_se_m90,pop_cf_h1_onlyWetland_se_m90),
            effect=rep(c("Total","Developed","Wetland"),each=length(pop_cf_h1_se_m))))
    }
    print(i)
    }



    df_counterfactual <- merge(pop_land,df_pcf2,by.x=c("FIPS","year"),by.y=c("GEOID","year"),all=TRUE)
    
    a <- ggplot(df_counterfactual[which(df_counterfactual$FIPS==22023 & df_counterfactual$effect=="Wetland"),]) +
        #geom_line(aes(x=year,y=Population.Data,group=FIPS, color="Observed"),size=1.5) +
        geom_line(aes(x=year,y=-100*(pop_observed-pop_cf)/pop_observed, color=effect, group=effect)) +
        geom_ribbon(aes(x=year,ymin=-100*(pop_observed-pop_cf_sep90)/pop_observed,ymax=-100*(pop_observed-pop_cf_sem90)/pop_observed, fill=effect, group=effect),alpha=0.1) +
        theme_bw()  +
        xlab("Year") +
        ylab("Population difference in counterfactual \n(% of observed)") +
        ggtitle("Counterfactual population in Cameron Parish") +
        scale_fill_manual(values=c("Developed"="indianred","Wetland"="seagreen3"), name="Effect")+
        scale_color_manual(values=c("Developed"="indianred","Wetland"="seagreen3"), name="Effect")+
        coord_cartesian(ylim=c(-2, 400)) +theme(legend.position="bottom")+
        scale_y_continuous(trans="log10")
    a
    df_subset <- df_counterfactual[df_counterfactual$year == 2021, ]
    df_subset <- df_subset[df_subset$effect == "Wetland", ]
    glimpse(df_subset)

    load("Data/totpop_county.Rda")
    glimpse(totpop_county)
    df_subset <- merge(df_subset,totpop_county,by.x=c("FIPS"),by.y=c("GEOID"))
    
    df_subset$NAME2 <- gsub("Parish, Louisiana", "", df_subset$NAME, fixed = TRUE)
    glimpse(df_subset)
    
   df_subset$NAME2[which(df_subset$NAME2=="St. John the Baptist ")] <-"St. JTB"
    df_subset$perc_loss <- -(df_subset$pop_observed-df_subset$pop_cf)/df_subset$pop_observed*100
     df_subset$total_loss <- -(df_subset$pop_observed-df_subset$pop_cf)
    df_subset[which(names(df_subset) %in% c("NAME2","total_loss","perc_loss"))]
    
    b <- ggplot(df_subset, aes(y = -(pop_observed-pop_cf)/pop_observed*100, x =NAME2)) +
        geom_boxplot(
        aes(ymin =-(pop_observed-pop_cf_sem)/pop_observed*100, 
                lower = -(pop_observed-pop_cf_sem90)/pop_observed*100, 
                middle =  -(pop_observed-pop_cf)/pop_observed*100, 
                upper = -(pop_observed-pop_cf_sep90)/pop_observed*100, 
                ymax = -(pop_observed-pop_cf_sep)/pop_observed*100,fill=effect),
        stat = "identity") +
       
        theme_bw()+
        coord_cartesian(ylim=c(-2, 400)) + scale_y_continuous(trans="log10")+
        ylab("")+
        xlab("")+
        ggtitle("Counterfactual population in 2021 for all Parishes") +
        scale_fill_manual(values=c("Developed"="indianred","Wetland"="seagreen3"), name="Effect")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+theme(legend.position="none")

        
        library(ggpubr)
        ggarrange(a,b,common.legend=FALSE)
        
       
    
    df_wp_cf <- merge(pop_land,df_pcf,by.x=c("FIPS","year"),by.y=c("GEOID","year"),all=TRUE)

    load("Data/totpop_county.Rda")
    glimpse(totpop_county)
    df_wp_cf <- merge(df_wp_cf,totpop_county,by.x=c("FIPS"),by.y=c("GEOID"))
    
    df_wp_cf$NAME2 <- gsub("Parish, Louisiana", "", df_wp_cf$NAME, fixed = TRUE)
    
    df_wp_cf$NAME2[df_wp_cf$NAME2=="St. John the Baptist "] <- "St. JTB"

    df_wp_cf$FIPS[which(df_wp_cf$NAME2=="Cameron ")]



    
    pop_counterfactual <- ggplot(df_wp_cf)+
    geom_line(aes(x=year,y=Population.Data-pop_cf_h1_onlyWetland,group=FIPS),color="black")+
    theme_bw()+
    ylab("Difference between actual population\nand the counterfactual (number of persons)")+
    xlim(1990,2025)+
    geom_text(data=df_wp_cf[which(df_wp_cf$year==2021),],
        aes(x=year+2,y=Population.Data-pop_cf_h1_onlyWetland,label=NAME2),size=2,color="black")

    pop_counterfactual

    

    df_wp_cf$Perc_pop_loss <- 100*(df_wp_cf$Population.Data - df_wp_cf$pop_cf_h1_onlyWetland)/df_wp_cf$pop_cf_h1_onlyWetland
    
    

   df_wp_cf2 <- df_wp_cf %>%
    group_by(FIPS) %>%
    arrange(year) %>%
    mutate(CumPop_loss = cumsum( Population.Data - pop_cf_h1))


    df_wp_cf2 <- df_wp_cf2 %>%
    group_by(FIPS) %>%
    arrange(year) %>%
    mutate(CumPop_loss_onlyD = cumsum( Population.Data - pop_cf_h1_onlyDeveloped))

    df_wp_cf2 <- df_wp_cf2 %>%
    group_by(FIPS) %>%
    arrange(year) %>%
    mutate(CumPop_loss_onlyW = cumsum( Population.Data - pop_cf_h1_onlyWetland))


    cum_poploss <- ggplot(df_wp_cf2)+
    geom_line(aes(x=year,y=CumPop_loss_onlyW,group=FIPS),color="black")+
    theme_bw()+
    xlab("year")+
    ylab("Cumulative Landloss-driven population change since 1990 (persons)")+
    geom_text(data=df_wp_cf2[which(df_wp_cf2$year==2021),],aes(x=year+2,y=CumPop_loss_onlyW,label=NAME2),color="black")
    cum_poploss

    library("ggpubr")
    ggarrange(year_poploss,cum_poploss)

    glimpse(df_wp_cf2)
    df_wp_cf2 <- df_wp_cf2 %>%
    filter(year == 1990) %>%
    select(FIPS, Population.Data) %>%
    rename(Pop_1990 = Population.Data) %>%
    inner_join(df_wp_cf2, by = "FIPS")

    df_wp_cf2 <- df_wp_cf2 %>%
    filter(year == 2021) %>%
    select(FIPS, Population.Data) %>%
    rename(Pop_2021 = Population.Data) %>%
    inner_join(df_wp_cf2, by = "FIPS")
    

    levels(factor(df_wp_cf2$NAME2))
    
    cum_poploss2021 <- ggplot(df_wp_cf2)+
    geom_line(aes(x=year,y=100*CumPop_loss_onlyW/Pop_2021,group=FIPS),color="black")+
    #geom_line(data=df_wp_cf2[which(df_wp_cf2$NAME2=="Terrebonne "),],aes(x=year,y=100*CumPop_loss/Pop_1990,group=FIPS),color="red")+
    theme_bw()+
    xlab("year")+
    ylab("Cumulative landloss-driven population change
    since 1990 (% of 2021 Parish Population)")+
    geom_text_repel(data=df_wp_cf2[which(df_wp_cf2$year==2021),],aes(x=year,y=100*CumPop_loss_onlyW/Pop_2021,label=NAME2),size=2.5,color="black")+
    xlim(1990,2025)

    cum_poploss2021
    ggarrange(pop_counterfactual,cum_poploss2021) #Final Figure


    
    #ggsave("Figures/cumlandloss_byParish_March24_3lags.png",dpi=600)
    myLevels <- data.frame(NAME2=df_wp_cf2$NAME2[df_wp_cf2$year==2021],loss=df_wp_cf2$CumPop_loss_onlyW[df_wp_cf2$year==2021])
    myLevels <- myLevels[order(myLevels$loss),]
    
    df_wp_cf2$NAME2 <- factor(df_wp_cf2$NAME2 , levels=(myLevels$NAME2) )
    levels(factor(df_wp_cf2$NAME2))
    df_wp_cf2$Parish <- df_wp_cf2$NAME2
    
    library(viridis)
    #install.packages("hrbrthemes")
    library(hrbrthemes)
    cumstack <- ggplot(df_wp_cf2, aes(x=year,y=CumPop_loss_onlyW,group=Parish,fill=Parish)) + 
    geom_area(alpha=0.8 , size=.5, colour="white") +
    scale_fill_viridis(option="B",discrete = T) +
    theme_ipsum() + 
    guides(fill = guide_legend(reverse = TRUE) )+
    ylab("Cumulative landloss-driven population loss since 1990\n(number of persons)")
    cumstack

    cumstack_theme_bw <- ggplot(df_wp_cf2, aes(x=year,y=CumPop_loss_onlyW,group=Parish,fill=Parish)) + 
    geom_area(alpha=0.9 , size=.5, colour="white") +
    scale_fill_viridis(option="B",discrete = T) +
    theme_bw() + 
    guides(fill = guide_legend(reverse = TRUE) )+
    ylab("Cumulative landloss-driven population loss since 1990\n(number of persons)")
    cumstack_theme_bw
    #ggsave("Figures/Landloss_cumulative_people_March24.png",dpi=600) #Final Figure

    glimpse(df_wp_cf2)
    df_wp_cf3 <- df_wp_cf2
    aggregate(pop_cf~year,data=df_wp_cf3,FUN='sum')
    
    aggregate(pop_cf_h1_onlyWetland~year,data=df_wp_cf3,FUN='sum')

    popdif_stack_theme_bw <- ggplot(df_wp_cf3, aes(x=year,y=pop_cf-pop_cf_h1_onlyWetland,group=Parish,fill=Parish)) + 
    geom_area(alpha=1 , size=1) +
    scale_fill_viridis(option="H",discrete = T,direction=-1) +
    theme_bw() + 
    guides(fill = guide_legend(reverse = TRUE,title.position="top") )+
    xlab("Year")+
    ylab("Annual population loss due to wetland loss \n(number of persons)")+
    #ylim(c(-350000,0))+
    labs(x = "year", y = "Annual population loss due to wetland loss \n(number of persons)", 
            title = "") + scale_y_continuous(labels = scales::comma,limits=c(-330000,0))
    
    popdif_stack_theme_bw<-popdif_stack_theme_bw+ scale_y_continuous(labels = scales::comma,limits=c(-330000,0))
    popdif_stack_theme_bw
    
  
    meandiff <- df_wp_cf %>%
    group_by(NAME) %>%
    mutate(mean_diff_g = 10000*(100*pop_g - 100*growth_rate_h1_onlyWetland) / we2w,
    mean_diff_pop=10000*(pop_cf - pop_cf_h1_onlyDeveloped) / we2w) %>%
    summarize(mean_diff_g = mean(mean_diff_g, na.rm = TRUE),mean_diff_pop = mean(mean_diff_pop, na.rm = TRUE),
    sum_wetlandlosss = mean(0.0001*we2w, na.rm = TRUE))

    meandiff
    #write.csv(meandiff,"Data/ChangeInGrowthRate_Wetlandmeter_new.csv")

    df_subset[which(names(df_subset) %in% c("NAME2","total_loss","perc_loss"))]
    #write.csv(df_subset[which(names(df_subset) %in% c("NAME2","total_loss","perc_loss"))],"Data/WetlandLoss.csv")

    meanwelandloss <- df_wp_cf %>%
    group_by(NAME) %>%
    summarize(mean_diff = mean(we2w/1000, na.rm = TRUE))
    #write.csv(meanwelandloss,"Data/meanwelandloss_Wetlandmeter.csv")


## Section 6 Counterfactual pop


## Section 7 Monte Carlo (start)
    glimpse(pop_land)
    
    for (run_i in c(1:10000)){
        for (i in 1:length(unique(pop_land$FIPS))) {
        # Subset data for the current GEOID
        df_g <- pop_land %>% filter(FIPS == unique(pop_land$FIPS)[i])
        
        #glimpse(df_g)
        # Calculate the growth rate for the current GEOID using the pop_growth_counterfactual variable
        draw_coef <- rnorm(n=length(df_g$pop_g),mean=pop_land$coef_persistent_wew[1], sd=pop_land$coef_persistent_wew_se[1])
        
        growth_rate <- (100+df_g$pop_g_perc - draw_coef*(100*df_g$we2w/df_g$Land_area))/100
        
        if(sum(df_g$water_change,na.rm=TRUE)==0){
            print(paste0("water0",i))
            next
        }

        if(sum(is.na(growth_rate))==length(growth_rate)){
            print(paste0("nodata",i))
            next
        }
        # Calculate the counterfactual growth using the for loop
        pop_cf <- rep(NA, length(df_g$year))
    
        for(jj in 1:length(pop_cf)){
            
            pop_cf[1:jj] <-df_g$Population.Data[1:jj]
            
            year_data <- 1
            if(!is.na(pop_cf[jj])){
                year_data <- jj
                break
            }

        }

        for (j in (year_data+1):length(pop_cf)) {
            pop_cf[j]<- pop_cf[j-1] * growth_rate[j]
            
        }
        if(i==1){
            df_pcf <- data.frame(GEOID=unique(pop_land$FIPS)[i],year=df_g$year,pop_cf=pop_cf)
        } else{
            df_pcf <- rbind(df_pcf,data.frame(GEOID=unique(pop_land$FIPS)[i],year=df_g$year,pop_cf=pop_cf))
        }
        #print(i)
        }

      
        df_wp_cf <- merge(pop_land,df_pcf,by.x=c("FIPS","year"),by.y=c("GEOID","year"),all=TRUE)
        

        df_wp_cf2 <- df_wp_cf %>%
        group_by(FIPS) %>%
        arrange(year) %>%
        #mutate(CumPop_loss = cumsum( Population.Data - pop_cf))
        mutate(CumPop_loss = ( Population.Data - pop_cf))



        mc_run <- aggregate(CumPop_loss~year, data=df_wp_cf2, FUN="sum")
        mc_run$run = run_i
        if(run_i==1){
            mc_runs <- mc_run
        }else{mc_runs <- bind_rows(mc_runs,mc_run)}
        print(run_i)
    }

    #write.csv(mc_runs,"Data/mcruns_wetland_lags2.csv")

    mc_runs <- read.csv("Data/mcruns_wetland_lags2.csv")

    glimpse(mc_runs)
    aggregate(CumPop_loss~year,data=mc_runs,FUN=mean)
    aggregate(CumPop_loss~year,data=mc_runs,FUN=mean)
    
    mc_summary <- mc_runs %>%
    group_by(year) %>%
    summarise(mean_loss = mean(CumPop_loss),
    quant75_loss = quantile(CumPop_loss, 0.75),
    quant25_loss = quantile(CumPop_loss, 0.25),
    quant95_loss = quantile(CumPop_loss, 0.95),
    quant05_loss = quantile(CumPop_loss, 0.05),
    quant99_loss = quantile(CumPop_loss, 0.99),
    quant01_loss = quantile(CumPop_loss, 0.01))

    options(scipen = 999)
    MCSim <- ggplot(mc_summary, aes(x = year, y = mean_loss)) +
        geom_ribbon(aes(ymin = quant25_loss, ymax =  quant75_loss), 
                    alpha = 0.5, fill = "indianred") +
        geom_ribbon(aes(ymin = quant05_loss, ymax =  quant95_loss), 
                    alpha = 0.3, fill = "indianred") +
        geom_ribbon(aes(ymin = quant01_loss, ymax =  quant99_loss), 
                    alpha = 0.2, fill = "indianred") +
        geom_line(color = "indianred") +
        labs(x = "year", y = "Cumulative population loss", 
            title = "") +
        theme_bw()+ scale_y_continuous(labels = scales::comma) + 
        guides(fill = guide_legend(reverse = TRUE) )+
        ylab("")+ 
    guides( y = "none")+
    ylim(c(-390000,0))
    ggarrange(popdif_stack_theme_bw, MCSim,common.legend=FALSE,legend="bottom")
    ggsave("figures/Stacked_2Figures_March25_1lag.png")
    as.data.frame(mc_summary[which(mc_summary$year==2021),])

    indianred_rgb <- col2rgb("indianred") / 255
    indianred_alpha_05 <- rgb(indianred_rgb[1], indianred_rgb[2], indianred_rgb[3], alpha = 0.7)
    indianred_alpha_03 <- rgb(indianred_rgb[1], indianred_rgb[2], indianred_rgb[3], alpha = 0.5)
    indianred_alpha_02 <- rgb(indianred_rgb[1], indianred_rgb[2], indianred_rgb[3], alpha = 0.2)



    percentiles <- data.frame(
    alpha = rep(c(0.5, 0.3, 0.2),each=length( mc_summary$quant01_loss)),
    label = rep(c("25-75th Percentile", "5-95th Percentile", "1-99th Percentile"),each=length( mc_summary$quant01_loss)),
    ymin = c(mc_summary$quant25_loss, mc_summary$quant05_loss, mc_summary$quant01_loss),
    ymax = c(mc_summary$quant75_loss, mc_summary$quant95_loss, mc_summary$quant99_loss),
    year = rep(mc_summary$year,3)
    )

    percentiles$label <- factor(percentiles$label, levels = c("1-99th Percentile", "5-95th Percentile", "25-75th Percentile"))

    MCSim <- ggplot() +
    geom_ribbon(data = percentiles, aes(x=year,ymin = ymin, ymax = ymax, group=factor(label),fill = factor(label)), alpha = rep(c(0.2, 0.3, 0.5),each=length( mc_summary$quant01_loss)), color = NA) +
    geom_line(data=mc_summary, aes(x = year, y = mean_loss),color = "indianred") +
    labs(x = "year", y = "Cumulative population loss", title = "") +
    theme_bw() +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(values = c(indianred_alpha_02, indianred_alpha_03,indianred_alpha_05 ), name = "Uncertainty") +
    scale_alpha(guide = "none") +
    guides(fill = guide_legend(reverse=TRUE,title.position="top")) +
    ylab("") +
    guides(y = "none") +
    ylim(c(-390000, 0))

  
    ggarrange(popdif_stack_theme_bw, MCSim,common.legend=FALSE,legend="bottom",align="hv")
    ggsave("figures/Stacked_2Figures_April5_1lag.png")




    df_wp_cf %>%
    group_by(NAME) %>%
    mutate(mean_diff = (pop_g - growth_rate_h1_onlyWetland) / we2w) %>%
    summarize(mean_diff = mean(mean_diff, na.rm = TRUE))
## Section 7 Monte Carlo (end)
