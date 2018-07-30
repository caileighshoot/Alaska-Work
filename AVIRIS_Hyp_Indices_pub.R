## AVIRIS Hyperspectral Veg Indices
require(raster)
require(rgdal)

#########################################
## Calculate Hyperspectral Vegetation Indices from AVIRIS-NG Data
########################################
## This function grabs the band closest to a specified wavelength
R <- function(band, ## Specify wavelength
              d = data, ## Specify the data frame of hyperspectral data
              nc = 0){ ## Specify the number of columns to include as offset (are there columns at the beginning of the df that you want to omit?)
  ## AVIRIS-NG Wavelengths
  wavelength <- c(376.86,381.87,386.88,391.89,396.89,401.9,406.91,
                  411.92,416.93,421.94,426.95,431.96,436.96,441.97,
                  446.98,451.99,457.0,462.01,467.02,472.02,477.03,
                  482.04,487.05,492.06,497.07,502.08,507.09,512.09,
                  517.1,522.11,527.12,532.13,537.14,542.15,547.15,
                  552.16,557.17,562.18,567.19,572.2,577.21,582.22,
                  587.22,592.23,597.24,602.25,607.26,612.27,617.28,
                  622.28,627.29,632.3,637.31,642.32,647.33,652.34,
                  657.35,662.35,667.36,672.37,677.38,682.39,687.4,
                  692.41,697.41,702.42,707.43,712.44,717.45,722.46,
                  727.47,732.48,737.48,742.49,747.5,752.51,757.52,
                  762.53,767.54,772.54,777.55,782.56,787.57,792.58,
                  797.59,802.6,807.61,812.61,817.62,822.63,827.64,
                  832.65,837.66,842.67,847.67,852.68,857.69,862.7,
                  867.71,872.72,877.73,882.74,887.74,892.75,897.76,
                  902.77,907.78,912.79,917.8,922.81,927.81,932.82,
                  937.83,942.84,947.85,952.86,957.87,962.87,967.88,
                  972.89,977.9,982.91,987.92,992.93,997.94,1002.94,
                  1007.95,1012.96,1017.97,1022.98,1027.99,1033.0,
                  1038.0,1043.01,1048.02,1053.03,1058.04,1063.05,
                  1068.06,1073.07,1078.07,1083.08,1088.09,1093.1,
                  1098.11,1103.12,1108.13,1113.13,1118.14,1123.15,
                  1128.16,1133.17,1138.18,1143.19,1148.2,1153.2,1158.21,
                  1163.22,1168.23,1173.24,1178.25,1183.26,1188.26,1193.27,
                  1198.28,1203.29,1208.3,1213.31,1218.32,1223.33,1228.33,
                  1233.34,1238.35,1243.36,1248.37,1253.38,1258.39,1263.39,
                  1268.4,1273.41,1278.42,1283.43,1288.44,1293.45,1298.46,
                  1303.46,1308.47,1313.48,1318.49,1323.5,1328.51,1333.52,
                  1338.52,1343.53,1348.54,1353.55,1358.56,1363.57,1368.58,
                  1373.59,1378.59,1383.6,1388.61,1393.62,1398.63,1403.64,
                  1408.65,1413.65,1418.66,1423.67,1428.68,1433.69,1438.7,
                  1443.71,1448.72,1453.72,1458.73,1463.74,1468.75,1473.76,
                  1478.77,1483.78,1488.78,1493.79,1498.8,1503.81,1508.82,
                  1513.83,1518.84,1523.85,1528.85,1533.86,1538.87,1543.88,
                  1548.89,1553.9,1558.91,1563.91,1568.92,1573.93,1578.94,
                  1583.95,1588.96,1593.97,1598.98,1603.98,1608.99,1614.0,
                  1619.01,1624.02,1629.03,1634.04,1639.04,1644.05,1649.06,
                  1654.07,1659.08,1664.09,1669.1,1674.11,1679.11,1684.12,
                  1689.13,1694.14,1699.15,1704.16,1709.17,1714.17,1719.18,
                  1724.19,1729.2,1734.21,1739.22,1744.23,1749.24,1754.24,
                  1759.25,1764.26,1769.27,1774.28,1779.29,1784.3,1789.3,
                  1794.31,1799.32,1804.33,1809.34,1814.35,1819.36,1824.37,
                  1829.37,1834.38,1839.39,1844.4,1849.41,1854.42,1859.43,
                  1864.44,1869.44,1874.45,1879.46,1884.47,1889.48,1894.49,
                  1899.5,1904.5,1909.51,1914.52,1919.53,1924.54,1929.55,
                  1934.56,1939.57,1944.57,1949.58,1954.59,1959.6,1964.61,
                  1969.62,1974.63,1979.63,1984.64,1989.65,1994.66,1999.67,
                  2004.68,2009.69,2014.7,2019.7,2024.71,2029.72,2034.73,
                  2039.74,2044.75,2049.76,2054.76,2059.77,2064.78,2069.79,
                  2074.8,2079.81,2084.82,2089.83,2094.83,2099.84,2104.85,
                  2109.86,2114.87,2119.88,2124.89,2129.89,2134.9,2139.91,
                  2144.92,2149.93,2154.94,2159.95,2164.96,2169.96,2174.97,
                  2179.98,2184.99,2190.0,2195.01,2200.02,2205.02,2210.03,
                  2215.04,2220.05,2225.06,2230.07,2235.08,2240.09,2245.09,
                  2250.1,2255.11,2260.12,2265.13,2270.14,2275.15,2280.15,
                  2285.16,2290.17,2295.18,2300.19,2305.2,2310.21,2315.22,
                  2320.22,2325.23,2330.24,2335.25,2340.26,2345.27,2350.28,
                  2355.28,2360.29,2365.3,2370.31,2375.32,2380.33,2385.34,
                  2390.35,2395.35,2400.36,2405.37,2410.38,2415.39,2420.4,
                  2425.41,2430.41,2435.42,2440.43,2445.44,2450.45,2455.46,
                  2460.47,2465.48,2470.48,2475.49,2480.5,2485.51,2490.52,
                  2495.53,2500.54)
  if(length(band) == 1){
    out <- which(abs(wavelength - band) == min(abs(wavelength - band)))
    d.out <- d[,out + nc]
  } else {
    out <- c()
    for(i in 1:length(band)){
      out <- c(out, which(abs(wavelength - band[i]) == min(abs(wavelength - band[i]))))
      out <- unique(out)
    }
    d.out <- rowMeans(d[,out + nc])
  }
  return(d.out)
}

calculate_veg_indices <- function(data, # avirisng@data[complete.cases(avirisng@data),] 
                                  n_col = 0){
  # Define the wavelengths that are of interest...
  NIR   <- 800
  REDEDGE <- 710
  RED   <- 650
  GREEN <- 550
  BLUE  <- 475
  SWIR_short <- 1650
  SWIR_long <- 2200
  
  ## Basic 5 bands
  NIR.w <- R(NIR)
  REDEDGE.w <- R(REDEDGE)
  RED.w <- R(RED)
  GREEN.w <- R(GREEN)
  BLUE.w <- R(BLUE)
  
  ## Multispectral Agricultural Indices from Auburn University
  NDVI <- (R(NIR) - R(RED))/(R(NIR) + R(RED))
  NDRE <- (R(NIR) - R(REDEDGE))/(R(NIR) + R(REDEDGE))
  SR <- R(NIR)/R(RED)
  SR_RE <- R(NIR)/R(REDEDGE)
  ISR <- (1-NDVI)/(1+NDVI)
  ISR_NDRE <- (1-NDRE)/(1+NDRE)
  CSM <- R(RED)/R(NIR)
  CSM_RE <- R(REDEDGE)/R(NIR)
  Datt <-(R(NIR) - R(REDEDGE))/(R(NIR) - R(RED))
  
  ## Additional Hyperspectral Indices Obtained From:
  ## Thenkabail, Prasad S., and John G. Lyon. Hyperspectral remote sensing of vegetation. CRC Press, 2016.
  ## https://www.amazon.com/Hyperspectral-Remote-Sensing-Vegetation-Thenkabail/dp/1439845379
  ### Structure
  EVI <- ((2.5*R(NIR))-R(RED))/((R(NIR))+(6*R(RED))-(7.5*R(BLUE))+1)
  VARI <- (R(GREEN)-R(RED))/(R(GREEN)+R(RED)-R(BLUE))
  VIgreen <- (R(GREEN)-R(RED))/(R(GREEN)+R(RED))
  NDWI <- (R(857)-R(1241))/(R(857) + R(1241))
  WBI <- (R(900))/(R(970))
  
  
  ### Biochemical
  ##### Pigments
  SIPI <- (R(NIR)-R(445))/(R(NIR)-R(680))
  PSSR_a <- R(NIR)/R(675)
  PSSR_b <- R(NIR)/R(650)
  PSND_a <- (R(NIR)-R(675))/(R(NIR)+R(675))
  
  PSND_b <- (R(NIR)-R(650))/(R(NIR)+R(650))
  
  PSRI <- (R(680)-R(500))/R(750)
  
  ##### Chlorophyll
  CARI <- (R(700)-R(670))- #ERROR: THIS WAS / INSTEAD OF - 
           (.2*(R(700)-R(550)))
  
  MCARI <- (R(700)-R(670))- #ERROR: THIS WAS / INSTEAD OF - 
           (.2*(R(700)-R(550))*(R(700)/R(670)))
  
  CI_RE <- (R(NIR)/R(REDEDGE))-1
  
  ##### Anthrocyanins
  ARI <- (1/R(GREEN))-(1/R(REDEDGE))
  mARI <- ((1/R(GREEN))-(1/R(REDEDGE)))*R(NIR)
  RGRI <- R(RED)/R(GREEN)                              
  ACI  <- R(GREEN)/R(NIR) 
  
  ##### Carotenoids 
  CRI1 <- (1/R(510))-(1/R(550))
  CRI2 <- (1/R(510))-(1/R(700))
  
  ##### Water
  NDII_short <- (R(830) - R(SWIR_short))/(R(830) + R(SWIR_short)) 
  NDII_long <- (R(830) - R(SWIR_long))/(R(830) + R(SWIR_long)) 
  MSI <- R(SWIR_short)/R(830)
  
  ### Lignin and Cellulose
  CAI <- 100*(0.5*(R(2031)+R(2211))-R(2101))
  NDLI <- (log10(1/R(1754))-log10(1/R(1680)))/(log10(1/R(1754))+log10(1/R(1680)))
  
  ##### Nitrogen
  NDNI <- (log10(1/R(1510))-log10(1/R(1680)))/(log10(1/R(1510))+log10(1/R(1680)))
  
  ### Physiology
  ##### Light Use Efficiency
  PRI <- (R(530)-R(570))/(R(530)+R(570))
  
  ##### Stress
  RVSI <- ((R(714) + R(752))/2) - R(733)
  
  ######################################################
  ### Hyperspectral Vegegation Indices Obtained from Fred Huemmrich @ NASA Goddard (karl.f.huemmrich@nasa.gov)
  ######################################################
  Carotenoid_Reflectance_Index_2 <- (1/R(510))-(1/R(700))
  DMC <- (R(1649)-R(1720))/(R(1649)+R(1720))
  TM3.TM1 <- R(620:690)/R(450:520)
  RE3 <- R(730:745)
  Water_Band_Index <- R(900)/R(970)
  Carotenoid_Reflectance_Index_1 <- (1/R(510))-(1/R(550))
  Datt_Chl <- R(672) / (R(550)*R(708))
  G032 <- (R(750)-R(445))/(R(700)-R(445))
  Physiological_Reflectance_Index_1_PRI <- (R(531)-R(570))/(R(531)+R(570))
  ChlPI_B <- (R(740)*R(740)*R(740))/(R(675)*R(695)*R(800))
  RF_Green <- R(525)-R(550)
  Physiological_Reflectance_Index_2 <- (R(530)-R(550))/(R(530)+R(550))
  Simple_Ratio_430.762 <- R(430)/R(762)
  G035 <- R(860)/(R(708)*R(550))
  Curvature_Index <- (R(675)*R(690))/(R(683)*R(683))
  Anthocyanin_Reflectance_Index_1 <- (1/R(550))-(1/R(700))
  Transformed_Chl_Abs_R_Index <- 3*((R(700)-R(670))-0.2*(R(700)-R(550))*(R(700)/R(670)))
  MCARI_Chl_a <- ((R(700)-R(675))-(0.2*(R(700)-R(550))*(R(700)/R(675))))
  RFf_r <- (R(730)-R(650))/(R(685)+R(650))
  ChlPI_A <- (R(740)*R(740))/(R(675)*R(800))
  Anthocyanin_Reflectance_Index_2 <- ((1/R(550))-(1/R(700)))*R(800)
  RI <- (R(678)-R(667))/(R(678)+R(667))
  Simple_Ratio_900.680 <- R(900)/R(680)
  Simple_Ratio_800.680 <- R(800)/R(680)
  Simple_Ratio_752.690 <- R(800)/R(680)
  Simple_Ratio_705.715 <- R(705)/R(715)
  Simple_Ratio_550.430 <- R(550)/R(430)
  Simple_Ratio_775.675 <- R(775)/R(675)
  Simple_Ratio_801.670 <- R(801)/R(670)
  Phyt2 <- R(724)/(R(724)+R(654))
  Phyt21 <- (R(724)-R(654))/(R(724)+R(654))
  G034 <- (R(750:800)/R(520:585))-1
  Simple_Ratio_750.550 <- R(750)/R(550)
  G036 <- (1/R(550))-(1/R(750))
  Simple_Ratio_750.705 <- R(750)/R(705)
  Simple_Ratio_860.550 <- R(860)/R(550)
  MCARI_Chl_b <- (R(700)-R(630))-(0.23*(R(700)-R(550)))*(R(700)/R(630))
  G037 <- (1/R(700))-(1/R(800))
  Simple_Ratio_800.650 <- R(800)/R(650)
  Simple_Ratio_685.655 <- R(685)/R(655)
  Gitelson_Chl <- ((1/R(549))-(1/R(793)))*R(793)
  Simple_Ratio_690.655 <- R(690)/R(655)
  Pigment_specific_simple_ratio_2 <- R(800)/R(635)
  Greenness_Index <- R(554)/R(675)
  Temperature_condition_index <- 1.2*(R(700)-R(550))-1.5*(R(670)-R(550))*(sqrt(R(700)/R(670)))
  RFfr <- R(730)-R(650)
  PRI4 <- (R(531)-R(667))/(R(531)+R(667))
  Simple_Ratio_550.650 <- R(550)/R(650)
  RE2 <- R(714:725)
  Boreal_Index <- (R(800)/R(670) - 1) / (sqrt(R(800)/R(670) +1))
  Normalized_Difference_553 <- (R(682)-R(553))/(R(682)+R(553))
  Red_green_ratio <- (R(612) +R(660)) / (R(510) +R(560))
  Simple_Ratio_545_NBRVI <- R(708)/R(545)
  Carter_2 <- R(695)/R(420)
  Reflectance_Phytochrome <- R(730)/(R(730)+R(665))
  Physiological_Reflectance_Index_3 <- (R(531)-R(670))/(R(531)+R(670))
  Phyt11 <- (R(730)-R(652))/(R(730)+R(652))
  Phyt1 <- R(730)/(R(730)+R(652))
  RE1 <- R(675:705)
  Phyt3 <- R(730)/(R(730)+R(666))
  Phyt31 <- (R(730)-R(666))/(R(730)+R(666))
  ZTM <- R(750)/R(710)
  Normalized_Difference_Lignin_Index <- (log10(1/R(1754))-log10(1/R(1680)))/(log10(1/R(1754))+log10(1/R(1680)))
  Normalized_Difference_NBNDVI <- (R(708)-R(546))/(R(708)+R(546))
  Visible_atmospherically_resistant_index <- (R(555)-R(680))/(R(555)+R(680)-R(480))
  RFRed <- R(690)-R(650)
  Simple_Ratio_672.550 <- R(672)/R(550)
  Simple_Ratio_787.765 <- R(787)/R(765)
  Structure_Independent_Pigment_Index <- (R(800)-R(450))/(R(800)-R(650))
  Cellulose_Absorption_Index <- 0.5*(R(2015)+R(2195))-R(2106)
  NDVIII <- (R(750)-R(667))/(R(750)+R(667))
  G031 <- (R(750)-R(705))/(R(750)+R(705))
  Carter_1 <- R(695)/R(760)
  Simple_Ratio_705.930 <- R(705)/R(930)
  Plant_Senescence_Reflectance_Index <- (R(680)-R(500))/R(750)
  ChlPII_A <- (R(685)*R(685))/(R(675)*R(800))
  ChlPII_B <- (R(685)*R(685)*R(685))/(R(675)*R(695)*R(800))
  NDVI_TM_narrow_band <- (R(774)-R(677))/(R(774)+R(677))
  Normalized_Difference_680 <- (R(800)-R(680))/(R(800)+R(680))
  NDVI_2 <- (R(773)-R(681))/(R(773)+R(681))
  NDVII <- (R(800)-R(667))/(R(800)+R(667))
  G945 <- (R(800)-R(700))/(R(800)+R(700))
  Pigment_specific_normalized_difference_1 <- (R(800)-R(635))/(R(800)+R(635))
  Normalized_Difference_Nitrogen_Index <- (log10(1/R(1510))-log10(1/R(1680)))/(log10(1/R(1510))+log10(1/R(1680)))
  Difference_index <- R(800)-R(550)
  Simple_Ratio_800.750 <- R(800)/R(750)
  Fsensitive_inds <- R(750)/R(800)
  TM5.TM4 <- R(1550:1750)/R(760:900)
  mND705 <- (R(750)-R(705))/(R(750)+R(705)-2*R(445))
  Moisture_Stress_Index <- R(1599)/R(819)
  Transformed_vegetation_index <- 0.5*(120*(R(750)-R(550))-200*(R(670)-R(550)))
  Optimized_Soil_Adjusted_Veg_Index <- 1.16*(R(800)-R(670))/(R(800)+R(670)+0.16)
  Modified_soil_adjusted_vegetation_index <- 0.5*(2*R(800)+1-sqrt(((2*R(800)+1)^2)-8*(R(800)-R(670))))
  Re_normalized_difference_vegetation_index <- (R(800)-R(670))/sqrt(R(800)+R(670))
  Difference_vegetation_index <- R(800)-R(680)
  Enhanced_Vegetation_Index_2_Band <- 2.5* (R(840:875)-R(620:670)) / (R(840:875) + 6 *R(620:670) + 1)
  G033 <- ((R(750:800))/(R(695:740)))-1
  Modified_transformed_vegetation_index <- 1.2 * (1.2 * (R(800)-R(550)) - 2.5 * (R(670)-R(550)) )
  Enhanced_Vegetation_Index <- 2.5*(R(813)-R(691))/(R(813)+6*R(691)-7.5*R(508)+1)
  Normalized_Difference_R1013_R895 <- (R(1013)-R(895))/(R(1013)+R(895))
  MERIS_terrestrial_chlorophyll_index <- (R(750)-R(710))/(R(710)-R(680))
  Vogelman_Red_Edge_Index_2 <- (R(734)-R(747))/(R(715)+R(726))
  Double_difference_index <- (R(750)-R(720))-(R(700)-R(670))
  Vogelman_Red_Edge_Index_1 <- R(740)/R(720)
  Optimized_vegetation_index_2 <- 100 * (log(R(760))- log(R(730)))
  Optimized_vegetation_index_1 <- R(760) /R(730)
  Red_edge_model_index <- (R(750)/R(720))-1
  Red_Edge_Chlorophyll_R_Index <- (R(770:800)/R(720:740))-1
  Normalized_Difference_Infrared_Index <- (R(819)-R(1649))/(R(819)+R(1649))
  Normalized_Difference_Water_Index <- (R(870)-R(1240))/(R(870)+R(1240))
                                                                           
  
  ### Bind together into matrix of predictors
  h.predictors <- data.frame(cbind(ACI, Anthocyanin_Reflectance_Index_1, Anthocyanin_Reflectance_Index_2, 
                                   ARI, BLUE.w, Boreal_Index, CAI, CARI, Carotenoid_Reflectance_Index_1, 
                                   Carotenoid_Reflectance_Index_2, Carter_1, Carter_2, Cellulose_Absorption_Index, 
                                   ChlPI_A, ChlPI_B, ChlPII_A, ChlPII_B, CI_RE, CRI1, CRI2, CSM, CSM_RE, 
                                   Curvature_Index, Datt, Datt_Chl, Difference_index, Difference_vegetation_index, 
                                   DMC, Double_difference_index, Enhanced_Vegetation_Index, Enhanced_Vegetation_Index_2_Band, 
                                   EVI, Fsensitive_inds, G031, G032, G033, G034, G035, G036, G037, G945, Gitelson_Chl, GREEN.w, 
                                   Greenness_Index, ISR, ISR_NDRE, mARI, MCARI, MCARI_Chl_a, MCARI_Chl_b, 
                                   MERIS_terrestrial_chlorophyll_index, mND705, Modified_soil_adjusted_vegetation_index, 
                                   Modified_transformed_vegetation_index, Moisture_Stress_Index, MSI, NDII_long, NDII_short, 
                                   NDLI, NDNI, NDRE, NDVI, NDVI_2, NDVI_TM_narrow_band, NDVII, NDVIII, NDWI, NIR.w, 
                                   Normalized_Difference_553, Normalized_Difference_680, Normalized_Difference_Infrared_Index, 
                                   Normalized_Difference_Lignin_Index, Normalized_Difference_NBNDVI, 
                                   Normalized_Difference_Nitrogen_Index, Normalized_Difference_R1013_R895, 
                                   Normalized_Difference_Water_Index, Optimized_Soil_Adjusted_Veg_Index, 
                                   Optimized_vegetation_index_1, Optimized_vegetation_index_2, Physiological_Reflectance_Index_1_PRI,
                                   Physiological_Reflectance_Index_2, Physiological_Reflectance_Index_3, Phyt1, Phyt11, Phyt2, 
                                   Phyt21, Phyt3, Phyt31, Pigment_specific_normalized_difference_1, Pigment_specific_simple_ratio_2, 
                                   Plant_Senescence_Reflectance_Index, PRI, PRI4, PSND_a, PSND_b, PSRI, PSSR_a, PSSR_b, 
                                   Re_normalized_difference_vegetation_index, RE1, RE2, RE3, RED.w, Red_Edge_Chlorophyll_R_Index, 
                                   Red_edge_model_index, Red_green_ratio, REDEDGE.w, Reflectance_Phytochrome, RF_Green, RFf_r, 
                                   RFfr, RFRed, RGRI, RI, RVSI, Simple_Ratio_430.762, Simple_Ratio_545_NBRVI, Simple_Ratio_550.430, 
                                   Simple_Ratio_550.650, Simple_Ratio_672.550, Simple_Ratio_685.655, Simple_Ratio_690.655, 
                                   Simple_Ratio_705.715, Simple_Ratio_705.930, Simple_Ratio_750.550, Simple_Ratio_750.705, 
                                   Simple_Ratio_752.690, Simple_Ratio_775.675, Simple_Ratio_787.765, Simple_Ratio_800.650, 
                                   Simple_Ratio_800.680, Simple_Ratio_800.750, Simple_Ratio_801.670, Simple_Ratio_860.550, 
                                   Simple_Ratio_900.680, SIPI, SR, SR_RE, Structure_Independent_Pigment_Index, 
                                   Temperature_condition_index, TM3.TM1, TM5.TM4, Transformed_Chl_Abs_R_Index, 
                                   Transformed_vegetation_index, VARI, VIgreen, Visible_atmospherically_resistant_index, 
                                   Vogelman_Red_Edge_Index_1, Vogelman_Red_Edge_Index_2, Water_Band_Index, WBI, ZTM)) ## Carotenoids
  
  return(h.predictors)
}

avirisng <- readGDAL("AvirisNGData")
indices <- calculate_veg_indices(data = avirisng@data[complete.cases(avirisng@data),])
