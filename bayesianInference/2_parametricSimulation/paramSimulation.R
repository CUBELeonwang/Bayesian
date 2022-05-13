# this code is generated for the journal paper of doha reference building Bayesian inference calibration

library(lhs)
library(eplusr)
library(dplyr)
#library(QuantPsyc)
#library(randomForest)

#buildArea <- 46320.38 # unit: m2

# LHS sample
set.seed(1111)
sampleNum <- 700 # simulation times
paramNum <- 14 # selected parameter number for sensitivity analysis
y <- randomLHS(sampleNum,paramNum) 

# p1: wall insulation u-value 0.01~0.25
# p2: wall U-Value 0.01-0.3
# p3: floor U-Value 0.5-1.8
# p4: window U-Value 0.01-1.8
# p5: window SHGC 0-0.2
# p6: equipment power density 11-15
# p7: lighting power density 5-9
# p8: occupancy density 15-25
# p9: infiltration 0-0.002
# p10: ventilation 0.0025-0.005
# p11: cooling setpoint 22.5-25.5
# p12: heating setpoint 18-22.5
# p13: chiller cop 3.3-6
# p14: boiler efficiency 0.8-0.98

# roof, wall, and floor u value should be converted to conductivity
# u value is equal to conductivity/thickness
# thickness roof insulation is 0.1

ROFU <- y[,1]*0.024+0.001 # conductivity
WALU <- y[,2]*0.029+0.001
FLOU <- y[,3]*0.13+0.05
WINU <- y[,4]*1.3+0.5
SHGC <- y[,5]*0.15+0.05
EPD <- y[,6]*4+11
LPD <- y[,7]*4+5
OCC <- y[,8]*10+15
INF <- y[,9]*0.002
VEN <- y[,10]*0.0025+0.0025
CSP <- y[,11]*3+22.5
HSP <- y[,12]*4.5+18
COP <- y[,13]*2.7+3.3
EFF <- y[,14]*0.18+0.8



input <- data.frame(ROFU, WALU, FLOU, WINU, SHGC, EPD, LPD, OCC, INF, VEN, CSP, HSP, COP, EFF)
save(input, file = "input.RData")
write.csv(input, "input.csv")

# revise input data since for some cases, if the wall u vallue is too small, the simulation is failed
#input <- read.csv(file = 'input.csv')
thickn <- 0.1
# convert conductivity of ROFU, WALU, FLOU to U value
input_revise <- data.frame(ROFU/0.1, WALU/0.1, FLOU/0.1, WINU, SHGC, EPD, LPD, OCC, INF, VEN, CSP, HSP, COP, EFF)
save(input_revise, file = "input_revise.RData")

# read the EP model
idf <- read_idf("set_inputValue_151_29.idf")
epw <- read_epw("QATAR_Doha_2018.epw")

# create a parametric job
param <- param_job(idf = idf, epw = epw)
param

# function to modify selected inputs
set_inputValue <- function (idf, ROFU, WALU, FLOU, WINU, SHGC, EPD, LPD, OCC, INF, VEN, CSP, HSP, COP, EFF) {
  # validate input value
  # this is optional, as validations will be made when setting values
  stopifnot(is.numeric(ROFU), ROFU >= 0)
  stopifnot(is.numeric(WALU), WALU >= 0)
  stopifnot(is.numeric(FLOU), FLOU >= 0)
  stopifnot(is.numeric(WINU), WINU >= 0)
  stopifnot(is.numeric(SHGC), SHGC >= 0)
  stopifnot(is.numeric(EPD), EPD >= 0)
  stopifnot(is.numeric(LPD), LPD >= 0)
  stopifnot(is.numeric(OCC), OCC >= 0)
  stopifnot(is.numeric(INF), INF >= 0)
  stopifnot(is.numeric(VEN), VEN >= 0)
  stopifnot(is.numeric(CSP), CSP >= 0)
  stopifnot(is.numeric(HSP), HSP >= 0)
  stopifnot(is.numeric(COP), COP >= 0)
  stopifnot(is.numeric(EFF), EFF >= 0)
  
  if (!idf$is_valid_class("ElectricEquipment"))
    stop("Input model does not have any object in class `ElectricEquipment`")
  if (!idf$is_valid_class("Lights"))
    stop("Input model does not have any object in class `Lights`")
  if (!idf$is_valid_class("Schedule:Compact"))
    stop("Input model does not have any object in class `Schedule:Compact`")
  if (!idf$is_valid_class("People"))
    stop("Input model does not have any object in class `People`")
  if (!idf$is_valid_class("ZoneInfiltration:DesignFlowRate"))
    stop("Input model does not have any object in class `ZoneInfiltration:DesignFlowRate`")
  if (!idf$is_valid_class("Material"))
    stop("Input model does not have any object in class `Material`")
  if (!idf$is_valid_class("WindowMaterial:SimpleGlazingSystem"))
    stop("Input model does not have any object in class `WindowMaterial:SimpleGlazingSystem`")
  if (!idf$is_valid_class("DesignSpecification:OutdoorAir"))
    stop("Input model does not have any object in class `DesignSpecification:OutdoorAir`")
  if (!idf$is_valid_class("Chiller:Electric:ReformulatedEIR"))
    stop("Input model does not have any object in class `Chiller:Electric:ReformulatedEIR`")
  if (!idf$is_valid_class("Boiler:HotWater"))
    stop("Input model does not have any object in class `Boiler:HotWater`")
  
  
  #-----------------------------------P1--------------------------------
  # get roof object
  ids1 <- idf$object_id("Material", simplify = TRUE)[2]
  new_val1 <- list(Conductivity = ROFU)
  val1 <- rep(list(new_val1), length(ids1))
  names(val1) <- paste0("..", ids1)
  idf$set(val1)
  
  #-----------------------------------P2--------------------------------
  # get wall object
  ids2 <- idf$object_id("Material", simplify = TRUE)[1]
  new_val2 <- list(Conductivity = WALU)
  val2 <- rep(list(new_val2), length(ids2))
  names(val2) <- paste0("..", ids2)
  idf$set(val2)
  
  #-----------------------------------P3--------------------------------
  # get floor object
  ids3 <- idf$object_id("Material", simplify = TRUE)[10]
  new_val3 <- list(Conductivity = FLOU)
  val3 <- rep(list(new_val3), length(ids3))
  names(val3) <- paste0("..", ids3)
  idf$set(val3)
  
  #-----------------------------------P4--------------------------------
  # get window object U VALUE
  ids4 <- idf$object_id("WindowMaterial:SimpleGlazingSystem", simplify = TRUE)
  new_val4 <- list(U_Factor = WINU)
  val4 <- rep(list(new_val4), length(ids4))
  names(val4) <- paste0("..", ids4)
  idf$set(val4)
  
  #-----------------------------------P5--------------------------------
  # get window object SHGC
  ids5 <- idf$object_id("WindowMaterial:SimpleGlazingSystem", simplify = TRUE)
  new_val5 <- list(Solar_Heat_Gain_Coefficient = SHGC )
  val5 <- rep(list(new_val5), length(ids5))
  names(val5) <- paste0("..", ids5)
  idf$set(val5)
  
  #-----------------------------------P6--------------------------------
  # get all object IDS: Equipment power density
  ids6 <- idf$object_id("ElectricEquipment", simplify = TRUE)
  # make a list of new values to set
  new_val6 <- list(design_level_calculation_method = "Watts/Area", watts_per_zone_floor_area = EPD)
  # create proper format for all objects in that class
  val6 <- rep(list(new_val6), length(ids6))
  names(val6) <- paste0("..", ids6)
  idf$set(val6)
  
  #-----------------------------------P7--------------------------------
  # get all object IDS: Lighting power density 
  ids7 <- idf$object_id("Lights", simplify = TRUE)[2]
  # make a list of new values to set
  new_val7 <- list(design_level_calculation_method = "Watts/Area", watts_per_zone_floor_area = LPD)
  # create proper format for all objects in that class
  val7 <- rep(list(new_val7), length(ids7))
  names(val7) <- paste0("..", ids7)
  idf$set(val7)
  
  #-----------------------------------P8--------------------------------
  # get eleventh object: occupancy
  ids8 <- idf$object_id("People", simplify = TRUE)
  # make a list of new values to set
  new_val8 <- list(number_of_people_calculation_method = "Area/Person", zone_floor_area_per_person =  OCC )
  # create proper format for all objects in that class
  val8 <- rep(list(new_val8), length(ids8))
  names(val8) <- paste0("..", ids8)
  idf$set(val8)
  
  #-----------------------------------P9--------------------------------
  # get all object IDS
  ids9 <- idf$object_id("ZoneInfiltration:DesignFlowRate", simplify = TRUE)
  # make a list of new values to set
  new_val9 <- list(design_flow_rate_calculation_method = "Flow/ExteriorArea", flow_per_exterior_surface_area = INF)
  # create proper format for all objects in that class
  val9 <- rep(list(new_val9), length(ids9))
  names(val9) <- paste0("..", ids9)
  idf$set(val9)
  
  #-----------------------------------P10--------------------------------
  # get all object IDS
  ids10 <- idf$object_id("DesignSpecification:OutdoorAir", simplify = TRUE)
  # make a list of new values to set
  new_val10 <- list(Outdoor_air_method = "Flow/Person", outdoor_air_flow_per_person = VEN)
  # create proper format for all objects in that class
  val10 <- rep(list(new_val10), length(ids10))
  names(val10) <- paste0("..", ids10)
  idf$set(val10)
  
  #-----------------------------------P11--------------------------------
  # get twelveth object: cooling setpoint
  ids11 <- idf$object_id("Schedule:Compact", simplify = TRUE)[11]
  # make a list of new values to set
  new_val11 <- list(field_3 = "Until: 06:00", field_4 =as.character(CSP+3), 
                   field_5 = "Until: 22:00", field_6 = as.character(CSP), 
                   field_7 = "Until: 24:00", field_8 = as.character(CSP+3))
  # create proper format for all objects in that class
  val11 <- rep(list(new_val11), length(ids11)) 
  names(val11) <- paste0("..", ids11)
  idf$set(val11)
  
  #-----------------------------------P12--------------------------------
  # get twelveth object: Heating setpoint
  ids12 <- idf$object_id("Schedule:Compact", simplify = TRUE)[12]
  # make a list of new values to set
  new_val12 <- list(field_3 = "Until: 06:00", field_4 =as.character(HSP-3), 
                    field_5 = "Until: 22:00", field_6 = as.character(HSP), 
                    field_7 = "Until: 24:00", field_8 = as.character(HSP-3))
  # create proper format for all objects in that class
  val12 <- rep(list(new_val12), length(ids12)) 
  names(val12) <- paste0("..", ids12)
  idf$set(val12)
  
  #-----------------------------------P13--------------------------------
  # get twelveth object: chiller cop
  ids13 <- idf$object_id("Chiller:Electric:ReformulatedEIR", simplify = TRUE)
  # make a list of new values to set
  new_val13 <- list(reference_cop = COP)
  # create proper format for all objects in that class
  val13 <- rep(list(new_val13), length(ids13)) 
  names(val13) <- paste0("..", ids13)
  idf$set(val13)
  
  #-----------------------------------P14--------------------------------
  # get twelveth object: BOILER efficiency
  ids14 <- idf$object_id("Boiler:HotWater", simplify = TRUE)
  # make a list of new values to set
  new_val14 <- list(nominal_thermal_efficiency = EFF)
  # create proper format for all objects in that class
  val14 <- rep(list(new_val14), length(ids14)) 
  names(val14) <- paste0("..", ids14)
  idf$set(val14)
 
  idf
}

param$apply_measure(set_inputValue, input$ROFU, input$WALU, input$FLOU, input$WINU, input$SHGC, input$EPD, input$LPD,
                    input$OCC, input$INF, input$VEN, input$CSP, input$HSP, input$COP, input$EFF, .names = NULL)

# run the parametric job
param$run(wait = TRUE)
param

save(param, file = "param.RData")




