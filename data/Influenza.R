gdata <- list(
  #################
  ### Infection ###
  #################

  # daily incidence (https://www.gouvernement.fr/info-coronavirus/carte-et-donnees)
  I = 185,
  # disease duration (days)
  d = 10,
  #  basic reproduction number
  R0 = 1.29, # https://www.gouvernement.fr/info-coronavirus/carte-et-donnees

  ################
  ### Duration ###
  ################

  tIC  = 15,   # average duration of stay in intensive care
  tSL  = 14,   # average duration of sick leave
  tESL = 28,   # average duration of extended sick leave
  tE  = 5, # duration epidemiological state E
  tEA = 2, # duration epidemiological state EA
  tES = 2, # duration epidemiological state ES
  tIA = 7, # duration epidemiological state IA
  tIM = 8, # duration epidemiological state IM
  tIS = 9, # duration epidemiological state IS
  tLI = 60, # duration of partial immunity before return to non immune status
  tHI = 150, # duration of full immunity before return to partial immune status

  #####################
  ### Probabilities ###
  #####################

  psympNI = 0.5, # probability to be symptomatic when non immune
  psympLI = 0.2, # probability to be symptomatic when partially immune
  psympHI = 0.1, # probability to be symptomatic when fully immune

  psevNI = 0.5, # probability to develop severe symptoms when non immune
  psevLI = 0.3, # probability to develop severe symptoms when partially immune
  psevHI = 0.1, # probability to develop severe symptoms when fully immune

  pSL = 0.3, # probability to take sick leave
  pESL = 1, # probability to take extended sick leave
  pSLT = 0.01, # probability to take EL/ESL after positive test

  pIC = 0.3, # probability to be transfer in intensive care
  pdieIC = 0.005, # probability to die in intensive care

  pLI = 0.40, # probability to be PI at the admission (proportion of PI in the population)
  pHI = 0.5, # probability to be FI at the admission (proportion of FI in the population)

  #####################
  ### Rates & ratio ###
  #####################

  hNI2LI = 1/30, # daily probability to become partially immune
  hLI2HI = 1/60, # daily probability to become fully immune

  rinfLI = 0.7, # partial immunity efficiency % FIX ME better explain that this is the ratio of reduction of probability to be infected compared to non immune
  rinfHI = 0.5, # partial immunity efficiency % FIX ME better explain that this is the ratio of reduction of probability to be infected compared to non immune

  rsymp = 1, # Ratio adjusting probability of symptoms for patients compared to general population (professionals)
  rsev = 1, # Ratio adjusting probability of severity if symptoms for patients compared to general population (professionals)

  # Ratio adjusting the excretion rates based on epidemiological stage
  rEA = 0.35,
  rES = 1,
  rIA = 0.35,
  rIM = 1,
  rIS = 1,

  # Tests sensitivity and specificity
  sensAg = 0.85,
  speAg = 0.95,
  sensPCR = 0.85,
  spePCR = 0.95)

saveRDS(gdata, file = "./data/Influenza.rds")
