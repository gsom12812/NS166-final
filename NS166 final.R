#amf = data name
amf <- NS166_R_data_1_

sd(amf$`Percent Increase`)

p_yesf <- amf[amf$FERTP == "Pyes",]
p_nof <- amf[amf$FERTP == "Pno",]

sd(p_yesf$`Percent Increase`)
sd(p_nof$`Percent Increase`)

summary(p_yesf)
plot(p_yesf$`Percent Increase`)
plot(p_nof$`Percent Increase`)

plot(amf$`Percent Increase`, xlab = "Sample number", ylab="Percent Difference") 

#removing outliers with percent increase above 2000%
amf_noout <- amf[amf$`Percent Increase`< 20,]

sd(amf_noout$`Percent Increase`)

plot(amf_noout$`Percent Increase`, xlab = "Sample number", ylab="Percent Difference") 


mics_added <- amf_noout[amf_noout$NONMYCOCONTROL2 == "microbes_added",]
no_mics <- amf_noout[amf_noout$NONMYCOCONTROL2 == "mics_not_added",]
p_yes <- amf_noout[amf_noout$FERTP == "Pyes",]
p_no <- amf_noout[amf_noout$FERTP == "Pno",]
ster <- amf_noout[amf_noout$STERILIZED == "STERyes",]
no_ster <- amf_noout[amf_noout$STERILIZED == "STERno",]

summary(amf_noout$`Percent Increase`)

#P fertilizer
sd(p_yes$`Percent Increase`)
summary(p_yes)
sd(p_no$`Percent Increase`)
summary(p_no)

barplot(c(mean(p_no$`Percent Increase`), mean(p_yes$`Percent Increase`), mean(amf_noout$`Percent Increase`)), 
        names.arg = c("p_no", "p_yes", "combined"), xlab = "P fertilizer application",
        ylab = "Percent Difference", col = c("gray"), ylim = c(0,1.2))

#microbes
sd(mics_added$`Percent Increase`)
summary(mics_added)
sd(no_mics$`Percent Increase`)
summary(no_mics)
barplot(c(mean(no_mics$`Percent Increase`),mean(mics_added$`Percent Increase`)), 
        names.arg = c("no_mics", "mics_added"), ylab = "Percent Difference", xlab = "Microbial treatment",
        col = c("gray"), ylim = c(0,1.5))

#sterilization
sd(ster$`Percent Increase`)
summary(ster)
sd(no_ster$`Percent Increase`)
summary(no_ster)

barplot(c(mean(no_ster$`Percent Increase`), mean(ster$`Percent Increase`)), 
        names.arg = c("no_ster", "ster"), ylab = "Percent Difference",
        xlab = "Sterilization condition", col = c("gray"),
        ylim = c(0,1.2))

barplot(c(mean(no_ster$trt_mass),  mean(ster$trt_mass), mean(no_ster$ctrl_mass),
          mean(ster$ctrl_mass)),axes = TRUE, 
        names.arg = c("no ster trt", "ster trt", "no ster ctrl", "ster ctrl"),
        ylim = c(0,250), ylab = "Average biomass (g)", xlab = "Sterilization condition", col = c("lightblue", "lightblue", "lightgreen", "lightgreen"))

