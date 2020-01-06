library(sjmisc)
library(sjPlot)
library(ggplot2)
library(glmmTMB)

model4.1x2 <- glmmTMB(feeding ~ I(Exact_age_chick^2)*Std_jdate + Peeped_chick_count + Exact_age_chick + (1|Brood_ID) + (1|Subject)  + offset(log(Usable_video)), data = bbyvid, ziformula = ~1, family = nbinom2(link = "log"))


plot_model(plotmodel4.1x, type = "pred", terms = c("Exact_age_chick", "Peeped_chick_count")) + 
  theme_bw() + 
  geom_line(size=1)

plot_model(plotmodel4.1x, type = "pred", terms = c("Exact_age_chick", "Std_jdate")) + 
  theme_bw() + 
  geom_line(size=1)

plot_model(plotmodel4.1x, type = "int", terms = c("Exact_age_chick", "Std_jdate"),
           legend.title = "Standard Julian Date") +
  theme_bw() + 
  geom_line(size=1)

plot_models(plotmodel4.1x, model4.3x, model4.2x, transform = "exp", 
            show.p = TRUE, title = "Top 3 provisioning models (dAIC < 2)",
            axis.labels = c("Brood size 2", "Brood size 3", "Chick age", "Chick age*date", 
                            "Habitat", "Chick age^2", "Sex", "Julian date"),
            m.labels = c("Chick age*date + brood", "Chick age*date + brood + habitat", 
                         "Chick age*date + brood + sex")) + 
  theme_bw()

plot_model(plotmodel4.1x, transform = "exp", 
           show.p = TRUE, title = "Top provisioning model", sort.est = TRUE, show.values = TRUE,
           value.offset = .3, axis.lim = c(0.75, 2),
           axis.labels = c("Brood size 2", "Brood size 3", "Chick age", "Chick age*date", 
                           "Chick age^2", "Julian date")) + 
  theme_bw()


##########
# Generate dataframes with needed model variables to generate prediction
# https://github.com/glmmTMB/glmmTMB/issues/378

preddata <- bbyvid[c("Video_number", "feeding", "Subject", "Brood_ID", "Exact_age_chick", "Habitat", 
                     "Julian_date", "Std_jdate", "Usable_video", "Peeped_chick_count")]
preddata <- preddata[!duplicated(preddata$Video_number), ]
# Removing duplicates because there is no difference bw sexes

## Feeding per min
initial <- predict(model4.1x2, preddata, type="response")
preddata$pred.f.per.vid <- initial
preddata$per_min <- preddata$feeding/preddata$Usable_video
preddata$per_hr <- preddata$per_min*60
preddata$predper_min <- preddata$pred.f.per.vid/preddata$Usable_video
preddata$predper_hr <- preddata$predper_min*60
preddata$predper_chkhr <- preddata$predper_hr/preddata$Peeped_chick_count
preddata$Date <- ifelse(preddata$Julian_date >= 190, "Late summer", "Early summer")

bbyvid$provrate <- (bbyvid$feeding/bbyvid$Usable_video)*60
bbyvid$cleanrate <- (bbyvid$cleaning/bbyvid$Usable_video)*60
bbyvid$feedingperchkhr <- ((bbyvid$feeding/bbyvid$Peeped_chick_count)/bbyvid$Usable_video)*60
bbyvid$Date <- ifelse(bbyvid$Julian_date >= 190, "Late summer", "Early summer")


library(ggplot2)
# Colors
colors_sex <- c("female" = "#F47C89", "male" = "#7b758e")
colors_hab <- c("CC" = "#76d6b0", "Savanna" = "#FFB84D")
colors_dat <- c("Early summer" = "#6bd2db", "Late summer" = "#fc913a")

# BY BROOD SIZE
ggplot(preddata, aes(Exact_age_chick, pred.f.per.vid)) +
  stat_smooth(method = glm, formula = y ~ x + I(x^2), 
              aes(y = pred.f.per.vid, color=as.factor(Peeped_chick_count), 
                  fill=as.factor(Peeped_chick_count)), alpha = 0.2,
              se = TRUE, level = 0.95, fullrange = TRUE) +
  labs(x = "Chick age (day)", y = "Predicted provisioning (count)") + 
  guides(color=guide_legend("Brood Size"), fill = FALSE) +
  theme_classic() +
  theme(text=element_text(size=12, family="mono"), legend.position = "bottom") 


### Brood size prov per hr
ggplot(preddata, aes(Exact_age_chick, predper_hr), color=as.factor(Peeped_chick_count), 
       fill=as.factor(Peeped_chick_count)) +
  stat_smooth(method = glm, formula = y ~ x + I(x^2), 
              aes(y = predper_hr, color=as.factor(Peeped_chick_count), 
                  fill=as.factor(Peeped_chick_count)), alpha = 0.2,
              se = TRUE, level = 0.95, fullrange = TRUE) +
  labs(x = "Chick age (day)", y = "Predicted provisioning \n (per hour)") + 
  guides(color=guide_legend("Brood Size"), fill = FALSE) +
  theme_classic() +
  theme(text=element_text(size=12, family="mono"), legend.position = "bottom") 


## Overall prov/hr
ggplot(preddata, aes(Exact_age_chick, predper_hr)) +
  stat_smooth(method = glm, formula = y ~ x + I(x^2), 
              aes(y = predper_hr, color="deeppink4", 
                  fill="deeppink4"), alpha = 0.2,
              se = TRUE, level = 0.95, fullrange = TRUE) +
  labs(x = "Chick age (day)", y = "Predicted provisioning (per hour)") +
  theme_classic() +
  theme(text=element_text(size=12, family="mono"), legend.position = "none") 


# Overall prov/chk/hr
ggplot(preddata, aes(Exact_age_chick, predper_chkhr)) +
  stat_smooth(method = glm, formula = y ~ x + I(x^2), 
              aes(y = predper_chkhr, color="deeppink4", 
                  fill="deeppink4"), alpha = 0.2,
              se = TRUE, level = 0.95, fullrange = TRUE) +
  labs(x = "Chick age (day)", y = "Predicted provisioning \n (per chick/hour)") +
  theme_classic() +
  theme(text=element_text(size=12, family="mono"), legend.position = "none") 


#### Predicted provisioning rate for date
ggplot(preddata, aes(Exact_age_chick, predper_chkhr)) +
  stat_smooth(method = glm, formula = y ~ x + I(x^2), 
              aes(y = predper_chkhr, color=Date, 
                  fill=Date), alpha = 0.2,
              se = TRUE, level = 0.95, fullrange = TRUE) +
  labs(x = "Chick age (day)", y = "Predicted provisioning \n (per chick/hour)") + 
  guides(color=guide_legend("Date"), fill = FALSE) +
  scale_fill_manual(values = colors_dat) +
  scale_color_manual(values = colors_dat) +
  theme_classic() +
  theme(text=element_text(size=12, family="mono"), legend.position = "bottom")

# Regular font 
ggplot(preddata, aes(Exact_age_chick, predper_chkhr)) +
  stat_smooth(method = glm, formula = y ~ x + I(x^2), 
              aes(y = predper_chkhr, color=Date, 
                  fill=Date), alpha = 0.2,
              se = TRUE, level = 0.95, fullrange = TRUE) +
  labs(x = "Chick age (day)", y = "Predicted provisioning \n (per chick/hour)") + 
  guides(color=guide_legend("Date")) +
  scale_fill_manual(values = colors_dat) +
  scale_color_manual(values = colors_dat) +
  theme_classic() +
  theme(legend.position = "bottom")

# Big font for powerpoint
ggplot(preddata, aes(Exact_age_chick, predper_chkhr)) +
  stat_smooth(method = glm, formula = y ~ x + I(x^2), 
              aes(y = predper_chkhr, color=Date, 
                  fill=Date), alpha = 0.2,
              se = TRUE, level = 0.95, fullrange = TRUE) +
  labs(x = "Chick age (day)", y = "Predicted provisioning \n (per chick/hour)") + 
  guides(color=guide_legend("Date")) +
  scale_fill_manual(values = colors_dat) +
  scale_color_manual(values = colors_dat) +
  theme_classic() +
  theme(axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14),
        legend.text = element_text(size=10),
        text = element_text(size=14),
        legend.position = "bottom")

# Poster 1000 x 550
ggplot(preddata, aes(Exact_age_chick, predper_chkhr)) +
  stat_smooth(method = glm, formula = y ~ x + I(x^2), 
              aes(y = predper_chkhr, color=Date, 
                  fill=Date), alpha = 0.2,
              se = TRUE, level = 0.95, fullrange = TRUE) +
  labs(x = "Chick age (day)", y = "Predicted provisioning \n (per chick/hour)") + 
  guides(color=guide_legend("Date")) +
  scale_fill_manual(values = colors_dat) +
  scale_color_manual(values = colors_dat) +
  theme_classic() +
  theme(legend.position = "right", text=element_text(size=24)) +
  guides(size=FALSE)

### Grayscale significant model chickage*Jdate plot RATE  
ggplot(
  bbyvid, aes(x=Exact_age_chick, y=feedingperchkhr, color=Date, fill=Date)) + 
  geom_point(aes(colour=Date, shape=Date)) + 
  labs(x = "Chick age (day)", 
       y = "Provisioning (per chick/hour)") + 
  geom_smooth(method=glm, formula = y ~ x + I(x^2), aes(y=feedingperchkhr, 
                                                        color=as.factor(Date), fill=as.factor(Date)),
              se = TRUE, level = 0.95, fullrange = TRUE, alpha = 0.2) +
  scale_color_grey(start = 0.4, end = 0.7) +
  scale_fill_grey(start = 0.3, end = 0.8) +
  theme_bw() +
  theme(text=element_text(size=12, family="mono"), legend.position = "bottom") 