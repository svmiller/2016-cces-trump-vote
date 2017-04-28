setwd("~/Dropbox/projects/blog-posts/2016-cces-trump")

# Load stuff.
# -----------

library(foreign)
library(ggplot2)
library(car)
library(mirt)
library(maps)
library(rvest)
library(stringr)
library(arm)
library(blme)
library(tidyverse)
library(stargazer)
library(AICcmodavg)

theme_steve <- function() {
  theme_bw() +
    theme(panel.border = element_blank(), 
          plot.caption=element_text(hjust=1, size=9,
                                    margin=margin(t=10),
                                    face="italic"),
          plot.title=element_text(hjust=0, size=18,
                                  margin=margin(b=10),
                                  face="bold"),
          axis.title.y=element_text(size=12,hjust=1,
                                    face="italic"),
          axis.title.x=element_text(hjust=1, size=12, face="italic"))
  
}



CCES <- read.dta("~/Dropbox/data/cces/CCES2016_Common_FirstRelease.dta", convert.factors=F)



# Dive into CCES data.
# --------------------

CCES$uid <- seq(1, nrow(CCES))

# Let's start with the race questions.
# ------------------------------------

CCES$angryracism <- CCES$CC16_422c
CCES$whiteadv <- CCES$CC16_422d
CCES$fearraces <- with(CCES, car::recode(CC16_422e, "1=5; 2=4; 3=3; 4=2; 5=1"))
CCES$racerare <- with(CCES, car::recode(CC16_422f, "1=5; 2=4; 3=3; 4=2; 5=1"))

CCES$acograc <- with(CCES, (.5)*(whiteadv + racerare))
CCES$aemprac <- with(CCES, (.5)*(angryracism + fearraces))

# GRM for cognitive racism.

Cograc <- with(CCES, data.frame(uid, whiteadv, racerare))

Cograc$removeme <- with(Cograc, ifelse(is.na(whiteadv) & 
                                         is.na(racerare) , 1, 0))
Cograc <- subset(Cograc, removeme == 0)
Cograc$removeme <- NULL

CogracM <- mirt(Cograc[ ,  c(2,3)], model = 1,
              itemtype = "graded", SE = TRUE, verbose = FALSE)

cogracscores <- fscores(CogracM, full.scores = TRUE, full.scores.SE = TRUE)
Cograc <- cbind(Cograc, cogracscores)
Cograc <- plyr::rename(Cograc, c("F1" = "lcograc", "SE_F1" = "se_lcograc"))
Cograc <- with(Cograc, data.frame(uid, lcograc, se_lcograc))

CCES <- plyr::join(CCES, Cograc, by=c("uid"), type="left", match="first")

# GRM for empathetic racism.

Emprac <- with(CCES, data.frame(uid, angryracism, fearraces))

Emprac$removeme <- with(Emprac, ifelse(is.na(angryracism) & 
                                         is.na(fearraces) , 1, 0))
Emprac <- subset(Emprac, removeme == 0)
Emprac$removeme <- NULL

EmpracM <- mirt(Emprac[ ,  c(2,3)], model = 1,
                itemtype = "graded", SE = TRUE, verbose = FALSE)

empracscores <- fscores(EmpracM, full.scores = TRUE, full.scores.SE = TRUE)
Emprac <- cbind(Emprac, empracscores)
Emprac <- plyr::rename(Emprac, c("F1" = "lemprac", "SE_F1" = "se_lemprac"))
Emprac <- with(Emprac, data.frame(uid, lemprac, se_lemprac))

CCES <- plyr::join(CCES, Emprac, by=c("uid"), type="left", match="first")

# Vote choice.
# ------------

# CCES$votetrump <- with(CCES, ifelse(CC16_410a == 1, 1, 0))
CCES$votetrump <- with(CCES, car::recode(CC16_410a, "1=1; 2:5=0; 6:7=NA; 8=1"))
CCES$voteobama <- with(CCES, ifelse(CC16_326 == 1, 1, 0))
CCES$voteswitchdr <- with(CCES, ifelse(voteobama == 1 & votetrump == 1, 1, 0))

CCES$voteclinton <- with(CCES, car::recode(CC16_410a, "1=0; 2=1; 3:5=0; 6:7=NA; 8=1"))
CCES$voteromney <- with(CCES, ifelse(CC16_326 == 2, 1, 0))
CCES$voteswitchrd <- with(CCES, ifelse(voteclinton == 1 & voteromney == 1, 1, 0))


# Play with Party ID questions
# ----------------------------

# seven-point party id appears to be similar to GSS and ANES items.
# Indies from pid3 get asked in pid7 to lean themselves (or not).
# I'll treat the 8s as NAs

CCES$pid7na <- with(CCES, car::recode(pid7, "8=NA"))

CCES$dem <- with(CCES, ifelse(pid3 == 1, 1, 0))
CCES$gop <- with(CCES, ifelse(pid3 == 2, 1, 0))
CCES$ind <- with(CCES, ifelse(pid3 == 3, 1, 0)) # I'll keep the "Others" out.

# Identifying information for random effects.
# -------------------------------------------

fipsurl <- read_html("https://www.census.gov/geo/reference/ansi_statetables.html")

Fips <- html_table(html_nodes(fipsurl, "table")[[1]])

names(Fips) <-c("state","inputstate","stateabb")

CCES <- plyr::join(CCES, Fips, by=c("inputstate"), type="left", match="first")

regionurl <- read_html("https://github.com/cphalpert/census-regions/blob/master/us%20census%20bureau%20regions%20and%20divisions.csv")
Regions <- html_table(html_nodes(regionurl, "table")[[1]])

Regions$X1 <- Regions$X2 <- NULL
Regions <- Regions[-1,]
names(Regions) <- c("stateabb", "regioncon","region")

CCES <- plyr::join(CCES, Regions, by=c("stateabb"), type="left", match="first")

# "Controls"...
# -------------

CCES$birthyr[CCES$birthyr == 2] <- NA
CCES$age <- 2016-CCES$birthyr

CCES$generation <- NA # http://www.pewresearch.org/fact-tank/2016/04/25/millennials-overtake-baby-boomers/
CCES$generation <- with(CCES, ifelse(birthyr > 1900 & birthyr <= 1945, 
                                       "1: Greatest/Silent", generation ))
CCES$generation <- with(CCES, ifelse(birthyr >= 1946 & birthyr <= 1964, 
                                       "2: Baby Boomer", generation ))
CCES$generation <- with(CCES, ifelse(birthyr >= 1965 & birthyr <= 1980, 
                                       "3: Gen X", generation ))
CCES$generation <- with(CCES, ifelse(birthyr >= 1981, 
                                       "4: Millennial", generation ))

CCES$female <- CCES$gender - 1
CCES$collegeed <- with(CCES, ifelse(educ >= 5, 1, 0))

CCES$white <- with(CCES, ifelse(race == 1, 1, 0))
CCES$black <- with(CCES, ifelse(race == 2, 1, 0))
CCES$hisplat <- with(CCES, ifelse(race == 3, 1, 0))
CCES$otherrace <- with(CCES, ifelse(race == 4, 1, 0))

CCES$famincr <- with(CCES, ifelse(faminc == 31 | faminc == 97, NA, faminc))
CCES$famincr <- with(CCES, ifelse(famincr >= 13, 13, famincr))

CCES$ideo <- with(CCES, ifelse(ideo5 == 6, NA, ideo5))

CCES$bornagain <- with(CCES, ifelse(pew_bornagain == 1, 1, 0))
CCES$religimp <- with(CCES, car::recode(pew_religimp, "4=1; 3=2; 2=3; 1=4"))
CCES$churchatd <- with(CCES, car::recode(pew_churatd, "7=NA; 6=1; 5=2; 4=3; 3=4; 2=5; 1=6"))
CCES$prayerfreq <- with(CCES, car::recode(pew_prayer, "8=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1"))

Relig <- with(CCES, data.frame(uid, religimp, churchatd, prayerfreq))

Relig$removeme <- with(Relig, ifelse(is.na(religimp) & is.na(churchatd) & 
                                         is.na(prayerfreq) , 1, 0))
Relig <- subset(Relig, removeme == 0)
Relig$removeme <- NULL

ReligM <- mirt(Relig[ ,  c(2,3, 4)], model = 1,
                itemtype = "graded", SE = TRUE, verbose = FALSE)

religscores <- fscores(ReligM, full.scores = TRUE, full.scores.SE = TRUE)
Relig <- cbind(Relig, religscores)
Relig <- plyr::rename(Relig, c("F1" = "lrelig", "SE_F1" = "se_lrelig"))

Relig <- with(Relig, data.frame(uid, lrelig, se_lrelig))

CCES <- plyr::join(CCES, Relig, by=c("uid"), type="left", match="first")


# Start scaling...
# ----------------

CCES$z_acograc <- arm::rescale(CCES$acograc)
CCES$z_aemprac <- arm::rescale(CCES$aemprac)
CCES$z_lcograc <- arm::rescale(CCES$lcograc)
CCES$z_lemprac <- arm::rescale(CCES$lemprac)
CCES$z_pid7na <- arm::rescale(CCES$pid7na)
CCES$z_lrelig <- arm::rescale(CCES$lrelig)
CCES$z_ideo <- arm::rescale(CCES$ideo)
CCES$z_famincr <- arm::rescale(CCES$famincr)
CCES$z_age <- arm::rescale(CCES$age)

Data <- CCES %>%
  select(uid:inputstate, race, angryracism:z_age)

write.table(Data,file="2016-cces-trump.csv",sep=",",row.names=F,na="")

# Start estimating
# ----------------

M1 <- glmer(votetrump ~ z_age + I(z_age^2) + female + collegeed +
              z_famincr + I(z_famincr^2) + 
              black + hisplat + otherrace + 
              z_pid7na + z_ideo +
              z_lrelig +
              z_lcograc + z_lemprac + 
              z_lcograc:z_pid7na + (1 | state),
            data=Data, family=binomial(link="logit"))

M2 <- glmer(votetrump ~ z_age + I(z_age^2) + female + collegeed +
              z_famincr + I(z_famincr^2) +
              z_pid7na + z_ideo +
              z_lrelig +
              z_lcograc + z_lemprac  + 
              z_lcograc:z_pid7na + (1 | state),
            data=subset(Data, white == 1), family=binomial(link="logit"))

M3 <- glmer(votetrump ~ z_age + I(z_age^2) + female + collegeed +
              z_famincr + I(z_famincr^2) + 
              black + hisplat + otherrace + 
              z_pid7na + z_ideo +
              z_lrelig +
              z_lcograc + z_lemprac   + 
              z_lcograc:z_pid7na + (1 | state),
            data=subset(Data, voteobama == 1), family=binomial(link="logit"))

M4 <- glmer(votetrump ~ z_age + I(z_age^2) + female + collegeed +
              z_famincr + I(z_famincr^2) + 
              black + hisplat + otherrace + 
               z_ideo +
              z_lrelig +
              z_lcograc + z_lemprac  + (1 | state),
            data=subset(Data, dem == 1), family=binomial(link="logit"))

# Get table
# ---------

stargazer(M1, M2, M3, M4, type="html", style="ajps",
          column.labels = c("All Voters", "White Voters", "Obama Voters", "Democrats"),
          dep.var.labels = c("Voted for Trump"),
          covariate.labels = c("Age", "Age-squared", "Female", "College Educated",
                               "Family Income", "Family Income-squared",
                               "Black", "Hispanic/Latino", "Other Race",
                               "Partisanship (D to R)", "Ideology (L to C)",
                               "Religiosity", "Cognitive Racism",
                               "Empathetic Racism","Partisanship*Cognitive Racism"),
          omit.stat=c("ll", "aic", "bic"),
          notes=c("All models include a random effect for state, which I omit here for presentation."),
          title="Four Models Explaining a Vote for Trump (CCES, 2016)"
          )

listhold <- list()
for (i in 1:4) {
  
  listhold[[i]] <- data.frame(Variable = rownames(summary(get(paste0("M", i)))$coef),
                              Coefficient = summary(get(paste0("M", i)))$coef[, 1],
                              SE = summary(get(paste0("M", i)))$coef[, 2],
                              modelName = i)
  Allmodels <- do.call(rbind, listhold)
  assign(paste0("M", i, "f"), listhold[[i]])
  
}

interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2) # 95% multiplier

Allmodels$Variable <- plyr::revalue(Allmodels$Variable,
                              c("(Intercept)"="Intercept", 
                                "z_age"="Age",
                                "I(z_age^2)"="Age^2",
                                "collegeed"="College Educated",
                                "z_famincr"="Family Income",
                                "I(z_famincr^2)"="Family Income^2",
                                "z_pid7na"="Partisanship (D to R)",
                                "z_ideo"="Ideology (L to C)",
                                "z_lrelig"="Religiosity",
                                "z_lcograc"="Cognitive Racism",
                                "z_lemprac"="Empathetic Racism",
                                "hisplat"="Hispanic/Latino",
                                "black"="Black",
                                "otherrace"= "Other Race",
                                "female"="Female",
                                "z_pid7na:z_lcograc"="Partisanship*Cognitive Racism"
                              ))

Allmodels$Variable <- factor(Allmodels$Variable,
                             levels(Allmodels$Variable)[c(1,16,13,12,14,11,15,8,5,2,7,10,3,4,6,9)])

# 1, 13, 12, 14, 11, 15, 8, 5, 2, 7,10,3,4,6,9
# 1,9,6,4,3,10,7,2,5,8,15,11,14,12,13

ggplot(Allmodels[Allmodels$Variable != "Intercept",],
       aes(colour = as.factor(modelName), shape=as.factor(modelName))) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  fill = "WHITE") + 
  coord_flip() + theme_steve() +
  scale_colour_discrete(name="Model Specification", 
                        labels=c("Voted for Trump (All Voters)", "Voted for Trump (White Voters)", 
                                 "Vote Switch (Obama Voters)", "Voted for Trump (Democrats)")) +
  scale_shape_manual(name="Model Specification", 
                     labels=c("Voted for Trump (All Voters)", "Voted for Trump (White Voters)", 
                              "Vote Switch (Obama Voters)", "Voted for Trump (Democrats)"),
values=c(15, 16, 17, 18)) +
  ggtitle("A Coefficient Plot of Four Models Explaining a Vote for Trump (CCES, 2016)")

ggsave(file="2016-cces-coefplot.png", width=13.33, height=6.77)

# Simulate quantities of interest.
# --------------------------------

# Start with age.

m1_newdat_age <- with(Data, 
                data.frame(z_age=rep(seq(min(z_age), max(z_age), length.out=300), 3), 
                           age=rep(seq(min(age), max(age), length.out=300), 3),
                           female=1, 
                           collegeed=0, z_famincr=0,
                           "I(z_famincr^2)"=0,black=0,
                           hisplat=0, otherrace=0,
                           z_pid7na=c(rep(unique(Data$z_pid7na)[7], 300), 
                                      rep(median(Data$z_pid7na, na.rm=T), 300),
                                      rep(unique(Data$z_pid7na)[1], 300)),
                           z_ideo=median(Data$z_ideo, na.rm=TRUE),
                           z_lrelig=0, z_lcograc=0,
                           z_lemprac=0
                           )
)

m1_newdat_age$"I(z_age^2)" <- with(m1_newdat_age, z_age^2)
m1_newdat_age$"z_pid7na:z_lcograc" <- with(m1_newdat_age, z_pid7na*z_lcograc)

m1_predvalues_age <- cbind(m1_newdat_age, predictSE(M1, newdata = m1_newdat_age, 
                                            type = "link", se.fit = TRUE, re.form=NA))
m1_predvalues_age$pp <- with(m1_predvalues_age, plogis(fit))
m1_predvalues_age$lb <- with(m1_predvalues_age, plogis(fit - (1.96 * se.fit)))
m1_predvalues_age$ub <- with(m1_predvalues_age, plogis(fit + (1.96 * se.fit)))

m1_predvalues_age$Category <- NA
m1_predvalues_age$Category[m1_predvalues_age$z_pid7na == unique(Data$z_pid7na)[7]] <- "Independent, lean Democrat"
m1_predvalues_age$Category[m1_predvalues_age$z_pid7na == unique(Data$z_pid7na)[1]] <- "Independent, lean Republican"
m1_predvalues_age$Category[m1_predvalues_age$z_pid7na == median(Data$z_pid7na, na.rm=T)] <- "Independent"
m1_predvalues_age$Category <- as.factor(m1_predvalues_age$Category)
m1_predvalues_age$Category <- factor(m1_predvalues_age$Category,
                             levels(m1_predvalues_age$Category)[c(3,1,2)])


ggplot(m1_predvalues_age, aes(x = age, y = pp)) + 
  theme_steve() +
  geom_errorbar(aes(ymin = lb, ymax = ub), alpha = 0.3, linetype = 1) + 
  geom_line(aes(colour = Category, linetype = Category), size = 1) +
#  geom_line(aes(x=z_age), size = 1, color="red", linetype="dashed") +
  ylab("Predicted Probability of a Trump Vote") +
  xlab("Range of Age") +
  theme(legend.position="bottom") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by=.1)) +
  scale_x_continuous(breaks=c(seq(20, 100, by=5)), limits=c(18,100)) +
  geom_hline(aes(yintercept=.5), linetype="dashed") +
  ggtitle("Vote for Trump (All Voters, by Age)")

ggsave(file="2016-cces-trump-m1-age.png")

# 1000/508

# now family income...


m1_newdat_faminc <- with(Data, 
                      data.frame(z_age=0, 
                                 "I(z_famincr^2)"=0,
                                 female=1, 
                                 collegeed=0, 
                                 z_famincr=c(rep(unique(Data$z_famincr)[2:14], 3)),
                                 famincr=c(rep(unique(Data$famincr)[2:14], 3)),
                                 black=0,
                                 hisplat=0, otherrace=0,
                                 z_pid7na=c(rep(unique(Data$z_pid7na)[7], 13), 
                                            rep(median(Data$z_pid7na, na.rm=T), 13),
                                            rep(unique(Data$z_pid7na)[1], 13)),
                                 z_ideo=median(Data$z_ideo, na.rm=TRUE),
                                 z_lrelig=0, z_lcograc=0,
                                 z_lemprac=0
                      )
)

m1_newdat_faminc$"I(z_famincr^2)" <- with(m1_newdat_faminc, z_famincr^2)
m1_newdat_faminc$"z_pid7na:z_lcograc" <- with(m1_newdat_faminc, z_pid7na*z_lcograc)

m1_predvalues_faminc <- cbind(m1_newdat_faminc, predictSE(M1, newdata = m1_newdat_faminc, 
                                                    type = "link", se.fit = TRUE, re.form=NA))
m1_predvalues_faminc$pp <- with(m1_predvalues_faminc, plogis(fit))
m1_predvalues_faminc$lb <- with(m1_predvalues_faminc, plogis(fit - (1.96 * se.fit)))
m1_predvalues_faminc$ub <- with(m1_predvalues_faminc, plogis(fit + (1.96 * se.fit)))

m1_predvalues_faminc$Category <- NA
m1_predvalues_faminc$Category[m1_predvalues_faminc$z_pid7na == unique(Data$z_pid7na)[7]] <- "Independent, lean Democrat"
m1_predvalues_faminc$Category[m1_predvalues_faminc$z_pid7na == unique(Data$z_pid7na)[1]] <- "Independent, lean Republican"
m1_predvalues_faminc$Category[m1_predvalues_faminc$z_pid7na == median(Data$z_pid7na, na.rm=T)] <- "Independent"
m1_predvalues_faminc$Category <- as.factor(m1_predvalues_faminc$Category)
m1_predvalues_faminc$Category <- factor(m1_predvalues_faminc$Category,
                                     levels(m1_predvalues_faminc$Category)[c(3,1,2)])


ggplot(m1_predvalues_faminc, aes(x = famincr, y = pp)) + 
  theme_steve() +
  geom_errorbar(aes(ymin = lb, ymax = ub), alpha = 0.5, linetype = 1, width=.25) + 
  geom_line(aes(colour = Category, linetype = Category), size = 1) +
  geom_point(aes(colour = Category)) +
  #  geom_line(aes(x=z_age), size = 1, color="red", linetype="dashed") +
  ylab("Predicted Probability of a Trump Vote") +
  xlab("Range of Family Income") +
  theme(legend.position="bottom") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by=.1)) +
  scale_x_continuous(breaks=c(seq(1, 13, by=1)), limits=c(0,14)) +
  geom_hline(aes(yintercept=.5), linetype="dashed") +
  ggtitle("Vote for Trump (All Voters, by Family Income)") +
  labs(caption="1 = Less than $10,000 | 2 = $10,000 - $19,999 | 3 = $20,000 - $29,999 | 4 = $30,000 - $39,999 | 5 = $40,000 - $49,999 | 6 = $50,000 - $59,999 | 7 = $60,000 - $69,999 | 8 = $70,000 - $79,999
       9 = $80,000 - $99,999 | 10 = $100,000 - $119,999 | 11 = $120,000 - $149,999 | 12 = $150,000 - $199,999 | 13 = $200,000 or more")

ggsave(file="2016-cces-trump-m1-faminc.png", width=13.33, height=6.77)

m1_newdat_faminc_colled <- with(Data, 
                                data.frame(z_age=0, 
                                           "I(z_famincr^2)"=0,
                                           female=1, 
                                           collegeed=c(rep(0,13),rep(1,13)), 
                                           z_famincr=c(rep(unique(Data$z_famincr)[2:14], 2)),
                                           famincr=c(rep(unique(Data$famincr)[2:14], 2)),
                                           black=0,
                                           hisplat=0, otherrace=0,
                                           z_pid7na=c(rep(unique(Data$z_pid7na)[1], 26)),
                                           z_ideo=median(Data$z_ideo, na.rm=TRUE),
                                           z_lrelig=0, z_lcograc=0,
                                           z_lemprac=0
                                )
)


m1_newdat_faminc_colled$"I(z_famincr^2)" <- with(m1_newdat_faminc_colled, z_famincr^2)
m1_newdat_faminc_colled$"z_pid7na:z_lcograc" <- with(m1_newdat_faminc_colled, z_pid7na*z_lcograc)

m1_newdat_faminc_colled <- cbind(m1_newdat_faminc_colled, predictSE(M1, newdata = m1_newdat_faminc_colled, 
                                                          type = "link", se.fit = TRUE, re.form=NA))
m1_newdat_faminc_colled$pp <- with(m1_newdat_faminc_colled, plogis(fit))
m1_newdat_faminc_colled$lb <- with(m1_newdat_faminc_colled, plogis(fit - (1.96 * se.fit)))
m1_newdat_faminc_colled$ub <- with(m1_newdat_faminc_colled, plogis(fit + (1.96 * se.fit)))

m1_newdat_faminc_colled$Category <- NA
m1_newdat_faminc_colled$Category[m1_newdat_faminc_colled$collegeed == 0] <- "No College Education"
m1_newdat_faminc_colled$Category[m1_newdat_faminc_colled$collegeed == 1] <- "College Education"
m1_newdat_faminc_colled$Category <- as.factor(m1_newdat_faminc_colled$Category)
m1_newdat_faminc_colled$Category <- factor(m1_newdat_faminc_colled$Category,
                                        levels(m1_newdat_faminc_colled$Category)[c(2,1)])

ggplot(m1_newdat_faminc_colled, aes(x = famincr, y = pp)) + 
  theme_steve() +
  geom_errorbar(aes(ymin = lb, ymax = ub), alpha = 0.5, linetype = 1, width=.25) + 
  geom_line(aes(colour = Category, linetype = Category), size = 1) +
  geom_point(aes(colour = Category)) +
  ylab("Predicted Probability of a Trump Vote") +
  xlab("Range of Family Income") +
  theme(legend.position="bottom") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by=.1)) +
  scale_x_continuous(breaks=c(seq(1, 13, by=1)), limits=c(0,14)) +
  geom_hline(aes(yintercept=.5), linetype="dashed") +
  ggtitle("Vote for Trump (Republican-Leaning Independents, by Family Income and College Education)") +
  labs(caption="1 = Less than $10,000 | 2 = $10,000 - $19,999 | 3 = $20,000 - $29,999 | 4 = $30,000 - $39,999 | 5 = $40,000 - $49,999 | 6 = $50,000 - $59,999 | 7 = $60,000 - $69,999 | 8 = $70,000 - $79,999
       9 = $80,000 - $99,999 | 10 = $100,000 - $119,999 | 11 = $120,000 - $149,999 | 12 = $150,000 - $199,999 | 13 = $200,000 or more")

ggsave(file="2016-cces-trump-m1-faminc-colled.png", width=13.33, height=6.77)

m1_predvalues_faminc$Category[m1_predvalues_faminc$z_pid7na == unique(Data$z_pid7na)[7]] <- "Independent, lean Democrat"
m1_predvalues_faminc$Category[m1_predvalues_faminc$z_pid7na == unique(Data$z_pid7na)[1]] <- "Independent, lean Republican"
m1_predvalues_faminc$Category[m1_predvalues_faminc$z_pid7na == median(Data$z_pid7na, na.rm=T)] <- "Independent"
m1_predvalues_faminc$Category <- as.factor(m1_predvalues_faminc$Category)
m1_predvalues_faminc$Category <- factor(m1_predvalues_faminc$Category,
                                        levels(m1_predvalues_faminc$Category)[c(3,1,2)])


# cognitive racism...

m1_newdat_cograc <- with(Data, 
                      data.frame(z_age=0,
                                 "I(z_age^2)"=0,
                                 female=1, 
                                 collegeed=0, z_famincr=0,
                                 "I(z_famincr^2)"=0,black=0,
                                 hisplat=0, otherrace=0,
                                 z_pid7na=c(rep(unique(Data$z_pid7na)[3], 300),
                                            rep(unique(Data$z_pid7na)[4], 300),
                                            rep(unique(Data$z_pid7na)[7], 300), 
                                            rep(median(Data$z_pid7na, na.rm=T), 300),
                                            rep(unique(Data$z_pid7na)[1], 300),
                                            rep(unique(Data$z_pid7na)[5], 300),
                                            rep(unique(Data$z_pid7na)[6], 300)),
                                 z_ideo=median(Data$z_ideo, na.rm=TRUE),
                                 z_lrelig=0, 
                                 z_lcograc=rep(seq(min(z_lcograc, na.rm=T), max(z_lcograc, na.rm=T), length.out=300), 7),
                                 z_lemprac=0
                      )
)

m1_newdat_cograc$"z_pid7na:z_lcograc" <- with(m1_newdat_cograc, z_pid7na*z_lcograc)


m1_predvalues_cograc <- cbind(m1_newdat_cograc, predictSE(M1, newdata = m1_newdat_cograc, 
                                                          type = "link", se.fit = TRUE, re.form=NA))
m1_predvalues_cograc$pp <- with(m1_predvalues_cograc, plogis(fit))
m1_predvalues_cograc$lb <- with(m1_predvalues_cograc, plogis(fit - (1.96 * se.fit)))
m1_predvalues_cograc$ub <- with(m1_predvalues_cograc, plogis(fit + (1.96 * se.fit)))

m1_predvalues_cograc$Category <- NA
m1_predvalues_cograc$Category[m1_predvalues_cograc$z_pid7na == unique(Data$z_pid7na)[7]] <- "Independent, lean Democrat"
m1_predvalues_cograc$Category[m1_predvalues_cograc$z_pid7na == unique(Data$z_pid7na)[1]] <- "Independent, lean Republican"
m1_predvalues_cograc$Category[m1_predvalues_cograc$z_pid7na == median(Data$z_pid7na, na.rm=T)] <- "Independent"
m1_predvalues_cograc$Category[m1_predvalues_cograc$z_pid7na == unique(Data$z_pid7na)[4]] <- "Not Strong Democrat"
m1_predvalues_cograc$Category[m1_predvalues_cograc$z_pid7na == unique(Data$z_pid7na)[5]] <- "Not Strong Republican"
m1_predvalues_cograc$Category[m1_predvalues_cograc$z_pid7na == unique(Data$z_pid7na)[6]] <- "Strong Republican"
m1_predvalues_cograc$Category[m1_predvalues_cograc$z_pid7na == unique(Data$z_pid7na)[3]] <- "Strong Democrat"


m1_predvalues_cograc$Category <- as.factor(m1_predvalues_cograc$Category)
m1_predvalues_cograc$Category <- factor(m1_predvalues_cograc$Category,
                                        levels(m1_predvalues_cograc$Category)[c(6,4,2,1,3,5,7)])


ggplot(m1_predvalues_cograc, aes(x = z_lcograc, y = pp)) + 
  theme_steve() +
  geom_errorbar(aes(ymin = lb, ymax = ub), alpha = 0.3, linetype = 1) + 
  geom_line(aes(colour = Category, linetype = Category), size = 1) +
  geom_point(aes(colour = Category)) +
  #  geom_line(aes(x=z_age), size = 1, color="red", linetype="dashed") +
  ylab("Predicted Probability of a Trump Vote") +
  xlab("Range of Cognitive Racism") +
  theme(legend.position="bottom") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by=.1)) +
  scale_x_continuous(breaks=c(seq(-.75, 1.25, by=.25)), limits=c(-.75, 1.25)) +
  geom_hline(aes(yintercept=.5), linetype="dashed") +
  ggtitle("Vote for Trump (All Voters, by Cognitive Racism)") +
  scale_colour_manual(values= c('#0000aa', '#204ef8', '#97aaed', '#bebec0', '#ff8181', '#ff0000', '#aa0000'))

ggsave(file="2016-cces-trump-m1-cograc-partisanship.png", width=13.33, height=6.77)

