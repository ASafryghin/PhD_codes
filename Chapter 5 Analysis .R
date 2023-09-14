#Load data
library(dplyr)
modeldata.morph<-read.csv("/Users/alexsafry/Downloads/modeldata.morph.chp5.csv")
modeldata.ga<-read.csv("/Users/alexsafry/Downloads/modeldata.ga.chp5.csv")
modeldata.ga.seq<-read.csv("/Users/alexsafry/Downloads/modeldata.ga.seq.chp5.csv")
modeldata.morph.seq<-read.csv("/Users/alexsafry/Downloads/modeldata.morph.seq.chp5.csv")

modeldata.ga$kin<-as.factor(modeldata.ga$kin)
modeldata.ga$Social_unit<-as.factor(modeldata.ga$Social_unit)
modeldata.ga$Social_unit<-relevel(modeldata.ga$Social_unit, ref="Sonso")

#Model 1 GA_PAU duration
library(brms)

Model1.ga.full.red <- brm(Ga_duration ~
                            SRI + kin*Social_unit+ RankRcp*Social_unit+SexSigRcp*Social_unit +AgeSigRcp*Social_unit+
                            (1|Signaller) + (SRI + kin + RankRcp+AgeSigRcp+SexSigRcp ||Gesture_record) + (SRI + kin + RankRcp +AgeSigRcp +SexSigRcp||Goal),
                          family = lognormal(),
                          prior = prior(cauchy(0,1), class = 'b'),
                          data = modeldata.ga,
                          chains = 3,
                          cores = 8,
                          warmup = 1000,
                          iter = 5000,backend = 'rstan')


Model1.ga.null <- brm(Ga_duration ~
                        (1|Signaller) + (1|Gesture_record) + (1|Goal),
                      family = lognormal(),
                      data = modeldata.ga,
                      chains = 3,
                      cores = 8,
                      warmup = 1000,
                      iter = 5000,backend = 'rstan')#Done

Model1.ga.full <- add_criterion(Model1.ga.full, "loo")
Model1.ga.null <- add_criterion(Model1.ga.null, "loo")
full.null.compare.GA <-
  loo_compare(Model1.ga.full, Model1.ga.null)
summary(Model1.ga.full)

Model1.ga.full.red <- brm(Ga_duration ~
                            SRI + kin*Social_unit+ RankRcp*Social_unit+SexSigRcp*Social_unit +AgeSigRcp*Social_unit+
                            (1|Signaller) + (SRI + kin + RankRcp+AgeSigRcp+SexSigRcp ||Gesture_record) + (SRI + kin + RankRcp +AgeSigRcp +SexSigRcp||Goal),
                          family = lognormal(),
                          prior = prior(cauchy(0,1), class = 'b'),
                          data = modeldata.ga,
                          chains = 3,
                          cores = 8,
                          warmup = 1000,
                          iter = 5000,backend = 'rstan')
Model1.ga.full.red <- add_criterion(Model1.ga.full.red, "loo")

full.null.compare.GA <-
  loo_compare(Model1.ga.full.red, Model1.ga.null)

tab_model(Model1.ga.full.red)
summary(Model1.ga.full.red)

##Post hoc
library(brms)
library(emmeans)
library(gt)
library(dplyr)
library(kableExtra)
myModelSlopes <- lstrends(Model1.ga.full.red, "Social_unit", var="SRI")
pairs(myModelSlopes)%>%kbl(digits = 2)%>% row_spec(0, bold = TRUE)%>% kable_classic(full_width = F, html_font = "Cambria")
RankComPost<-test(emmeans(Model1.ga.full.red, pairwise ~Social_unit*RankRcp))
RankComPost$contrasts%>%kbl(digits = 2)%>% row_spec(0, bold = TRUE)%>% kable_classic(full_width = F, html_font = "Cambria")
SexComPost<-test(emmeans(Model1.ga.full.red, pairwise ~Social_unit*SexSigRcp))
SexComPost$contrasts%>%kbl(digits = 2)%>% row_spec(0, bold = TRUE)%>% kable_classic(full_width = F, html_font = "Cambria")
AgeComPost<-test(emmeans(Model1.ga.full.red, pairwise ~Social_unit*AgeSigRcp))
AgeComPost$contrasts%>%kbl(digits = 2)%>% row_spec(0, bold = TRUE)%>% kable_classic(full_width = F, html_font = "Cambria")
kin<-test(emmeans(Model1.ga.full.red, pairwise ~Social_unit*kin))
kin$contrasts%>%kbl(digits = 2)%>% row_spec(0, bold = TRUE)%>% kable_classic(full_width = F, html_font = "Cambria")


#Morph PAU Duration
Model1.morph <- brm(Ga_duration ~1+
                      SRI*Social_unit + kin*Social_unit+ RankRcp*Social_unit+
                      (1|Signaller) + (SRI + kin + RankRcp ||morph) + (SRI + kin + RankRcp ||Goal),
                    family = lognormal(),
                    prior = prior(cauchy(0,1), class = 'b'),
                    data = modeldata.morphh,
                    chains = 3,
                    cores = 8,
                    warmup = 1000,
                    iter = 5000,backend = 'rstan')#Done

Model1.morph.null <- brm(Ga_duration ~
                        (1|Signaller) + (1|Morph) + (1|Goal),
                      family = lognormal(),
                      data = modeldata.morph,
                      chains = 3,
                      cores = 8,
                      warmup = 1000,
                      iter = 5000,backend = 'rstan')#Done

Model1.morph <- add_criterion(Model1.morph, "loo")
Model1.morph.null <- add_criterion(Model1.morph.null, "loo")
full.null.compare.GA <-
  loo_compare(Model1.morph, Model1.morph.null)
summary(Model1.morph)


Model1.morph.full.red <- brm(Ga_duration ~
                               SRI + kin*Social_unit+ RankRcp*Social_unit+SexSigRcp*Social_unit +AgeSigRcp*Social_unit+
                               (1|Signaller) + (SRI + kin + RankRcp+AgeSigRcp+SexSigRcp ||morph) + (SRI + kin + RankRcp +AgeSigRcp +SexSigRcp||Goal),
                             family = lognormal(),
                             prior = prior(cauchy(0,1), class = 'b'),
                             data = modeldata.morph,
                             chains = 3,
                             cores = 4,
                             warmup = 1000,
                             iter = 5000,backend = 'rstan')


Model1.morph.full.red <- add_criterion(Model1.morph.full.red, "loo")

full.null.compare.GA <-
  loo_compare(Model1.morph.full.red, Model1.morph.null)

tab_model(Model1.morph.full.red)
summary(Model1.morph.full.red)

##Posthoc
myModelSlopes <- lstrends(Model1.morph.full.red, "Social_unit", var="SRI")
pairs(myModelSlopes)%>%kbl(digits = 2)%>% row_spec(0, bold = TRUE)%>% kable_classic(full_width = F, html_font = "Cambria")
RankComPost<-test(emmeans(Model1.morph.full.red, pairwise ~Social_unit*RankRcp))
RankComPost$contrasts%>%kbl(digits = 2)%>% row_spec(0, bold = TRUE)%>% kable_classic(full_width = F, html_font = "Cambria")
SexComPost<-test(emmeans(Model1.morph.full.red, pairwise ~Social_unit*SexSigRcp))
SexComPost$contrasts%>%kbl(digits = 2)%>% row_spec(0, bold = TRUE)%>% kable_classic(full_width = F, html_font = "Cambria")
AgeComPost<-test(emmeans(Model1.morph.full.red, pairwise ~Social_unit*AgeSigRcp))
AgeComPost$contrasts%>%kbl(digits = 2)%>% row_spec(0, bold = TRUE)%>% kable_classic(full_width = F, html_font = "Cambria")
kin<-test(emmeans(Model1.morph.full.red, pairwise ~Social_unit*kin))
kin$contrasts%>%kbl(digits = 2)%>% row_spec(0, bold = TRUE)%>% kable_classic(full_width = F, html_font = "Cambria")


#Sequence stuff ----
modeldata.ga.seq$kin<-as.factor(modeldata.ga.seq$kin)
modeldata.ga.seq$Social_unit<-as.factor(modeldata.ga.seq$Social_unit)
modeldata.ga.seq$Social_unit<-relevel(modeldata.ga.seq$Social_unit, ref="Sonso")
modeldata.ga.seq$ScaleSRI<-scale(modeldata.ga.seq$SRI)

modeldata.morph.seq$kin<-as.factor(modeldata.morph.seq$kin)
modeldata.morph.seq$Social_unit<-as.factor(modeldata.morph.seq$Social_unit)
modeldata.morph.seq$Social_unit<-relevel(modeldata.morph.seq$Social_unit, ref="Sonso")
modeldata.morph.seq$ScaleSRI<-scale(modeldata.morph.seq$SRI)

ModelSeque.morph <- brm(sequenceyes ~
                          ScaleSRI*Social_unit + kin*Social_unit+ RankRcp*Social_unit+ + SexSigRcp*Social_unit + AgeSigRcp*Social_unit+
                          (1|Signaller)  + (ScaleSRI + kin + RankRcp + SexSigRcp + AgeSigRcp||Goal) +  (ScaleSRI + kin + RankRcp + SexSigRcp + AgeSigRcp||morph),
                        family = bernoulli(link='logit'),
                        prior = prior(cauchy(0,2.5), class = 'b'),
                        data = modeldata.morph.seq,
                        chains = 3,
                        cores = 8,
                        warmup = 1000,
                        iter = 5000,
                        control = list(adapt_delta = 0.95))

ModelSeque.null.morph <- brm(sequenceyes ~
                               1+
                               (1|Signaller)  + (1|Goal) +  (1|morph),
                             family = bernoulli(link='logit'),
                             
                             data = modeldata.morph.seq,
                             chains = 3,
                             cores = 4,
                             warmup = 1000,
                             iter = 5000,
                             control = list(adapt_delta = 0.95))

ModelSeque.morph <- add_criterion(ModelSeque.morph, "loo")
ModelSeque.null.morph <- add_criterion(ModelSeque.null.morph, "loo")

full.null.compare.morph.seq <-
  loo_compare(ModelSeque.morph, ModelSeque.null.morph)
full.null.compare.morph.seq

#Reduced model
ModelSeque.morph.red <- brm(sequenceyes ~
                              ScaleSRI + kin*Social_unit+ RankRcp*Social_unit+ + SexSigRcp*Social_unit + AgeSigRcp*Social_unit+
                              (1|Signaller)  + (ScaleSRI + kin + RankRcp + SexSigRcp + AgeSigRcp||Goal) +  (ScaleSRI + kin + RankRcp + SexSigRcp + AgeSigRcp||morph),
                            family = bernoulli(link='logit'),
                            prior = prior(cauchy(0,2.5), class = 'b'),
                            data = modeldata.morph.seq,
                            chains = 3,
                            cores = 8,
                            warmup = 1000,
                            iter = 5000,
                            control = list(adapt_delta = 0.95))

ModelSeque.morph.red <- add_criterion(ModelSeque.morph.red, "loo")
Compare.seq.morph <-
  loo_compare(ModelSeque.morph.red, ModelSeque.null.morph)
Compare.seq.morph


#Create tables for reduced models
summary(ModelSeque.morph.red)
RankComPost.m.red<-test(emmeans(ModelSeque.morph.red, pairwise ~Social_unit*RankRcp))
RankComPost.m.red$contrasts%>%kbl(digits = 2)%>% row_spec(0, bold = TRUE)%>% kable_classic(full_width = F, html_font = "Cambria")
SexComPost.m.red<-test(emmeans(ModelSeque.morph.red, pairwise ~Social_unit*SexSigRcp))
SexComPost.m.red$contrasts%>%kbl(digits = 2)%>% row_spec(0, bold = TRUE)%>% kable_classic(full_width = F, html_font = "Cambria")
AgeComPost.m.red<-test(emmeans(ModelSeque.morph.red, pairwise ~Social_unit*AgeSigRcp))
AgeComPost.m.red$contrasts%>%kbl(digits = 2)%>% row_spec(0, bold = TRUE)%>% kable_classic(full_width = F, html_font = "Cambria")
kin.m.red<-test(emmeans(ModelSeque.morph.red, pairwise ~Social_unit*kin))
kin.m.red$contrasts%>%kbl(digits = 2)%>% row_spec(0, bold = TRUE)%>% kable_classic(full_width = F, html_font = "Cambria")



#Repeat for GA ----
ModelSeque.ga <- brm(sequenceyes ~
                       ScaleSRI*Social_unit + kin*Social_unit+ RankRcp*Social_unit+  SexSigRcp*Social_unit + AgeSigRcp*Social_unit+
                       (1|Signaller)  + (ScaleSRI + kin + RankRcp + SexSigRcp + AgeSigRcp||Goal) +  (ScaleSRI + kin + RankRcp + SexSigRcp + AgeSigRcp||Gesture_record),
                     family = bernoulli(link='logit'),
                     prior = prior(cauchy(0,2.5), class = 'b'),
                     data = modeldata.ga.seq,
                     chains = 3,
                     cores = 8,
                     warmup = 1000,
                     iter = 5000,
                     control = list(adapt_delta = 0.95))

ModelSeque.ga.null <- brm(sequenceyes ~
                            1+
                            (1|Signaller)  + (1|Goal) +  (1|Gesture_record),
                          family = bernoulli(link='logit'),
                          
                          data = modeldata.ga.seq,
                          chains = 3,
                          cores = 4,
                          warmup = 1000,
                          iter = 5000,
                          control = list(adapt_delta = 0.95))

ModelSeque.ga <- add_criterion(ModelSeque.ga, "loo")
ModelSeque.ga.null <- add_criterion(ModelSeque.ga.null, "loo")

full.null.compare.GA.seq <-
  loo_compare(ModelSeque.ga, ModelSeque.ga.null)
full.null.compare.GA.seq
summary(ModelSeque.ga)

#reduced model
ModelSeque.ga.red <- brm(sequenceyes ~
                           ScaleSRI + kin*Social_unit+ RankRcp*Social_unit+  SexSigRcp*Social_unit + AgeSigRcp*Social_unit+
                           (1|Signaller)  + (ScaleSRI + kin + RankRcp + SexSigRcp + AgeSigRcp||Goal) +  (ScaleSRI + kin + RankRcp + SexSigRcp + AgeSigRcp||Gesture_record),
                         family = bernoulli(link='logit'),
                         prior = prior(cauchy(0,2.5), class = 'b'),
                         data = modeldata.ga.seq,
                         chains = 3,
                         cores = 8,
                         warmup = 1000,
                         iter = 5000,
                         control = list(adapt_delta = 0.95))


ModelSeque.ga.red <- add_criterion(ModelSeque.ga.red, "loo")
Modelseq.ga.compare <-
  loo_compare(ModelSeque.ga.red, ModelSeque.ga.null)

#Post hoc
summary(ModelSeque.ga.red)
tab_model(ModelSeque.ga.red)
kin.seqga.red<-test(emmeans(ModelSeque.ga.red, pairwise ~Social_unit*kin))
kin.seqga.red$contrasts%>%kbl(digits = 2)%>% row_spec(0, bold = TRUE)%>% kable_classic(full_width = F, html_font = "Cambria")
RankComPost.seqga.red<-test(emmeans(ModelSeque.ga.red, pairwise ~Social_unit*RankRcp))
RankComPost.seqga.red$contrasts%>%kbl(digits = 2)%>% row_spec(0, bold = TRUE)%>% kable_classic(full_width = F, html_font = "Cambria")
SexComPost.seqga.red<-test(emmeans(ModelSeque.ga.red, pairwise ~Social_unit*SexSigRcp))
SexComPost.seqga.red$contrasts%>%kbl(digits = 2)%>% row_spec(0, bold = TRUE)%>% kable_classic(full_width = F, html_font = "Cambria")
AgeComPost.seqga.red<-test(emmeans(ModelSeque.ga.red, pairwise ~Social_unit*AgeSigRcp))
AgeComPost.seqga.red$contrasts%>%kbl(digits = 2)%>% row_spec(0, bold = TRUE)%>% kable_classic(full_width = F, html_font = "Cambria")

