load(file="hfi_cc_2018.RData")
data<-hfi_cc_2018

summary(data)
names(data)

sub <- subset(data,year==2016)
head(sub)

rel_data <- data.frame("country" = sub$countries, "region" = sub$region,
                       
                       "personalfreedom" = sub$pf_score, "rol" = sub$pf_rol,
                       "religion" = sub$pf_religion, "expression" = sub$pf_expression,
                       "safety" = sub$pf_ss, "relationships" = sub$pf_identity,
                       "movement" = sub$pf_movement, "assembly" = sub$pf_association,
                       
                       "economicfreedom" = sub$ef_score, "govsize" = sub$ef_government,
                       "legalsystems" = sub$ef_legal, "money" = sub$ef_money,
                       "trade" = sub$ef_trade, "regulation" = sub$ef_regulation,
                       
                       "hf_score" = sub$hf_score, "hf_quartile" =sub$hf_quartile)

attach(rel_data)

rel_data$hf_quartile <- as.factor(rel_data$hf_quartile)
plot(personalfreedom, economicfreedom)
reg_pf <- lm(economicfreedom ~ personalfreedom)
summary(reg_pf)
#abline(reg_pf)

plot(rol, economicfreedom)
reg_rol <- lm(economicfreedom ~ rol)
summary(reg_rol)
#abline(reg_rol)

plot(religion, economicfreedom)
reg_religion <- lm(economicfreedom ~ religion)
summary(reg_religion)
#abline(reg_religion)

plot(expression, economicfreedom)
reg_expression <- lm(economicfreedom ~ expression)
summary(reg_expression)
#abline(reg_expression)

plot(safety, economicfreedom)
reg_safety <- lm(economicfreedom ~ safety)
summary(reg_safety)
#abline(reg_safety)

plot(relationships, economicfreedom)  ######## help #########
plot(movement, economicfreedom)       ######## help #########
plot(assembly, economicfreedom)       ######## help #########

mr_ef <- lm(economicfreedom ~ rol + religion + expression + safety + relationships + movement + assembly)
summary(mr_ef)

mr_rol <- lm(rol ~ govsize + legalsystems + money + trade + regulation)
summary(mr_rol)


cor(cbind(govsize,legalsystems,money, trade, regulation), rol, use='pairwise.complete.obs')

cor(cbind(rol,govsize,legalsystems,money, trade, regulation), use='pairwise.complete.obs')
