####
# PRESENTATION ANALYSIS
####

# calculate sd and mean
GSS_CRIT <- GSS_14[ ,c(var_n_criteria, "criteria_plus")]
cp_sd <- SD(GSS_CRIT$criteria_plus, na.rm = TRUE)
cp_mean <- mean(GSS_CRIT$criteria_plus, na.rm = TRUE)

#### FIGURE 1 - DISTRIBUTION CRITERIA OF BELONGING

dist_cb <- ggplot(GSS_CRIT, aes(x = criteria_plus)) +
  geom_density(color = "white",
               fill = "#66c2a5") +  
  annotate(geom = "rect",
           xmin = cp_mean - cp_sd,
           xmax = cp_mean + cp_sd,
           ymin = -Inf, ymax = Inf,
           fill = "grey", alpha = 0.4) +
  geom_vline(aes(xintercept = cp_mean),
             color = "#d95f02",
             linetype = "dashed",
             alpha = 0.9) +  
  geom_rug(sides = "b",
           aes(y = 0),
           position = position_jitter(w = 0.020, h = 0),
           colour = "black",
           alpha = 0.05) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size = 11)) +
  labs(title = "Criteria of belonging")

#### FIGURE 2 - WHO CAN TRULY BE AMERICAN - ANSWERS

GSS_CRIT_L <- gather(GSS_CRIT[,1:8],
                     key = "question",
                     value = "answer",
                     na.rm = TRUE,
                     factor_key = TRUE)
GSS_CRIT_L$answer <- as.factor(GSS_CRIT_L$answer)
GSS_CRIT_L$answer <- factor(GSS_CRIT_L$answer, levels(GSS_CRIT_L$answer)[c(2, 3, 1, 4)])
levels(GSS_CRIT_L$question) <- c("born in the US", "have US citizenship",
                                 "lived most of life here", "able to speak English",
                                 "be a Christian", "respect US laws",
                                 "feel American", "have US ancestry")
GSS_CRIT_L$question <- factor(GSS_CRIT_L$question,
                              levels(GSS_CRIT_L$question)[c(2, 4, 6, 7, 3, 1, 5, 8)])
levels(GSS_CRIT_L$answer) <- c("Not at all", "Not very", "Fairly", "Very")
  
bp_crit <- ggplot(GSS_CRIT_L, aes(x = question,
                                  fill = answer)
                  ) +
  geom_bar(position = "fill",
           alpha = 0.8) +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank()
        ) +
  scale_fill_brewer(name = element_blank(),
                    guide = guide_legend(reverse = TRUE),
                    palette = "PRGn"
                    ) +
  scale_y_continuous(labels = percent) +
  labs(subtitle = "How important is each of the following for being truly American?") +
  coord_flip()





##### FIGURE 3. - Criteria of belonging by curent place and of childhood


bp_cb_place <- ggplot(GSS_14, aes(x = r_srcbelt,
                   y = criteria_plus,
                   color = r_res16)) +
  geom_jitter(height = 0.05,
              alpha = 0.7) +
  scale_color_manual(name = "Childhood:",
                     labels = c("Large city or sub.", "City", "Rural"),
                     values = c("#1b9e77", "#7570b3", "#d95f02")
                      ) +
  geom_segment(aes(x = 0.6, xend = 1.4, y = 1.94, yend = 1.94),
               colour = "#525252",
               size = 0.1,
               alpha = 0.7) +
  geom_segment(aes(x = 1.4, xend = 1.6, y = 1.94, yend = 1.96),
               colour = "#525252",
               linetype = "dotted",
               size = 0.1,
               alpha = 0.7) +
  geom_segment(aes(x = 1.6, xend = 2.4, y = 1.96, yend = 1.96),
               colour = "#525252",
               size = 0.1,
               alpha = 0.7) +
  geom_segment(aes(x = 2.6, xend = 3.4, y = 1.77, yend = 1.77),
               colour = "#525252",
               size = 0.1,
               alpha = 0.7) +
  geom_segment(aes(x = 3.4, xend = 3.6, y = 1.77, yend = 1.71),
               colour = "#525252",
               linetype = "dotted",
               size = 0.1,
               alpha = 0.7) +
  geom_segment(aes(x = 3.6, xend = 4.4, y = 1.71, yend = 1.71),
               colour = "#525252",
               size = 0.1,
               alpha = 0.7) +
  geom_segment(aes(x = 4.5, xend = 4.7, y = 2.02, yend = 2.02),
               colour = "#1b9e77",
               alpha = 0.9) +
  geom_segment(aes(x = 4.5, xend = 4.7, y = 1.86, yend = 1.86),
               colour = "#7570b3",
               alpha = 0.9) +
  geom_segment(aes(x = 4.5, xend = 4.7, y = 1.62, yend = 1.62),
               colour = "#d95f02",
               alpha = 0.9) +
  labs(title = "Criteria of belonging and rurality") +
  scale_y_continuous(limits = c(0.95,4)) +
  scale_x_discrete(labels = c("Large city", "Suburbs",
                              "Other urban", "Rural")) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))




#### FIGURE 4. Regressions plot

# first extract coefficients (av. marg. effects) for all thre models
# that are goind to be plotted
# mc_s - small model; mc_r - model with childhood place; mc_f - full model
mc_s <- as.data.frame(summary(margins(clm_small)))
mc_r <- as.data.frame(summary(margins(clm_small_res16)))
mc_f <- as.data.frame(summary(margins(clm_full)))

# copy of the full name of factors from lm models which will be used later
mc_s$full_factor <- mc_s$factor
mc_r$full_factor <- mc_r$factor
mc_f$full_factor <- mc_f$factor

# create new variable with names of the variables from the model

mc_s$var <- mc_s$factor
mc_r$var <- mc_r$factor
mc_f$var <- mc_f$factor

mc_s$var <- c(
  "Age", "Immigrant", "Parents' education", "Income",
  "Education", "Education", "SMSA", "SMSA", "SMSA", "Race",
  "Race", "Sex", "Veteran"
)
mc_r$var <- c(
  "Age", "Immigrant", "Parents' education", "Income", "Education",
  "Education", "Childhood", "Childhood", "SMSA", "SMSA", "SMSA",
  "Race", "Race", "Sex", "Veteran"
)
mc_f$var <- c(
  "Age", "Immigrant", "Political views", "Political views",
  "Political views", "Political views", "Political views",
  "Political views", "Parents' education", "Income", "Education",
  "Education", "Region", "Region", "Region", "Religion",
  "Religion", "Religion", "Religion", "Religion", "Childhood",
  "Childhood", "SMSA", "SMSA", "SMSA", "Race", "Race", "Sex", "Veteran"
)


## remove variable name from the name of the factor

mc_s$factor <- c(
  "One year older", "Yes", "Either has BA", "Increase of $20,000",
  "BA or more", "HS or JC", "Other urban", "Rural",
  "Suburban (top 100)", "Black", "Other", "Female", "Yes"
)
mc_r$factor <- c(
  "One year older", "Yes", "Either has BA", "Increase of $20,000",
  "BA or more", "HS or JC", "Large city or suburbs", "Rural",
  "Other urban", "Rural", "Suburban (top 100)", "Black",
  "Other", "Female", "Yes"
)
mc_f$factor <- c(
  "One year older", "Yes", "Conservative", "Extremly Liberal",
  "Extremly Conservative", "Liberal", "Slightly Conservative",
  "Slightly Liberal", "Either has BA", "Increase of $20,000",
  "BA or more", "HS or JC", "Northeast", "South",
  "West", "Cath. or orth.", "Christian NGG", "Luth., Epsic., or Morm.",
  "None, other, or Jewish", "Sect. or Bapt.", "Large city or suburbs",
  "Rural", "Other urban", "Rural", "Suburban (top 100)", "Black",
  "Other", "Female", "Yes"
)


## assign model name as a new variable to each mdodel
mc_f$model <- as.factor(c("Large"))
mc_s$model <- as.factor(c("Reduced"))
mc_r$model <- as.factor(c("W/ childhood"))

## combine all thre models into one dataframe
MB <- rbind(mc_f, mc_s)
MB <- rbind(MB, mc_r)

## make all the labels to be factors
MB$full_factor <- as.factor(MB$full_factor)
MB$factor <- as.factor(MB$factor)
MB$var <- as.factor(MB$var)
                   
## sort the factors for plotting
MB <- sort_models(MB)


## Change the names of variables, include refernce category,
## so they look nice when plotted.

levels(MB$var)
levels(MB$var)[1] <- "Religion\n[ref: mod. or lib. prot.]"
levels(MB$var)[2] <- "Political views\n[ref: moderate]"
levels(MB$var)[3] <- "Region\n[ref: Midwest]"
levels(MB$var)[4] <- "Childhood\n[ref: medium city]"
levels(MB$var)[5] <- "Education\n[ref: LT HS]"
levels(MB$var)[10] <- "Metrop. area\n[ref: city (top 100)]"
levels(MB$var)[13] <- "Race\n[ref: white]"

## create variable for significance of each variable in the model, and 
## manually inspect which do not reach level of significance
## and mark them appropriately (with FALSE)

MB$sig <- TRUE
Anova(clm_full)
# not sig.
# full - immigrant, veteran, parent BA, srcbelt, region
Anova(clm_small)
# not sig.
# small - immigrant, 
Anova(clm_small_res16)
# not sig.
# small, res16 - immigrant, prt.ba, srcbelt

MB$sig[MB$var == "Immigrant"] <- FALSE
MB$sig[(MB$var == "Veteran") & (MB$model == "full")] <- FALSE
MB$sig[(MB$var == "Parents' education") & (MB$model == "full")] <- FALSE
MB$sig[(MB$var == "Metrop. area\n[ref: city (top 100)]") & (MB$model == "full")] <- FALSE
MB$sig[(MB$var == "Region\n[ref: Midwest]") & (MB$model == "full")] <- FALSE
MB$sig[(MB$var == "Parents' education") & (MB$model == "childhood")] <- FALSE
MB$sig[(MB$var == "Metrop. area\n[ref: city (top 100)]") & (MB$model == "childhood")] <- FALSE


## create data frame that will shade boxes behind variables that 
## we are interested looking at

intrst <- data.frame(var = unique(MB$var),
                     xmn = NA,
                     xmx = NA,
                     ymn = NA,
                     ymx = NA
)

intrst[9,2:5] <- c(-Inf, Inf, -Inf, Inf)
intrst[10,2:5] <- c(-Inf, Inf, -Inf, Inf)


## and finally, create the model


p_m <- ggplot(data = MB, aes(x = reorder(factor, ordr),
                             y = AME,
                             ymin = lower,
                             ymax = upper,
                             colour = model,
                             alpha = sig)) +
  scale_colour_manual(values = c("#1b9e77", "#d95f02", "#7570b3"))

# add intercept line
p_m <- p_m + geom_hline(yintercept = 0,
                        color = "dark gray")

# add marginal effects
p_m <- p_m + geom_pointrange(position = position_dodge(width = 0.5),
                             shape = 21,
                             fill = "white")

# make vars. that do not reach stat.sig. transparent
p_m <- p_m + scale_alpha_discrete(range = c(0.5, 1.0))

# rotate plot and add some labels
p_m <- p_m + coord_flip() +
  labs(x = NULL,
       y = "Average Marginal Effect",
       colour = "Models",
       alpha = "Variable\nsig at p<.5",
       title = "Regression on criteria of national belonging (openess)")

# add facets according to model variables, fix aestethics and add rectangle
p_m_1 <- p_m + facet_grid(var ~ .,
                          scales = "free_y",
                          space = "free_y",
                          switch = "y") +
  theme(strip.text.y = element_text(angle = 180, vjust = 0.9),
        strip.placement = "outside",
        strip.background = element_rect(fill = "#d9d9d9", color = NA),
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(colour = "#d9d9d9"),
        panel.grid.minor.x = element_line(colour = "#d9d9d9"),
        panel.grid.major.y = element_line(colour = "#d9d9d9"),
        panel.grid.minor.y = element_line(colour = "#d9d9d9"),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.title = element_text(size = 10),
        axis.title =  element_text(size = 10),
        title = element_text(size = 11)
  ) +
  geom_rect(data = intrst,
            aes(xmin = xmn,
                xmax = xmx,
                ymin = ymn,
                ymax = ymx),
            alpha = 0.09,
            fill = "black",
            inherit.aes = FALSE,
            show.legend = FALSE)

# save all the plots!
save(dist_cb, cp_mean, cp_sd, bp_crit, bp_cb_place, p_m_1, file = "plots.Rdata")