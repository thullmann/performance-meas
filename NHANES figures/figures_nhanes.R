# NHANES data example for introduction 
# association of age with BMI

library(ggplot2)
library(gridExtra)
library(splines)
library(mfp2)
library(rms)

my_theme = function(){
  theme_bw() +
    theme(panel.grid = element_blank(), text = element_text(size=18))
}

# preprocessed NHANES data
explanation_data = readRDS(file = "explanation_data.rds")

# Figure 1 ########################################################################

colors <- c("mean BMI by age" = "black", "linear fit" = "#009E73", "fractional polynomials" = "#0072B2", "B-splines df=3" = "#E69F00", "B-splines df=15" = "#CC79A7")
plot_fig1a = ggplot(explanation_data, aes(age, bmi)) + 
  geom_smooth(method="lm", formula=y ~ cut(x, breaks=seq(20, 80)), se= TRUE, aes(color = "mean BMI by age"), size = 1.5) + 
  geom_smooth(method="lm", formula=y ~ x, aes(color = "linear fit"), se = TRUE, size = 1.5) +
  my_theme() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 12)) +
  scale_color_manual(values = colors, breaks = c("mean BMI by age", "linear fit")) +
  labs(color = "") +
  theme(legend.position = "inside", legend.position.inside = c(.95, .05),
        legend.justification = c("right", "bottom")) +
  coord_cartesian(ylim = c(24, 31))

# mfp2
mod_fp2 <- mfp2(bmi ~ fp(age), family = "gaussian", data = explanation_data)
mod_fp2$transformations
intercept = coef(mod_fp2)[1] - sum(mod_fp2$centers * coef(mod_fp2)[2:length(coef(mod_fp2))])
fp2_f = function(x) intercept + coef(mod_fp2)[2]*log(x/10) + coef(mod_fp2)[3]*(x/10)^3 
explanation_data$bmi_mfp2 = fp2_f(explanation_data$age)

plot_fig1b = ggplot(explanation_data, aes(age, bmi)) + 
  geom_smooth(method = "lm", formula = y ~ cut(x, breaks = seq(20, 80)), se = FALSE, size = 1.5, aes(color = "mean BMI by age")) + 
  geom_line(aes(age, bmi_mfp2, color = "fractional polynomials"), size = 1.5) +
  geom_smooth(method="lm", formula = y ~ bs(x, degree = 1, df = 3), aes(color = "B-splines df=3"), se = FALSE, size = 1.5) +#, linetype ="dashed") +
  geom_smooth(method = "lm", formula = y ~ bs(x, degree = 3, df = 15), aes(color = "B-splines df=15"), se = FALSE, size = 1.5)+ #, linetype = "dotted")+
  my_theme() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 12)) +
  scale_color_manual(values = colors, breaks = c("mean BMI by age", "fractional polynomials", "B-splines df=3", "B-splines df=15")) + 
  theme(legend.position = "inside", legend.position.inside = c(.93, .025),
        legend.justification = c("right", "bottom"), legend.title=element_blank()) +
  coord_cartesian(ylim = c(24, 31))

plot_fig1 = grid.arrange(plot_fig1a, plot_fig1b, nrow = 2)
ggsave(paste0("figures/figure1.pdf"), plot = plot_fig1, dpi = 600, width = 17, height = 18, units = "cm")

# Figure 2 ########################################################################

# restricted cubic splines
fit_rms = ols(formula = bmi ~ rcs(age, c(15, 20, 26.66667, 33.33333, 46.66667, 53.33333, 60, 66.66667, 73.33333, 80, 85)), data = explanation_data)
explanation_data$bmi_rms = predict(fit_rms, explanation_data$age)

colors <- c("ground truth" = "black", "linear fit" = "#009E73", "fractional polynomials" = "#0072B2", "B-splines df=3" = "#E69F00", "B-splines df=15" = "#CC79A7")
plot_fig2 = ggplot(explanation_data, aes(age, bmi)) + 
  geom_line(aes(age, bmi_rms, color = "ground truth"), size = 1.5) +
  geom_line(aes(age, bmi_mfp2, color = "fractional polynomials"), size = 1.5) +
  geom_smooth(method="lm", formula = y ~ bs(x, degree = 1, df = 3), aes(color = "B-splines df=3"), se = FALSE, size = 1.5) +
  geom_smooth(method = "lm", formula = y ~ bs(x, degree = 3, df = 15), aes(color = "B-splines df=15"), se = FALSE, size = 1.5) + 
  my_theme() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 12)) +
  scale_color_manual(values = colors, breaks = c("ground truth", "fractional polynomials", "B-splines df=3", "B-splines df=15")) +
  theme(legend.position = "inside", legend.position.inside = c(.93, .025),
        legend.justification = c("right", "bottom"), legend.title=element_blank())
ggsave(paste0("figures/figure2.pdf"), plot = plot_fig2, dpi = 600, width = 17, height = 9, units = "cm")


# Graphical abstract ###############################################################

plot_graphabstract = ggplot(explanation_data, aes(age, bmi)) + 
  geom_line(aes(age, bmi_mfp2, color = "fractional polynomials"), size = 1.5) +
  geom_smooth(method="lm", formula = y ~ bs(x, degree = 1, df = 3), aes(color = "B-splines df=3"), se = FALSE, size = 1.5) +
  geom_smooth(method = "lm", formula = y ~ bs(x, degree = 3, df = 15), aes(color = "B-splines df=15"), se = FALSE, size = 1.5) + 
  geom_line(aes(age, bmi_rms, color = "ground truth"), size = 1.5) +
  my_theme() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 12)) +
  scale_color_manual(values = colors, breaks = c("ground truth", "fractional polynomials", "B-splines df=3", "B-splines df=15")) +
  theme(legend.position = "none", axis.text.x = element_blank(), axis.text.y = element_blank()) + xlab("X") + ylab("Y")
ggsave(paste0("figures/graphabstract.pdf"), plot = plot_graphabstract, dpi = 600, width = 17, height = 9, units = "cm")

