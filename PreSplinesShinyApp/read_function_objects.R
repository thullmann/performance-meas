# read function objects

###############################################################################
# truth 1 #####################################################################
###############################################################################

truth1_list = readRDS("function objects/truth1.rds")
f_truth1 = truth1_list$f
f_truth1_deriv1 = truth1_list$f_deriv1
f_truth1_deriv2 = truth1_list$f_deriv2
range_truth1 = truth1_list$range
range_truth1_deriv1 = truth1_list$range_deriv1
range_truth1_deriv2 = truth1_list$range_deriv2
roots_truth1 = truth1_list$roots
roots_truth1_deriv1 = truth1_list$roots_deriv1
roots_truth1_deriv2 = truth1_list$roots_deriv2

truth1_estim1_list = readRDS("function objects/truth1_estim1.rds")
f_truth1_estim1 = truth1_estim1_list$f
f_truth1_estim1_deriv1 = truth1_estim1_list$f_deriv1
f_truth1_estim1_deriv2 = truth1_estim1_list$f_deriv2
coef_truth1_estim1 = truth1_estim1_list$coef 
roots_truth1_estim1 = truth1_estim1_list$roots
roots_truth1_estim1_deriv1 = truth1_estim1_list$roots_deriv1
roots_truth1_estim1_deriv2 = truth1_estim1_list$roots_deriv2

truth1_estim2_list = readRDS("function objects/truth1_estim2.rds")
f_truth1_estim2 = truth1_estim2_list$f
f_truth1_estim2_deriv1 = truth1_estim2_list$f_deriv1
f_truth1_estim2_deriv2 = truth1_estim2_list$f_deriv2
coef_truth1_estim2 = truth1_estim2_list$coef 
roots_truth1_estim2 = truth1_estim2_list$roots
roots_truth1_estim2_deriv1 = truth1_estim2_list$roots_deriv1
roots_truth1_estim2_deriv2 = truth1_estim2_list$roots_deriv2

truth1_estim3_list = readRDS("function objects/truth1_estim3.rds")
f_truth1_estim3 = truth1_estim3_list$f
f_truth1_estim3_deriv1 = truth1_estim3_list$f_deriv1
f_truth1_estim3_deriv2 = truth1_estim3_list$f_deriv2
coef_truth1_estim3 = truth1_estim3_list$coef 
roots_truth1_estim3 = truth1_estim3_list$roots
roots_truth1_estim3_deriv1 = truth1_estim3_list$roots_deriv1
roots_truth1_estim3_deriv2 = truth1_estim3_list$roots_deriv2

truth1_estim4_list = readRDS("function objects/truth1_estim4.rds")
f_truth1_estim4 = truth1_estim4_list$f
f_truth1_estim4_deriv1 = truth1_estim4_list$f_deriv1
f_truth1_estim4_deriv2 = truth1_estim4_list$f_deriv2
w_truth1_estim4 = truth1_estim4_list$w
knots_truth1_estim4 = truth1_estim4_list$knots
coef_truth1_estim4 = truth1_estim4_list$coef 
roots_truth1_estim4 = truth1_estim4_list$roots
roots_truth1_estim4_deriv1 = truth1_estim4_list$roots_deriv1
roots_truth1_estim4_deriv2 = truth1_estim4_list$roots_deriv2

truth1_estim5_list = readRDS("function objects/truth1_estim5.rds")
f_truth1_estim5 = truth1_estim5_list$f
f_truth1_estim5_deriv1 = truth1_estim5_list$f_deriv1
f_truth1_estim5_deriv2 = truth1_estim5_list$f_deriv2
w_truth1_estim5 = truth1_estim5_list$w
knots_truth1_estim5 = truth1_estim5_list$knots
coef_truth1_estim5 = truth1_estim5_list$coef 
roots_truth1_estim5 = truth1_estim5_list$roots
roots_truth1_estim5_deriv1 = truth1_estim5_list$roots_deriv1
roots_truth1_estim5_deriv2 = truth1_estim5_list$roots_deriv2

###############################################################################
# truth 2 #####################################################################
###############################################################################

truth2_list = readRDS("function objects/truth2.rds")
f_truth2 = truth2_list$f
f_truth2_deriv1 = truth2_list$f_deriv1
f_truth2_deriv2 = truth2_list$f_deriv2
w_truth2 = truth2_list$w 
knots_truth2 = truth2_list$knots 
coef_truth2 = truth2_list$coef 
range_truth2 = truth2_list$range
range_truth2_deriv1 = truth2_list$range_deriv1
range_truth2_deriv2 = truth2_list$range_deriv2
roots_truth2 = truth2_list$roots
roots_truth2_deriv1 = truth2_list$roots_deriv1
roots_truth2_deriv2 = truth2_list$roots_deriv2

truth2_estim1_list = readRDS("function objects/truth2_estim1.rds")
f_truth2_estim1 = truth2_estim1_list$f
f_truth2_estim1_deriv1 = truth2_estim1_list$f_deriv1
f_truth2_estim1_deriv2 = truth2_estim1_list$f_deriv2
coef_truth2_estim1 = truth2_estim1_list$coef 
roots_truth2_estim1 = truth2_estim1_list$roots
roots_truth2_estim1_deriv1 = truth2_estim1_list$roots_deriv1
roots_truth2_estim1_deriv2 = truth2_estim1_list$roots_deriv2

truth2_estim2_list = readRDS("function objects/truth2_estim2.rds")
f_truth2_estim2 = truth2_estim2_list$f
f_truth2_estim2_deriv1 = truth2_estim2_list$f_deriv1
f_truth2_estim2_deriv2 = truth2_estim2_list$f_deriv2
roots_truth2_estim2 = truth2_estim2_list$roots
roots_truth2_estim2_deriv1 = truth2_estim2_list$roots_deriv1
roots_truth2_estim2_deriv2 = truth2_estim2_list$roots_deriv2

truth2_estim3_list = readRDS("function objects/truth2_estim3.rds")
f_truth2_estim3 = truth2_estim3_list$f
f_truth2_estim3_deriv1 = truth2_estim3_list$f_deriv1
f_truth2_estim3_deriv2 = truth2_estim3_list$f_deriv2
w_truth2_estim3 = truth2_estim3_list$w
knots_truth2_estim3 = truth2_estim3_list$knots
coef_truth2_estim3 = truth2_estim3_list$coef 
roots_truth2_estim3 = truth2_estim3_list$roots
roots_truth2_estim3_deriv1 = truth2_estim3_list$roots_deriv1
roots_truth2_estim3_deriv2 = truth2_estim3_list$roots_deriv2

truth2_estim4_list = readRDS("function objects/truth2_estim4.rds")
f_truth2_estim4 = truth2_estim4_list$f
f_truth2_estim4_deriv1 = truth2_estim4_list$f_deriv1
f_truth2_estim4_deriv2 = truth2_estim4_list$f_deriv2
#w_truth2_estim4 = truth2_estim4_list$w
#knots_truth2_estim4 = truth2_estim4_list$knots
#coef_truth2_estim4 = truth2_estim4_list$coef 
roots_truth2_estim4 = truth2_estim4_list$roots
roots_truth2_estim4_deriv1 = truth2_estim4_list$roots_deriv1
roots_truth2_estim4_deriv2 = truth2_estim4_list$roots_deriv2

truth2_estim5_list = readRDS("function objects/truth2_estim5.rds")
f_truth2_estim5 = truth2_estim5_list$f
f_truth2_estim5_deriv1 = truth2_estim5_list$f_deriv1
f_truth2_estim5_deriv2 = truth2_estim5_list$f_deriv2
w_truth2_estim5 = truth2_estim5_list$w
knots_truth2_estim5 = truth2_estim5_list$knots
coef_truth2_estim5 = truth2_estim5_list$coef 
roots_truth2_estim5 = truth2_estim5_list$roots
roots_truth2_estim5_deriv1 = truth2_estim5_list$roots_deriv1
roots_truth2_estim5_deriv2 = truth2_estim5_list$roots_deriv2

###############################################################################
# truth 3 #####################################################################
###############################################################################

truth3_list = readRDS("function objects/truth3.rds")
f_truth3 = truth3_list$f
f_truth3_deriv1 = truth3_list$f_deriv1
f_truth3_deriv2 = truth3_list$f_deriv2
w_truth3 = truth3_list$w 
knots_truth3 = truth3_list$knots 
coef_truth3 = truth3_list$coef 
range_truth3 = truth3_list$range
range_truth3_deriv1 = truth3_list$range_deriv1
range_truth3_deriv2 = truth3_list$range_deriv2
roots_truth3 = truth3_list$roots
roots_truth3_deriv1 = truth3_list$roots_deriv1
roots_truth3_deriv2 = truth3_list$roots_deriv2

truth3_estim1_list = readRDS("function objects/truth3_estim1.rds")
f_truth3_estim1 = truth3_estim1_list$f
f_truth3_estim1_deriv1 = truth3_estim1_list$f_deriv1
f_truth3_estim1_deriv2 = truth3_estim1_list$f_deriv2
coef_truth3_estim1 = truth3_estim1_list$coef 
roots_truth3_estim1 = truth3_estim1_list$roots
roots_truth3_estim1_deriv1 = truth3_estim1_list$roots_deriv1
roots_truth3_estim1_deriv2 = truth3_estim1_list$roots_deriv2

truth3_estim2_list = readRDS("function objects/truth3_estim2.rds")
f_truth3_estim2 = truth3_estim2_list$f
f_truth3_estim2_deriv1 = truth3_estim2_list$f_deriv1
f_truth3_estim2_deriv2 = truth3_estim2_list$f_deriv2
coef_truth3_estim2 = truth3_estim2_list$coef 
roots_truth3_estim2 = truth3_estim2_list$roots
roots_truth3_estim2_deriv1 = truth3_estim2_list$roots_deriv1
roots_truth3_estim2_deriv2 = truth3_estim2_list$roots_deriv2

truth3_estim3_list = readRDS("function objects/truth3_estim3.rds")
f_truth3_estim3 = truth3_estim3_list$f
f_truth3_estim3_deriv1 = truth3_estim3_list$f_deriv1
f_truth3_estim3_deriv2 = truth3_estim3_list$f_deriv2
w_truth3_estim3 = truth3_estim3_list$w
knots_truth3_estim3 = truth3_estim3_list$knots
coef_truth3_estim3 = truth3_estim3_list$coef 
roots_truth3_estim3 = truth3_estim3_list$roots
roots_truth3_estim3_deriv1 = truth3_estim3_list$roots_deriv1
roots_truth3_estim3_deriv2 = truth3_estim3_list$roots_deriv2

truth3_estim4_list = readRDS("function objects/truth3_estim4.rds")
f_truth3_estim4 = truth3_estim4_list$f
f_truth3_estim4_deriv1 = truth3_estim4_list$f_deriv1
f_truth3_estim4_deriv2 = truth3_estim4_list$f_deriv2
w_truth3_estim4 = truth3_estim4_list$w
knots_truth3_estim4 = truth3_estim4_list$knots
coef_truth3_estim4 = truth3_estim4_list$coef 
roots_truth3_estim4 = truth3_estim4_list$roots
roots_truth3_estim4_deriv1 = truth3_estim4_list$roots_deriv1
roots_truth3_estim4_deriv2 = truth3_estim4_list$roots_deriv2

truth3_estim5_list = readRDS("function objects/truth3_estim5.rds")
f_truth3_estim5 = truth3_estim5_list$f
f_truth3_estim5_deriv1 = truth3_estim5_list$f_deriv1
f_truth3_estim5_deriv2 = truth3_estim5_list$f_deriv2
w_truth3_estim5 = truth3_estim5_list$w
knots_truth3_estim5 = truth3_estim5_list$knots
coef_truth3_estim5 = truth3_estim5_list$coef 
roots_truth3_estim5 = truth3_estim5_list$roots
roots_truth3_estim5_deriv1 = truth3_estim5_list$roots_deriv1
roots_truth3_estim5_deriv2 = truth3_estim5_list$roots_deriv2

###############################################################################
# truth 4 #####################################################################
###############################################################################

truth4_list = readRDS("function objects/truth4.rds")
f_truth4 = truth4_list$f
f_truth4_deriv1 = truth4_list$f_deriv1
f_truth4_deriv2 = truth4_list$f_deriv2
w_truth4 = truth4_list$w 
knots_truth4 = truth4_list$knots 
coef_truth4 = truth4_list$coef 
range_truth4 = truth4_list$range
range_truth4_deriv1 = truth4_list$range_deriv1
range_truth4_deriv2 = truth4_list$range_deriv2
roots_truth4 = truth4_list$roots
roots_truth4_deriv1 = truth4_list$roots_deriv1
roots_truth4_deriv2 = truth4_list$roots_deriv2

truth4_estim1_list = readRDS("function objects/truth4_estim1.rds")
f_truth4_estim1 = truth4_estim1_list$f
f_truth4_estim1_deriv1 = truth4_estim1_list$f_deriv1
f_truth4_estim1_deriv2 = truth4_estim1_list$f_deriv2
coef_truth4_estim1 = truth4_estim1_list$coef 
roots_truth4_estim1 = truth4_estim1_list$roots
roots_truth4_estim1_deriv1 = truth4_estim1_list$roots_deriv1
roots_truth4_estim1_deriv2 = truth4_estim1_list$roots_deriv2

truth4_estim2_list = readRDS("function objects/truth4_estim2.rds")
f_truth4_estim2 = truth4_estim2_list$f
f_truth4_estim2_deriv1 = truth4_estim2_list$f_deriv1
f_truth4_estim2_deriv2 = truth4_estim2_list$f_deriv2
coef_truth4_estim2 = truth4_estim2_list$coef 
roots_truth4_estim2 = truth4_estim2_list$roots
roots_truth4_estim2_deriv1 = truth4_estim2_list$roots_deriv1
roots_truth4_estim2_deriv2 = truth4_estim2_list$roots_deriv2

truth4_estim3_list = readRDS("function objects/truth4_estim3.rds")
f_truth4_estim3 = truth4_estim3_list$f
f_truth4_estim3_deriv1 = truth4_estim3_list$f_deriv1
f_truth4_estim3_deriv2 = truth4_estim3_list$f_deriv2
w_truth4_estim3 = truth4_estim3_list$w
knots_truth4_estim3 = truth4_estim3_list$knots
coef_truth4_estim3 = truth4_estim3_list$coef 
roots_truth4_estim3 = truth4_estim3_list$roots
roots_truth4_estim3_deriv1 = truth4_estim3_list$roots_deriv1
roots_truth4_estim3_deriv2 = truth4_estim3_list$roots_deriv2

truth4_estim4_list = readRDS("function objects/truth4_estim4.rds")
f_truth4_estim4 = truth4_estim4_list$f
f_truth4_estim4_deriv1 = truth4_estim4_list$f_deriv1
f_truth4_estim4_deriv2 = truth4_estim4_list$f_deriv2
w_truth4_estim4 = truth4_estim4_list$w
knots_truth4_estim4 = truth4_estim4_list$knots
coef_truth4_estim4 = truth4_estim4_list$coef 
roots_truth4_estim4 = truth4_estim4_list$roots
roots_truth4_estim4_deriv1 = truth4_estim4_list$roots_deriv1
roots_truth4_estim4_deriv2 = truth4_estim4_list$roots_deriv2

truth4_estim5_list = readRDS("function objects/truth4_estim5.rds")
f_truth4_estim5 = truth4_estim5_list$f
f_truth4_estim5_deriv1 = truth4_estim5_list$f_deriv1
f_truth4_estim5_deriv2 = truth4_estim5_list$f_deriv2
w_truth4_estim5 = truth4_estim5_list$w
knots_truth4_estim5 = truth4_estim5_list$knots
coef_truth4_estim5 = truth4_estim5_list$coef 
roots_truth4_estim5 = truth4_estim5_list$roots
roots_truth4_estim5_deriv1 = truth4_estim5_list$roots_deriv1
roots_truth4_estim5_deriv2 = truth4_estim5_list$roots_deriv2

###########################################################################################################