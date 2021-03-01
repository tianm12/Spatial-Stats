# FINAL PROJECT

library("tigris")
library("spdep")
library("acs")
library("dplyr")
library("ggplot2")
library("geoR")
library("viridis")

api.key.install("e42a5452b52f02ee38f8d083c335eb0664c7fad5")

#First, take a look at all the states but get rid of Alaska, Hawaii, Puerto Rico, and DC
statedata <- states(cb=TRUE, resolution = "20m")
class(statedata)
statedata$STUSPS
lower48inds <- !(statedata$STATEFP %in% c("02","15","72"))
lower48fips <- statedata$STATEFP[ lower48inds ]
plot(lower48)

#create county lines
countylines <- tigris::counties(state = lower48fips,cb=TRUE,resolution="20m")

head(countylines@data)
ncounties <- nrow(countylines@data)

# make a county plot
plot(countylines)

nbcounty <- poly2nb(countylines)
nbcounty # some counties have no neighbors
listwcounty <- nb2listw(nbcounty, style="B")

# get the centroids
centroids <- coordinates(countylines)

# create a data frame for use with ggplot
countylines_df <- fortify(countylines)

#get some ACS data
geog_county <- acs::geo.make(state = as.numeric(lower48fips),county = "*", combine = FALSE)

pop <- acs::acs.fetch(endyear=2015, span = 5, geography = geog_county, 
                 table.number = "B01003")
# sum(is.na(pop@estimate))   # no missing values

sex <- acs.fetch(endyear=2015, span = 5, geography = geog_county, 
                 table.number = "B01001")
# sum(is.na(race@estimate))   # no missing values

inc <- acs::acs.fetch(geography = geog_county, endyear = 2015, span = 5, table.number = "B19013") 
inc@estimate[ is.na(inc@estimate) ] <- mean( inc@estimate, na.rm = TRUE )

race <- acs.fetch(endyear=2015, span = 5, geography = geog_county, 
                 table.number = "B02001")
# sum(is.na(race@estimate))   # no missing values

edu <- acs.fetch(endyear=2015, span = 5, geography = geog_county, 
                 table.number = "B15002")

white <- acs.fetch(endyear=2015, span = 5, geography = geog_county, 
                 table.number = "B19301A")
sum(is.na(white@estimate))

black <- acs.fetch(endyear=2015, span = 5, geography = geog_county, 
                 table.number = "B19301B")
black@estimate[ is.na(black@estimate) ] <- mean( black@estimate, na.rm = TRUE )

asian <- acs.fetch(endyear=2015, span = 5, geography = geog_county, 
                 table.number = "B19301D")
asian@estimate[ is.na(asian@estimate) ] <- mean(asian@estimate, na.rm = TRUE )



# create a GEOID variable from sex@geography
# need some tweaking because sex@geography$state is numeric
sex_geoid <- paste0( sprintf( "%02d", sex@geography$state ),sex@geography$county )
# add this column to the data frame
sex_withgeoid <- data.frame(GEOID=sex_geoid,sex@estimate)
# same for other variables
pop_geoid <- paste0( sprintf( "%02d", pop@geography$state ),pop@geography$county )
inc_geoid <- paste0( sprintf( "%02d", inc@geography$state ),inc@geography$county )
race_geoid <- paste0( sprintf( "%02d", race@geography$state ),race@geography$county )
edu_geoid <- paste0( sprintf( "%02d", edu@geography$state ),edu@geography$county )
white_geoid <- paste0( sprintf( "%02d", white@geography$state ),white@geography$county )
black_geoid <- paste0( sprintf( "%02d", black@geography$state ),black@geography$county )
asian_geoid <- paste0( sprintf( "%02d", asian@geography$state ),asian@geography$county )

# add this column to the data frame
pop_withgeoid <- data.frame(GEOID=pop_geoid,pop@estimate)
inc_withgeoid <- data.frame(GEOID=inc_geoid,inc@estimate)
race_withgeoid <- data.frame(GEOID=race_geoid,race@estimate)
edu_withgeoid <- data.frame(GEOID=edu_geoid,edu@estimate)
white_withgeoid <- data.frame(GEOID=white_geoid,white@estimate)
black_withgeoid <- data.frame(GEOID=black_geoid,black@estimate)
asian_withgeoid <- data.frame(GEOID=asian_geoid,asian@estimate)


# use left_join to join countylines@data with edu_withgeoid
sv1 <- left_join(countylines@data,sex_withgeoid,by="GEOID")
sv2 <- left_join(sv1,pop_withgeoid,by="GEOID")
sv3 <- left_join(sv2,inc_withgeoid,by="GEOID")
sv4 <- left_join(sv3,edu_withgeoid,by="GEOID")
sv5 <- left_join(sv4,race_withgeoid,by="GEOID")
sv6 <- left_join(sv5,white_withgeoid,by="GEOID")
sv7 <- left_join(sv6,black_withgeoid,by="GEOID")
df1 <- left_join(sv7,asian_withgeoid,by="GEOID")

# the ID is needed for plotting with ggplot
df1$id <- rownames(countylines@data)

# Population specifics
# Male
df1$male <- df1$B01001_007 + df1$B01001_008 + df1$B01001_009 + df1$B01001_010 + df1$B01001_011 +
  df1$B01001_012 + df1$B01001_013 + df1$B01001_014 + df1$B01001_015 + df1$B01001_016 + df1$B01001_017 + 
  df1$B01001_018 + df1$B01001_019
# Proportion Male
df1$propmale <- df1$male/df1$B01001_001

# Female
df1$female <- df1$B01001_031 + df1$B01001_032 + df1$B01001_033 + df1$B01001_034 + df1$B01001_035 +
  df1$B01001_036 + df1$B01001_037 + df1$B01001_038 + df1$B01001_039 + df1$B01001_040 + df1$B01001_041 + 
  df1$B01001_042 + df1$B01001_043
# Proportion Female
df1$propfemale <- df1$female/df1$B01001_001

# college or higher variable
df1$collegeorhigher <- df1$B15002_015 + df1$B15002_016 + df1$B15002_017 + df1$B15002_018 +
  df1$B15002_032 + df1$B15002_033 + df1$B15002_034 + df1$B15002_035
df1$propcollegeorhigher <- df1$collegeorhigher/df1$B15002_001


# population density variable
df1$popdens <- df1$B01003_001/as.numeric(df1$ALAND)
df1$logpopdens <- log(as.numeric(df1$popdens))

# log median income
df1$logmedianincome <- log(df1$B19013_001)

# Log white income
df1$logwhiteincome <- log(df1$B19301A_001)

# Log black income
df1$logblackincome <- log(df1$B19301B_001)

# Log asian income
df1$logasianincome <- log(df1$B19301D_001)

# Whites Only
df1$white <- df1$B02001_002
df1$propwhite <- df1$white/df1$B02001_001

# Blacks Only
df1$black <- df1$B02001_003
df1$propblack <- df1$black/df1$B02001_001

# Asians Only
df1$asian <- df1$B02001_005
df1$propasian <- df1$asian/df1$B02001_001

# make a giant data frame so we can use ggplot
ggdf1 <- left_join(countylines_df,df1,by="id")

# make a map of total population
ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = B01003_001), 
               data = ggdf1) +
  coord_map() +
  scale_fill_viridis(trans="sqrt")

# make a map of median income 
ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = logmedianincome), 
               data = ggdf1) +
  scale_fill_viridis(trans="identity") +
  coord_map()

# make map of proportion of population that is female
ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = female/B01001_001), 
               data = ggdf1) +
  scale_fill_viridis(trans="identity") +
  coord_map()

# make map of proportion of population that is male
ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = male/B01001_001), 
               data = ggdf1) +
  scale_fill_viridis(trans="identity") +
  coord_map()

# plot of male population density with income as color
ggplot(data = df1, mapping = aes(x=logpopdens,y=propmale) ) +
  geom_point(aes(color=logmedianincome)) +
  scale_color_viridis() + labs(title = "Male population density with Income as Color")

# plot of female population density with income as color
ggplot(data = df1, mapping = aes(x=logpopdens,y=propfemale) ) +
  geom_point(aes(color=logmedianincome)) +
  scale_color_viridis() + labs(title = "Female population density with Income as Color")


# plot of white population density with income as color
ggplot(data = df1, mapping = aes(x=logpopdens,y=propwhitedens) ) +
  geom_point(aes(color=logwhiteincome)) +
  scale_color_viridis() + labs(title = "White population density with Income as Color")

# plot of black population density with income as color
ggplot(data = df1, mapping = aes(x=logpopdens,y=propblack) ) +
  geom_point(aes(color=logblackincome)) +
  scale_color_viridis() + labs(title = "Black population density with Income as Color")

# plot of asian population density with income as color
ggplot(data = df1, mapping = aes(x=logpopdens,y=propasian) ) +
  geom_point(aes(color=logasianincome)) +
  scale_color_viridis() + labs(title = "Asian population density with Income as Color")





##### WHITES ONLY #####
fit_table <- matrix(NA, 3, 9)
rownames(fit_table) <- c("unc","CAR","SAR")
colnames(fit_table) <- c("int","se","pop","se","popsq","se","inc","se","loglik")

m1 <- lm(df1$logwhiteincome ~ df1$propwhite + I(df1$propwhite^2) + df1$propcollegeorhigher )
summary(m1)
logLik(m1)
plot(m1)
fit_table[1,c(1,3,5,7)] <- m1$coefficients
fit_table[1,c(2,4,6,8)] <- summary(m1)$coefficients[,2]
fit_table[1,9] <- logLik(m1)

# CAR model
nbcounty <- poly2nb(countylines)
listwcounty <- nb2listw(nbcounty, zero.policy = TRUE)

m2 <- spautolm(df1$logwhiteincome ~ df1$propwhite + I(df1$propwhite^2) + df1$propcollegeorhigher,
               listw = listwcounty, family = "CAR", zero.policy = TRUE )
logLik(m2)
fit_table[2,c(1,3,5,7)] <- m2$fit$coefficients
fit_table[2,c(2,4,6,8)] <- sqrt(diag(m2$fit$imat))*sqrt(m2$fit$s2)
fit_table[2,9] <- logLik(m2)

# SAR Model
m3 <- spautolm(df1$logwhiteincome ~ df1$propwhite + I(df1$propwhite^2) + df1$propcollegeorhigher,
               listw = listwcounty, family = "SAR", zero.policy = TRUE )
logLik(m3)
summary(m3)
fit_table[3,c(1,3,5,7)] <- m3$fit$coefficients
fit_table[3,c(2,4,6,8)] <- sqrt(diag(m3$fit$imat))*sqrt(m3$fit$s2)
fit_table[3,9] <- logLik(m3)

# note: for CAR and SAR you have to multiply imat by
# s2 in order to get covariance matrix for beta


# geostat model
""" county_centroids <- coordinates(countylines)
m4 <- likfit(data = df1$logwhiteincome, coords = county_centroids,
             trend = ~ df1$propwhite + I(df1$propwhite^2) + df1$propcollegeorhigher,
             cov.model = "matern", ini.cov.pars = c(1,1), fix.kappa = FALSE,
             kappa = 1, fix.nugget = FALSE, nugget = 0.1 )
logLik(m4)
fit_table[4,c(1,3,5,7)] <- m4$beta
fit_table[4,c(2,4,6,8)] <- sqrt(diag(m4$beta.var))
fit_table[4,9] <- logLik(m4)"""


fit_table
# parameter estimates are pretty similar, except for intercept,
# but intercept is not very interpretable here
# standard errors are similar to standard errors from 
# uncorrelated residuals. This can happen if the predictors
# or the response are not strongly correlated in space

#### BLACK ONLY ####
modelblack <- spautolm(df1$logblackincome ~ df1$propblack + I(df1$propblack^2) + df1$propcollegeorhigher,
               listw = listwcounty, family = "SAR", zero.policy = TRUE )
summary(modelblack)
logLik(modelblack)

#### ASIAN ONLY ####
modelasian <- spautolm(df1$logasianincome ~ df1$propasian + I(df1$propasian^2) + df1$propcollegeorhigher,
                       listw = listwcounty, family = "SAR", zero.policy = TRUE )
summary(modelasian)
logLik(modelasian)

### WOMEN ONLY ### 
m1 <- lm(df1$logwhiteincome ~ df1$propwhite + I(df1$propwhite^2) + df1$propcollegeorhigher )



