# loading data
data <- readRDS(file = 'data/BL1_2005-2015.Rds')
stats <- data$stats
matches <- data$matches
odds <- data$odds

# Reading Form enriched Data
formEnrichedStats <- readRDS(file="data/formEnriched/222.Rds")
describe(formEnrichedStats$formForecast)
### Execute Feature Extraction ###
# Engineer features
source(file = 'featureEngineering/positionFeatureExtraction.R', echo = FALSE, encoding = 'UTF-8')
assignedPositions <- c('tw', 'def', 'def', 'def', 'mid', 'mid', 'mid', 
                       'mid', 'off', 'off', 'off', 'off', 'off')
relNormalAssignments <- c('DURCHGESPIELT', 'AUSGEWECHSELT')
priceFuncts <- c('min', 'max', 'avg', 'sum')
benchPriceFuncts <- c('max', 'avg')

########################################
########## Form Exploration ############
########################################

formFuncts <- c('min', 'max', 'avg')
benchFormFuncts <- c('max', 'avg')
featuredMatches <- extractMatchResultFeatures(formEnrichedStats, matches, assignedPositions, relNormalAssignments,
                                              priceFuncts = priceFuncts, formFuncts = formFuncts, 
                                              benchPriceFuncts = benchPriceFuncts, benchFormFuncts = benchFormFuncts)
saveRDS(featuredMatches, file="data/featuredMatches/222.Rds")
###

# Load featured Matches
featuredMatches <- readRDS(file = "data/featuredMatches/222.Rds")
describe(featuredMatches)

### Interaction Features ###

interactFeaturedMatches <- extractInteractionFeatures(featuredMatches)
describe(interactFeaturedMatches[, grepl('tw', colnames(interactFeaturedMatches))])
saveRDS(interactFeaturedMatches, file="data/featuredMatches/interact_222.Rds")



# All feature correlation
featureCols <- grepl('_Form', colnames(featuredMatches)) |
  grepl('_Price', colnames(featuredMatches))
features <- featuredMatches[, featureCols]
homeFeatures <- features[,grepl('_Home', colnames(features))]
colnames(homeFeatures) <- gsub('_Home', '', colnames(homeFeatures))
homeCorrData <- cor(homeFeatures)
corrplot(homeCorrData, order = 'AOE')

# Form and Function correlation
formHomeCols <- grepl('Form', colnames(features)) & 
  grepl('Home', colnames(features))
formHomeData <- features[, formHomeCols]
colnames(formHomeData) <- gsub('_Form', '', gsub('_Home', '', colnames(formHomeData)))
formCorrData <- cor(formHomeData)
corrplot(formCorrData, order = 'AOE')
## => min_Bench is highly correlated with avg_Bench => ommit one of both
## => form features do not correlate as much as price features

# Price-Form Correlations
priceFormCols <- (grepl('Form', colnames(features)) | grepl('Price', colnames(features))) &
  grepl('Home', colnames(features)) & grepl('avg', colnames(features))
priceFormData <- features[, priceFormCols]
colnames(priceFormData) <- gsub('_avg', '', gsub('_Home', '', colnames(priceFormData)))
priceFormCorrData <- cor(priceFormData)
corrplot(priceFormCorrData, order = 'AOE')



####### Correlation between results and features ##########
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)

# Correlation between the goalDiff and different aggregation 
# Functions of the Price of offensive players
offPriceCols <- grepl('Price', colnames(colsToExplore)) & grepl('off', colnames(colsToExplore)) &
  grepl('Home', colnames(colsToExplore)) & !grepl('Bench', colnames(colsToExplore))
offPriceData <- colsToExplore[, offPriceCols]
colnames(offPriceData) <- gsub('off_', '', gsub('_Price', '', gsub('Home_', '', colnames(offPriceData))))

featurePlot(x = offPriceData, y = colsToExplore$goalDiff,
            plot = 'scatter', layout = c(2, 2),
            type = c('p', 'smooth'), span = .5)
## => off-price-home-features are slightly correlated with goalDiff

# Correlation between the goalDiff and different aggregation 
# Functions of the Form of offensive players
offFormCols <- grepl('Form', colnames(colsToExplore)) & grepl('off', colnames(colsToExplore)) &
  grepl('Home', colnames(colsToExplore)) & !grepl('Bench', colnames(colsToExplore))
offFormData <- colsToExplore[, offFormCols]
colnames(offFormData) <- gsub('off_', '', gsub('_Form', '', gsub('Home_', '', colnames(offFormData))))

featurePlot(x = offFormData, y = colsToExplore$goalDiff,
            plot = 'scatter', layout = c(3, 1),
            type = c('p', 'smooth'), span = .5)
## => form seems to correlate better with goaldiff than price ?! Lets check this.
formPriceCorrData <- as.data.frame(cbind('price' = offPriceData[, 'avg'], 
                                         'form' = offFormData[, 'avg'], 
                                         'goalDiff' = colsToExplore$goalDiff))
formPriceCorr <- cor(formPriceCorrData)
corrplot(formPriceCorr, method = 'number')
## => Price correlates significantly better with goalDiff than form @toProof
## => Price and form are highly negatively correlated => cheap players get bad grades

priceFeaturedMatches <- extractMatchResultFeatures(stats, matches, assignedPositions, relNormalAssignments,
                                                   priceFuncts = priceFuncts, benchPriceFuncts = benchPriceFuncts)
saveRDS(priceFeaturedMatches, file = 'data/featuredMatches/priceFeatured.Rds')
priceFeaturedMatches <- readRDS(file = 'data/featuredMatches/priceFeatured.Rds')


######### Just Price Exploration ###

features <- priceFeaturedMatches[, grepl('Price', colnames(priceFeaturedMatches))]
# Position and Function correlation for price features of home teams
priceHomeData <- features[, grepl('Home', colnames(features))]
colnames(priceHomeData) <- gsub('_Price', '', gsub('_Home', '', colnames(priceHomeData)))
priceCorrData <- cor(priceHomeData)
corrplot(priceCorrData, order = 'AOE',
         title = 'Position and Function correlations for price features of home teams')
## => Reduce def_sum, off_sum, mid_sum, avg_Bench - features
