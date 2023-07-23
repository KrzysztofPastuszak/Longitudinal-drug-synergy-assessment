options(stringsAsFactors = F) 
virtualEnv = "/home/kp/Downloads/environments/my_env/"
library(SpatialPack)
library(imager) 
shouldTrain = F
trainDescPath = "./data/train_aug_cl.tsv"
valDescPath = "./data/val_aug_cl.tsv"
testDescPath = "./data/test_aug_cl.tsv"
trainDesc = read.csv(trainDescPath, sep = "\t", header = T)
valDesc = read.csv(valDescPath, sep = "\t", header = T)
testDesc = read.csv(testDescPath, sep = "\t", header = T) 
 

picWidth = 224
picHeight = 224
### used to determine number of extra samples generated during data augmentation
multiFactor = 4



testP1 = testDesc[which(testDesc$Rep == 0),]
testP2 = testDesc[which(testDesc$Rep == 1),]
testP1$NewBase = paste0(testP1$Base, "_",testP1$BasePlate)
testP2$NewBase = paste0(testP2$Base, "_",testP2$BasePlate)
missingP1 = which(!is.element(testP1$NewBase, testP2$NewBase))
if(length(missingP1) > 0)
  testP1 = testP1[-missingP1,]
missingP2 = which(!is.element(testP2$NewBase, testP1$NewBase))
summary(match(testP1$NewBase, testP2$NewBase))
rownames(testP1) = testP1$NewBase
rownames(testP2) = testP2$NewBase

testP1$ValueOtherRep = testP2[rownames(testP1), "Value"]
testP2$ValueOtherRep = testP1[rownames(testP2), "Value"]
testDesc = rbind(testP1, testP2)


trainX =  array(0, dim = c(nrow(trainDesc),picWidth,picHeight,3))
trainAugmentedX =  array(0, dim = c(multiFactor*nrow(trainDesc),picWidth,picHeight,3))
valX =  array(0, dim = c(nrow(valDesc),picWidth,picHeight,3))
valX2 =  array(0, dim = c(multiFactor*nrow(valDesc),picWidth,picHeight,3))
testX =  array(0, dim = c(nrow(testDesc),picWidth,picHeight,3))
trainDesc$Name[1] 
trainY = trainDesc$Value
valY = valDesc$Value
valAugmentedY = rep(NA, multiFactor* length(valY))
trainAugmentedY = rep(NA, multiFactor* length(trainY))
testY = testDesc$Value
testY2 = testDesc$Mean
gc()

library(png)
lowValCutOff = 0.095
# library(rayshader)
set.seed(123)
for(i in 1:nrow(trainDesc))
{
  p = paste0( trainDesc$Path[i])
  if(file.exists(p))
  {
    imgRaw = readPNG(p)
    # img = imgRaw[[1]]
    img = RGB2gray(imgRaw)
    img = 1 - img
    lowVal = which(img < lowValCutOff, arr.ind = T)
    img[lowVal] = 0
    pSplit = strsplit(p, "/")
    pSplit = unlist(pSplit)
    pSplit[grep("well", pSplit)] = "well0016"
    pRef = paste(pSplit, collapse = "/")
    imgRef = readPNG(pRef)
    imgRef = RGB2gray(imgRef)
    imgRef = 1 - imgRef
    lowVal = which(imgRef < lowValCutOff, arr.ind = T)
    imgRef[lowVal] = 0
    imgRaw[,,1] = img
    imgRaw[,,2] = imgRef
    imgRaw[,,3] = 0
    trainX[i, , , ] =  imgRaw
    for(j in 0:(multiFactor-1))
    { 
      val = trainY[i]
      eps = runif(1,-0.07,  0.07) * val
      trainAugmentedY[multiFactor*i - j] = max(val + eps,0)
      fact = runif(1, 0.1, 0.5)
      imgAugmented = resize(as.cimg( img), size_y = fact*nrow(imgRaw), size_x = fact*ncol(imgRaw), interpolation_type	
                    = 5)
      imgAugmented  = as.matrix(imgAugmented)
      imgRefAugmented = resize(as.cimg( imgRef), size_y = fact*nrow(imgRaw), size_x = fact*ncol(imgRaw), interpolation_type	
                       = 5)
      imgRefAugmented  = as.matrix(imgRefAugmented)
      imgRawAugmented = imgRaw
      offs = ceiling((dim(imgRaw)[1] - dim(imgAugmented)[1])/2)
      imgRawAugmented[,,1:3] = 0
      imgRawAugmented[offs:(offs+dim(imgAugmented)[1] - 1),offs:(offs+dim(imgAugmented)[1] - 1),1] = imgAugmented
      imgRawAugmented[offs:(offs+dim(imgAugmented)[1] - 1),offs:(offs+dim(imgAugmented)[1] - 1),1] = imgRefAugmented
      trainAugmentedX[multiFactor*i - j, , , ] =  imgRawAugmented
    }
    
    j = multiFactor - 1 
    val = trainY[i]
    eps = runif(1,-0.07,  0.07) * val
    trainAugmentedY[multiFactor*i - j] = max(val + eps,0)
    fact = runif(1, 0.01, 0.1)
    imgAugmented = resize(as.cimg( img), size_y = fact*nrow(imgRaw), size_x = fact*ncol(imgRaw), interpolation_type	
                  = 5)
    imgAugmented  = as.matrix(imgAugmented)
    imgRefAugmented = resize(as.cimg( imgRef), size_y = fact*nrow(imgRaw), size_x = fact*ncol(imgRaw), interpolation_type	
                     = 5)
    imgRefAugmented  = as.matrix(imgRefAugmented)
    imgRawAugmented = imgRaw
    offs = ceiling((dim(imgRaw)[1] - dim(imgAugmented)[1])/2)
    imgRawAugmented[,,1:3] = 0
    imgRawAugmented[offs:(offs+dim(imgAugmented)[1] - 1),offs:(offs+dim(imgAugmented)[1] - 1),1] = imgAugmented
    imgRawAugmented[offs:(offs+dim(imgAugmented)[1] - 1),offs:(offs+dim(imgAugmented)[1] - 1),1] = imgRefAugmented
    trainAugmentedX[multiFactor*i - j, , , ] =  imgRawAugmented
  } else{
    trainY[i] = NA
  }
}

trainNA = which(is.na(trainY))

if(length(trainNA)>0)
{
  trainX = trainX[-trainNA,,, ]
  trainY = trainY[-trainNA]
}
trainAugmentedNA = which(is.na(trainAugmentedY))

if(length(trainAugmentedNA)>0)
{
  trainAugmentedX = trainAugmentedX[-trainAugmentedNA,,, ]
  trainAugmentedY = trainAugmentedY[-trainAugmentedNA]
}
gc()
for(i in 1:nrow(valDesc))
{
  p = paste0(  valDesc$Path[i])
  if(file.exists(p))
  {
    
    imgRaw = readPNG(p)
    # img = imgRaw[[1]]
    img = RGB2gray(imgRaw)
    img = 1 - img
    lowVal = which(img < lowValCutOff, arr.ind = T)
    img[lowVal] = 0
    pSplit = strsplit(p, "/")
    pSplit = unlist(pSplit)
    pSplit[grep("well", pSplit)] = "well0016"
    pRef = paste(pSplit, collapse = "/")
    imgRef = readPNG(pRef)
    imgRef = RGB2gray(imgRef)
    imgRef = 1 - imgRef
    lowVal = which(imgRef < lowValCutOff, arr.ind = T)
    imgRef[lowVal] = 0
    imgRaw[,,1] = img
    imgRaw[,,2] = imgRef
    imgRaw[,,3] = 0
    valX[i, , , ] =  imgRaw
    for(j in 0:(multiFactor-2))
    {
      val = valY[i]
      eps = runif(1,-0.07,  0.07) * val
      valAugmentedY[multiFactor*i - j] = max(val + eps,0)
      fact = runif(1, 0.1, 0.5)
      imgAugmented = resize(as.cimg( img), size_y = fact*nrow(imgRaw), size_x = fact*ncol(imgRaw), interpolation_type	
                    = 5)
      imgAugmented  = as.matrix(imgAugmented)
      imgRefAugmented = resize(as.cimg( imgRef), size_y = fact*nrow(imgRaw), size_x = fact*ncol(imgRaw), interpolation_type	
                       = 5)
      imgRefAugmented  = as.matrix(imgRefAugmented)
      imgRawAugmented = imgRaw
      offs = ceiling((dim(imgRaw)[1] - dim(imgAugmented)[1])/2)
      imgRawAugmented[,,1:3] = 0
      imgRawAugmented[offs:(offs+dim(imgAugmented)[1] - 1),offs:(offs+dim(imgAugmented)[1] - 1),1] = imgAugmented
      imgRawAugmented[offs:(offs+dim(imgAugmented)[1] - 1),offs:(offs+dim(imgAugmented)[1] - 1),1] = imgRefAugmented
      valX2[multiFactor*i - j, , , ] =  imgRawAugmented
    }
    j = multiFactor - 1 
    val = valY[i]
    eps = runif(1,-0.07,  0.07) * val
    valAugmentedY[multiFactor*i - j] = max(val + eps,0)
    fact = runif(1, 0.01, 0.1)
    imgAugmented = resize(as.cimg( img), size_y = fact*nrow(imgRaw), size_x = fact*ncol(imgRaw), interpolation_type	
                  = 5)
    imgAugmented  = as.matrix(imgAugmented)
    imgRefAugmented = resize(as.cimg( imgRef), size_y = fact*nrow(imgRaw), size_x = fact*ncol(imgRaw), interpolation_type	
                     = 5)
    imgRefAugmented  = as.matrix(imgRefAugmented)
    imgRawAugmented = imgRaw
    offs = ceiling((dim(imgRaw)[1] - dim(imgAugmented)[1])/2)
    imgRawAugmented[,,1:3] = 0
    imgRawAugmented[offs:(offs+dim(imgAugmented)[1] - 1),offs:(offs+dim(imgAugmented)[1] - 1),1] = imgAugmented
    imgRawAugmented[offs:(offs+dim(imgAugmented)[1] - 1),offs:(offs+dim(imgAugmented)[1] - 1),1] = imgRefAugmented
    valX2[multiFactor*i - j, , , ] =  imgRawAugmented
  } else{
    valY[i] = NA
  }
}

valNA = which(is.na(valY))
if(length(valNA)>0)
{
  valX = valX[-valNA,,, ]
  valY = valY[-valNA]
}

valAugmentedNAs = which(is.na(valAugmentedY))
if(length(valAugmentedNA)>0)
{
  valX2 = valX2[-valAugmentedNA,,, ]
  valAugmentedY = valAugmentedY[-valAugmentedNA]
}

testNames = c()
for(i in 1:nrow(testDesc))
{
  p = paste0( testDesc$Path[i])
  if(file.exists(p))
  {
    testNames = c(testNames, testDesc$Name[i])
    
    imgRaw = readPNG(p)
    img = RGB2gray(imgRaw)
    img = 1 - img
    lowVal = which(img < lowValCutOff, arr.ind = T)
    img[lowVal] = 0
    pSplit = strsplit(p, "/")
    pSplit = unlist(pSplit)
    pSplit[grep("well", pSplit)] = "well0016"
    pRef = paste(pSplit, collapse = "/")
    imgRef = readPNG(pRef)
    imgRef = RGB2gray(imgRef)
    imgRef = 1 - imgRef
    lowVal = which(imgRef < lowValCutOff, arr.ind = T)
    imgRef[lowVal] = 0
    imgRaw[,,1] = img
    imgRaw[,,2] = imgRef
    imgRaw[,,3] = 0
    testX[i, , , ] =  imgRaw
  } else{
    testY[i] = NA
  }
}
gc()
testNA = which(is.na(testY))
if(length(testNA)>0)
{
  testX = testX[-testNA,,, ]
  testY = testY[-testNA]
  testY2 = testY2[-testNA]
}
gc() 
gc() 
library(abind)
library(reticulate)
use_virtualenv(virtualEnv)
library(keras)
library(abind)
trainX = abind(trainX, trainAugmentedX, along = 1)
rm(trainAugmentedX)
gc()
trainY = c(trainY, trainAugmentedY)
valX = abind(valX, valX2, along = 1)
rm(valX2)
gc()
valY = c(valY, valAugmentedY)
gc()
gpu <- tensorflow::tf$config$experimental$get_visible_devices('GPU')[[1]]
tensorflow::tf$config$experimental$set_memory_growth(device = gpu, enable = TRUE)
if(shouldTrain == T)
{
  base_model = application_densenet121(include_top = F, input_shape = c(picWidth,picHeight,3)) 
  model <- keras_model_sequential() %>% base_model %>%   layer_flatten() %>%  layer_dense(units =16, activation = "relu" ) %>%   
    layer_dense(units = 16, activation = "relu") %>%
    layer_dense(units = 4, activation = "relu") %>%   layer_dense(units = 1, activation = "relu") 
  model %>% compile(optimizer = 'adam', loss = 'mse')
   
  model %>% fit(  trainX ,  trainY , validation_data = list(valX, valY), epochs =  30,
                  batch_size = 32, shuffle = T
  )
} else{
  model = load_model_hdf5("./models/densenet_paired_no_gbm.hdf5")
}

gc() 
set.seed(123)
library(caret)



gc()
preds = model %>% predict(testX)

summary(abs(testY - preds))

gc()
 
testDesc$Predicted = preds 

df1 = data.frame(testDesc$Value, testDesc$Predicted)
colnames(df1) = c("Real", "Predicted")

png(file = paste0("GBM8_rep1_vs_rep2.png"), width = 600, height = 500)
options(scipen = 100)
plot(
  NA,
  NA,
  ylim = c(0, 170),
  xlim = c(0, 170),
  main = paste0("Scatter plot of viability (GBM8 day 18, N = ", nrow(testP1), ")"),
  ylab = 'Repetition 1 viability',
  xlab = 'Repetition 2 viability'
)
abline(a = 0, b = 1, lty = 2, lwd = 2, col = 'darkgray')
abline(h = 0, lty = 2, lwd = 2, col = 'red')
abline(v = 0, lty = 2, lwd = 2, col = 'red')
points(
  testP1$Value ,
  testP1$ValueOtherRep ,
  pch = 21,
  col = 'blue',
  bg = alpha('blue', alpha = 0.4)
)
corTest = cor.test(testP1$Value , 
                   testP1$ValueOtherRep , method = "spearman")
pVal = round(corTest$p.value[[1]], 8)
if(pVal ==0)
  pVal = "< 0.00000001"
toAdd = paste0("Correlation: ", round(corTest$estimate[[1]], 2),
               "\n p  ", pVal)
text(x = 45, y = 150, toAdd )

dev.off()


png(file = paste0("GBM8_predicted_vs_observed.png"), width = 600, height = 500)
options(scipen = 100)
plot(
  NA,
  NA,
  ylim = c(0, 170),
  xlim = c(0, 170),
  main = paste0("Scatter plot of viability (GBM8 day 18, N = ", nrow(testP1), ")"),
  ylab = 'Predicted viability',
  xlab = 'Observed viability'
)
abline(a = 0, b = 1, lty = 2, lwd = 2, col = 'darkgray')
abline(h = 0, lty = 2, lwd = 2, col = 'red')
abline(v = 0, lty = 2, lwd = 2, col = 'red')
points(
  df1$Real ,
  df1$Predicted ,
  pch = 21,
  col = 'blue',
  bg = alpha('blue', alpha = 0.4)
)
corTest = cor.test(df1$Predicted , 
                   df1$Real , method = "spearman")
pVal = round(corTest$p.value[[1]], 8)
if(pVal ==0)
  pVal = "< 0.00000001"
toAdd = paste0("Correlation: ", round(corTest$estimate[[1]], 2),
               "\n p  ", pVal)
text(x = 45, y = 150, toAdd )

dev.off()
 