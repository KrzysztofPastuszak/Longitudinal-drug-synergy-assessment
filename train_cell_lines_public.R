options(stringsAsFactors = F) 
virtualEnv = "path_to_virtualenv"
library(SpatialPack)
library(imager)
trainDescPath = "/data/amsterdamGBM224_aug_cl/train_aug_cl.tsv"
valDescPath = "/data/amsterdamGBM224_aug_cl/val_aug_cl.tsv"
testDescPath = "/data/amsterdamGBM224_aug_cl/test_aug_cl.tsv"
trainDesc = read.csv(trainDescPath, sep = "\t", header = T)
valDesc = read.csv(valDescPath, sep = "\t", header = T)
testDesc = read.csv(testDescPath, sep = "\t", header = T)
testRep1 = testDesc[which(testDesc$Rep == 0),]
testRep2 = testDesc[which(testDesc$Rep == 1),]
testRep1$NewBase = paste0(testRep1$Base, "_",testRep1$BasePlate)
testRep2$NewBase = paste0(testRep2$Base, "_",testRep2$BasePlate)
missingP1 = which(!is.element(testRep1$NewBase, testRep2$NewBase))
if(length(missingP1) > 0)
  testRep1 = testRep1[-missingP1,]
missingP2 = which(!is.element(testRep2$NewBase, testRep1$NewBase)) 
rownames(testRep1) = testRep1$NewBase
rownames(testRep2) = testRep2$NewBase

testRep1$Value2 = testRep2[rownames(testRep1), "Value"]
testRep2$Value2 = testRep1[rownames(testRep2), "Value"]
testDesc = rbind(testRep1, testRep2)
library(ggpubr)  
cpbPlot = ggscatter(
  testRep1,
  x= "Value", y= "Value2",
  xlab = "Repetition 1",
  ylab = "Repetition 2", 
  shape = 16,
  size = 2, 
  add = "reg.line", conf.int = TRUE)+
  stat_cor(method = "spearman", position = position_nudge(y = 50) )  
print(cpbPlot) 
picWidth = 224
picHeight = 224
multiFactor = 4
trainX =  array(0, dim = c(nrow(trainDesc),picWidth,picHeight,3))
trainAugmentedX =  array(0, dim = c(multiFactor*nrow(trainDesc),picWidth,picHeight,3))
valX =  array(0, dim = c(nrow(valDesc),picWidth,picHeight,3))
valAugmentedX =  array(0, dim = c(multiFactor*nrow(valDesc),picWidth,picHeight,3))
testX =  array(0, dim = c(nrow(testDesc),picWidth,picHeight,3))
trainDesc$Name[1] 
trainY = trainDesc$Value
valY = valDesc$Value
valAugmentedY = rep(NA, multiFactor* length(valY))
trainAugmentedY = rep(NA, multiFactor* length(trainY))
testY = testDesc$Value 
gc()

library(png)
lowValCutOff = 0.095
set.seed(123)
for(i in 1:nrow(trainDesc))
{
  p = paste0( trainDesc$Path[i])
  if(file.exists(p))
  {
    imgRaw = readPNG(p)
    img = RGB2gray(imgRaw)
    img = 1 - img
    lowVal = which(img < lowValCutOff, arr.ind = T)
    img[lowVal] = 0
    pSplit = strsplit(p, "/")
    pSplit = unlist(pSplit)
    ### well 16 contains control sample with no treatment added
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
      imgResized = resize(as.cimg( img), size_y = fact*nrow(imgRaw), size_x = fact*ncol(imgRaw), interpolation_type	
                    = 5)
      imgResized  = as.matrix(imgResized)
      imgRefAugmented = resize(as.cimg( imgRef), size_y = fact*nrow(imgRaw), size_x = fact*ncol(imgRaw), interpolation_type	
                       = 5)
      imgRefAugmented  = as.matrix(imgRefAugmented)
      imgRawAugmented = imgRaw
      offs = ceiling((dim(imgRaw)[1] - dim(imgResized)[1])/2)
      imgRawAugmented[,,1:3] = 0
      imgRawAugmented[offs:(offs+dim(imgResized)[1] - 1),offs:(offs+dim(imgResized)[1] - 1),1] = imgResized
      imgRawAugmented[offs:(offs+dim(imgResized)[1] - 1),offs:(offs+dim(imgResized)[1] - 1),1] = imgRefAugmented
      trainAugmentedX[multiFactor*i - j, , , ] =  imgRawAugmented
    }
    
    j = multiFactor - 1 
    val = trainY[i]
    eps = runif(1,-0.07,  0.07) * val
    trainAugmentedY[multiFactor*i - j] = max(val + eps,0)
    fact = runif(1, 0.01, 0.1)
    imgResized = resize(as.cimg( img), size_y = fact*nrow(imgRaw), size_x = fact*ncol(imgRaw), interpolation_type	
                  = 5)
    imgResized  = as.matrix(imgResized)
    imgRefAugmented = resize(as.cimg( imgRef), size_y = fact*nrow(imgRaw), size_x = fact*ncol(imgRaw), interpolation_type	
                     = 5)
    imgRefAugmented  = as.matrix(imgRefAugmented)
    imgRawAugmented = imgRaw
    offs = ceiling((dim(imgRaw)[1] - dim(imgResized)[1])/2)
    imgRawAugmented[,,1:3] = 0
    imgRawAugmented[offs:(offs+dim(imgResized)[1] - 1),offs:(offs+dim(imgResized)[1] - 1),1] = imgResized
    imgRawAugmented[offs:(offs+dim(imgResized)[1] - 1),offs:(offs+dim(imgResized)[1] - 1),1] = imgRefAugmented
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
      imgResized = resize(as.cimg( img), size_y = fact*nrow(imgRaw), size_x = fact*ncol(imgRaw), interpolation_type	
                    = 5)
      imgResized  = as.matrix(imgResized)
      imgRefAugmented = resize(as.cimg( imgRef), size_y = fact*nrow(imgRaw), size_x = fact*ncol(imgRaw), interpolation_type	
                       = 5)
      imgRefAugmented  = as.matrix(imgRefAugmented)
      imgRawAugmented = imgRaw
      offs = ceiling((dim(imgRaw)[1] - dim(imgResized)[1])/2)
      imgRawAugmented[,,1:3] = 0
      imgRawAugmented[offs:(offs+dim(imgResized)[1] - 1),offs:(offs+dim(imgResized)[1] - 1),1] = imgResized
      imgRawAugmented[offs:(offs+dim(imgResized)[1] - 1),offs:(offs+dim(imgResized)[1] - 1),1] = imgRefAugmented
      valAugmentedX[multiFactor*i - j, , , ] =  imgRawAugmented
    }
    j = multiFactor - 1 
    val = valY[i]
    eps = runif(1,-0.07,  0.07) * val
    valAugmentedY[multiFactor*i - j] = max(val + eps,0)
    fact = runif(1, 0.01, 0.1)
    imgResized = resize(as.cimg( img), size_y = fact*nrow(imgRaw), size_x = fact*ncol(imgRaw), interpolation_type	
                  = 5)
    imgResized  = as.matrix(imgResized)
    imgRefAugmented = resize(as.cimg( imgRef), size_y = fact*nrow(imgRaw), size_x = fact*ncol(imgRaw), interpolation_type	
                     = 5)
    imgRefAugmented  = as.matrix(imgRefAugmented)
    imgRawAugmented = imgRaw
    offs = ceiling((dim(imgRaw)[1] - dim(imgResized)[1])/2)
    imgRawAugmented[,,1:3] = 0
    imgRawAugmented[offs:(offs+dim(imgResized)[1] - 1),offs:(offs+dim(imgResized)[1] - 1),1] = imgResized
    imgRawAugmented[offs:(offs+dim(imgResized)[1] - 1),offs:(offs+dim(imgResized)[1] - 1),1] = imgRefAugmented
    valAugmentedX[multiFactor*i - j, , , ] =  imgRawAugmented
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

valAugmentedNA = which(is.na(valAugmentedY))
if(length(valAugmentedNA)>0)
{
  valAugmentedX = valAugmentedX[-valAugmentedNA,,, ]
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
} 
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
valX = abind(valX, valAugmentedX, along = 1)
rm(valAugmentedX)
gc()
valY = c(valY, valAugmentedY)
gc()
gpu <- tensorflow::tf$config$experimental$get_visible_devices('GPU')[[1]]
tensorflow::tf$config$experimental$set_memory_growth(device = gpu, enable = TRUE)
base_model = application_densenet121(include_top = F, input_shape = c(picWidth,picHeight,3))  
model <- keras_model_sequential() %>% base_model %>%   layer_flatten() %>%  layer_dense(units =16, activation = "relu" ) %>%   
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 4, activation = "relu") %>%   layer_dense(units = 1, activation = "relu")
 
model %>% compile(optimizer = 'adam', loss = 'mse')
 
model %>% fit(  trainX ,  trainY , validation_data = list(valX, valY), epochs =  30,
                batch_size = 32, shuffle = T
)


gc() 
set.seed(123)
library(caret)



gc()
preds = model %>% predict(testX)

summary(abs(testY - preds))

gc()


model %>% save_model_hdf5(filepath = "densenet_paired_cl.hdf5") 
testDesc$Predicted = preds
save(testDesc, file = "densenet_paired_CL.RDs")
View(data.frame(testNames , testY, preds, abs(testY - preds)))


library(ggpubr)  
df0 = data.frame(preds, testY)
colnames(df0) = c("Predicted", "Real")
ggscatter(
  df0,
  x= "Real", y= "Predicted",
  xlab = "Real viability",
  ylab = "Observed viability", 
  shape = 16,# color = "pink",
  size = 2, , alpha = 0.3,
  add = "reg.line", conf.int = TRUE)+
  stat_cor(method = "spearman"  ) + ggtitle(paste0("GBM \n N = ", nrow(df0)))
 