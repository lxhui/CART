library(tidyverse)
library(magrittr)
library(hash)
binSplitDataSet <- function(dataSet,feature,value){
  mat0 = dataSet %>% filter(dataSet[,feature] > value)
  mat1 = dataSet %>% filter(dataSet[,feature] <= value)
  list(mat0 = mat0,mat1 = mat1) %>%
    return
}
regLeaf <- function(dataSet){
  last(dataSet) %>%
    mean(na.rm = TRUE) %>%
    return
}
regErr <- function(dataSet){
  (last(dataSet) - mean(last(dataSet)))^2 %>%
    sum %>%
    return
}
chooseBestSplit <- function(dataSet,leafType = regLeaf, errType = regErr, ops = c(1,4)){
  tolS = ops[1]
  tolN = ops[2]
  if ((unique(dataSet %>% last) %>% length) == 1) {
    return(list(bestIndex = "NULL",bestValue = leafType(dataSet)))
  }
  m = dim(dataSet)[1]
  ###如果待分割的样本数量，小于tolN，则直接返回，此分支结束
  if (m < tolN) {
    return(list(bestIndex = "NULL",bestValue = leafType(dataSet)))
  }
  n = dim(dataSet)[2]
  S = errType(dataSet)
  bestS = Inf
  bestIndex = 0
  bestValue = 0
  for (featIndex in seq_len( n - 1)) {
    temp = dataSet[,featIndex] %>% 
      unique
    for (splitVal in temp) {
      mat = binSplitDataSet(dataSet = dataSet, feature = featIndex,value = splitVal)
      mat0 = mat$mat0
      mat1 = mat$mat1
      if (dim(mat0)[1] < tolN || dim(mat1)[1] < tolN) {
        next()
      }
      newS = errType(mat0) + errType(mat1)
      if (newS < bestS) {
        bestIndex = featIndex
        bestValue = splitVal
        bestS = newS
      }
    }
  }
  
  if ((S - bestS) < tolS) {
    return(list(bestIndex = "NULL",bestValue = leafType(dataSet)))
  }
  mat = binSplitDataSet(dataSet = dataSet, feature = bestIndex, value = bestValue)
  mat0 = mat$mat0
  mat1 = mat$mat1
  if (dim(mat0)[1] < tolN || dim(mat1)[1] < tolN) {
    return(c("NULL",leafType(dataSet)))
  }
  return(list(bestMat = mat,bestIndex = bestIndex,bestValue = bestValue))
}

createTree <- function(dataSet,leafType = regLeaf, errType = regErr, ops = c(0.03,4)){
  temp = chooseBestSplit(dataSet = dataSet,leafType = regLeaf,errType = regErr, ops)
  feat = temp$bestIndex
  val = temp$bestValue
  if (feat == "NULL") {
    return(val)
  }
  lSet = temp$bestMat$mat0
  rSet = temp$bestMat$mat1
  retTree = hash()
  retTree$spInd = feat
  retTree$spVal = val
  print(retTree)
  retTree$left = createTree(lSet,leafType,errType,ops) 
  retTree$right = createTree(rSet,leafType,errType,ops)
  return(retTree)
}


getMean <- function(tree){
  if (is.hash(tree$right)) {
    tree$right = getMean(tree$right)
  }
  if (is.hash(tree$left)) {
    tree$left = getMean(tree$left)
  }
  return((tree$left + tree$right)/2)
}

prune <- function(tree,testData) {
  if (is.null(dim(testData)[1])) {
    return(getMean(tree))
  }
  if (is.hash(tree$left) || is.hash(tree$right)) {
    mat = binSplitDataSet(dataSet = testData,tree$spInd,tree$spVal)
    lSet = mat$mat0
    rSet = mat$mat1
  }
  if (is.hash(tree$left)) {
    tree$left = prune(tree$left,lSet)
  }
  if (is.hash(tree$right)) {
    tree$right = prune(tree$right,rSet)
  }
  if (!is.hash(tree$left) && !is.hash(tree$right)) {
    mat = binSplitDataSet(dataSet = testData,tree$spInd,tree$spVal)
    lSet = mat$mat0
    rSet = mat$mat1
    errorNoMerge = sum((last(lSet) - tree$left)^2) + sum((last(rSet) - tree$right)^2)
    treeMean = (tree$left + tree$right)/2
    erroeMerge = sum((last(testData) - treeMean)^2)
    if (erroeMerge < errorNoMerge) {
      print("merging")
      return(treeMean)
    }else{
      return(tree)
    }
  } else{
    return(tree)
  }
}

myDat = read.table(file = "ex2.txt",header = FALSE)
myTree = createTree(dataSet = myDat,leafType = regLeaf, errType = regErr,ops = c(1,4))
myDatTest = read.table(file = "ex2test.txt",header = FALSE)
test = prune(tree = myTree,testData = myDatTest)


