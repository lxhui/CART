from numpy import *


def loadDataSet(fileName):
    dataMat = []
    fr = open(fileName)
    curLine  = [line.strip().split('\t') for line in fr.readlines()]
    for line in curLine:
        dataMat.append(list(map(float,line)))
    return dataMat
def binSplitDataSet(dataSet, feature, value):
    mat0 = dataSet[nonzero(dataSet[:, feature] > value)[0], :]
    mat1 = dataSet[nonzero(dataSet[:, feature] <= value)[0], :]
    return mat0, mat1


def regLeaf(dataSet):
    return mean(dataSet[:, -1])


def regErr(dataSet):
    return var(dataSet[:, -1]) * dataSet.shape[0]

def chooseBestSplit(dataSet, leafType=regLeaf, errType=regErr, ops=(1, 4)):
    tolS = ops[0];
    tolN = ops[1];
    if len(set(dataSet[:, -1].T.tolist()[0])) == 1:
        return None, leafType(dataSet)
    # m,n = shape(dataSet)
    m, n = dataSet.shape
    S = errType(dataSet)
    print(S)
    bestS = inf;
    bestIndex = 0
    bestValue = 0
    for featIndex in range(n - 1):
        temp = dataSet[:, featIndex].tolist()
        for splitVal in set([a[0] for a in temp]):
            mat0, mat1 = binSplitDataSet(dataSet, featIndex, splitVal)
            if (mat0.shape[0] < tolN) or (mat1.shape[0] < tolN): continue
            newS = errType(mat0) + errType(mat1)
            if newS < bestS:
                bestIndex = featIndex
                bestValue = splitVal
                bestS = newS
    if (S - bestS) < tolS:
        return None, leafType(dataSet)
    mat0, mat1 = binSplitDataSet(dataSet, bestIndex, bestValue)
    if (mat0.shape[0] < tolN) or (mat1.shape[0] < tolN):
        return None, leafType(dataSet)
    return bestIndex, bestValue


def createTree(dataSet, leafType=regLeaf, errType=regErr, ops=(1, 4)):
    feat, val = chooseBestSplit(dataSet, leafType, errType, ops)
    #print(feat)
    #print(val)
    if feat == None: return val;
    retTree = {}
    retTree['spInd'] = feat
    retTree['spVal'] = val
    lSet, rSet = binSplitDataSet(dataSet, feat, val)
    retTree['left'] = createTree(lSet, leafType, errType, ops)
    retTree['right'] = createTree(rSet, leafType, errType, ops)
    return retTree



def isTree(obj):
    return (type(obj).__name__ == 'dict')


def getMean(tree):
    if isTree(tree['right']): tree['right'] = getMean(tree['right'])
    if isTree(tree['left']): tree['left'] = getMean(tree['left'])
    return (tree['left'] + tree['right']) / 2.0


def prune(tree, testData):
    if shape(testData)[0] == 0: return getMean(tree)  # if we have no test data collapse the tree
    if (isTree(tree['right']) or isTree(tree['left'])):  # if the branches are not trees try to prune them
        lSet, rSet = binSplitDataSet(testData, tree['spInd'], tree['spVal'])
    if isTree(tree['left']): tree['left'] = prune(tree['left'], lSet)
    if isTree(tree['right']): tree['right'] = prune(tree['right'], rSet)
    # if they are now both leafs, see if we can merge them
    if not isTree(tree['left']) and not isTree(tree['right']):
        lSet, rSet = binSplitDataSet(testData, tree['spInd'], tree['spVal'])
        errorNoMerge = sum(power(lSet[:, -1] - tree['left'], 2)) + \
                       sum(power(rSet[:, -1] - tree['right'], 2))
        treeMean = (tree['left'] + tree['right']) / 2.0
        errorMerge = sum(power(testData[:, -1] - treeMean, 2))
        if errorMerge < errorNoMerge:
            print
            "merging"
            return treeMean
        else:
            return tree
    else:
        return tree


myData2 = loadDataSet('ex2.txt')
myMat2 = mat(myData2)
myTree = createTree(myMat2, ops=(1, 4))
print(myTree)
#myDatTest = loadDataSet('ex2test.txt')
#myMat2Test = mat(myDatTest)
#print(prune(myTree, myMat2Test))
