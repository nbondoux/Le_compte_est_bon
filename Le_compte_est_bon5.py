#!/bin/env python
# -*- coding: utf-8 -*-

import sys
import copy

compteur2 = 0

# inspiré de http://stackoverflow.com/questions/280243/python-linked-list
class LinkedList(object):
    __slots__ = 'head', 'tail'
    
    def __new__(cls, l=[]):
        it = iter(l)
        try:
            head = it.next()
        except:
            return cls.emptyList
        
        tail = LinkedList(it)
        
        obj = super(LinkedList, cls).__new__(cls)
        obj.head = head
        obj.tail = tail
        return obj
    
    def __nonzero__(self):
        return True
    
    def __iter__(self):
        x = self
        while x:
            yield x.head
            x = x.tail
    
    def __repr__(self):
        return "LinkedList([{0}])".format(', '.join(map(repr, self)))
    
    def __len__(self):
        return sum(1 for elmt in self)
    

class EmptyList(LinkedList):
    def __new__(cls):
        return object.__new__(cls)
    
    def __nonzero__(self):
        return False
    
    def __iter__(self):
        return iter([])
    
    @property
    def head(self):
        raise IndexError("End of list")
    
    @property
    def tail(self):
        raise IndexError("End of list")
    
    def __len__(self):
        return 0

LinkedList.emptyList = EmptyList()
del EmptyList



class Node(object):
    __slots__ = 'leftNode', 'rightNode', 'operation', 'value', 'depth'
    
    def __init__(self):
        self.leftNode = None
        self.rightNode = None
        self.operation = None
        self.value = 0
        self.depth = 0
    
    def duplicateTree(self):
        a = copy.copy(self)
        a.leftNode = self.leftNode.duplicateTree()
        a.rightNode = self.rightNode.duplicateTree()
        return a
    
    def operationPriority(self):
        if self.operation == Node.Add or self.operation == Node.Minus:
            return 1
        elif self.operation == Node.Mult or self.operation == Node.Divi:
            return 2
        else:
            return 0
    
    def __repr__(self):
        #trivial method for display; should be made better
        strLeft = repr(self.leftNode)
        strRight = repr(self.rightNode)
        
        if self.leftNode.operationPriority() < self.operationPriority():
            strLeft = "({0})".format(strLeft)
        
        if self.rightNode.operationPriority() <= self.operationPriority():
            strRight = "({0})".format(strRight)
        
        if self.operation == Node.Add:
            return "{0}+{1}".format(strLeft, strRight)
        elif self.operation == Node.Minus:
            return "{0}-{1}".format(strLeft, strRight)
        elif self.operation == Node.Mult:
            return "{0}*{1}".format(strLeft, strRight)
        elif self.operation == Node.Divi:
            return "{0}/{1}".format(strLeft, strRight)

Node.Add, Node.Minus, Node.Mult, Node.Divi = range(0, 4)

class FinalNode(object):
    __slots__ = 'value', 'depth'
    
    def __init__(self, iValue):
        self.value = iValue
        self.depth = 0
    
    def operationPriority(self):
        return 100
    
    def duplicateTree(self):
        return copy.copy(self)
    
    def __repr__(self):
        return repr(self.value)

class BestSolution(object):
    #__slots__ = '_bestSolution', 'depth'
    
    def __init__(self, iBestSolution=None):
        if iBestSolution:
            self._bestSolution = iBestSolution
            self.depth = iBestSolution.depth
        else:
            self._bestSolution = None
            self.depth = None
    
    @property
    def bestSolution(self):
        return self._bestSolution
    
    @bestSolution.setter
    def bestSolution(self, iBestSolution):
        self._bestSolution = iBestSolution
        self.depth = iBestSolution.depth


#the chain of MaxDepth represents maximum authorized depth
#from top of algo (linked by overallMaxDepth)
#to the algos in depth (leafs of the tree)
#maxDepth at a level is always calculated as the one of the level
#above minus a delta
class MaxDepth(object):
    #__slots__ = 'childA', 'childB', '_maxDepth', '_delta', 'maxDepthMinusDelta'
    
    def __init__(self, iMaxDepth=99999,iDelta=0):
        self.childA = None
        self.childB = None
        self._maxDepth = iMaxDepth
        self._delta = iDelta
        self.maxDepthMinusDelta = iMaxDepth - iDelta
        
    @property
    def maxDepth(self):
        return self._maxDepth
    
    @maxDepth.setter
    def maxDepth(self, iMaxDepth):
        self._maxDepth = iMaxDepth
        self.maxDepthMinusDelta = iMaxDepth - self._delta
    
    @property
    def delta(self):
        return self._delta
    
    @delta.setter
    def delta(self, iDelta):
        self._delta = iDelta
        self.maxDepthMinusDelta = self._maxDepth - iDelta

    def propagateMaxDepth(self):
        if self.childA:
            self.childA.maxDepth = self.maxDepthMinusDelta
            self.childA.propagateMaxDepth()
        if self.childB:
            self.childB.maxDepth = self.maxDepthMinusDelta
            self.childB.propagateMaxDepth()
    
    @property
    def currentMaxDepth(self):
        return self.maxDepthMinusDelta


class Algo(object):
    #__slots__ = 'target', 'bestSol', 'overallMaxDepth', 'compteur'
    
    def __init__(self, iTarget):
        self.target = iTarget
        self.bestSol = BestSolution()
        self.overallMaxDepth = MaxDepth()
        self.compteur = 0
    
    def __checkNode(self, iNode):
        global compteur2
        compteur2 += 1
        if iNode.value == self.target:
            if (not self.bestSol.bestSolution) or iNode.depth < self.bestSol.depth:
                #propagte new max-depth constraints to all the algorithm
                self.overallMaxDepth.maxDepth = iNode.depth
                self.overallMaxDepth.propagateMaxDepth()
                
                self.bestSol.bestSolution = iNode.duplicateTree()
                print "{0} = {1}; {2}".format(self.target, self.bestSol.bestSolution, self.bestSol.depth)

    def __algo(self, iL, iLSize, iMinDepth, iMaxDepth, func):
        if iMinDepth < 0:
            iMinDepth = 0
        currentMaxDepth = iMaxDepth.currentMaxDepth
        
        if (not iL) or (iLSize < iMinDepth + 1) or (currentMaxDepth <= iMinDepth):
            # if l.size is one
            pass
        elif iL.tail == LinkedList.emptyList:
            elmt = iL.head
            #optim:
            #depth of iL elemts should be 0
            # so we replace here elmt.depth with 0
            if 0 >= iMinDepth and 0 < currentMaxDepth:
                func(elmt)
        else:
            def sub_block(l1, l2):
                if l1:
                    l1_size = len(l1)
                    l2_size = len(l2)
                    
                    maxDepthForL1 = iMaxDepth.childA
                    if not maxDepthForL1:
                        maxDepthForL1 = MaxDepth(iMaxDepth.currentMaxDepth, 0)
                        iMaxDepth.childA = maxDepthForL1
                    else:
                        maxDepthForL1.maxDepth = iMaxDepth.currentMaxDepth
                        maxDepthForL1.delta = 0
                    
                    maxDepthForL2 = iMaxDepth.childB
                    if not maxDepthForL2:
                        maxDepthForL2 = MaxDepth(iMaxDepth.currentMaxDepth, 0)
                        iMaxDepth.childB = maxDepthForL2
                    else:
                        maxDepthForL2.maxDepth = iMaxDepth.currentMaxDepth
                        maxDepthForL2.delta = 0
                    
                    # yield all eligible elements of l1
                    minDepthForL1 = l1_size - 1
                    if iMinDepth > minDepthForL1:
                        minDepthForL1 = iMinDepth
                    
                    self.__algo(l1, l1_size, minDepthForL1, maxDepthForL1, func)
                    
                    
                    minDepthForL2 = l2_size - 1
                    if iMinDepth > minDepthForL2:
                        minDepthForL2 = iMinDepth
                    
                    self.__algo(l2, l2_size, minDepthForL2, maxDepthForL2, func)
                    
                    # yield all combination of elements of l1 X l2
                    newNode = Node()
                    
                    maxDepthForL2.delta = 1
                    def sub_block_l2(elmt2):
                        # MinDepth = minDepthForL1 + 1 + depthOfL2Elmt          
                        minDepthForL1 = iMinDepth - 1 - elmt2.depth
                        
                        # MaxDepth = maxDepthForL1 + 1 + depthOfL2Elmt
                        maxDepthForL1.delta = 1 + elmt2.depth
                        
                        def sub_block_l1(elmt1):
                            newDepth = elmt1.depth + elmt2.depth + 1
                            
                            if iMaxDepth.currentMaxDepth > newDepth:
                                
                                newNode.depth = newDepth
                                newNode.leftNode = elmt1
                                newNode.rightNode = elmt2
                                val1 = elmt1.value
                                val2 = elmt2.value
                                
                                newNode.value = val1 + val2
                                newNode.operation = Node.Add
                                self.__checkNode(newNode)
                                func(newNode)
                                
                                if val2 > val1:
                                    if elmt2.__class__ != Node or elmt2.operation != Node.Add:
                                        newNode.value = val2 - val1
                                        newNode.leftNode = elmt2
                                        newNode.rightNode = elmt1
                                        newNode.operation = Node.Minus
                                        
                                        self.__checkNode(newNode)
                                        func(newNode)
                                        newNode.leftNode = elmt1
                                        newNode.rightNode = elmt2
                                
                                if val1 > val2:
                                    if elmt1.__class__ != Node or elmt1.operation != Node.Add:
                                        newNode.value = val1 - val2
                                        newNode.operation = Node.Minus
                                        self.__checkNode(newNode)
                                        func(newNode)
                                
                                if val1 > 1 and val2 > 1:
                                    newNode.value = val1 * val2
                                    newNode.operation = Node.Mult
                                    
                                    self.__checkNode(newNode)
                                    func(newNode)
                                
                                if elmt2.__class__ != Node or elmt2.operation != Node.Mult:
                                    if val2 > val1 and val1 > 1 and (val2 % val1) == 0:
                                        newNode.value = val2 / val1
                                        newNode.leftNode = elmt2
                                        newNode.rightNode = elmt1
                                        newNode.operation = Node.Divi
                                        
                                        self.__checkNode(newNode)
                                        func(newNode)
                                        newNode.leftNode = elmt1
                                        newNode.rightNode = elmt2
                                
                                if elmt1.__class__ != Node or elmt1.operation != Node.Mult:
                                    if val1 > val2 and val2 > 1 and (val1 % val2) == 0:
                                        newNode.value = val1 / val2
                                        newNode.operation = Node.Divi
                                        
                                        self.__checkNode(newNode)
                                        func(newNode)

                        self.__algo(l1, l1_size, l1_size - 1, maxDepthForL1, sub_block_l1)

                    self.__algo(l2, l2_size, l2_size - 1, maxDepthForL2, sub_block_l2)
            
            getAllSubCombinations(iL, iLSize, sub_block)

    def run(self, iL):
        global compteur2
        self.compteur = 0
        l = LinkedList([FinalNode(e) for e in iL])
        for i in l:
            self.__checkNode(i)
        
        def update_compteur(elmt):
            self.compteur += 1

        self.__algo(l, len(l), 0, self.overallMaxDepth, update_compteur)
        
        print self.compteur
        print compteur2
        
        if self.bestSol.bestSolution:
            print "{0} = {1}".format(self.target, self.bestSol.bestSolution)
        else:
            print "No Solution"
       
        return self.bestSol.bestSolution


# call the block for all possible couple (l1,l2) where
# l1 is made of iL1Size elements of iL and l2 contains the complementary elmts
# of iL
def getSubCombinationsFixedL1Size(iL, iL1Size, iLSize):
    if iL1Size == 0:
        yield (LinkedList.emptyList, iL)
    else:
        duplicate_head = LinkedList([iL.head])
        
        #all combinations that contain head of list
        for sl1, sl2 in getSubCombinationsFixedL1Size(iL.tail, iL1Size - 1, iLSize - 1):
            duplicate_head.tail = sl1
            yield (duplicate_head, sl2)
        
        #all combinations that do not contain head of list
        if iL1Size <= iLSize - 1:
            for sl1, sl2 in getSubCombinationsFixedL1Size(iL.tail, iL1Size, iLSize - 1):
                duplicate_head.tail = sl2
                yield (sl1, duplicate_head)


# return all non_ordered subcombinations of iL where iL1 has size
# iL1Size; it differs from getSubCombinationsFixedL1Size, as
# if iL1Size*2 == iLSize, we must not return (['a'],['b'])  and (['b','a'])
def getSubCombinationsFixedL1Size_bis(iL, iL1Size, iLSize, func):
    if iL1Size * 2 != iLSize:
        for sl1, sl2 in getSubCombinationsFixedL1Size(iL, iL1Size, iLSize):
            func(sl1, sl2)
    else:
        # pop the first element of iL
        duplicate_head = LinkedList([iL.head])
        # and get all the combinations where iL1 contains this element
        for sl1, sl2 in getSubCombinationsFixedL1Size(iL.tail, iL1Size - 1, iLSize - 1):
            duplicate_head.tail = sl1
            func(duplicate_head, sl2)

#returns all non-ordered couple of sub combinations of l!!!
def getAllSubCombinations(iL, iLSize, func):
    i = 0
    while i*2 <= iLSize:
        getSubCombinationsFixedL1Size_bis(iL, i, iLSize, func)
        i+=1


if __name__ == '__main__':
    
    def help_message(iExecName):
        print "{0} number1 [number 2 [number 3...]] target".format(iExecName)
    
    execName = sys.argv[0]
    
    if len(sys.argv) < 2:
        help_message(execName)
        sys.exit(1)
    
    if "-h" in sys.argv or "--help" in sys.argv:
        help_message(execName)
        sys.exit(0)
    
    inputNumbers = []
    
    for elmt in sys.argv[1:]:
        i = int(elmt)
        if (i < 0):
            print "Input numbers must be positive !!!"
            sys.exit(1)
        inputNumbers.append(i)
    
    target = inputNumbers.pop()
    
    #print inputNumbers, target
    
    algo = Algo(target)
    algo.run(inputNumbers)