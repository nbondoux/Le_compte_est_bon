#!/bin/env python
# -*- coding: utf-8 -*-

#Concept of this algorithm:
# The value generated (by combinations of all the operations) from a list l
# (size L) are the values made of N numbers generated by each of its  sublists
# of size N + the values made of combinations by an operation of the values
# generated by the couple of lists whose combined total number of elements is L

# no duplicate value is therefore ever generated for l or any other sub-lists

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
        s = 0
        l = self
        while l:
            s += 1
            l = l.tail
        return s

class EmptyList(LinkedList):
    def __new__(cls):
        return object.__new__(cls)
    
    def __nonzero__(self):
        return False
    
    def __iter__(self):
        return iter([])
    
    def get_head(self):
        raise IndexError("End of list")
    
    def get_tail(self):
        raise IndexError("End of list")
    
    def __len__(self):
        return 0
        
    tail = property(get_tail)
    head = property(get_head)

LinkedList.emptyList = EmptyList()
del EmptyList


def single_value_to_LinkedList(v):
    obj = object.__new__(LinkedList)
    obj.head = v
    obj.tail = LinkedList.emptyList
    return obj

class Node(object):
    __slots__ = 'leftNode', 'rightNode', 'operation', 'value', 'length'
    Add, Minus, Mult, Divi = range(0, 4)
    
    # length is the number of numbers in a forumla
    
    def __init__(self):
        self.leftNode = None
        self.rightNode = None
        self.operation = Node.Add
        self.value = 0
        self.length = 0
    
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

class FinalNode(object):
    __slots__ = 'value', 'length'
    
    def __init__(self, iValue):
        self.value = iValue
        self.length = 1
    
    def operationPriority(self):
        return 100
    
    def __repr__(self):
        return repr(self.value)

class BestSolution(object):
    __slots__ = '_bestSolution', 'length'
    
    def __init__(self, iBestSolution=None):
        if iBestSolution:
            self._bestSolution = iBestSolution
            self.length = iBestSolution.length
        else:
            self._bestSolution = None
            self.length = None
    
    def get_bestSolution(self):
        return self._bestSolution
    
    def set_bestSolution(self, iBestSolution):
        self._bestSolution = iBestSolution
        self.length = iBestSolution.length
    
    #bestSolution = property(get_bestSolution, set_bestSolution)


#the tree of MaxLength represents maximum authorized length
#from top of algo (linked by overallMaxLength)
#to the algos in length (leafs of the tree)
#maxLength at a level is always calculated as the one of the level
#above minus a delta; the tree structure is useful to propagate in all the
#levels the consequencies of finding a new solution

class MaxLength(object):
    __slots__ = 'childA', 'childB', '_maxLength', '_delta', 'maxLengthMinusDelta'
    
    def __init__(self, iMaxLength=99999,iDelta=0):
        self.childA = None
        self.childB = None
        self._maxLength = iMaxLength
        self._delta = iDelta
        self.maxLengthMinusDelta = iMaxLength - iDelta
        
    def get_maxLength(self):
        return self._maxLength
    
    def set_maxLength(self, iMaxLength):
        self._maxLength = iMaxLength
        self.maxLengthMinusDelta = iMaxLength - self._delta
        
    def get_delta(self):
        return self._delta
    
    def set_delta(self, iDelta):
        self._delta = iDelta
        self.maxLengthMinusDelta = self._maxLength - iDelta
        
    def propagateMaxLength(self):
        if self.childA:
            self.childA.set_maxLength(self.maxLengthMinusDelta)
            self.childA.propagateMaxLength()
        if self.childB:
            self.childB.set_maxLength(self.maxLengthMinusDelta)
            self.childB.propagateMaxLength()
    
    #def get_currentMaxLength(self):
        #return self.maxLengthMinusDelta
    
    #maxLength = property(get_maxLength, set_maxLength)
    #delta = property(get_delta, set_delta)
    #currentMaxLength = property(get_currentMaxLength)


class Algo(object):
    #__slots__ = 'target', 'bestSol', 'overallMaxLength', 'compteur'
    
    def __init__(self, iTarget):
        self.target = iTarget
        self.bestSol = BestSolution()
        self.overallMaxLength = MaxLength()
        self.compteur = 0
    
    def __checkNode(self, iNode):
        global compteur2
        compteur2 += 1
        if iNode.value == self.target:
            if (not self.bestSol.get_bestSolution()) or iNode.length < self.bestSol.length:
                #propagte new max-length constraints to all the algorithm
                self.overallMaxLength.set_maxLength(iNode.length)
                self.overallMaxLength.propagateMaxLength()
                
                self.bestSol.set_bestSolution(copy.deepcopy(iNode))
                print "{0} = {1}; {2}".format(self.target, self.bestSol.get_bestSolution(), self.bestSol.length - 1)

    def __algo(self, iL, iLSize, iMinLength, iMaxLength, func):
        checkNode = self.__checkNode
        algo = self.__algo
        
        
        if iMinLength < 1:
            iMinLength = 1
        currentMaxLength = iMaxLength.maxLengthMinusDelta
        
        if (iLSize < iMinLength) or (currentMaxLength <= iMinLength):
            # if l.size is one
            pass
        elif iL.tail == LinkedList.emptyList:
            elmt = iL.head
            #optim:
            #length of iL elemts should be 1
            # so we replace here elmt.length with 1
            if 1 >= iMinLength and 1 < currentMaxLength:
                func(elmt)
        else:
            def sub_block(l1, l2):
                if l1:
                    l1_size = len(l1)
                    l2_size = len(l2)
                    
                    maxLengthForL1 = iMaxLength.childA
                    if not maxLengthForL1:
                        maxLengthForL1 = MaxLength(iMaxLength.maxLengthMinusDelta, 0)
                        iMaxLength.childA = maxLengthForL1
                    else:
                        maxLengthForL1.set_maxLength(iMaxLength.maxLengthMinusDelta)
                        maxLengthForL1.set_delta(0)
                    
                    maxLengthForL2 = iMaxLength.childB
                    if not maxLengthForL2:
                        maxLengthForL2 = MaxLength(iMaxLength.maxLengthMinusDelta, 0)
                        iMaxLength.childB = maxLengthForL2
                    else:
                        maxLengthForL2.set_maxLength(iMaxLength.maxLengthMinusDelta)
                        maxLengthForL2.set_delta(0)
                    
                    
                    # note that the set of  "l1" and "l2" generated from l is the
                    # set of sublists of l
                    # these two "algo" calls are used for the
                    # "the values made of N numbers generated by each of its sublists
                    # of size N" part of the algorithm described on top of the file
                    
                    # return all elements of length l1_size
                    if iMinLength <= l1_size:
                        algo(l1, l1_size, l1_size, maxLengthForL1, func)
                    
                    # return all elements of length l2_size
                    if iMinLength <= l2_size:
                        algo(l2, l2_size, l2_size, maxLengthForL2, func)
                    
                    # yield all combination of elements of l1 X l2
                    # it is generate "the values made of combinations by an operation
                    # of the values generated by the couple of lists whose combined
                    # total number of elements is L" part of the algorithm described
                    # on top of the file
                    
                    newNode = Node()
                    
                    maxLengthForL2.set_delta(1)
                    
                    def sub_block_l2(elmt2):
                        # MaxLength = maxLengthForL1 + 1 + lengthOfL2Elmt
                        maxLengthForL1.set_delta(elmt2.length)
                        
                        def sub_block_l1(elmt1):
                            newLength = elmt1.length + elmt2.length
                            
                            if iMaxLength.maxLengthMinusDelta > newLength:
                                
                                newNode.length = newLength
                                newNode.leftNode = elmt1
                                newNode.rightNode = elmt2
                                val1 = elmt1.value
                                val2 = elmt2.value
                                
                                newNode.value = val1 + val2
                                newNode.operation = Node.Add
                                checkNode(newNode)
                                func(newNode)
                                
                                if val2 > val1:
                                    if elmt2.__class__ != Node or elmt2.operation != Node.Add:
                                        newNode.value = val2 - val1
                                        newNode.leftNode = elmt2
                                        newNode.rightNode = elmt1
                                        newNode.operation = Node.Minus
                                        
                                        checkNode(newNode)
                                        func(newNode)
                                        newNode.leftNode = elmt1
                                        newNode.rightNode = elmt2
                                
                                if val1 > val2:
                                    if elmt1.__class__ != Node or elmt1.operation != Node.Add:
                                        newNode.value = val1 - val2
                                        newNode.operation = Node.Minus
                                        checkNode(newNode)
                                        func(newNode)
                                
                                if val1 > 1 and val2 > 1:
                                    newNode.value = val1 * val2
                                    newNode.operation = Node.Mult
                                    
                                    checkNode(newNode)
                                    func(newNode)
                                
                                if elmt2.__class__ != Node or elmt2.operation != Node.Mult:
                                    if val2 > val1 and val1 > 1 and (val2 % val1) == 0:
                                        newNode.value = val2 / val1
                                        newNode.leftNode = elmt2
                                        newNode.rightNode = elmt1
                                        newNode.operation = Node.Divi
                                        
                                        checkNode(newNode)
                                        func(newNode)
                                        newNode.leftNode = elmt1
                                        newNode.rightNode = elmt2
                                
                                if elmt1.__class__ != Node or elmt1.operation != Node.Mult:
                                    if val1 > val2 and val2 > 1 and (val1 % val2) == 0:
                                        newNode.value = val1 / val2
                                        newNode.operation = Node.Divi
                                        
                                        checkNode(newNode)
                                        func(newNode)

                        algo(l1, l1_size, l1_size, maxLengthForL1, sub_block_l1)

                    algo(l2, l2_size, l2_size, maxLengthForL2, sub_block_l2)
            
            getAllSubCombinations(iL, iLSize, sub_block)

    def run(self, iL):
        global compteur2
        self.compteur = 0
        l = LinkedList([FinalNode(e) for e in iL])
        for i in l:
            self.__checkNode(i)
        
        self.overallMaxLength.set_maxLength(len(l) + 1)
        
        def update_compteur(elmt):
            self.compteur += 1

        self.__algo(l, len(l), 1, self.overallMaxLength, update_compteur)
        
        print self.compteur
        print compteur2
        
        if self.bestSol.get_bestSolution():
            print "{0} = {1}".format(self.target, self.bestSol.get_bestSolution())
        else:
            print "No Solution"
       
        return self.bestSol.get_bestSolution()


# call the block for all possible couple (l1,l2) where
# l1 is made of iL1Size elements of iL and l2 contains the complementary elmts
# of iL
def getSubCombinationsFixedL1Size(iL, iL1Size, iLSize):
    if iL1Size == 0:
        yield (LinkedList.emptyList, iL)
    else:
        duplicate_head = single_value_to_LinkedList(iL.head)
        
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
        duplicate_head = single_value_to_LinkedList(iL.head)
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