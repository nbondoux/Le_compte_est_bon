#!/usr/bin/ruby
#Concept of this algorithm:
# The value generated (all extracted combinations of numbers by the 4
# operations) from a list l (size L) are the values made of N numbers
# generated by each of its  sublists of size N + the values made of
# combinations by an operation of the values generated by the couple of lists
# whose combined total number of elements is L

# no duplicate value is therefore ever generated for l or any other sub-lists

$compteur2=0
module Le_Compte_Est_Bon
  
  class SingleChainedListAbstract
    attr_reader :next
    attr_writer :next
    
    class SingleChainedListEnd
      def empty
        true
      end
      def size
        0
      end
      def each(&block)
      end
      def reverse_each(&block)
      end
      
      def to_s
        return ""
      end
    end

    @@emptyList = SingleChainedListEnd.new

    def initialize
      @next = @@emptyList
    end
    
    def SingleChainedListAbstract.emptyList
      @@emptyList
    end    
    
    def empty
      false
    end

    def size
      1+@next.size
    end

    def each(&block)
      block.call(self)
      @next.each(&block)
    end

    def reverse_each(&block)
      @next.reverse_each(self)
      block.call(content)
    end
    
  end

  class SingleChainedList < SingleChainedListAbstract
    attr_reader :content
    attr_writer :content


    def each(&block)
      block.call(content)
      @next.each(&block)
    end

    def reverse_each(&block)
      @next.reverse_each(&block)
      block.call(content)
    end
    
    def to_s
      "#{content.to_s}, "+@next.to_s
    end   
  end
  
  def Le_Compte_Est_Bon.arrayToSingleChained a
    l = SingleChainedList.emptyList
    a.reverse_each {|elmt|
      node = SingleChainedList.new
      node.content = elmt
      node.next = l
      l = node
    }
    return l
  end

  # call the block for all possible couple (l1,l2) where
  # l1 is made of iL1Size elements of iL and l2 contains the complementary elmts
  # of iL

  def Le_Compte_Est_Bon.getSubCombinationsFixedL1Size(iL,iL1Size, iLSize)
    if iL1Size == 0
      yield SingleChainedList.emptyList,iL
    else
      duplicate_head = SingleChainedList.new
      duplicate_head.content = iL.content
      
      #all combinations that contain head of list
      getSubCombinationsFixedL1Size(iL.next,iL1Size-1,iLSize-1) {|sl1,sl2|
        duplicate_head.next = sl1
        yield duplicate_head,sl2
      }
      #all combinations that do not contain head of list
      if(iL1Size <= iLSize - 1)
        getSubCombinationsFixedL1Size(iL.next,iL1Size,iLSize-1) {|sl1,sl2|
          duplicate_head.next = sl2
          yield sl1,duplicate_head
        }
      end
    end
  end

  # return all non_ordered subcombinations of iL where iL1 has size
  # iL1Size; it differs from getSubCombinationsFixedL1Size, as
  # if iL1Size*2 == iLSize, we must not return (['a'],['b'])  and (['b','a'])
  

  def Le_Compte_Est_Bon.getSubCombinationsFixedL1Size_bis(iL,iL1Size, iLSize,&block)
    if iL1Size * 2 != iLSize
      getSubCombinationsFixedL1Size(iL,iL1Size,iLSize,&block)
    else
      # pop the first element of iL
      duplicate_head = SingleChainedList.new
      duplicate_head.content = iL.content
      # and get all the combinations where iL1 contains this element
      getSubCombinationsFixedL1Size(iL.next,iL1Size-1,iLSize-1) {|sl1,sl2|
        duplicate_head.next = sl1
        block.call(duplicate_head,sl2)
      }       
    end
  end

  #returns all non-ordered couple of sub combinations of l!!!
  def Le_Compte_Est_Bon.getAllSubCombinations(iL,iLSize,&block)
    i = 0;
    while i*2 <= iLSize
      getSubCombinationsFixedL1Size_bis(iL,i,iLSize,&block)
      i=i+1
    end
  end


  class Node
    attr_reader :leftNode, :rightNode, :operation, :value, :length
    attr_writer :leftNode, :rightNode, :operation, :value, :length
    
    # length is the number of numbers in a formula

    def duplicateTree
      a = clone
      a.leftNode =@leftNode.duplicateTree
      a.rightNode =@rightNode.duplicateTree
      return a
    end

    def operationPriority
     if operation == :Add or operation == :Minus
        return 1
      elsif operation == :Mult or operation == :Divi
       return 2
     else
       return 0
     end
    end

    def to_s
      #trivial method for display; should be made better
      strLeft = @leftNode.to_s
      strRight = @rightNode.to_s
      
      if @leftNode.operationPriority < operationPriority
        strLeft = "(#{strLeft})"
      end

      if @rightNode.operationPriority <= operationPriority
        strRight = "(#{strRight})"
      end

      if(@operation == :Add)
        return "#{strLeft}+#{strRight}"
      elsif(@operation == :Minus)
        return "#{strLeft}-#{strRight}"
      elsif(@operation == :Mult)
        return "#{strLeft}*#{strRight}"
      elsif(@operation == :Divi)
        return "#{strLeft}/#{strRight}"
      end
    end
  end

  class FinalNode
    attr_reader :value
    def initialize(iValue)
      @value = iValue
    end

    def length
      return 1
    end

    def operationPriority
       return 100
    end
    
    def duplicateTree
      clone
    end

    def to_s
      "#{@value}"
    end
  end
 
  class BestSolution
    attr_reader :length, :bestSolution
    
    def initialize(iBestSolution=nil)
      if iBestSolution
        @bestSolution = iBestSolution
        @length = iBestSolution.length
      else
        @bestSolution = nil
        @length = nil
      end
    end
    def bestSolution= (iBestSolution)
      @bestSolution = iBestSolution
      @length = iBestSolution.length
    end
  end

  #the tree of MaxLength represents maximum authorized length
  #from top of algo (linked by overallMaxLength)
  #to the algos in length (leafs of the tree)
  #maxLength at a level is always calculated as the one of the level
  #above minus a delta; the tree structure is useful to propagate in all the
  #levels the consequencies of finding a new solution
  
  class MaxLength
    attr_reader :delta,:maxLength,:childA,:childB
    attr_writer :childA,:childB

    def initialize (iMaxLength=99999,iDelta=0)
      @childA = nil
      @childB = nil
      @maxLength = iMaxLength
      @delta = iDelta
      @maxLengthMinusDelta = iMaxLength - iDelta
    end
    
    def maxLength=(iMaxLength)
      @maxLength=iMaxLength
      @maxLengthMinusDelta=iMaxLength - @delta
    end
    
    def delta=(iDelta)
      @delta=iDelta
      @maxLengthMinusDelta=@maxLength - iDelta
    end
    
    def propagateMaxLength
      if @childA
        @childA.maxLength=@maxLengthMinusDelta
        @childA.propagateMaxLength
      end
      if @childB
        @childB.maxLength=@maxLengthMinusDelta
        @childB.propagateMaxLength
      end
    end
    
    def currentMaxLength
      @maxLengthMinusDelta
    end
  end

  class Algo
    attr_reader :target,:bestSol,:overallMaxLength

    def initialize (iTarget)
      @target = iTarget
      @bestSol = BestSolution.new
      @overallMaxLength = MaxLength.new
    end

    def checkNode(iNode)
      $compteur2 = $compteur2 + 1

      if iNode.value == @target
        if (not @bestSol.bestSolution) or iNode.length < @bestSol.length
          #propagte new max-length constraints to all the algorithm
          @overallMaxLength.maxLength=iNode.length
          @overallMaxLength.propagateMaxLength
          
          @bestSol.bestSolution=iNode.duplicateTree
          puts "#{@target} = #{@bestSol.bestSolution}; #{@bestSol.length - 1}"
        end
      end
    end
    private :checkNode
    
    def algo (iL, iLSize, iMinLength, iMaxLength, &block)
      iMinLength = 1 if (iMinLength < 1)
      currentMaxLength = iMaxLength.currentMaxLength
      
      if iLSize < iMinLength or currentMaxLength <= iMinLength
        # if l.size is one
      elsif iL.next == SingleChainedList.emptyList
        elmt = iL.content
        #optim:
        #length of iL elemts should be 1
        # so we replace here elmt.length with 1
        if 1 >= iMinLength and 1 < currentMaxLength
          yield elmt
        end
      else
        Le_Compte_Est_Bon.getAllSubCombinations(iL,iLSize) {|l1,l2|
          if not l1.empty
            l1_size = l1.size
            l2_size = l2.size

            maxLengthForL1 = iMaxLength.childA
            if not maxLengthForL1
              maxLengthForL1 = MaxLength.new(iMaxLength.currentMaxLength,
                                           0)
              iMaxLength.childA = maxLengthForL1
            else
              maxLengthForL1.maxLength=iMaxLength.currentMaxLength
              maxLengthForL1.delta=0
            end

            maxLengthForL2 = iMaxLength.childB
            if not maxLengthForL2
              maxLengthForL2 = MaxLength.new(iMaxLength.currentMaxLength,
                                           0)
              iMaxLength.childB = maxLengthForL2
            else
              maxLengthForL2.maxLength=iMaxLength.currentMaxLength
              maxLengthForL2.delta=0
            end


            # note that the set of  "l1" and "l2" generated from l is the 
            # set of sublists of l
            # these two "algo" calls are used for the 
            # "the values made of N numbers generated by each of its sublists
            # of size N" part of the algorithm described on top of the file

            # return all elements of length l1_size
            algo(l1, l1_size, l1_size, maxLengthForL1, &block) if (iMinLength <= l1_size)

            # return all elements of length l2_size
            algo(l2, l2_size, l2_size, maxLengthForL2, &block) if (iMinLength <= l2_size)

            # yield all combination of elements of l1 X l2
            # it is generate "the values made of combinations by an operation
            # of the values generated by the couple of lists whose combined
            # total number of elements is L" part of the algorithm described
            # on top of the file

            newNode = Node.new

            maxLengthForL2.delta=1                 

            algo(l2, l2_size, l2_size, maxLengthForL2) {|elmt2|
              # MaxLength = maxLengthForL1 + lengthOfL2Elmt
              maxLengthForL1.delta = elmt2.length
              
              algo(l1, l1_size, l1_size, maxLengthForL1) {|elmt1|
                newLength = elmt1.length + elmt2.length
                
                if iMaxLength.currentMaxLength > newLength

                  newNode.length = newLength
                  newNode.leftNode = elmt1
                  newNode.rightNode = elmt2
                  val1 = elmt1.value
                  val2 = elmt2.value
                  
     
                  newNode.value=val1 + val2
                  newNode.operation = :Add
                  checkNode(newNode)
                  yield newNode
                    
                  if val2 > val1
                    if elmt2.class != Node or elmt2.operation != :Add
                      newNode.value=val2 - val1
                      
                      newNode.leftNode = elmt2
                      newNode.rightNode = elmt1
                      newNode.operation = :Minus
                      
                      checkNode(newNode)
                      yield newNode
                      newNode.leftNode = elmt1
                      newNode.rightNode = elmt2
                    end
                  end
                  
                  if val1 > val2
                    if elmt1.class != Node or elmt1.operation != :Add
                      newNode.value=val1 - val2
                      newNode.operation = :Minus
                      checkNode(newNode)
                      yield newNode
                    end
                  end
                  
                  if (val1 > 1 and val2 > 1)
                    newNode.value=val1 * val2
                    newNode.operation = :Mult
                    
                    checkNode(newNode)
                    yield newNode
                  end
                  
                  if elmt2.class != Node or elmt2.operation != :Mult  
                    if(val2 > val1 and val1 > 1 and (val2 % val1) == 0)
                      newNode.value=val2 / val1
                      newNode.leftNode = elmt2
                      newNode.rightNode = elmt1
                      newNode.operation = :Divi
                      
                      checkNode(newNode)
                      yield newNode
                      newNode.leftNode = elmt1
                      newNode.rightNode = elmt2
                    end
                  end

                  
                  if (elmt1.class != Node or elmt1.operation != :Mult)
                    if( val1 > val2 and val2 > 1 and (val1 % val2) == 0)
                      newNode.value=val1 / val2
                      newNode.operation = :Divi
                      
                      checkNode(newNode)
                      yield newNode
                    end
                  end
                end
              }
            }        
          end
        }
      end 
    end
    private :algo
    
    def run(iL)
      compteur = 0
      l=Le_Compte_Est_Bon.arrayToSingleChained(iL.collect {|elmt| FinalNode.new(elmt)})
      l.each {|i| checkNode(i)}
      @overallMaxLength.maxLength = l.size + 1
      algo(l,l.size(), 1,@overallMaxLength) {|elmt|
        compteur = compteur+1 }
      #puts "#{elmt}"}
      #to-do: cleaning; remove "compteur" variables
      puts compteur
      puts $compteur2
      
      if @bestSol.bestSolution
        puts "#{@target} = #{@bestSol.bestSolution}"
      else
        puts "No Solution"
      end
      
      return @bestSol.bestSolution
    end
  end

  def Le_Compte_Est_Bon.algoNew(iTarget)
    Algo.new(iTarget)
  end



end #end of the module

def help_message(iExecName)
  puts "#{iExecName} number1 [number 2 [number 3 ...]] target\n"
end

execName = $0

if ARGV.size < 2
  help_message execName
  exit 1
end

if ARGV.include? "-h" or ARGV.include? "--help"
  help_message execName
  exit 0
end

inputNumbers = Array.new

ARGV.each {|elmt|
  i = elmt.to_i
  if (i < 0)
    puts "Input numbers must be positive !!!"
    exit 1
  end
  inputNumbers.push i
}

target = inputNumbers.pop

algo = Le_Compte_Est_Bon.algoNew(target)
algo.run(inputNumbers)

