#!/usr/bin/ruby
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
    attr_reader :leftNode, :rightNode, :operation, :value, :depth
    attr_writer :leftNode, :rightNode, :operation, :value, :depth
    
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

    def depth
      return 0
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
    attr_reader :depth, :bestSolution
    
    def initialize(iBestSolution=nil)
      if iBestSolution
        @bestSolution = iBestSolution
        @depth = iBestSolution.depth
      else
        @bestSolution = nil
        @depth = nil
      end
    end
    def bestSolution= (iBestSolution)
      @bestSolution = iBestSolution
      @depth = iBestSolution.depth
    end
  end

  #the chain of MaxDepth represents maximum authorized depth
  #from top of algo (linked by overallMaxDepth)
  #to the algos in depth (leafs of the tree)
  #maxDepth at a level is always calculated as the one of the level
  #above minus a delta
  
  class MaxDepth
    attr_reader :delta,:maxDepth,:overallMaxDepth,:childA,:childB
    attr_writer :childA,:childB

    def initialize (iOverallMaxDepth=self,iMaxDepth=99999,iDelta=0)
      @childA = nil
      @childB = nil
      @maxDepth = iMaxDepth
      @delta = iDelta
      @maxDepthMinusDelta = iMaxDepth - iDelta
    end
    
    def maxDepth=(iMaxDepth)
      @maxDepth=iMaxDepth
      @maxDepthMinusDelta=iMaxDepth - @delta
    end
    
    def delta=(iDelta)
      @delta=iDelta
      @maxDepthMinusDelta=@maxDepth - iDelta
    end
    
    def propagateMaxDepth
      if @childA
        @childA.maxDepth=@maxDepthMinusDelta
        @childA.propagateMaxDepth
      end
      if @childB
        @childB.maxDepth=@maxDepthMinusDelta
        @childB.propagateMaxDepth
      end
    end
    
    def currentMaxDepth
      @maxDepthMinusDelta
    end
  end

  class Algo
    attr_reader :target,:bestSol,:overallMaxDepth

    def initialize (iTarget)
      @target = iTarget
      @bestSol = BestSolution.new
      @overallMaxDepth = MaxDepth.new
    end

    def checkNode(iNode)
      $compteur2 = $compteur2 + 1
      if iNode.value == @target
        if (not @bestSol.bestSolution) or iNode.depth < @bestSol.depth
          #propagte new max-depth constraints to all the algorithm
          @overallMaxDepth.maxDepth=iNode.depth
          @overallMaxDepth.propagateMaxDepth
          
          @bestSol.bestSolution=iNode.duplicateTree
          puts "#{@target} = #{@bestSol.bestSolution}; #{@bestSol.depth}"
        end
      end
    end
    private :checkNode
    
    def algo (iL, iLSize, iMinDepth, iMaxDepth, &block)
      currentMaxDepth = iMaxDepth.currentMaxDepth

      if iL.empty or iLSize < iMinDepth + 1 or currentMaxDepth + 1 <= iLSize
        # if l.size is one
      elsif iL.next == SingleChainedList.emptyList
        elmt = iL.content
        if elmt.depth >= iMinDepth
          yield elmt
        end
      else
        Le_Compte_Est_Bon.getAllSubCombinations(iL,iLSize) {|l1,l2|
          if not l1.empty
            l1_size = l1.size
            l2_size = l2.size
          
            # yield all eligible elements of l1
            algo(l1, l1_size, iMinDepth, iMaxDepth, &block)

            minDepthForL2 = 0

            # yield all eligible elements of l2
            
            if (l1_size + 1 < l2_size)
              minDepthForL2 = l2_size - 1
            end

            minDepthForL2alone = minDepthForL2

            if iMinDepth > minDepthForL2alone
              minDepthForL2alone = iMinDepth
            end

            algo(l2, l2_size, minDepthForL2alone, iMaxDepth, &block)

            # yield all combination of elements of l1 X l2
            
            maxDepthForL1 = iMaxDepth.childA
            if not maxDepthForL1
              maxDepthForL1 = MaxDepth.new(iMaxDepth.currentMaxDepth,
                                           0)
              iMaxDepth.childA = maxDepthForL1
            else
              maxDepthForL1.maxDepth=iMaxDepth.currentMaxDepth
              maxDepthForL1.delta=0
            end

            maxDepthForL2 = iMaxDepth.childB
            if not maxDepthForL2
              maxDepthForL2 = MaxDepth.new(iMaxDepth.currentMaxDepth,
                                           0)
              iMaxDepth.childB = maxDepthForL2
            else
              maxDepthForL2.maxDepth=iMaxDepth.currentMaxDepth
              maxDepthForL2.delta=0
            end

            newNode = Node.new
                 
            algo(l2, l2_size, minDepthForL2, maxDepthForL2) {|elmt2|
              # MinDepth = minDepthForL1 + 1 + depthOfL2Elmt          
              minDepthForL1 = iMinDepth - 1 - elmt2.depth
              if minDepthForL1 < 0
                minDepthForL1 = 0
              end

              # MaxDepth = maxDepthForL1 + 1 + depthOfL2Elmt
              maxDepthForL1.delta = 1 + elmt2.depth
              
              algo(l1, l1_size, minDepthForL1, maxDepthForL1) {|elmt1|
                newDepth = elmt1.depth + elmt2.depth + 1
                
                if iMaxDepth.currentMaxDepth > newDepth

                  newNode.depth = newDepth
                  newNode.leftNode = elmt1
                  newNode.rightNode = elmt2
                  val1 = elmt1.value
                  val2 = elmt2.value
                  
     
                  newNode.value=val1 + val2
                  newNode.operation = :Add
                  checkNode(newNode)
                  block.call(newNode)
                    
                  if val2 > val1
                    if elmt2.class != Node or elmt2.operation != :Add
                      newNode.value=val2 - val1
                      
                      newNode.leftNode = elmt2
                      newNode.rightNode = elmt1
                      newNode.operation = :Minus
                      
                      checkNode(newNode)
                      block.call(newNode)
                      newNode.leftNode = elmt1
                      newNode.rightNode = elmt2
                    end
                  end
                  
                  if val1 > val2
                    if elmt1.class != Node or elmt1.operation != :Add
                      newNode.value=val1 - val2
                      newNode.operation = :Minus
                      checkNode(newNode)
                      block.call(newNode)
                    end
                  end
                  
                  if (val1 > 1 and val2 > 1)
                    newNode.value=val1 * val2
                    newNode.operation = :Mult
                    
                    checkNode(newNode)
                    block.call(newNode)
                  end
                  
                  if elmt2.class != Node or elmt2.operation != :Mult  
                    if(val2 > val1 and val1 > 1 and (val2 % val1) == 0)
                      newNode.value=val2 / val1
                      newNode.leftNode = elmt2
                      newNode.rightNode = elmt1
                      newNode.operation = :Divi
                      
                      checkNode(newNode)
                      block.call(newNode)
                      newNode.leftNode = elmt1
                      newNode.rightNode = elmt2
                    end
                  end

                  
                  if (elmt1.class != Node or elmt1.operation != :Mult)
                    if( val1 > val2 and val2 > 1 and (val1 % val2) == 0)
                      newNode.value=val1 / val2
                      newNode.operation = :Divi
                      
                      checkNode(newNode)
                      block.call(newNode)
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
      
      algo(l,l.size(), 0,@overallMaxDepth) {|elmt|
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

