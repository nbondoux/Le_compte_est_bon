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
  
  class SinglyLinkedListAbstract
    attr_reader :next
    attr_writer :next
    
    class SinglyLinkedListEnd
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

    @@emptyList = SinglyLinkedListEnd.new

    def initialize
      @next = @@emptyList
    end
    
    def SinglyLinkedListAbstract.emptyList
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

  class SinglyLinkedList < SinglyLinkedListAbstract
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
  
  def Le_Compte_Est_Bon.arrayToSinglyLinked a
    l = SinglyLinkedList.emptyList
    a.reverse_each {|elmt|
      node = SinglyLinkedList.new
      node.content = elmt
      node.next = l
      l = node
    }
    return l
  end

  # call the block for all possible l1 where
  # l1 is made of iL1Size elements of iL

  def Le_Compte_Est_Bon.getSubCombinationsFixedLSize(iL,iL1Size, iLSize,&block)
    if iL1Size == 0
      yield SinglyLinkedList.emptyList
    else    
      duplicate_head = SinglyLinkedList.new
      duplicate_head.content = iL.content

      #all combinations that contain head of list
      getSubCombinationsFixedLSize(iL.next,iL1Size-1,iLSize-1) {|sl|
        duplicate_head.next = sl
        yield duplicate_head
      }

      #all combinations that do not contain head of list
      if (iL1Size < iLSize)
        getSubCombinationsFixedLSize(iL.next,iL1Size,iLSize-1,&block)
      end
    end
  end

  # call the block for all possible l1 where l1 is made n elements of iL,
  # iMinSize <= n < iMaxSize
  # l1 are returned by growing order of n

  def Le_Compte_Est_Bon.getSubCombinations(iL,iLSize,iMinSize,iMaxSize,&block)
    n = iMinSize
    while n < iMaxSize and n <= iLSize
      getSubCombinationsFixedLSize(iL,n,iLSize,&block)
      n = n + 1
    end
  end


  # call the block for all possible couple (l1,l2) where
  # l1 is made of iL1Size elements of iL and l2 contains the complementary elmts
  # of iL

  def Le_Compte_Est_Bon.getSubCombinationCouplesFixedL1Size(iL,iL1Size, iLSize)
    if iL1Size == 0
      yield SinglyLinkedList.emptyList,iL
    else
      duplicate_head = SinglyLinkedList.new
      duplicate_head.content = iL.content
      
      #all combinations that contain head of list
      getSubCombinationCouplesFixedL1Size(iL.next,iL1Size-1,iLSize-1) {|sl1,sl2|
        duplicate_head.next = sl1
        yield duplicate_head,sl2
      }
      #all combinations that do not contain head of list
      if (iL1Size < iLSize)
        getSubCombinationCouplesFixedL1Size(iL.next,iL1Size,iLSize-1) {|sl1,sl2|
          duplicate_head.next = sl2
          yield sl1,duplicate_head
        }
      end
    end
  end

  # return all non_ordered subcombinations of iL where iL1 has size
  # iL1Size; it differs from getSubCombinationCouplesFixedL1Size, as
  # if iL1Size*2 == iLSize, we must not return (['a'],['b'])  and (['b','a'])
  

  def Le_Compte_Est_Bon.getSubCombinationCouplesFixedL1Size_bis(iL,iL1Size, iLSize,&block)
    if iL1Size * 2 != iLSize
      getSubCombinationCouplesFixedL1Size(iL,iL1Size,iLSize,&block)
    else
      # pop the first element of iL
      duplicate_head = SinglyLinkedList.new
      duplicate_head.content = iL.content
      # and get all the combinations where iL1 contains this element
      getSubCombinationCouplesFixedL1Size(iL.next,iL1Size-1,iLSize-1) {|sl1,sl2|
        duplicate_head.next = sl1
        block.call(duplicate_head,sl2)
      }       
    end
  end

  #returns all non-ordered couple of sub combinations of l!!!
  def Le_Compte_Est_Bon.getAllSubCombinationCouples(iL,iLSize,&block)
    i = 0;
    while i*2 <= iLSize
      getSubCombinationCouplesFixedL1Size_bis(iL,i,iLSize,&block)
      i=i+1
    end
  end


  class Node
    attr_reader :leftNode, :rightNode, :operation, :value
    attr_writer :leftNode, :rightNode, :operation, :value
    
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
 
  class Algo
    attr_reader :target,:bestSolution

    def initialize (iTarget)
      @target = iTarget
      @bestSolution = nil
    end

    def checkNode(iNode)
      $compteur2 = $compteur2 + 1

      if iNode.value == @target
        @bestSolution=iNode.duplicateTree
      end
    end
    private :checkNode
    
    def algo_l_size (iL, iLSize, &block)
      if iL.next == SinglyLinkedList.emptyList
        # if l.size is one
        
        elmt = iL.content
        yield elmt
      else
        
        # Let's yield all combination of elements of l1 X l2
        # it is generate "the values made of combinations by an operation
        # of the values generated by the couple of lists whose combined
        # total number of elements is L" part of the algorithm described
        # on top of the file
        
        Le_Compte_Est_Bon.getAllSubCombinationCouples(iL,iLSize) {|l1,l2|
          break if @bestSolution
          if not l1.empty
            l1_size = l1.size
            l2_size = l2.size
            
            newNode = Node.new

            algo_l_size(l1, l1_size) {|elmt1|
              break if @bestSolution

              algo_l_size(l2, l2_size) {|elmt2|
                break if @bestSolution

                newNode.leftNode = elmt1
                newNode.rightNode = elmt2
                val1 = elmt1.value
                val2 = elmt2.value
                
                if val1 > 0 and val2 > 0
                  newNode.value=val1 + val2
                  newNode.operation = :Add
                  checkNode(newNode)
                  yield newNode
                end
                
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
                
                if val1 >= val2
                  if elmt1.class != Node or elmt1.operation != :Add
                    newNode.value=val1 - val2
                    newNode.operation = :Minus
                    checkNode(newNode)
                    yield newNode
                  end
                end
                
                if val1 > 1 and val2 > 1
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
                  if( val1 >= val2 and val2 > 1 and (val1 % val2) == 0)
                    newNode.value=val1 / val2
                    newNode.operation = :Divi
                    
                    checkNode(newNode)
                    yield newNode
                  end
                end
              }
            }      
          end
        }
      end 
    end

    def algo_all_sizes (iL, iLSize, &block)    
      # this "algo" calls are used for the 
      # "the values made of N numbers generated by each of its sublists
      # of size N" part of the algorithm described on top of the file
      
      #gets all combinations of size n verifying iMinLength <= n < iLSize

      Le_Compte_Est_Bon.getSubCombinations(iL,iLSize,1,iLSize) {|sl|
        break if @bestSolution
        sl_size = sl.size
        algo_l_size(sl, sl_size, &block)
      }
      algo_l_size(iL, iLSize, &block)
    end

    private :algo_l_size, :algo_all_sizes
    
    def run(iL)
      compteur = 0
      l=Le_Compte_Est_Bon.arrayToSinglyLinked(iL.collect {|elmt| FinalNode.new(elmt)})
      l.each {|i| checkNode(i)}
      algo_all_sizes(l,l.size()) {|elmt|
        compteur = compteur+1 }
      #puts "#{elmt}"}
      #to-do: cleaning; remove "compteur" variables
      puts compteur
      puts $compteur2
      
      if @bestSolution
        puts "#{@target} = #{@bestSolution}"
      else
        puts "No Solution"
      end
      
      return @bestSolution
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

