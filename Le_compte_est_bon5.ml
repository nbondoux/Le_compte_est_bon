type 'a lazyList = LazyListEnd | LazyList of 'a * (unit -> ('a lazyList));;

let rec lazyListMap tree f = match tree with
    LazyList (res,next) -> LazyList (f res ,fun () ->  lazyListMap (next ()) f)
  | LazyListEnd -> LazyListEnd
;;


let rec lazyListMerge map1 nmap2 =
    match map1 with
        LazyList (res, next) -> LazyList (res, fun () -> lazyListMerge (next()) nmap2)
      |LazyListEnd -> (nmap2());;

(* simple exemple of generator with CSP
let rec fac x future =
  if x > 1 then
    fac (x-1) (fun a -> LazyList (x*a, fun () -> future (x*a)))
  else
    LazyList (1,fun () -> future 1)
;;
let fac2 x = fac x (fun _ -> LazyListEnd);;
*)


(* call the block for all possible l1 where
   l1 is made of iL1Size elements of iL
*)

let rec getSubCombinationsFixedLSize iL iL1Size iLSize =
    if iL1Size == 0 then
      LazyList ([],fun () -> LazyListEnd)
    else
      match iL with 
          h::t -> (
            let listsWithH = lazyListMap (getSubCombinationsFixedLSize t (iL1Size - 1) (iLSize -1 )) (fun l -> h::l) in
            if iL1Size < iLSize then
              lazyListMerge listsWithH (fun () -> (getSubCombinationsFixedLSize t (iL1Size) (iLSize - 1)))
            else
              listsWithH
          )
        | [] -> LazyListEnd
;;
  

(*  call the block for all possible l1 where l1 is made n elements of iL,
    iMinSize <= n < iMaxSize
    l1 are returned by growing order of n
*)

let rec getSubCombinations iL iLSize iMinSize iMaxSize =
  let rec getSubCombinations_rec n =
      if n >= iMaxSize then
        LazyListEnd
      else
        lazyListMerge (getSubCombinationsFixedLSize iL n iLSize) (fun () -> getSubCombinations_rec (n+1))
  in
  getSubCombinations_rec iMinSize;;
        



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

    def isOperationAssociative
     if operation == :Add or operation == :Mult
        return true
      elsif operation == :Minus or operation == :Divi
       return false
     else
       return 0
     end
    end


    def to_s
      strLeft = @leftNode.to_s
      strRight = @rightNode.to_s
      
      if @leftNode.operationPriority < operationPriority
        strLeft = "(#{strLeft})"
      end

      if ((not isOperationAssociative) and @rightNode.operationPriority <= operationPriority) or
          @rightNode.operationPriority < operationPriority
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
    
    def isOperationAssociative
      false
    end
    
    def duplicateTree
      clone
    end

    def to_s
      "#{@value}"
    end
  end

  class Algo
    
    class BestSolution
      attr_reader :node   
      attr_writer :node
      
      def initialize(iTarget)
        @target = iTarget
        @node = nil
        @delta = nil
      end

      def isSolutionFound
        @delta == 0
      end

      def tryBestSolution(iNode)
        if not @delta or 
            (iNode.value - @target < @delta and @target - iNode.value < @delta)
          @delta = @target - iNode.value
          @delta = - @delta if @delta < 0
          @node=iNode.duplicateTree

          puts "Best so far: #{@node.value} = #{@node}"
        end
      end
    end

    attr_reader :target,:bestSolution

    def initialize (iTarget)
      @target = iTarget
      @bestSolution = BestSolution.new(target)
    end
   
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
          break if @bestSolution.isSolutionFound
          if not l1.empty
            l1_size = l1.size
            l2_size = l2.size
            
            newNode = Node.new

            algo_l_size(l1, l1_size) {|elmt1|
              break if @bestSolution.isSolutionFound

              algo_l_size(l2, l2_size) {|elmt2|
                break if @bestSolution.isSolutionFound

                newNode.leftNode = elmt1
                newNode.rightNode = elmt2
                val1 = elmt1.value
                val2 = elmt2.value
                
                if val1 > 0 and val2 > 0
                  if (elmt1.class != Node or elmt1.operation != :Minus) and
                      (elmt2.class != Node or elmt2.operation != :Minus)
                    newNode.value=val1 + val2
                    newNode.operation = :Add
                    yield newNode
                  end
                end
                
                if val2 > val1
                  newNode.value=val2 - val1
                  
                  newNode.leftNode = elmt2
                  newNode.rightNode = elmt1
                  newNode.operation = :Minus
                  
                  yield newNode
                  newNode.leftNode = elmt1
                    newNode.rightNode = elmt2
                end
                
                if val1 >= val2
                  newNode.value=val1 - val2
                  newNode.operation = :Minus
                  yield newNode
                end
                
                if val1 > 1 and val2 > 1
                  if ((elmt1.class != Node or elmt1.operation != :Divi) and
                      (elmt2.class != Node or elmt2.operation != :Divi))
                    newNode.value=val1 * val2
                    newNode.operation = :Mult
                    yield newNode
                  end
                end
                
                if(val2 > val1 and val1 > 1 and (val2 % val1) == 0)
                  newNode.value=val2 / val1
                  newNode.leftNode = elmt2
                  newNode.rightNode = elmt1
                  newNode.operation = :Divi
                  
                  yield newNode
                  newNode.leftNode = elmt1
                  newNode.rightNode = elmt2
                end
                
                
                if( val1 >= val2 and val2 > 1 and (val1 % val2) == 0)
                  newNode.value=val1 / val2
                  newNode.operation = :Divi
                  
                  yield newNode
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
        break if @bestSolution.isSolutionFound
        sl_size = sl.size
        algo_l_size(sl, sl_size, &block)
      }
      algo_l_size(iL, iLSize, &block)
    end

    private :algo_l_size, :algo_all_sizes
    
    def run(iL)
      l=Le_Compte_Est_Bon.arrayToSinglyLinked(iL.collect {|elmt| FinalNode.new(elmt)})
      algo_all_sizes(l,l.size()) {|elmt|
        @bestSolution.tryBestSolution(elmt)
      }

      if @bestSolution.isSolutionFound
        puts "#{@target} = #{@bestSolution.node}"
      elsif @bestSolution.node
        puts "No Solution; nearest solution is: #{@bestSolution.node.value} = #{@bestSolution.node}"
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

