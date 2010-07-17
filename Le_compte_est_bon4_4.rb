#!/usr/bin/ruby
module Le_Compte_Est_Bon
  
  class Node
    attr_reader :leftNode, :rightNode, :operation, :value
    attr_writer :leftNode, :rightNode, :operation, :value
    
    def duplicateTree
      a = clone
      a.leftNode =@leftNode.duplicateTree
      a.rightNode =@rightNode.duplicateTree
      return a
    end

    def depth
      return @leftNode.depth + @rightNode.depth + 1
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
    
    def initialize(iBestSolution)
      @bestSolution = iBestSolution
      @depth = iBestSolution.depth
    end
  end

  def Le_Compte_Est_Bon.algo(iL,iLSize,iDepth,iBestSol,iTarget,iLastComputedTree,iLeftBoundForNewProductions)
    
    aSolution = nil
    if not iLastComputedTree
      i = 0
      while (i < iLSize  and iL[i].value != iTarget )
        i=i+1
      end
      if i != iLSize
        aSolution = iL[i]
      end
    elsif iLastComputedTree.value == iTarget
      aSolution = iLastComputedTree
    end
 
    if aSolution
      iBestSol = BestSolution.new(aSolution.duplicateTree)
      puts "#{iTarget} = #{iBestSol.bestSolution} ; #{iBestSol.depth}"
    else
      iDepth=iDepth+1

      if iBestSol and iDepth >= iBestSol.depth 
        return iBestSol
      end
      
      a = 0
           
      lSizeMinusOne = iLSize -1
      
      while a < lSizeMinusOne
        node_a = iL[a]
        val_a = node_a.value

        newNode = Node.new
        
        # to be restored at the end of current for
        iL[a] = newNode         
        newNode.leftNode = node_a

        b = a+1

        # this optimization seems ugly and could be maybe be better ?
        # the principle is that: if we are in state:
        # a1 ..... am am+1 ... an where am+1 has just been produced,
        # we can expect the cases with all combinations of a1...am have already
        # been tested (by current design)

        if (iLeftBoundForNewProductions > b)
          b = iLeftBoundForNewProductions
        end

        while b < iLSize
      
          node_b = iL[b]
          val_b = node_b.value

          # to be restored at the end of this for
          iL[b] = iL[lSizeMinusOne]
          newNode.rightNode = node_b

          
          # for optimization purposes,
          # the following operations are skipped:
          # a+(b+c), (b+c)-a
          # a*(b*c), (b*c)/a

          if node_b.class != Node or node_b.operation != :Add
            newNode.value=val_a + val_b
            newNode.operation = :Add
          
            iBestSol = algo(iL, lSizeMinusOne, iDepth, iBestSol, iTarget, newNode, a)
          
            if val_b > val_a
              newNode.value=val_b - val_a
              
              newNode.leftNode = node_b
              newNode.rightNode = node_a
              newNode.operation = :Minus
              
              iBestSol = algo(iL, lSizeMinusOne, iDepth, iBestSol, iTarget, newNode, a)
              
              newNode.leftNode = node_a
              newNode.rightNode = node_b
            end
          end
          
          if val_a > val_b
            if node_a.class != Node or node_a.operation != :Add
              newNode.value=val_a - val_b
              newNode.operation = :Minus
              iBestSol = algo(iL, lSizeMinusOne, iDepth, iBestSol, iTarget, newNode, a)
            end
          end

          if node_b.class != Node or node_b.operation != :Mult
            if (val_a > 1 and val_b > 1)
              newNode.value=val_a * val_b
              newNode.operation = :Mult
              
              iBestSol = algo(iL, lSizeMinusOne, iDepth, iBestSol, iTarget, newNode, a)
            end
            
            if(val_b > val_a and val_a > 1 and (val_b % val_a) == 0)
              newNode.value=val_b / val_a
              newNode.leftNode = node_b
              newNode.rightNode = node_a
              newNode.operation = :Divi
          
              iBestSol = algo(iL, lSizeMinusOne, iDepth, iBestSol, iTarget, newNode, a)
              newNode.leftNode = node_a
              newNode.rightNode = node_b
            end
          end

          
          if (node_a.class != Node or node_a.operation != :Mult)
            if( val_a > val_b and val_b > 1 and (val_a % val_b) == 0)
              newNode.value=val_a / val_b
              newNode.operation = :Divi
              
              iBestSol = algo(iL, lSizeMinusOne, iDepth, iBestSol, iTarget, newNode, a)
            end
          end
                    
          iL[b] = node_b
          b=b+1
        end #for on b

        iL[a] = node_a
        a = a+1
      end #for on a
    end # if !aSolution
    return iBestSol
  end

  def Le_Compte_Est_Bon.run(iL,iTarget)
    aBestSol = algo(iL.collect {|elmt| FinalNode.new(elmt)},iL.size,0,nil,iTarget,nil,0)

    if aBestSol
      puts "#{iTarget} = #{aBestSol.bestSolution}"
    else
      puts "No Solution"
    end
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

Le_Compte_Est_Bon.run(inputNumbers,target)

