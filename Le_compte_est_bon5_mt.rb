#!/usr/bin/ruby
# -*- coding: utf-8 -*-

require 'thread'

module NB_Common

  #note: a SinglyLinkedList is also the first element of the (sub) list
  class SinglyLinkedListAbstract
    attr_reader :next
    attr_writer :next
    
    class SinglyLinkedListEnd
      def empty?
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
    
    def empty?
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
      @next.reverse_each(&block)
      block.call(self)
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

  # note: a DoublyLinkedList is an interface for a doubly linked list of
  # elements inherinting from DoublyLinkedListElmtAbstract

  class DoublyLinkedListElmtAbstract
    attr_reader :next, :previous
    protected
    attr_writer :next, :previous
    public
    
    def initialize
      @next=nil
      @previous=nil
    end

    def addElmtBefore(iElmt)
      #if needed, remove the element from its current list
      if not iElmt.next.nil?
        iElmt.next.previous = iElmt.previous
      end
      if not iElmt.previous.nil?
        iElmt.previous.next = iElmt.next
      end
      #then add it to the current list
      iElmt.next = self
      iElmt.previous = self.previous
      @previous.next = iElmt
      @previous = iElmt
    end

    def addElmtsBefore(iElmt1, iElmt2)
      #if needed, remove the elements from their current list
      if not iElmt2.next.nil?
        iElmt2.next.previous = iElmt1.previous
      end
      if not iElmt1.previous.nil?
        iElmt1.previous.next = iElmt2.next
      end
      #then add them to the current list
      iElmt2.next = self
      iElmt1.previous = self.previous
      @previous.next = iElmt1
      @previous = iElmt2
    end

    def rmElmtBefore
      exPrevious = @previous
      @previous = previous.previous
      @previous.next = self
      exPrevious.previous = nil
      exPrevious.next = nil
    end

    def rmElmt
      @next.rmElmtBefore
    end
  end

  class DoublyLinkedListElmtEnd < DoublyLinkedListElmtAbstract
    attr_reader :list
    def initialize(iList)
      @List = iList
      @next=self
      @previous=self
    end
  end

  class DoublyLinkedList
    def initialize
      @end = DoublyLinkedListElmtEnd.new(self)
    end

    def front
      @end.next
    end

    def back
      @end.previous
    end
   
    def getEnd
      @end
    end

    def empty?
      @end.next.equal? @end
    end
    def push(iElmt)
      @end.addElmtBefore(iElmt)
    end
    def pop
      @end.rmElmtBefore
    end

    def each(&block)
      current = front
      while not current.equal? @end
        aNext = current.next
        block.call(current)
        current=aNext
      end
    end

    def reverse_each(&block)
      current = back
      while not current.equal? @end
        aNext = current.previous
        block.call(current)
        current=current.aNext
      end
    end

    def size
      theSize=0
      each {|elmt|
        theSize+=1
      }
      theSize
    end
  end
end

module NB_Job_Framework
  class JobAbstract < NB_Common::DoublyLinkedListElmtAbstract
    attr_accessor :cancelled
    attr_accessor :jobFramework

    def initialize
      @cancelled = false
      @jobFramework = nil
    end
    
    def cancelled?
      cancelled
    end

    # must implement run method, which check regularly
    # cancelled? as an exit condition
  end

  class JobFramework
    def initialize
      @bigLock = Mutex.new
      @condJobListChanged = ConditionVariable.new
      @jobWaitList = NB_Common::DoublyLinkedList.new
      @jobExecList = NB_Common::DoublyLinkedList.new
    end
    
    def addJob iJob
      @bigLock.synchronize {
        @jobWaitList.push(iJob)
        @condJobListChanged.signal
        #puts "toto job added"
      }
    end
    
    private
    def threadLoop
      while true
        currentJob=nil
        while currentJob.nil?
          #puts "toto #{Thread.current} check new job"
          @bigLock.synchronize {
            if @jobWaitList.empty? and @jobExecList.empty?
              return
            elsif not @jobWaitList.empty?
              currentJob = @jobWaitList.front
              #detach job from waitlist and attach it to execList
              @jobExecList.push currentJob
            end
            #puts "toto #{Thread.current} wait for cond var" if currentJob.nil?
            @condJobListChanged.wait(@bigLock) if currentJob.nil?
          }
        end
        currentJob.jobFramework = self
        currentJob.run
        @bigLock.synchronize {
          #remove the job from exec list
          currentJob.rmElmt
          @condJobListChanged.broadcast if @jobWaitList.empty? and @jobExecList.empty?
        }
      end
    end

    public

    class AThreadWrapper < NB_Common::DoublyLinkedListElmtAbstract
      attr_accessor :thread
      def initialize iThread
        @thread = iThread
      end
    end

    def run iThreadNb
      puts "toto joblist size #{@jobWaitList.size}"
      threadList = NB_Common::DoublyLinkedList.new
      i = 0
      while i < iThreadNb
        i = i+1
        #puts "toto before thread creation"
        aNewThread = AThreadWrapper.new(
             Thread.new {threadLoop}
                                 )
        #puts "toto thread is born"
        threadList.push aNewThread
      end

      threadList.each { |threadWrapper|
        threadWrapper.thread.join
        #puts "toto thread is dead"
      }
    end
    
    def destroyJobIf (&block)
      @bigLock.synchronize {
        #puts "toto waitlist job destructions"
        @jobWaitList.each {|aJob|
          if block.call aJob
            #puts "toto destroy a job from waitlist"
            aJob.rmElmt
          end
        }
        #puts "toto execlist job destructions"
        @jobExecList.each {|aJob|
          if block.call aJob
            #puts "toto destroy a job from execlist"
            aJob.cancelled = true
          end
        }
        @condJobListChanged.broadcast
      }
    end
  end

end # end of module NB_Job_Framework

#Concept of this algorithm:
# The value generated (all extracted combinations of numbers by the 4
# operations) from a list l (size L) are the values made of N numbers
# generated by each of its  sublists of size N + the values made of
# combinations by an operation of the values generated by the couple of lists
# whose combined total number of elements is L

# no duplicate value is therefore ever generated for l or any other sub-lists

module Le_Compte_Est_Bon
 
  def Le_Compte_Est_Bon.arrayToSinglyLinked a
    l = NB_Common::SinglyLinkedList.emptyList
    a.reverse_each {|elmt|
      node = NB_Common::SinglyLinkedList.new
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
      yield NB_Common::SinglyLinkedList.emptyList
    else    
      duplicate_head = NB_Common::SinglyLinkedList.new
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



  # return all ordered subcombinations of iL;
  # iHead and iRemaining are appended to the respective two results list
  def Le_Compte_Est_Bon.getSubCombinationCouples_rec(iL,iHead,iRemaining,&block)
    if iL.empty?
      yield iHead,iRemaining
    else
      nextL = iL.next

      duplicateL = NB_Common::SinglyLinkedList.new
      duplicateL.content = iL.content
      duplicateL.next = iRemaining
      getSubCombinationCouples_rec(nextL,iHead,duplicateL,&block)

      duplicateL.next = iHead
      getSubCombinationCouples_rec(nextL,duplicateL,iRemaining,&block)
      
    end
  end

  #returns all non-ordered couple of sub combinations of iL!!!
  #principle: pop the first element of the list, generate all ordered 
  #subcombinations of iL, and systematically append to the first list the popped
  #element of iL
  def Le_Compte_Est_Bon.getAllSubCombinationCouples(iL,iNumCpu,iCpuExp,&block)
    if iL.empty?
      yield NB_Common::SinglyLinkedList.emptyList, NB_Common::SinglyLinkedList.emptyList
    else
      nextL = iL.next

      duplicateL = NB_Common::SinglyLinkedList.new
      duplicateL.content = iL.content

      head = duplicateL
      remaining = NB_Common::SinglyLinkedList.emptyList
      while iCpuExp != 0
        iCpuExp = iCpuExp - 1
        duplicateL = NB_Common::SinglyLinkedList.new
        duplicateL.content = nextL.content
        nextL = nextL.next
        if iNumCpu & 1 == 1
          duplicateL.next = head
          head = duplicateL
        else
          duplicateL.next = remaining
          remaining = duplicateL         
        end
        iNumCpu = iNumCpu >> 1
      end

      getSubCombinationCouples_rec(nextL,head,remaining,&block)
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
    
    def numberOfFinalNodes
      @leftNode.numberOfFinalNodes + @rightNode.numberOfFinalNodes
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

    def numberOfFinalNodes
      1
    end

    def to_s
      "#{@value}"
    end
  end

  class Algo
       
    class BestSolution
      attr_accessor :node
      
      def initialize(iTarget,iJobFramework)
        @bigLock = Mutex.new
        @target = iTarget
        @jobFramework = iJobFramework
        @node = nil
        @delta = nil
      end

      def isSolutionFound
        @delta == 0
      end

      def tryBestSolution(iNode)
        if not @delta or 
            (iNode.value - @target < @delta and @target - iNode.value < @delta)
          @bigLock.synchronize {
            if not @delta or 
                (iNode.value - @target < @delta and @target - iNode.value < @delta)
              @delta = @target - iNode.value
              @delta = - @delta if @delta < 0
              @node=iNode.duplicateTree

              puts "Best so far: #{@node.value} = #{@node}"

              solSize = iNode.numberOfFinalNodes
              if isSolutionFound
                @jobFramework.destroyJobIf {|job|                
                  job.lSize >= solSize
                }
              end
            end
          }
        end
      end
    end
    
    attr_reader :target

    def initialize (iTarget)
      @target = iTarget
    end
   
    def algo_l_size (iL, iJob, iNumCpu, iCpuExp, &block)
      if iL.next == NB_Common::SinglyLinkedList.emptyList
        # if l.size is one
        
        elmt = iL.content
        yield elmt
      else
        
        # Let's yield all combination of elements of l1 X l2
        # it is generate "the values made of combinations by an operation
        # of the values generated by the couple of lists whose combined
        # total number of elements is L" part of the algorithm described
        # on top of the file
        
        Le_Compte_Est_Bon.getAllSubCombinationCouples(iL, iNumCpu, iCpuExp) {|l1,l2|
          break if iJob.cancelled?
          if not l1.empty? and not l2.empty?
            newNode = Node.new

            algo_l_size(l1, iJob, 0, 0) {|elmt1|
              break if iJob.cancelled?

              algo_l_size(l2, iJob, 0, 0) {|elmt2|
                break if iJob.cancelled?

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
                
                if val2 > val1 && val1 > 0
                  newNode.value=val2 - val1
                  
                  newNode.leftNode = elmt2
                  newNode.rightNode = elmt1
                  newNode.operation = :Minus
                  
                  yield newNode
                  newNode.leftNode = elmt1
                    newNode.rightNode = elmt2
                end
                
                if val1 >= val2 && val2 > 0
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

    class AJob_algo_l_size < NB_Job_Framework::JobAbstract

      attr_writer :iJob
      def initialize (iAlgo_l_size,iL, iSubLSize, iNumCpu, iCpuExp, &block)
        @iAlgo_l_size = iAlgo_l_size
        @iL = iL
        @iSubLSize = iSubLSize
        @iNumCpu = iNumCpu
        @iCpuExp = iCpuExp
        @block = block
      end

      def lSize
        @iSubLSize
      end

      def run
        Le_Compte_Est_Bon.getSubCombinationsFixedLSize(@iL,@iSubLSize,@iL.size) { |subL|
          break if cancelled?
          @iAlgo_l_size.call(subL,self,@iNumCpu,@iCpuExp,@block)
        }
      end

    end
      

    def algo_all_sizes (iL, iLSize, iJobFramework, &block)
      # this "algo" calls are used for the 
      # "the values made of N numbers generated by each of its sublists
      # of size N" part of the algorithm described on top of the file
      
      #gets all combinations of size n verifying iMinLength <= n <= iLSize

      aAlgo_l_size = Proc.new {|l,job,numCpu,cpuExp,block|
        algo_l_size(l,job,numCpu,cpuExp,&block)
      }

      maxCpuExp = 3

      sl_size = 1

      while sl_size <= iLSize
        cpuExp = maxCpuExp
        if cpuExp + 1 >= sl_size
          cpuExp = sl_size - 1
        end
        maxNumCpu = 1 << cpuExp

        numCpu = 0
        while numCpu < maxNumCpu
          iJobFramework.addJob(AJob_algo_l_size.new(aAlgo_l_size,iL,sl_size,numCpu,cpuExp,&block))
          numCpu +=1
        end
        sl_size +=1
      end

      iJobFramework.run(2)
    end

    private :algo_l_size, :algo_all_sizes
    
    def run(iL)
      jobFramework = NB_Job_Framework::JobFramework.new
      bestSolution = BestSolution.new(@target,jobFramework)
      
      l=Le_Compte_Est_Bon.arrayToSinglyLinked(iL.collect {|elmt| FinalNode.new(elmt)})
      algo_all_sizes(l,l.size(),jobFramework) {|elmt|
        bestSolution.tryBestSolution(elmt)
      }

      if bestSolution.isSolutionFound
        puts "#{@target} = #{bestSolution.node}"
      elsif bestSolution.node
        puts "No Solution; nearest solution is: #{bestSolution.node.value} = #{bestSolution.node}"
      else
        puts "No Solution"
      end
      
      return bestSolution
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

algo = Le_Compte_Est_Bon::Algo.new(target)
algo.run(inputNumbers)

