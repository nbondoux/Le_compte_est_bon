#!/usr/bin/ruby
# -*- coding: utf-8 -*-

require 'getoptlong'
require 'drb'

#for mutexes:
require 'thread'


# NB Note: this is really ugly and dependant of DRb version, but it is needed to fix a bug
def DRb::here?(iUri)
  (self.uri rescue nil) == iUri
end

=begin
#Hack to have it run in ipv6!
TheIP = "2a01:e35:2f75:8810:1a3d:a2ff:fe47:5800"

module DRb


  class DRbTCPSocket

    def self.open_server(uri, config)
      host = TheIP
      port = 0
      soc = TCPServer.open(TheIP, port)
      
      port = soc.addr[1] if port == 0
      config[:tcp_port] = port
      uri = "druby://#{host}:#{port}"
      self.new(uri, soc, config)
    end
  end
end
=end

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

  module DoublyLinkedListElmtMixin
    attr_reader :next, :previous
    attr_writer :next, :previous
    protected

    public
    
    def initialize_DoublyLinkedListElmtMixin
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
      exPrevious
    end

    def rmElmt
      @next.rmElmtBefore
      self
    end

    def list
      aPrevious = @previous
      while not (aPrevious.class.eql? DoublyLinkedListElmtEnd)
        return nil if aPrevious.nil?
        aPrevious=aPrevious.previous
      end
      return aPrevious.list
    end
  end
  
  class DoublyLinkedListElmtAbstract
    include DoublyLinkedListElmtMixin
    def initialize
      initialize_DoublyLinkedListElmtMixin
    end
  end

  class DoublyLinkedListElmtEnd < DoublyLinkedListElmtAbstract
    attr_reader :list
    def initialize(iList)
      @list = iList
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

    def push_front(iElmt)
      @end.next.addElmtBefore(iElmt)
    end

    def pop_front
      @end.next.next.rmElmtBefore
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

    def clear
      #not optimal
      @end = DoublyLinkedListElmtEnd.new(self)
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
        if iNumCpu & 1 == 0
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

    class BestSolutionDescriptor
      include DRb::DRbUndumped
      # size of the exact solution currently found
      attr_accessor :delta
      def initialize
        @delta=nil
      end
    end

    class BestSolution
      attr_accessor :target
      
      def initialize
      end

      def setJob(iJob)
        @target = iJob.serverObject.target
        @remoteJob = iJob
        @delta = iJob.serverObject.bestSolutionDescriptor.delta
      end

      def isSolutionFound
        @delta == 0
      end

      def tryBestSolution(iNode)
        if @delta.nil? or 
            (iNode.value - @target < @delta and @target - iNode.value < @delta)
          bestSolutionDescriptor = @remoteJob.serverObject.bestSolutionDescriptor
          puts "titi: Push local best so far: #{iNode.value} = #{iNode} ; current delta = #{@delta}"
          @remoteJob.serverObject.pushBestSolution(iNode,@remoteJob)
          @delta = bestSolutionDescriptor.delta
          # in order to have the right value before next poll; not mandatory
        end
      end
      
    end
    attr_reader :target, :inputNumbers, :uri, :clientServerMode

    def initialize (iInputNumbers,iTarget,iClientServerMode,iUri)
      @target = iTarget
      @inputNumbers = iInputNumbers
      @uri = iUri
      @clientServerMode = iClientServerMode
    end
   
    def algo_l_size (iL, iNumCpu, iCpuExp, iClientInstance, &block)
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
          if not l1.empty? and not l2.empty?
            newNode = Node.new

            algo_l_size(l1, 0, 0, iClientInstance) {|elmt1|
              algo_l_size(l2, 0, 0, iClientInstance) {|elmt2|
                newNode.leftNode = elmt1
                newNode.rightNode = elmt2
                val1 = elmt1.value
                val2 = elmt2.value
                
                if val1 > 0 and val2 > 0
                  if (not (elmt1.class.eql? Node) or elmt1.operation != :Minus) and
                      (not (elmt2.class.eql? Node) or elmt2.operation != :Minus)
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
                
                if val1 > 1 && val2 > 1
                  if ((not (elmt1.class.eql? Node) or elmt1.operation != :Divi) and
                      (not (elmt2.class.eql? Node) or elmt2.operation != :Divi))
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

    class AJob_algo_l_size
      include DRb::DRbUndumped
      include NB_Common::DoublyLinkedListElmtMixin

      attr_reader :serverObject, :iL, :iSubLSize, :iNumCpu, :iCpuExp, :remoteClients
      attr_accessor :cancelled
      def initialize(iServerObject, iL, iSubLSize, iNumCpu, iCpuExp, &block)
        initialize_DoublyLinkedListElmtMixin

        @serverObject = iServerObject

        @iL = iL
        @iSubLSize = iSubLSize
        @iNumCpu = iNumCpu
        @iCpuExp = iCpuExp
        @remoteClients = Array.new
        @cancelled = false
      end

    end
      
    class LeCompteEstBonServer
      include DRb::DRbUndumped
      attr_reader :bestNode, :delta, :target, :bestSolutionDescriptor

      def initialize(iTarget)
        @bestNode = nil
        @delta = nil
        @target = iTarget
        @jobs = NB_Common::DoublyLinkedList.new
        @finishedJobs = NB_Common::DoublyLinkedList.new
        @bestSolutionDescriptor = BestSolutionDescriptor.new
        @bigLock = Mutex.new
      end
      
      class JobListContainer < NB_Common::DoublyLinkedList
        include NB_Common::DoublyLinkedListElmtMixin

        def initialize(*iVal)
          super(*iVal)
          initialize_DoublyLinkedListElmtMixin
        end
      end

      
      def enqueueJobs(iJobs)
        # the goal here is to put in front line
        # the longest jobs, following an heuristic;
        # the idea is that if the iCpuExp in binary contains
        # a big difference in number of 1 and 0, the job will be longer
        # (the generated lists will be less equilibrated: see 
        # Le_Compte_Est_Bon.getAllSubCombinationCouples)

        sortedJobs=Array.new
        iJobs.each {|job|
          #compute the heuristic weith of the job
          mask = job.iNumCpu
          maskLength = job.iCpuExp
          weightZero = 0
          weightOne = 0
          
          while weightOne + weightZero < maskLength
            if (mask & 1) == 1
              weightOne += (mask & 1)
            else
              weightZero += 1
            end
            mask = mask >> 1
          end

          if weightOne > weightZero + 1 #+1 because the first élément of iL always count as a 0 (see Le_Compte_Est_Bon.getAllSubCombinationCouples)
            weight = weightOne
          else
            weight = weightZero + 1
          end
          sortedJobs.push([job,weight])
        }
        sortedJobs.sort!{|a,b|
          #sort by inverse weight (do the longest jobs in first)
          b[1] <=> a[1]
        }
       
        @jobs.push(JobListContainer.new)
        sortedJobs.each{|job,weight|
          @jobs.back.push(job)
        }
      end
      
      def dequeueJob(iClient)
        @bigLock.synchronize {
          if not @jobs.empty?
            jobsForCurrentSize = @jobs.front
            
            if jobsForCurrentSize.empty?
              jobsForCurrentSize = jobsForCurrentSize.next
              jobsForCurrentSize.rmElmtBefore
            end
            if not jobsForCurrentSize.class.eql? NB_Common::DoublyLinkedListElmtEnd
              job = jobsForCurrentSize.pop_front
              job.remoteClients.push(iClient)
              jobsForCurrentSize.push job
              begin
                iClient.cancelled = false
                return job
              rescue
                return nil
              end
            end
          end
        }
        DRb.stop_service
        return nil
      end

      def jobGaveASolution_nolock(iJob)
        jobListForCurrentSize = iJob.list
        
        #let's cancel all jobs being currently treated for the same size
        jobListForCurrentSize.each {|aJob|
          aJob.remoteClients.each {|client|
            aJob.cancelled = true
            begin
              client.cancelled = true
            rescue
              #we lost the client ...
            end
          }
          @finishedJobs.push aJob
        }        
        #let's remove all job lists for bigger sizes
        jobListForNextSize = jobListForCurrentSize.next
        while not jobListForNextSize.class.eql? NB_Common::DoublyLinkedListElmtEnd
          jobListForNextSize.rmElmt
          jobListForNextSize = jobListForCurrentSize.next
        end
      end

      # to be called by any client process at the end of process of a job,
      # to unregister the process, and cancell any other process working on the job
      def finishJob(iClient,iJob)
        @bigLock.synchronize {
          iJob.remoteClients.delete(iClient)
          if iJob.remoteClients.empty?
            #this job shouldn't be referenced any where; it can be garbage collected
            iJob.rmElmt
          else
            if not iJob.cancelled
              iJob.cancelled = true
              iJob.remoteClients.each {|client|
                begin
                  client.cancelled = true
                rescue
                  #puts "client #{iClient.__drburi} lost ..."
                  #we lost the client ...
                end
              }
            end
            #detach the job from current list and push it in the to be deleted jobs
            @finishedJobs.push iJob
          end
        }
      end

      def pushBestSolution(iNode, iJob)
        @bigLock.synchronize {
          value = iNode.value
          delta = value - target
          delta = - delta if delta < 0
          solutionSize = iNode.numberOfFinalNodes
          if @delta.nil? or delta < @delta
            @bestNode = iNode
            @delta = delta
            @bestSolutionDescriptor.delta = delta
            puts "Best so far: #{@bestNode.value} = #{@bestNode}"
            if delta == 0
              jobGaveASolution_nolock(iJob)
            end
          end
        }
      end
    end
    
    class LeCompteEstBonClient
      include DRb::DRbUndumped
      attr_accessor :bestSolutionDescriptor,:cancelled

      def initialize
        @cancelled=false
        @bestSolutionDescriptor = BestSolutionDescriptor.new
      end
      def cancelled?
        @cancelled
      end
    end

    def algo_all_sizes (iL, iLSize, iLeCompteEstBonServer)
      # this "algo" calls are used for the 
      # "the values made of N numbers generated by each of its sublists
      # of size N" part of the algorithm described on top of the file
      
      #gets all combinations of size n verifying iMinLength <= n <= iLSize

      maxCpuExp = 6

      sl_size = 1

      while sl_size <= iLSize
        jobs = Array.new
        cpuExp = maxCpuExp
        if cpuExp + 1 >= sl_size
          cpuExp = sl_size - 1
        end
        maxNumCpu = 1 << cpuExp

        numCpu = 0
        while numCpu < maxNumCpu
          jobs.push(AJob_algo_l_size.new(iLeCompteEstBonServer,iL,sl_size,numCpu,cpuExp))
          numCpu +=1
        end
        iLeCompteEstBonServer.enqueueJobs(jobs)
        sl_size +=1
      end

    end

    private :algo_l_size, :algo_all_sizes
    
    class LeCompteEstBonClientCancelledException < Exception
    end

    def run()
      srand

      if clientServerMode == :server_client_mode
        @uri ||= "drbunix:/tmp/socket.lecompteestbon.serv.#{rand(1000000)}"
      end

      pidFromFork = nil
      childPids = nil
      if clientServerMode == :server_client_mode
        # create the child processes !!!!
        nbProcesses = 4 
        childPids = Array.new
        i=0
        while i < nbProcesses
          i += 1
          pidFromFork = Kernel.fork
          break if pidFromFork.nil?
          childPids.push pidFromFork
        end
      end
      
      if (clientServerMode == :server_client_mode and not pidFromFork.nil?) or clientServerMode == :server_mode
        leCompteEstBonServer = LeCompteEstBonServer.new(@target)

        l=Le_Compte_Est_Bon.arrayToSinglyLinked(@inputNumbers.collect {|elmt| FinalNode.new(elmt)})
        algo_all_sizes(l,l.size(),leCompteEstBonServer)

        DRb.start_service(uri, leCompteEstBonServer)
        if clientServerMode == :server_mode
          puts DRb.uri
        end

        DRb.thread.join
        if (clientServerMode == :server_client_mode)
          # quietly wait for children's death
          childPids.each {|pid|
            Process.wait(pid)
          }
        end

      elsif (clientServerMode == :server_client_mode and pidFromFork.nil?) or clientServerMode == :client_mode
        leCompteEstBonClient = LeCompteEstBonClient.new

        uriClient = nil
        if clientServerMode == :server_client_mode
          uriClient = "drbunix:/tmp/socket.lecompteestbon.serv.#{rand(1000000)}"
        end

        DRb.start_service(uriClient,nil)

        isConnectionOk = false
        while not isConnectionOk
          begin
            sleep 2
            leCompteEstBonServer = DRbObject.new(nil,uri)
            isConnectionOk = true
          rescue

          end
        end
              
        bestSolution = BestSolution.new
        theJob = leCompteEstBonServer.dequeueJob(leCompteEstBonClient)

        while not theJob.nil?
          bestSolution.setJob(theJob)
          #theJob is a reference to a distant DRbObject; lets retrieve
          #its content once:
          l = theJob.iL
          subLSize = theJob.iSubLSize
          numCpu = theJob.iNumCpu
          cpuExp = theJob.iCpuExp

	  puts "titi: Dequeue a Job (#{subLSize}) (#{numCpu})"

          begin
            Le_Compte_Est_Bon.getSubCombinationsFixedLSize(l,subLSize,l.size) { |subL|
              algo_l_size(subL,numCpu,cpuExp,leCompteEstBonClient) {|elmt|
                if leCompteEstBonClient.cancelled?
                  raise LeCompteEstBonClientCancelledException
                end
                bestSolution.tryBestSolution(elmt)
              }
            }
          rescue LeCompteEstBonClientCancelledException
            puts "titi: Job cancelled"
          end
          leCompteEstBonServer.finishJob(leCompteEstBonClient,theJob)
          theJob = leCompteEstBonServer.dequeueJob(leCompteEstBonClient)
        end
        DRb.stop_service
        Kernel.exit! 0
      end
      
      if leCompteEstBonServer.delta == 0
        puts "#{@target} = #{leCompteEstBonServer.bestNode}"
      elsif leCompteEstBonServer.bestNode
        puts "No Solution; nearest solution is: #{leCompteEstBonServer.bestNode.value} = #{leCompteEstBonServer.bestNode}"
      else
        puts "No Solution"
      end
      
      return bestSolution
    end
  end


end #end of the module

def help_message(iExecName)
  puts "#{iExecName} [{-s ,--server=}[uri] --] number1 [number 2 [number 3 ...]] target\n"
  puts "#{iExecName} [{-c ,--client=}uri]"
end

execName = $0

clientServerMode = nil
#the different values are
#:server_client_mode (the executable do both the tasks)
#:client_mode
#:server_mode


opts = GetoptLong.new(
  [ "--client", "-c", GetoptLong::REQUIRED_ARGUMENT],
  [ "--server", "-s", GetoptLong::OPTIONAL_ARGUMENT],
  [ "--help", "-h", GetoptLong::NO_ARGUMENT]
)

# process the parsed options
isError = false
uri = nil
opts.each { |opt, arg|
  if opt == "--client"
    uri=arg
    isError = true if not clientServerMode.nil?
    clientServerMode=:client_mode
  elsif opt == "--server"
    uri=arg if arg != ""
    isError = true if not clientServerMode.nil?
    clientServerMode=:server_mode
  elsif opt == "--help"
    help_message execName
    exit 0
  end
  break if isError
}

clientServerMode ||= :server_client_mode
if isError or clientServerMode == :client_mode and ARGV.size != 0 or 
    clientServerMode != :client_mode and ARGV.size == 0
  help_message execName
  exit 1
end


inputNumbers = Array.new
if clientServerMode != :client_mode
  
  ARGV.each {|elmt|
    i = elmt.to_i
    if (i < 0)
      puts "Input numbers must be positive !!!"
      exit 1
    end
    inputNumbers.push i
  }
end
target = inputNumbers.pop

algo = Le_Compte_Est_Bon::Algo.new(inputNumbers,target,clientServerMode,uri)
algo.run

