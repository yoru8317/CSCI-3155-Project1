package edu.colorado.csci3155.project1

import java.util

import scala.collection.JavaConverters._


/* -- Here are all the instructions to be supported --*/
sealed trait StackMachineInstruction
case class LoadI(s: String) extends StackMachineInstruction
case class  StoreI(s: String) extends StackMachineInstruction
case class PushI(f: Double) extends StackMachineInstruction
case object AddI extends StackMachineInstruction
case object SubI extends StackMachineInstruction
case object MultI extends StackMachineInstruction
case object DivI extends StackMachineInstruction
case object ExpI extends StackMachineInstruction
case object LogI extends StackMachineInstruction
case object SinI extends StackMachineInstruction
case object CosI extends StackMachineInstruction
case object PopI extends StackMachineInstruction


object StackMachineEmulator {



    /* Function emulateSingleInstruction
        Given a list of doubles to represent a stack
              a map from string to double precision numbers for the environment
        and   a single instruction of type StackMachineInstruction
        Return a tuple that contains the
              modified stack that results when the instruction is executed.
              modified environment that results when the instruction is executed.

        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.
     */
    def emulateSingleInstruction(stack: List[Double],
                                 env: Map[String, Double],
                                 ins: StackMachineInstruction): (List[Double], Map[String, Double]) = {
        // TODO: Your code here.

        ins match{
            case PushI(d) => (d:: stack,env)
            case PopI => stack match {
                case Nil => throw new IllegalArgumentException()
                case _ :: tail =>
                    (tail,env)
            }
            case AddI => {
                if(stack.length < 2){
                    throw new IllegalArgumentException()
                }else {
                    stack match {
                        case t1 :: t2 :: tail => (t1 + t2 :: tail,env)
                        case _ => throw new IllegalArgumentException ()
                    }
                }
            }
            case SubI => {
                if(stack.length < 2){
                    throw new IllegalArgumentException()
                }else{
                    stack match{
                        case t1 :: t2:: tail => (t2 - t1 :: tail,env)
                        case _ => throw new IllegalArgumentException()
                    }
                }
            }
            case MultI => {
                  if(stack.length< 2){
                      throw new IllegalArgumentException()
                  }else{
                      stack match{
                          case t1::t2::tail => (t1 * t2 :: tail,env)
                          case _ => throw new IllegalArgumentException()
                      }
                  }

            }
            case DivI => {
                if (stack.length< 2){
                    throw new IllegalArgumentException()
                }else{
                    stack match{
                        case t1::t2::tail => (t2 / t1 :: tail,env)
                        case _ => throw new IllegalArgumentException()
                    }
                }
            }
            case ExpI => {
                if (stack.length < 1){
                    throw new IllegalArgumentException()
                }else{
                    stack match{
                        case t1::tail => (scala.math.exp(t1)::tail,env)
                        case _ => throw new IllegalArgumentException()
                    }
                }
            }
            case LogI => {
                if (stack.length < 1){
                    throw new IllegalArgumentException()
                }else{
                    stack match{
                        case t1::tail => {
                            if(t1 > 0){ (scala.math.log(t1)::tail,env)}
                            else{throw new IllegalArgumentException()}
                        }
                        case _ => throw new IllegalArgumentException()
                    }
                }
            }
            case SinI => {
                if (stack.length < 1){
                    throw new IllegalArgumentException()
                }else{
                    stack match{
                        case t1::tail => (scala.math.sin(t1)::tail,env)
                        case _ => throw new IllegalArgumentException()
                    }
                }
            }
            case CosI => {
                if (stack.length < 1){
                    throw new IllegalArgumentException()
                }else{
                    stack match{
                        case t1::tail => (scala.math.cos(t1)::tail,env)
                        case _ => throw new IllegalArgumentException()
                    }
                }
            }
            case LoadI(s) => {
                val v = stack.head
                //def pop (stack2: List[Double], env2: Map[String, Double]) = emulateSingleInstruction(stack,env,PopI)
                val (stack2, env2) = emulateSingleInstruction(stack,env,PopI)
                val new_env = env2 ++ Map(s -> v)
                (stack2, new_env)
            }
            case StoreI(s)=>{
                if (env.contains(s)){
                    val v = env(s)
                    //def push (stack2: List[Double], env2: Map[String, Double]) = emulateSingleInstruction(stack,env,PushI(v))
                    val (stack2, env2) = emulateSingleInstruction(stack,env,PushI(v))
                    val new_env = env2 ++ Map(s->v)
                    (stack2 , new_env)
                    //push(stack, Map(s->v))
                }else{
                    throw new IllegalArgumentException()
                }
            }

        }
    }

    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Use foldLeft over list of instruction rather than a for loop if you can.
       Return value must be the final environment.

       Hint: accumulator for foldLeft must be a tuple (List[Double], Map[String,Double])
             initial value of this accumulator must be (Nil, Map.empty)
             You should use emulateSingleInstruction to update the accmulator.
             It will all fit nicely once you figure it out.
     */
    def emulateStackMachine(instructionList: List[StackMachineInstruction]): Map[String, Double] = {
        //TODO: Your Code here.
        val (stack, environment) = instructionList.foldLeft[(List[Double], Map[String,Double])](Nil, Map.empty)({
            case ((acc, env), elt: StackMachineInstruction) =>
                emulateSingleInstruction(acc, env, elt)

        })
        environment
    }
}