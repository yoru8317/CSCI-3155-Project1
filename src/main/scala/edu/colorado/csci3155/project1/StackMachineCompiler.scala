package edu.colorado.csci3155.project1

object StackMachineCompiler {



    /* Function compileToStackMachineCode
        Given expression e as input, return a corresponding list of stack machine instructions.
        The type of stack machine instructions are in the file StackMachineEmulator.scala in this same directory
        The type of Expr is in the file Expr.scala in this directory.
     */
    def compileToStackMachineCode(e: Expr): List[StackMachineInstruction] = {
        //TODO: Your code here
        e match{
            case Const(f) => List(PushI(f))
            case Plus(e1, e2) =>{
                val v1 = compileToStackMachineCode(e1)
                val v2 = compileToStackMachineCode(e2)
                v1 ++ v2 ++ List(AddI)
            }
            case Minus(e1, e2) =>{
                val v1 = compileToStackMachineCode(e1)
                val v2 = compileToStackMachineCode(e2)
                v1 ++ v2 ++ List(SubI)
            }
            case Mult(e1, e2) =>{
                val v1 = compileToStackMachineCode(e1)
                val v2 = compileToStackMachineCode(e2)
                v1 ++ v2 ++ List(MultI)
            }
            case Div(e1, e2) =>{
                val v1 = compileToStackMachineCode(e1)
                val v2 = compileToStackMachineCode(e2)
                v1 ++ v2 ++ List(DivI)
            }
            case Exp(e) =>{
                val v = compileToStackMachineCode(e)
                v ++ List(ExpI)
            }
            case Log(e) =>{
                val v = compileToStackMachineCode(e)
                v ++ List(LogI)
            }
            case Sine(e) =>{
                val v = compileToStackMachineCode(e)
                v ++ List(SinI)
            }
            case Cosine(e) => {
                val v = compileToStackMachineCode(e)
                v ++ List(CosI)
            }
            case Let(ident, e1, e2) =>{
                val v1 = compileToStackMachineCode(e1)
                val v2 = compileToStackMachineCode(e2)
                v1 ++ List(LoadI(ident)) ++ v2
            }
            case Ident(id) => List(StoreI(id))

        }
    }
}
