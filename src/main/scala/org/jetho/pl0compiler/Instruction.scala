
/** The instruction and register sets for the TAM Abstract machine.*/


package org.jetho.pl0compiler


case class Instruction( op:    Int,                             
                        var r: Int,                         
                        var n: Int,                         
                        var d: Int,
                        ident: Option[String] = None,         
                        env:   Option[CodeGenerator.RuntimeEnvironment] = None,
                        frame: Option[Frame] = None )


object Instruction { 

  final val CB = 0
  final val PB = 1024

  /** the instruction set.*/
  final val List( opLOAD, 
	              opLOADA, 
	              opLOADI, 
	              opLOADL, 
	              opSTORE, 
	              opSTOREI, 
	              opCALL, 
	              opCALLI, 
	              opRETURN, 
	              _,                // no OP-Code with Value 9 
	              opPUSH,
	              opPOP, 
	              opJUMP, 
	              opJUMPI, 
	              opJUMPIF, 
	              opHALT) = 0.until(16).toList


  /** a dummy Call-OP for patchable forward references.*/
  final val opCALL_DUMMY = 99


  /** the set of registers.*/
  final val List(rCB, rCT, rPB, rPT, rSB, rST, rHB, rHT, rLB, rL1, rL2, rL3, rL4, rL5, rL6, rCP) = 0.until(16).toList

  /** the offsets for the primitive routines.*/
  final val List( addDisplacement,
	              subDisplacement,
	              multDisplacement,
	              divDisplacement,
	              modDisplacement,
	              ltDisplacement,
	              leDisplacement,
	              geDisplacement,
	              gtDisplacement,
	              eqDisplacement,
	              neDisplacement) = 8.until(19).toList

  final val puteolDisplacement = 24
  final val putintDisplacement = 26

}
