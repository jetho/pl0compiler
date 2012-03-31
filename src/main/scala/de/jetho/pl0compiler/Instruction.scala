
/** The instruction and register sets of the TAM Abstract machine.*/


package de.jetho.pl0compiler


case class Instruction(op: Int, var r: Int, n: Int, var d: Int)


object Instruction { 

  val CB = 0
  val PB = 1024

  /** the instruction set.*/
  val List(opLOAD, 
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


  /** the set of registers.*/
  val List(rCB, rCT, rPB, rPT, rSB, rST, rHB, rHT, rLB, rL1, rL2, rL3, rL4, rL5, rL6, rCP) = 0.until(16).toList

  /** the offsets for the primitive routines.*/
  val List(addDisplacement,
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

  val puteolDisplacement = 24
  val putintDisplacement = 26

}
