
/** The CodeGenerator emits bytecode for the TAM Abstract machine.*/


package de.jetho.pl0compiler


object CodeGenerator {

  type RuntimeEnvironment = Environment[RuntimeEntity]
   
  var nextInstrAddress = Instruction.CB
  val code = new Array[Instruction](Instruction.PB)

  /** fills the array with bytecode instructions.*/
  def emit(op: Int, n: Int, r: Int, d: Int): Int = { 
    if(nextInstrAddress > Instruction.PB)
      sys.error("Can't process more than " + Instruction.PB + " Instructions!")
    val instrAddress = nextInstrAddress
    code(instrAddress) = Instruction(op, r, n, d)
    nextInstrAddress += 1
    instrAddress
  }

  /** utility function for code backpatching.*/
  def patch(address: Int, register: Option[Int], displacement: Option[Int]) { 
    register.map { code(address).r = _ }
    displacement.map { code(address).d = _ }    
  }

  /** backpatching for procedure calls.*/
  def patchProcCalls(patchLocations: List[PatchLocation]) { 
    patchLocations.foreach { 
      case PatchLocation(location, ident, env, frame) =>  
	env.resolve(ident) map { 
	  case Proc(Some(EntityAddress(addressLevel, displacement))) => 
	    patch(location, Some(displayRegister(frame.level, addressLevel)), Some(displacement))
	  case _ => sys.error("Unresolved Procedure " + ident)
	}
      }
    }

  /** register reolution for nested prodedure calls.*/
  def displayRegister(currentLevel: Int, objectLevel: Int) =
    objectLevel match { 
      case 0 => Instruction.rSB
      case _ if (currentLevel - objectLevel <= 6) => Instruction.rLB + currentLevel - objectLevel
      case _ => sys.error("Can't access data more than 6 levels out!")
    }

  /** generate TAM bytecode and backpatch if necessary.*/
  def encode(ast: AST): Option[List[Instruction]] = { 
    val globalFrame = Frame(0, 0)
    val globalEnv = EmptyEnvironment[RuntimeEntity].extend(primitiveRoutines())
    val patchLocations = encodeAst(ast, globalEnv, globalFrame)
    patchProcCalls(patchLocations)
    emit(Instruction.opHALT, 0, 0, 0)
    Some(code.toList.take(nextInstrAddress))
  }  

  /** generate bytecode for the various AST nodes.
      return locations that need code backpatching.*/
  def encodeAst(ast: AST, env: RuntimeEnvironment, currentFrame: Frame): List[PatchLocation] = 
    ast match {       
      
      case Block(constDecls, varDecls, procDecls, statement) => { 
	val constBindings = constDecls.map { case ConstDecl(id, num) => (id, Constant(num)) }
	val varBindings = varDecls.zipWithIndex.map { case (varDecl, index) => 
	  (varDecl.ident, Variable(EntityAddress(currentFrame.level, currentFrame.offset + index))) }
	val procBindings = procDecls.map { procDecl => (procDecl.ident, Proc(None)) }
	if (varBindings.length > 0)
	  emit(Instruction.opPUSH, 0, 0, varBindings.length)
        val newLexicalEnvironment = env.extend( constBindings ::: varBindings ::: procBindings )
	val patchLocations = procDecls.map { encodeAst(_, newLexicalEnvironment, currentFrame) }.flatten
        val patchLocationStmt = statement.map(encodeAst(_, newLexicalEnvironment, currentFrame)).getOrElse(Nil)
	if (varBindings.length > 0)
	  emit(Instruction.opPOP, 0, 0, varBindings.length)
	
	patchLocations ::: patchLocationStmt	
      }  

      case ProcDecl(ident, block) => { 	
	val jumpInstrAddress = emit(Instruction.opJUMP, 0, Instruction.rCB, 0)
	env.update(ident, Proc(Some(EntityAddress(currentFrame.level, nextInstrAddress))))
	val patchLocations = encodeAst(block, env, Frame(currentFrame.level + 1, 3))
	emit(Instruction.opRETURN, 0, 0, 0)
	patch(jumpInstrAddress, None, Some(nextInstrAddress))
	patchLocations
      }

      case CallStmt(ident) => { 
	env.resolve(ident) flatMap { 
	  case Proc(None) => { 
	    val patchLocation = PatchLocation(nextInstrAddress, ident, env, currentFrame)
	    emit(Instruction.opCALL, Instruction.rSB, Instruction.rCB, 0)
	    Some(patchLocation) 
	  }		
	  case knownRoutine @ _ => { 	
	    encodeRoutineCall(ident, knownRoutine, currentFrame) 
	    None
	  }
	}
      }.toList

      case SeqStmt(stmts) => stmts.map{ encodeAst(_, env, currentFrame) }.flatten

      case AssignStmt(ident, expr) => { 
	encodeAst(expr, env, currentFrame)
	env.resolve(ident) map { 
	  case Variable(EntityAddress(addressLevel, displacement)) =>
	    emit(Instruction.opSTORE, 1, displayRegister(currentFrame.level, addressLevel), displacement)
	}
	List()
      }      

      case IfStmt(condition, stmt) => { 
	encodeAst(condition, env, currentFrame)
	val jumpInstrAddress = emit(Instruction.opJUMPIF, 0, Instruction.rCB, 0)
 	val patchLocations = stmt.map { stmt => encodeAst(stmt, env, currentFrame) }.getOrElse(Nil)
	patch(jumpInstrAddress, None, Some(nextInstrAddress))
	patchLocations
      }

      case WhileStmt(condition, stmt) => { 
	val jumpInstrAddress = emit(Instruction.opJUMP, 0, Instruction.rCB, 0)
	val stmtAddress = nextInstrAddress
	val patchLocations = stmt.map { stmt => encodeAst(stmt, env, currentFrame) }.getOrElse(Nil)
	patch(jumpInstrAddress, None, Some(nextInstrAddress))	  // patch Jump to Condition Address
	encodeAst(condition, env, currentFrame)
	emit(Instruction.opJUMPIF, 1, Instruction.rCB, stmtAddress)
	patchLocations		
      }

      case PrintStmt(expr) => { 
      	encodeAst(expr, env, currentFrame)
	encodeAst(CallStmt("!"), env, currentFrame)
	encodeAst(CallStmt("$puteol"), env, currentFrame)
      }

      case BinaryCondition(cond, left, right) => { 
	encodeAst(left, env, currentFrame)
	encodeAst(right, env, currentFrame)
	if(cond == "=" || cond == "#")
	  emit(Instruction.opLOADL, 0, 0, 1)
	encodeAst(CallStmt(cond), env, currentFrame)
      }

      case OddCondition(expr) => { 
	encodeAst(expr, env, currentFrame)
	emit(Instruction.opLOADL, 0, 0, 2)
	encodeAst(CallStmt("$mod"), env, currentFrame)
	emit(Instruction.opLOADL, 0, 0, 0)
	emit(Instruction.opLOADL, 0, 0, 1)
	encodeAst(CallStmt("#"), env, currentFrame)
      }

      case BinOp(op, left, right) => { 
	encodeAst(left, env, currentFrame)
	encodeAst(right, env, currentFrame)
	encodeAst(CallStmt(op), env, currentFrame)
      }

      case Ident(name) => { 
	env.resolve(name) map { 
	  case Constant(value) => emit(Instruction.opLOADL, 0, 0, value)
	  case Variable(EntityAddress(addressLevel, displacement)) => 
	    emit(Instruction.opLOAD, 1, displayRegister(currentFrame.level, addressLevel), displacement)
	}
	List()
      }

      case IntLiteral(value) => { 
	emit(Instruction.opLOADL, 0, 0, value)
	List()
      }
      
      case _ => throw new RuntimeException("Unknown AST-Node: " + ast) 

    }


  /** encode procedure calls.*/
  def encodeRoutineCall(ident: String, proc: RuntimeEntity, currentFrame: Frame) = 
    proc match {    
      case Proc(Some(EntityAddress(addressLevel, displacement))) =>  
	emit(Instruction.opCALL, displayRegister(currentFrame.level, addressLevel), Instruction.rCB, displacement)
      case PrimitiveRoutine(displacement) => 
	emit(Instruction.opCALL, Instruction.rSB, Instruction.rPB, displacement) 
      case _ => sys.error("Unknown Routine " + ident)
    }
  
  /** addresses of the primitive routines.*/
  def primitiveRoutines(): List[(String, PrimitiveRoutine)] =  
    List(
      ("!", PrimitiveRoutine(Instruction.putintDisplacement)),
      ("$puteol", PrimitiveRoutine(Instruction.puteolDisplacement)),
      ("-", PrimitiveRoutine(Instruction.subDisplacement)),
      ("+", PrimitiveRoutine(Instruction.addDisplacement)),
      ("*", PrimitiveRoutine(Instruction.multDisplacement)),
      ("/", PrimitiveRoutine(Instruction.divDisplacement)),
      ("$mod", PrimitiveRoutine(Instruction.modDisplacement)),
      ("<", PrimitiveRoutine(Instruction.ltDisplacement)),
      ("<=", PrimitiveRoutine(Instruction.leDisplacement)),
      (">=", PrimitiveRoutine(Instruction.geDisplacement)),
      (">", PrimitiveRoutine(Instruction.gtDisplacement)),
      ("=", PrimitiveRoutine(Instruction.eqDisplacement)),
      ("#", PrimitiveRoutine(Instruction.neDisplacement))     
    )
  
}
	    
