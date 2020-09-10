# 2026 Konstantinos-Ioannis Vezos cs112026
# 2546 Marina Stergiou cse32546

#imports:
import sys
import os.path
import atexit
import copy
#define
id_Error = -1
id_Variable = 0 #{letter}({letter}|{digit})*
id_Number = 1	#{digit}({digit})*
id_MathSymbol = { "+" : 20, "-" : 21, "*" : 22, "/" : 23 } # "+" "-" "*" "/"
id_CorrelationOperator = { "<" : 30, ">" : 31,	# "<" ">"
						   "=" : 32, "<=" : 33,	# "=" "<="
						   ">=" : 34,"<>" : 35 }# ">=" "<>"
id_AssignmentOperator = { ":=" : 40, ":" : 41 }	# ":=" ":"
id_Separator = {";" : 50, "," : 51 } # ";" ","
id_GroupingSymbol = { "{" : 60, "}" : 61, 	# "{" "}"
					  "(" : 62, ")" : 63, 	# "(" ")"
					  "[" : 64, "]" : 65 } 	# "[" "]"
id_Comment = { "\\*" : 70, "*\\" : 71 }	# "\*" "*\"
id_EOF = 10

#table with commands
sysCommands = {	"and" : 1001, 			"declare" : 1002,
				"do" : 1003, 			"else" : 1004,
				"enddeclare" : 1005, 	"exit" : 1006, 
				"procedure" : 1007, 	"function" : 1008,
				"print" : 1009, 		"call" : 1010,
				"if" : 1011, 			"in" : 1012,
				"inout" : 1013, 		"not" : 1014,
				"select" : 1015, 		"program" : 1016,
				"or" : 1017, 			"return" : 1018,
				"while" : 1019, 		"default" : 1020 }

#global variables:
curr_File = ""	#current file working.
curr_Word = 1	#current word working.
lex_State = 0 #start state (0) by Default.
id_File = 0	#the files give in input.
id_Word = -1	#lex().1
result_Word = "" #lex().2
numberOfNextQuad = 100	#start from 100. openFile() assing.
rateOfChange = 10	#100, 110, 120.
counterTemp	= -1	#T_0, T_1, temps Variable, nextTemp()
hashMapCommands = {}	#commands e.x. ["=", 0, 0, "100"]
curr_BlockName = ""	#programName, procedureName, functionName
curr_BlockID = ""	#'program', 'procedure', 'function'
findReturn = False	
stackListScopes = []	#symbol list
doOrNot = []
converterFor_C = {"<>" : "!=", ":=" : "=", "=" : "==", "and" : "&&", "or" : "||" }	#for changes between Ciscal and C
strFileFor_C = ""
strFileFor_MIPS = ""
converterFor_MIPS = {	"=" : "beq", "<" : "blt", 
						">" : "bgt", "<=" : "ble",
						">=" : "bge", "<>" : "bne", 
						"+" : "add", "-" : "sub",
						"*" : "mul", "/" : "div" }

def openFile(): #openFile
	global curr_File
	global id_File
	global hashMapCommands
	global numberOfNextQuad, counterTemp
	global strFileFor_MIPS
	global lex_State, findReturn
	
	id_File = id_File + 1
	length_argv = len(sys.argv)
	if (length_argv) == 1:
		print "Error: file not found!"
		sys.exit()
	if not (length_argv > id_File):
		print "Done with files. See you next time!"
		sys.exit()
	if not (os.path.exists(sys.argv[id_File])):
		print "Error:", sys.argv[id_File], "does not exist!"
		sys.exit()
	curr_File = open(sys.argv[id_File],"r")
	numberOfNextQuad = 100	#reset for next file
	counterTemp = -1	#reset for next file.
	strFileFor_MIPS = ""	#reset for next file.
	hashMapCommands.clear()	#clear hashMapCommands for next file.
	del stackListScopes[:]
	del doOrNot[:]
	findReturn = False
	lex_State = 0
	print "open file:", curr_File.name
	atexit.register(closeFile)
	syntax()

def closeFile(): #closeFile
	global curr_File
	global hashMapCommands

	if (type(curr_File) is file and not curr_File.closed):
		curr_File.close()
		#endHashMapCommands = len(hashMapCommands) * rateOfChange + 100	#helper for print the quads
		#for key in range(100, endHashMapCommands, rateOfChange):
			#print key,":", hashMapCommands[key]
		#print "Length hashMap", len(hashMapCommands)
		print "closed file."
		openFile()

def lex(): 
	global lex_State
	global curr_Word

	char = curr_File.read(1)
	if (lex_State == 0):
		curr_Word = ""
		if ((char >= 'a' and char <= 'z') or (char >= 'A' and char <= 'Z')):
			lex_State = 1
		elif (char >= '0' and char <= '9'):
			lex_State = 2
		elif (char == '='):
			return id_CorrelationOperator[char], char
		elif (char in id_CorrelationOperator):	# "<" ">" "=" "<=" ">=" "<>"
			lex_State = 3
		elif (char == ':'):
			lex_State = 4
		elif (char in id_MathSymbol):	# "+" "-" "*" "/"
			return id_MathSymbol[char], char
		elif (char in id_Separator):	# ";" ","
			return id_Separator[char], char
		elif (char in id_GroupingSymbol):	# "{" "}" "(" ")" "[" "]"
			return id_GroupingSymbol[char], char
		elif (char == '\\' and curr_File.read(1) == '*'):
			lex_State = 5			
		elif (char == ''):
			return (id_EOF, char)
		elif not (char == '\n' or char == '\t' or char == ' ') :
			print "Error:", char, "its not supported from the system!"
			sys.exit()
	elif (lex_State == 1):	#Variable or Command
		if not ((char >= 'a' and char <= 'z') or 
				(char >= 'A' and char <= 'Z') or 
				(char >= '0' and char <= '9')):
			lex_State = 0
			curr_File.seek(-1, 1)
			if (curr_Word in sysCommands):
				return sysCommands[curr_Word], curr_Word
			return id_Variable, curr_Word
	elif (lex_State == 2):	#Nnumber
		if not (char >= '0' and char <= '9'):
			lex_State = 0
			curr_File.seek(-1, 1)
			if not (int(curr_Word) <= 32768):
				print "Error:", int(curr_Word), "out of bound!"
				sys.exit()
			return id_Number, int(curr_Word)
	elif (lex_State == 3): # "<" ">" "<=" ">=" "<>"
		lex_State = 0
		if ((curr_Word + char) in id_CorrelationOperator):
			return id_CorrelationOperator[curr_Word + char], (curr_Word + char)
		else : 
			curr_File.seek(-1,1)
			return id_CorrelationOperator[curr_Word], curr_Word
	elif (lex_State == 4): # ':=' assign or ':'
		lex_State = 0
		if (char == '='):
			return id_AssignmentOperator[curr_Word + char], (curr_Word + char)
		else :
			curr_File.seek(-1, 1)
			return id_AssignmentOperator[curr_Word], curr_Word
	#code for Comments
	elif (lex_State == 5): # '\*'
		if (char == '*'):
			lex_State = 6
		elif (char == ""):
			print "Error: EOF before comments end!"
			sys.exit()
	elif(lex_State == 6): # '\* *'
		if (char == '\\'): #done -> '\* *\'
			lex_State = 0
		elif (char == ""):
			print "Error: EOF before comments end!"
			sys.exit()
		elif (char == '*'):	# '\* *' and reading '*'
			lex_State = 6
		else : # '\**#' '#' = each char != '\'
			lex_State = 5
	#end code for Comments	
	if (lex_State == 2 and curr_Word != "" and int(curr_Word) == 0):	#convert number from 0x(to)x. e.x. 03->3
		curr_Word = ""	
	if not (len(curr_Word) > 29):
		curr_Word = curr_Word + char
	return lex()	#recursion loop

#============================================================
#		syntax code || PARSE code
#============================================================
def syntaxError(strError):
	print "SyntaxError:", strError ,"invalid syntax!"
	sys.exit()

#global id_Word, result_Word

def syntax():	#checking: EOF | <PROGRAM>
	global id_Word, result_Word
	
	id_Word, result_Word = lex()
	if (id_Word == 10):	#'EOF'
		print "empty file"
		sys.exit()
	syntaxProgram()	#<call PROGRAM>

def syntaxProgram():	#checking: program ID <BLOCK>.
	global id_Word, result_Word
	global curr_BlockName, curr_BlockID
	
	if not (id_Word == 1016):	#'program'
		syntaxError("'program' not found -> <PROGRAM>.")
	curr_BlockID = id_Word	#'program ID'
	id_Word, result_Word = lex()
	if not (id_Word == 0):	#'ID' Varibal: program name
		syntaxError("'Name' from program is missing -> <PROGRAM>.")
	
	curr_BlockName = result_Word	#beggin block program Name
	
	insertScope(curr_BlockName)	#['ID' [] ]
	
	id_Word, result_Word = lex()
	syntaxBlock()	#<call BLOCK>
	id_Word, result_Word = lex()
	if not (id_Word == id_EOF):	#EOF
		syntaxProgram()
	
	createQuadFile()	#met.int , file with quads
	
	writing_File = open((curr_File.name).split('.')[0] + ".c", 'w+')
	writing_File.write(strFileFor_C)
	writing_File.close()

	writing_File = open((curr_File.name).split('.')[0] + ".asm", 'w+')
	writing_File.write("j Lmain\n" + strFileFor_MIPS)
	writing_File.close()

def syntaxBlock():	#checking: { <DECLARATIONS> <SUBPROGRAMS> <SEQUENCE> }
	global id_Word, result_Word
	global findReturn
	global stackListScopes

	temp_BlockID = curr_BlockID
	temp_BlockName = curr_BlockName

	if not (id_Word == 60):	#'{'
		syntaxError("block start with '{' -> <BLOCK>.")
	id_Word, result_Word = lex()
	syntaxDeclarations()	#<call DECLARATIONS>
	syntaxSubPrograms()	#<call SUBPROGRAMS>

	genQuad("begin_block", temp_BlockName, "_", "_")	#begin_block, name, _, _
	findReturn = False	#reset count for each block.

	start_Quad = nextQuad()	#need it for update the stackListScope
	
	syntaxSequence()	#<call SEQUENCE>

	if (not findReturn and temp_BlockID == 1008):
		syntaxError("'return' is missing from Function -> <BLOCK>.")
	elif (findReturn and temp_BlockID != 1008):
		syntaxError("'return' is supported only from function -> <BLOCK>")
	if (temp_BlockID == 1016):	#'program'
		genQuad("halt", "_", "_", "_")	#halt, _, _, _
	genQuad("end_block", temp_BlockName, "_", "_")	#end_block, name, _, _

	offset = 12

	for i in range(len(stackListScopes[len(stackListScopes)-1][1])):
		if (len(stackListScopes[len(stackListScopes)-1][1][i]) < 5):
			offset += 4

	
	if (len(stackListScopes) > 1):
		stackListScopes[len(stackListScopes)-2][1][len(stackListScopes[len(stackListScopes)-2][1])-1][2] = start_Quad
		stackListScopes[len(stackListScopes)-2][1][len(stackListScopes[len(stackListScopes)-2][1])-1][3] = offset

	deleteScope()

	if not (id_Word == 61):
		syntaxError("block end with '}' -> <BLOCK>.")

def syntaxDeclarations():	#checking: e | 'declare' <VARLIST> 'enddeclare'
	global id_Word, result_Word

	if not (id_Word == 1002):	# 'declare'
		return
	id_Word, result_Word = lex()
	syntaxVarList()	#id_Word next of Variable
	if not (id_Word == 1005):	#'enddeclare'
		syntaxError("'enddeclare' is missing -> <DECLARATIONS>.")
	id_Word, result_Word = lex()

def syntaxVarList():	#checking: e | ID (, ID)*
	global id_Word, result_Word

	if not (id_Word == 0):	#ID Variable name
		return
	
	addVarInScope(result_Word)

	id_Word, result_Word = lex()
	while (id_Word == 51):	#Variable multi (, ID)*
		id_Word, result_Word = lex()
		if not (id_Word == 0):	#ID Variable name
			syntaxError("variable after ',' is missing -> <VARLIST>.")
		
		addVarInScope(result_Word)

		id_Word, result_Word = lex()
	return	#not Variable

def syntaxSubPrograms():	#checking: (<FUNC>)*
	global id_Word, result_Word
	global curr_BlockName, curr_BlockID
	
	while (id_Word == 1007 or id_Word == 1008):	#multi 'Function' or 'Procedure'
		
		curr_BlockID = id_Word

		if (id_Word == 1007): #'procedure'
			id_Word, result_Word = lex()
			if not (id_Word == 0):	#ID procedure name
				syntaxError("procedure 'name' not found -> <SUBPROGRAM>.")
		else :	#'function'
			id_Word, result_Word = lex()
			if not (id_Word == 0):	#ID function name
				syntaxError("function 'name' not found -> <SUBPROGRAM>.")

		curr_BlockName = result_Word

		addFunctionInScope(curr_BlockName, curr_BlockID)
		insertScope(curr_BlockName)

		syntaxFuncBody()	#<call FUNCBODY>
		id_Word, result_Word = lex()
		
def syntaxFuncBody():	#checking: <FORMALPARS> <BLOCK>
	global id_Word, result_Word
	
	id_Word, result_Word = lex()
	syntaxFormalPars()	#<call FORMALPARS>
	syntaxBlock()	#<call BLOCK>

def syntaxFormalPars():	#checking: ( e | <FORMALPARLIST> )
	global id_Word, result_Word
	
	if not (id_Word == 62): #'('
		syntaxError("'(' is missing after subprogram name -> <FORMALPARS>.")
	id_Word, result_Word = lex()
	if not (id_Word == 63):	#')'
		syntaxFormalParList()	#<call FORMALPARLIST>
		if not (id_Word == 63):	#')'
			syntaxError("')' is missing at end of -> <FORMALPARS>.")
	id_Word, result_Word = lex()

def syntaxFormalParList():	#checking: <FORMALPARITEM> (, <FORMALPARITEM>)*
	global id_Word, result_Word

	syntaxFormalParItem()	#<call FORMALPARITEM>
	id_Word, result_Word = lex()
	while (id_Word == 51):	#',' multi parametrs ( , <FORMALPARITEM>)*
		id_Word, result_Word = lex()
		syntaxFormalParItem()	#<call FORMALPARITEM>
		id_Word, result_Word = lex()

def syntaxFormalParItem():	#checking: in ID | inout ID
	global id_Word, result_Word

	if not (id_Word == 1012 or id_Word == 1013):	#'in' or 'inout'
		syntaxError("'in' or 'inout' is missing in subProgram -> <FORMALPARITEM>.")

	par_Mode = id_Word	#backUp 1012 (in) or 1013 (inout)

	id_Word, result_Word = lex()
	if not (id_Word == 0):	#ID Variable name
		syntaxError("Variable name from parametrs in subProgram missing -> <FORMALPARITEM>.")

	addParInScope(result_Word, par_Mode)	#e.x. [a, 12, 1012]
	
	
def syntaxSequence():	#checking <STATEMENTS> (; <STATEMENTS>)*
	global id_Word, result_Word

	syntaxStatement()	#<call STATEMENT>
	while (id_Word == 50):	# ';' multi <STATEMENTS>
		id_Word, result_Word = lex()
		syntaxStatement()	#<call STATEMENT>

def syntaxStatement():	#checking: all statements.
	global id_Word, result_Word	
	
	#if one of them return True stop finding
	if (syntaxAssignStat() or syntaxIfStat() or 
		syntaxDoWhileStat() or syntaxWhileStat() or 
		syntaxSelectStat() or syntaxExitStat() or
		syntaxReturnStat() or syntaxPrintStat() or 
		syntaxCallStat()):
		return

def syntaxAssignStat():	#checking: ID := <EXPRESSION>
	global id_Word, result_Word

	if not (id_Word == 0):	# 'ID' Varibal name
		return False
	
	varName = result_Word

	id_Word, result_Word = lex()
	if not (id_Word == 40):	# ':=' assign
		syntaxError("':=' is missing after Variable -> <ASSIGNMENT-STAT>.")
	id_Word, result_Word = lex()
	
	place_1 = syntaxExpression()	#<call EXPRESSION>
	genQuad(":=", place_1, "_", varName)
	
	return True

def syntaxExpression():	#checking: <OPTIONAL-SIGN> <TERM> (<ADD-OPER> <TERM>)*
	global id_Word, result_Word
	
	optionalSub = False
	if (id_Word == 21):	#'-'
		optionalSub = True

	syntaxOptionalSign()	#<call OPTIONAL-SIGN>
	place_1 = syntaxTerm()	#<call TERM>

	if (optionalSub):	# - <TERM>
		temp = newTemp()
		genQuad("-", 0, place_1, temp)	#'-' <TERM> place_1 = -place_1
		place_1 = temp

	while (syntaxAddOper()):	#<call ADD-OPER> multi (<ADD-OPER> <TERM>)*
	
		tempAddOper = result_Word	#backUp '+' '-'

		id_Word, result_Word = lex()
		place_2 = syntaxTerm()	#<TERM>
		
		temp = newTemp()
		genQuad(tempAddOper, place_1, place_2, temp)  #Operator, VarOrNumb, VarOrNumb2, Var
		place_1 = temp
		
	if (id_Word == 1 or id_Word == 62 or id_Word == 0):	#you cant stop with syntaxTerm
		syntaxError("you should use ';' between statement -> <EXPRESSION>.")

	return place_1

def syntaxTerm():	#checking: <FACTOR> (<MUL-OPER> <FACTOR>)*
	global id_Word, result_Word

	place_1 = syntaxFactor()	#<call FACTOR>
	
	while (syntaxMulOper()):	#<call MUL-OPER> multi (<MULL-OPER> <FACTOR>)*

		tempMulOper = result_Word	#backUp '*' '/'

		id_Word, result_Word = lex()
		place_2 = syntaxFactor()	#<call FACTOR>
		
		temp = newTemp()
		genQuad(tempMulOper, place_1, place_2, temp)	#Operator, VarOrNumb, VarOrNumb2, Var
		place_1 = temp

	return place_1

def syntaxFactor():	#checking: CONSANT | (<EXPRESSION>) | ID<IDTAIL>
	global id_Word, result_Word

	if (id_Word == 1):	#CONST

		place_1 = result_Word

		id_Word, result_Word = lex()
	elif (id_Word == 62):	#'('
		id_Word, result_Word = lex()
		place_1 = syntaxExpression()	#<call EXPRESSION>
		if not (id_Word == 63):	#')'
			syntaxError("')' is missing after EXPRESSION -> <FACTOR>.")
		id_Word, result_Word = lex()
	elif (id_Word == 0):	#'ID' Varibal name
		
		place_1 = result_Word	#

		id_Word, result_Word = lex()
		if (syntaxIDTail()):	#<call IDTAIL>
			temp = newTemp()
			genQuad("par",temp,"RET","_")
			genQuad("call","_","_",place_1)
			place_1 = temp
	else :
		syntaxError("Factor is missing -> <FACTOR>.")
	
	return place_1

def syntaxOptionalSign():	#checking: e | <ADD-OPER>
	global id_Word, result_Word

	if (syntaxAddOper()):	#<call ADD-OPER>
		id_Word, result_Word = lex()

def syntaxAddOper():	#checking: + | -
	global id_Word, result_Word

	if (id_Word == 20):	#'+'
		return True
	elif (id_Word == 21):	#'-'
		return True
	return False

def syntaxMulOper():	#checking: * | /
	global id_Word, result_Word

	if (id_Word == 22):	# '*'
		return True
	elif (id_Word == 23):	# '/'
		return True
	return False

def syntaxIDTail():	#checking: e | <IDTAIL>
	global id_Word, result_Word

	return syntaxActualPars()	#<call ACTUALPARS>
		

def syntaxActualPars():	#checking: ( e | <ACTUALPARLIST>)
	global id_Word, result_Word
	
	if not (id_Word == 62):	#'('
		return False
	id_Word, result_Word = lex()
	if not (id_Word == 63):	#')'
		syntaxActualParList()	#<call ACTUALPARLIST>
		if not (id_Word == 63):	#')'
			syntaxError("')' is missing after <ACTUALPARLIST> -> <ACTUALPARS>.")
	id_Word, result_Word = lex()
	return True
	

	
def syntaxActualParList():	#checking: <ACTUALPARITEM> (, <ACTUALPARITEM>)*
	global id_Word, result_Word

	syntaxActualParItem()	#<call ACTUALPARITEM>
	while (id_Word == 51):	#, multi parrametr items	(, <ACTUALPARITEM>)*
		id_Word, result_Word = lex()
		syntaxActualParItem()	#<call ACTUALPARITEM>
	
 
def syntaxActualParItem():	#checking: in <EXPRESSION> | inout ID
	global id_Word, result_Word

	if (id_Word == 1012):	#'in'
		id_Word, result_Word = lex()

		place_1 = syntaxExpression()	#<call EXPRESSION>

		genQuad("par", place_1, "CV", "_")

	elif (id_Word == 1013):	#'inout'
		id_Word, result_Word = lex()
		if not (id_Word == 0):	#ID Variable name
			syntaxError("not Variable after 'inout' -> <ACTUALPARITEM.")

		genQuad("par", result_Word, "REF", "_")

		id_Word, result_Word = lex()
	else :
		syntaxError("'in' or 'inout' is missing -> <ACTUALPARITEM>.")

def syntaxIfStat():	#if (<CONDITION>) <BRACK-OR-STAT> <ELSEPART>
	global id_Word, result_Word
	
	if not (id_Word == 1011):	#'if'
		return False
	id_Word, result_Word = lex()
	if not (id_Word == 62):	# '('
		syntaxError("'(' after if is missing -> <IF-STAT>.")
	id_Word, result_Word = lex()
	listTrue_1, listFalse_1 = syntaxCondition()	#<call CONDITION>
	if not (id_Word == 63):	#')'
		syntaxError("')' after condition in if -> <IF-STAT>.")
	id_Word, result_Word = lex()

	backPatch(listTrue_1, nextQuad())	#if (cond == TRUE)

	syntaxBrackOrStat()	#<call BRACK-OR-STAT>
	
	end_IfStatment = makeList(nextQuad())
	genQuad("jump", "_", "_", "_")	
	
	backPatch(listFalse_1, nextQuad())	#if (cond == FALSE)

	syntaxElsePart()	#<call ELSEPART>

	backPatch(end_IfStatment, nextQuad())


def syntaxCondition():	#checking: <BOOLTERM> (or <BOOLTERM>)*
	global id_Word, result_Word

	listTrue_1, listFalse_1 = syntaxBoolTerm()	#<BOOLTERM>
	while (id_Word == 1017):	#'or' multi (or <BOOLTERM>)*
		id_Word, result_Word = lex()

		backPatch(listFalse_1, nextQuad())	#second 'OR' cond here. cond1=false goto cond2

		listTrue_2, listFalse_2 = syntaxBoolTerm()	#<call BOOLTERM>

		listFalse_1 = listFalse_2	#done with listFalse_1.
		merge(listTrue_1, listTrue_2)	#i need both lists so merge in listTrue_1.

	return listTrue_1, listFalse_1
	

def syntaxBoolTerm():	#checking: <BOOLFACTOR> (and <BOOLFACTOR>)*
	global id_Word, result_Word

	listTrue_1, listFalse_1 = syntaxBoolFactor()	#<call BOOLFACTOR>
	while (id_Word == 1001):	#'and' multi (and <BOOLFACTOR>)*
		id_Word, result_Word = lex()

		backPatch(listTrue_1, nextQuad())	#second 'AND' cond here. cond1=True got cond2

		listTrue_2, listFalse_2 = syntaxBoolFactor()	#<call BOOLFACTOR>

		listTrue_1 = listTrue_2	#i dont know yet where stop.
		merge(listFalse_1, listFalse_2)	#merge and continue search for end if.

	return listTrue_1, listFalse_1

def syntaxBoolFactor():	#checking: not [<CONDITION>] | [<CONDITION>] | <EXPRESSION> <RELATIONAL-OPER> <EXPRESSION>
	global id_Word, result_Word
	flag_Condition = False
	
	if (id_Word == 1014):	#'not'
		id_Word, result_Word = lex()
		flag_Condition = True	#need '['
	if (id_Word == 64):	# '['
		id_Word, result_Word = lex()
		listTrue_1, listFalse_1 = syntaxCondition()	#<call CONDITION>
		if not (id_Word == 65):	#']'
			syntaxError("']' is missing after condition -> <BOOLFACTOR>.")
		id_Word, result_Word = lex()
	else :
		if (flag_Condition == True):
			syntaxError("'[' is missing after not -> <BOOLFACTOR>.")
		place_1 = syntaxExpression()	#<call EXPRESSION>
		
		temp_RelationOper = result_Word
		
		if not (syntaxRelationOper()):	#<call RELATION-OPER>
			syntaxError("Ralation Oper missing -> <BOOLFACTOR>.")
		id_Word, result_Word = lex()
		
		place_2 = syntaxExpression()	#<call EXPRESSION>
		listTrue_1 = makeList(nextQuad())	#hold the numberQuad for nextQuad.need it->backPatch
		genQuad(temp_RelationOper, place_1, place_2, "_")
		listFalse_1 = makeList(nextQuad())	#hold the numberQuad for nextQuad.need it-> backPatch
		genQuad("jump", "_", "_", "_")
	
	if (flag_Condition):	#not[True] not[False]
		return listFalse_1, listTrue_1
	return listTrue_1, listFalse_1

def syntaxRelationOper():	#checking: = | < | <= | <> | >= | >
	global id_Word, result_Word

	if (id_Word == 32):	#'='
		return True
	elif (id_Word == 30):	#'<'
		return True
	elif (id_Word == 33):	#'<='
		return True
	elif (id_Word == 35):	#'<>'
		return True
	elif (id_Word == 34):	#'>='
		return True
	elif (id_Word == 31):	#'>'
		return True
	return False

def syntaxBrackOrStat():	#checking: <BRACKET-SEQ> | <STATEMENT>;
	global id_Word, result_Word

	if not (syntaxBracketSeq()):	#<call BRACKET-SEQ>
		syntaxStatement()	#<call STATEMENT>
		if not (id_Word == 50):	# ';'
			syntaxError("';' after STAMENT is missing -> <BRACKET-OR-STAT>.")
		id_Word, result_Word = lex()

def syntaxBracketSeq():	#checking: { <SEQUENCE> }
	global id_Word, result_Word

	if not (id_Word == 60):	#'{'
		return False
	id_Word, result_Word = lex()
	syntaxSequence()	#<call SEQUENCE>
	if not (id_Word == 61):	#'}'
		syntaxError("'}' is missing -> <BRACKET-SEQ>.")
	id_Word, result_Word = lex()
	return True

def syntaxElsePart():	#checking: e | else <BRACK-OR-STAT>
	global id_Word, result_Word

	if not (id_Word == 1004):	#'else'
		return
	id_Word, result_Word = lex()
	syntaxBrackOrStat()	#<call BRACK-OR-STAT>

def syntaxDoWhileStat():	#checking: do <BRACK-OR-STAT> while ( <CONDITION> )
	global id_Word, result_Word
	global doOrNot

	if not (id_Word == 1003):	#'do'
		return False

	doOrNot.append([True, []])	#i am do i am wait exit.

	id_Word, result_Word = lex()
	
	temp_NextQuad = nextQuad()	#point of first do-While command.

	syntaxBrackOrStat()	#<call BRACK-OR-STAT>
	if not (id_Word == 1019):	#'while'
		syntaxError("'while' is missing after do <DO-WHILE-STAT>.")
	id_Word, result_Word = lex()
	if not (id_Word == 62):	#'('
		syntaxError("'(' is missing after while -> <DO-WHILE-STAT>.")
	id_Word, result_Word = lex()
	listTrue_1, listFalse_1 = syntaxCondition()	#<call CONDITION>
	if not (id_Word == 63):	#')'
		syntaxError("')' is missing after condition -> <DO-WHILE-STAT>.")
	
	backPatch(listTrue_1, temp_NextQuad)	#if do-While(cond == true) goto-> point of first do-While.
	backPatch(listFalse_1, nextQuad())	#if do-While(cod == false) goto outside do-While(cond).


	backPatch(doOrNot[len(doOrNot) -1][1], nextQuad())	#all each exit go here.

	doOrNot.pop()	#delete the last line.

	id_Word, result_Word = lex()
	return True

def	syntaxWhileStat():	#checking: while ( <CONDITION> ) <BRACK-OR-STAT>
	global id_Word, result_Word
	global doOrNot

	if not (id_Word == 1019):	#'while'
		return False

	doOrNot.append([False, []])	#i dont search for 'exit'

	jump_StartWhile = nextQuad()	#point of first while_Condition.

	id_Word, result_Word = lex()
	if not (id_Word == 62):	#'('
		syntaxError("'(' is missing after while -> <WHILE-STAT>.")
	id_Word, result_Word = lex()
	listTrue_1, listFalse_1 = syntaxCondition()	#<call CONDITION>
	if not (id_Word == 63):	#')'
		syntaxError("')' is missing after <CONDITION> -> <WHILE-STAT>.")
	id_Word, result_Word = lex()

	backPatch(listTrue_1, nextQuad())	

	syntaxBrackOrStat()	#<call BRACK-OR-STAT>

	genQuad("jump", "_", "_", jump_StartWhile)	#goTo while(condition)
	backPatch(listFalse_1, nextQuad())	

	doOrNot.pop()	#pop. search for next.

	return True

def syntaxSelectStat():	# select ('ID') (const: <BRACK-OR-STAT>)* DEFAULT: <BRACK-OR-STAT>
	global id_Word, result_Word
	
	if not (id_Word == 1015):	#'select'
		return False
	id_Word, result_Word = lex()
	if not (id_Word == 62):	#'('
		syntaxError("'(' is missing after select -> <SELECT-STAT>.")
	id_Word, result_Word = lex()

	backup_Value = result_Word	#hold this value.

	if not (id_Word == 0):	#'ID' Variable name
		syntaxError("'Variable name' is missing after '(' -> <SELECT-STAT>.")
	id_Word, result_Word = lex()
	if not (id_Word == 63):	#')'
		syntaxError("')' is missing after Variable name -> <SELECET-STAT>.")
	id_Word, result_Word = lex()

	jump_OutSelect = emptyList()	#each select need jump to out if find.
	
	select_Number = 1	#start 1

	while (id_Word == 1):	#'Const' (const : <BRACK-OR-STAT)*

		if (result_Word != select_Number): # 1: 2: increased + 1.
			syntaxError("CONST increase by '+1' each time. Start with '1:'. -> <SELECT-STAT>.")
		select_Number += 1	#increase +1

		trueList_1 = makeList(nextQuad())	#select found.
		genQuad("=", backup_Value, result_Word, "")
		falseList_1 = makeList(nextQuad())	#select not found.
		genQuad("jump", "_", "_", "_")
		backPatch(trueList_1,nextQuad())	#complete the select found.

		id_Word, result_Word = lex()
		if not (id_Word == 41):	#':'
			syntaxError("':' after CONST is missing -> <SELECT-STAT>.")
		id_Word, result_Word = lex()
		syntaxBrackOrStat()	#<call BRACK-OR-STAT>

		merge(jump_OutSelect, makeList(nextQuad()))	#when find select = True go out.
		genQuad("jump", "_", "_", "_")	#jump out select complete
		backPatch(falseList_1, nextQuad())	#go next select not found.

	if not (id_Word == 1020):	#'default' 
		syntaxError("'default' is missing -> <SELECT-STAT>.")
	id_Word, result_Word = lex()
	if not (id_Word == 41):	#':'
		syntaxError("':' is missing after default -> <SELECT-STAT>.")
	id_Word, result_Word = lex()
	syntaxBrackOrStat()	#<call BRACK-OR-STAT>

	backPatch(jump_OutSelect, nextQuad())	#out select

	return True

def syntaxExitStat():	#checking: exit
	global id_Word, result_Word
	
	if not (id_Word == 1006):	#'exit'
		return False

	if (len(doOrNot) == 0 or doOrNot[len(doOrNot) -1][0] != True):
		syntaxError("'exit' is only for Do-While -> <EXIT-STAT>.")

	doOrNot[len(doOrNot) -1][1].append(nextQuad())	#doOrNot, hold the returns.
	genQuad("jump", "_", "_", "_")

	id_Word, result_Word = lex()
	return True
	
def syntaxReturnStat():	#checking: return ( <EXPRESSION> )
	global id_Word, result_Word
	global findReturn

	if not (id_Word == 1018):	#'return'
		return False
	id_Word, result_Word = lex()
	if not (id_Word == 62):	#'('
		syntaxError("'(' is missing after return -> <RETURN-STAT>.")
	id_Word, result_Word = lex()
	place_1 = syntaxExpression()	#<call EXPRESSION>

	genQuad("ret", "_", "_", place_1)

	if not (id_Word == 63):	#')'
		syntaxError("')' is missing after <EXPRESION> -> <RETURN-STAT>.")
	id_Word, result_Word = lex()

	findReturn = True

	return True

def syntaxPrintStat():	#checking: print ( <EXPRESSION> )
	global id_Word, result_Word

	if not (id_Word == 1009):	#'print'
		return False
	id_Word, result_Word = lex()
	if not (id_Word == 62):	#'('
		syntaxError("'(' is missing after print -> <PRINT-STAT>.")
	id_Word, result_Word = lex()
	place_1 = syntaxExpression()	#<call EXPRESSION>
	
	genQuad("out", "_", "_", place_1)	#print(E) {P2}

	if not (id_Word == 63):	#')'
		syntaxError("')' is missing after <EXPRESSION> -> <PRINT-STAT>.")
	id_Word, result_Word = lex()
	return True

def syntaxCallStat():	#checking: call ID <ACTUALPARS>
	global id_Word, result_Word

	if not (id_Word == 1010):	#'call'
		return False
	id_Word, result_Word = lex()
	if not (id_Word == 0):	#'ID' Variable name
		syntaxError("Variable name not found after call -> <CALL-STAT>.")

	call_Name = result_Word

	id_Word, result_Word = lex()
	if not (syntaxActualPars()):	#<call ACTUALPARS>
		syntaxError("'(' is missing after Variable name -> <CALL-STAT>.")

	genQuad("call", "_", "_", call_Name)
	
	return True
		
#============================================================
#			Intermediate Code Generation
#============================================================

#help methods
def nextQuad():	#return the number of next quad.
	global numberOfNextQuad
	
	return	numberOfNextQuad

def newTemp():	#create a new name for temp variable.
	global counterTemp
	
	counterTemp += 1
	var_Name = "T_%d" % counterTemp
	addVarInScope(var_Name)
	return var_Name

def genQuad(op, x, y, z):	#generate one quad and add it in the end of the list.
	global hashMapCommands
	global rateOfChange, numberOfNextQuad

	listValue = [op, x, y, z]
	hashMapCommands[nextQuad()] = listValue
	numberOfNextQuad += rateOfChange

def emptyList():
	tempList = []
	return tempList

def makeList(x):

	tempList = []
	tempList.append(x)
	return tempList

def merge(list_1, list_2):	#merge list_1 with list_2.
	
	list_1 += list_2

def backPatch(list_1, z):	#search from list_1 in the HashMapCommands and ivnite Z
	global hashMapCommands

	for i in range(len(list_1)):
		hashMapCommands[list_1[i]][3] = z

def createQuadFile():

	writing_File = open((curr_File.name).split('.')[0] + ".int", 'w+')
	endHashMapCommands = len(hashMapCommands) * rateOfChange + 100
	for key in range(100, endHashMapCommands, rateOfChange):
		writing_File.write("["+str(key)+"]" + " : " + str (hashMapCommands[key]) + "\n")
	writing_File.close()
	
#============================================================
#			Symbol Code Generation
#============================================================

def insertScope(scope_Name):
	global stackListScopes

	stackListScopes.append([scope_Name, []])	# e.x. ['ID', [ [a,12], [b,16], [c,20], [f, id_ProcedureOrFunction, firstQuad_F, offset+(vars*4), [id_IN, id_OUT]] ] ]

def deleteScope():
	global stackListScopes

	for i in range(len(stackListScopes)):	#helper printer the table symbol
		print stackListScopes[i]
	print "\n\n"		

	if (len(stackListScopes) == 1):
		createCodeC()	#create met.c, file with commads in C
	
	commandMIPS()

	stackListScopes.pop()	#delete the last element

def addVarInScope(var_Name):
	global stackListScopes

	dublicateEntity(var_Name)

	offset = 12

	for i in range(len(stackListScopes[len(stackListScopes)-1][1])):
		if (len(stackListScopes[len(stackListScopes)-1][1][i]) < 5):
			offset += 4
	
	stackListScopes[len(stackListScopes) -1][1].append([var_Name, offset])
	
def addParInScope(var_Name, par_Mode):
	global stackListScopes
	
	offset = 12

	for i in range(len(stackListScopes[len(stackListScopes)-1][1])):
		if (len(stackListScopes[len(stackListScopes)-1][1][i]) < 5):
			offset += 4

	stackListScopes[len(stackListScopes)-1][1].append([var_Name, offset, par_Mode])	#need to update me
	stackListScopes[len(stackListScopes)-2][1][len(stackListScopes[len(stackListScopes)-2][1])-1][4].append(par_Mode)	#update my father i have got pars

def addFunctionInScope(function_Name, function_Mode):
	global stackListScopes

	dublicateEntity(function_Name)

	stackListScopes[len(stackListScopes)-1][1].append([function_Name, function_Mode, 0, 0, []])	

def searchInScope(entity):	#seach entity x
	global stackListScopes

	for i in range(len(stackListScopes)-1, -1 , -1):	#all scopes from the last to start
		for j in range(len(stackListScopes[i][1])):	#each scope[i][1] type
			if (stackListScopes[i][1][j][0] == entity):	#for all scopes in the second list all entitys the first element is the name char[] of entity.
				return i, stackListScopes[i][1][j]	#return all the entity.

	syntaxError("'entity' Variable -> (%s) its not declare -> <searchInScope>." % entity)

def dublicateEntity(entity):

	for j in range(len(stackListScopes[len(stackListScopes) -1][1])):
		if (stackListScopes[len(stackListScopes) -1][1][j][0] == entity):
			syntaxError("'entity' (%s) dublicate -> <searchInScope>. " % entity)


#============================================================
#			convert to C Code Generation
#============================================================
def createCodeC():
	global 	strFileFor_C

	strFileFor_C = ""
	#writing_File = open((curr_File.name).split('.')[0] + ".c", 'w+')
	strFileFor_C += "#include <stdio.h>\n\n"
	strFileFor_C += "int main()\n"
	strFileFor_C += "{\n"

	strVars = ""
	for i in range(len(stackListScopes[0][1])):
		if (len(stackListScopes[0][1][i]) == 2):
			strVars += stackListScopes[0][1][i][0] + ", "

	strVars = strVars[:-2] + ";"
	strFileFor_C += "\tint %s\n" % strVars
	

	endHashMapCommands = len(hashMapCommands) * rateOfChange + 100
	for key in range(100, endHashMapCommands, rateOfChange):	#loop1: find the begin PROGRAM
		if (stackListScopes[0][0] == hashMapCommands[key][1] and hashMapCommands[key][0] == "begin_block"):
			for programKey in range(key, endHashMapCommands, rateOfChange):	#loop2: while isnot the end of PROGRAM do this
				if (stackListScopes[0][0] == hashMapCommands[programKey][1] and hashMapCommands[programKey][0] == "end_block"):
					break	#break the loop2. find end_block
				strFileFor_C += "\tL_%d:" % ((programKey - key) / rateOfChange)
				if (hashMapCommands[programKey][0] == "halt"):
					strFileFor_C += " {}"
				elif (hashMapCommands[programKey][0] == "jump"):	#goto
					strFileFor_C += " goto L_%d" % ((hashMapCommands[programKey][3] - key) / rateOfChange)
					strFileFor_C += ";"
				elif (hashMapCommands[programKey][0] in id_MathSymbol):	# ['+' , x, y, d]
					
					searchInScope(hashMapCommands[programKey][3])
					if (type(hashMapCommands[programKey][1]) == str):
						searchInScope(hashMapCommands[programKey][1])
					if (type(hashMapCommands[programKey][2]) == str):
						searchInScope(hashMapCommands[programKey][2])

					strFileFor_C += " " + hashMapCommands[programKey][3] + " = " + str(hashMapCommands[programKey][1]) + " " + hashMapCommands[programKey][0] + " " + str(hashMapCommands[programKey][2])
					strFileFor_C += ";"
				elif (hashMapCommands[programKey][0] in id_CorrelationOperator):	#['>', a, b, 220]

					if (type(hashMapCommands[programKey][1]) == str):
						searchInScope(hashMapCommands[programKey][1])
					if (type(hashMapCommands[programKey][2]) == str):
						searchInScope(hashMapCommands[programKey][2])

					if (hashMapCommands[programKey][0] in converterFor_C):	#deference on CorrelationOperator between Python and C
						strFileFor_C += " if(" + str(hashMapCommands[programKey][1]) + " " + converterFor_C[hashMapCommands[programKey][0]] + " " + str(hashMapCommands[programKey][2]) + ")"
						strFileFor_C += " goto L_%d" % ((hashMapCommands[programKey][3] - key) / rateOfChange)
					else :
						strFileFor_C += " if(" + str(hashMapCommands[programKey][1]) + " " + hashMapCommands[programKey][0] + " " + str(hashMapCommands[programKey][2]) + ")"
						strFileFor_C += " goto L_%d" % ((hashMapCommands[programKey][3] - key) / rateOfChange)
					strFileFor_C += ";"
				elif (hashMapCommands[programKey][0] == ":="):	#assign python is dif in C

					searchInScope(hashMapCommands[programKey][3])
					if (type(hashMapCommands[programKey][1]) == str):
						searchInScope(hashMapCommands[programKey][1])

					strFileFor_C += " " + hashMapCommands[programKey][3] + " " + converterFor_C[hashMapCommands[programKey][0]] + " " + str(hashMapCommands[programKey][1])
					strFileFor_C += ";"
				elif (hashMapCommands[programKey][0] == "out"):	#printF

					if (type(hashMapCommands[programKey][3]) == str):
						searchInScope(hashMapCommands[programKey][3])

					#if (type(hashMapCommands[programKey][1]) == int or type(hashMapCommands[programKey][1]) == float):
					strFileFor_C += ' printf("%d", ' +  str(hashMapCommands[programKey][3]) + ')'
					strFileFor_C += ";"
				strFileFor_C += "\n"
			break	#break loop1.
	
	strFileFor_C += "}\n"
	#writing_File.close()


#============================================================
#			MIPS Code Generation
#============================================================

def gnvlcode(v):
	global strFileFor_MIPS

	strFileFor_MIPS += "\tlw $t0, -4($sp)\n" 
	nestingLevel, entity = searchInScope(v)

	for i in range(len(stackListScopes) -2, nestingLevel, -1):
		strFileFor_MIPS += "\tlw $t0, -4($t0)\n"

	strFileFor_MIPS += "\tadd $t0,$t0,-%d\n" % entity[1]


def loadvr(v, r):
	global strFileFor_MIPS

	if (type(v) == int):
		strFileFor_MIPS += "\tli $t%d, %d\n"	%  (r, v)	#Const
	elif (type(v) == str):
		nestingLevel, entity = searchInScope(v)
		
		if (nestingLevel == 0):
			strFileFor_MIPS += "\tlw $t%d,-%d($s0)\n" % (r, entity[1])	#global Var.
		else:
			if (nestingLevel == len(stackListScopes) -1):	#curr nestingLevel
				if (len(entity) == 2 or (len(entity) == 3 and entity[2] == 1012)):
					strFileFor_MIPS += "\tlw $t%d,-%d($sp)\n" % (r, entity[1])
				if (len(entity) == 3 and entity[2] == 1013):
					strFileFor_MIPS += "\tlw $t0,-%d($sp)\n" % entity[1]
					strFileFor_MIPS += "\tlw $t%d,($t0)\n" % r
			else:	#insider nestingLevel
				if (len(entity) == 2 or (len(entity) == 3 and entity[2] == 1012)):
					gnlvcode(v)
					strFileFor_MIPS += "\tlw $t%d,($t0)\n" % r
				if (len(entity) == 3 and entity[2] == 1013):
					gnlvcode(v)
					strFileFor_MIPS += "\tlw $t0,($t0)\n" 
					strFileFor_MIPS += "\tlw $t%d,($t0)\n" % r
			
def storerv(r, v):
	global strFileFor_MIPS

	if (type(v) == str):
		nestingLevel, entity = searchInScope(v)

		if (nestingLevel == 0):	#global var
			strFileFor_MIPS += "\tsw $t%d,-%d($s0)\n" % (r, entity[1])
		else:
			if (nestingLevel == len(stackListScopes) -1):	#curr nestingLevel
				if (len(entity) == 2 or (len(entity) == 3 and entity[2] == 1012)):
					strFileFor_MIPS += "\tsw $t%d,-%d($sp)\n" % (r, entity[1])
				if (len(entity) == 3 and entity[2] == 1013):
					strFileFor_MIPS += "\tlw $t0,-%d($sp)\n" % entity[1]
					strFileFor_MIPS += "\tsw $t%d,($t0)\n" % r
			else:	#insider nestingLevel
				if (len(entity) == 2 or (len(entity) == 3 and entity[2] == 1012)):
					gnlvcode(v)
					strFileFor_MIPS += "\tsw $t%d,($t0)\n" % r
				if (len(entity) == 3 and entity[2] == 1013):
					gnlvcode(v)
					strFileFor_MIPS += "\tlw $t0,($t0)\n"
					strFileFor_MIPS += "\tsw $t%d,($t0)\n" % r

def commandMIPS():
	global strFileFor_MIPS

	endHashMapCommands = len(hashMapCommands) * rateOfChange + 100
	for key in range(100, endHashMapCommands, rateOfChange):	#loop1: find the begin_block
		if (stackListScopes[len(stackListScopes)-1][0] == hashMapCommands[key][1] and hashMapCommands[key][0] == "begin_block"):
			programKey = key
			#for programKey in range(key, endHashMapCommands, rateOfChange):	#loop2: while isnot the end of PROGRAM do this
			while (programKey < endHashMapCommands):
				if (hashMapCommands[programKey][0] == "jump"):
					strFileFor_MIPS += "L%d:\tj L%d\n" % (((programKey - 100) / rateOfChange), ((hashMapCommands[programKey][3] - 100) / rateOfChange))
				elif (hashMapCommands[programKey][0] in id_CorrelationOperator):
					strFileFor_MIPS += "L%d:" % ((programKey - 100) / rateOfChange)
					loadvr(hashMapCommands[programKey][1], 1)
					loadvr(hashMapCommands[programKey][2], 2)
					strFileFor_MIPS += "\t%s,$t1,$t2,L%d\n" % ((converterFor_MIPS[hashMapCommands[programKey][0]]), ((hashMapCommands[programKey][3] - 100) / rateOfChange))
				elif (hashMapCommands[programKey][0] == ':='):
					strFileFor_MIPS += "L%d:" % ((programKey - 100) / rateOfChange)
					loadvr(hashMapCommands[programKey][1], 1)
					storerv(1, hashMapCommands[programKey][3])
				elif (hashMapCommands[programKey][0] in id_MathSymbol):
					strFileFor_MIPS += "L%d:" % ((programKey - 100) / rateOfChange)
					loadvr(hashMapCommands[programKey][1], 1)
					loadvr(hashMapCommands[programKey][2], 2)
					strFileFor_MIPS += "\t%s,$t1,$t1,$t2\n" % (converterFor_MIPS[hashMapCommands[programKey][0]])
					storerv(1, hashMapCommands[programKey][3])
				elif (hashMapCommands[programKey][0] == "out"):
					strFileFor_MIPS += "L%d:\tli $v0,1\n" % ((programKey - 100) / rateOfChange)
					strFileFor_MIPS += "\tli $a0,%s\n" % hashMapCommands[programKey][3]
					strFileFor_MIPS += "\tsyscall\n"
				elif (hashMapCommands[programKey][0] == "in"):
					strFileFor_MIPS += "L%d:\tli $v0,5\n" % ((programKey - 100) / rateOfChange)
					strFileFor_MIPS += "\tsyscall\n"
				elif (hashMapCommands[programKey][0] == "ret"):
					strFileFor_MIPS += "L%d:" % ((programKey - 100) / rateOfChange)
					loadvr(hashMapCommands[programKey][3], 1)
					strFileFor_MIPS += "\tlw $t0,-8($sp)\n"
					strFileFor_MIPS += "\tsw $t1,($t0)\n"
				elif (hashMapCommands[programKey][0] == "par"):
					call_Name = ""
					for searchCall in range(programKey, endHashMapCommands, rateOfChange):
						if (hashMapCommands[searchCall][0] == "call"):
							call_Name = hashMapCommands[searchCall][3]
							endPars = searchCall
							break
					nestingLevel, entity = searchInScope(call_Name)
					strFileFor_MIPS += "L%d:\tadd $fp,$sp,%d\n" % (((programKey - 100) / rateOfChange), entity[3])
					if (hashMapCommands[endPars - rateOfChange][2] == "RET"):
						if not (len(entity[4]) == ((endPars - programKey) / rateOfChange)-1):
							syntaxError("'" + entity[0] + "' required %d arguments!" % len(entity[4]))
					elif not (len(entity[4]) == (endPars - programKey) / rateOfChange):
						syntaxError("'" + entity[0] + "' required %d arguments!" % len(entity[4]))
					for startPars in range(programKey, endPars, rateOfChange):
						nestingLevel1, entity1 = searchInScope(hashMapCommands[startPars][1])
						if (hashMapCommands[startPars][2] == "CV"):
							if not (entity[4][(startPars - programKey) / rateOfChange] == 1012):
								syntaxError("'" + entity[0] + "' required inout as > %d < argument!" % ((startPars - programKey) / rateOfChange)+1)
							loadvr(hashMapCommands[startPars][1], 0)
							strFileFor_MIPS += "\tsw $t0,-%d($fp)\n" % (12 + (4 * ((startPars - programKey) / rateOfChange)))
						elif (hashMapCommands[startPars][2] == "REF"):
							searchInScope(hashMapCommands[startPars][1])
							if not (entity[4][(startPars - programKey) / rateOfChange] == 1013):
								syntaxError("'" + entity[0] + "' required in as > %d < argument!" % ((startPars - programKey) / rateOfChange)+1)
							if (nestingLevel1 == len(stackListScopes) -1):
								if (len(entity1) == 2 or (len(entity1) == 3 and entity[2] == 1012)):	#var or in
									strFileFor_MIPS += "\tadd $t0,$sp,-%d\n" % entity1[1]
									strFileFor_MIPS += "\tsw $t0,-%d($fp)\n" % (12 + (4 * ((startPars - programKey) / rateOfChange)))
								elif (len(entity1) == 3 and entity1[2] == 1013):	#inout
									strFileFor_MIPS += "\tlw $t0,-%d($sp)\n" % entity1[1]
									strFileFor_MIPS += "\tsw $t0,-%d($fp)\n" % (12 + (4 * ((startPars - programKey) / rateOfChange)))
							else:
								if (len(entity1) == 2 or (len(entity1) == 3 and entity1[2] == 1012)):	#var or in
									gnlvcode(hashMapCommands[startPars][1])
									strFileFor_MIPS += "\tsw $t0,-%d($fp)\n" % (12 + (4 * ((startPars - programKey) / rateOfChange)))
								elif (len(entity1) == 3 and entity1[2] == 1013):	#inout
									gnlvcode(hashMapCommands[startPars][1])
									strFileFor_MIPS += "\tsw $t0,$t0\n"
									strFileFor_MIPS += "\tsw $t0,-%d($fp)\n" % (12 + (4 * ((startPars - programKey) / rateOfChange)))
						elif (hashMapCommands[startPars][2] == "RET"):
							strFileFor_MIPS += "\tadd $t0,$sp,-%d\n" % entity1[1]
							strFileFor_MIPS += "\tsw $t0,-8($fp)\n"
					programKey = endPars - rateOfChange
				elif (hashMapCommands[programKey][0] == "call"):
					nestingLevel, entity = searchInScope(hashMapCommands[searchCall][3])
					if (nestingLevel == len(stackListScopes)-1):
						strFileFor_MIPS += "L%d:\tlw $t0,-4($sp)\n" % ((programKey - 100) / rateOfChange)
						strFileFor_MIPS += "\tsw $t0,-4($fp)\n"
					else:
						strFileFor_MIPS += "L%d:\tsw $sp,-4($fp)\n" % ((programKey - 100) / rateOfChange)
					strFileFor_MIPS += "\tadd $sp,$sp,%d\n" % entity[3]
					strFileFor_MIPS += "\tjal L%s\n" % (((entity[2] - 100) / rateOfChange)-1)
					strFileFor_MIPS += "\tadd $sp,$sp,-%d\n" % entity[3]
				elif (hashMapCommands[programKey][0] == "begin_block"):
					if (len(stackListScopes) == 1):	#program
						strFileFor_MIPS += "Lmain:"
						offset = 12
						for i in range(len(stackListScopes[len(stackListScopes)-1][1])):
							if (len(stackListScopes[len(stackListScopes)-1][1][i]) < 5):
								offset += 4
						strFileFor_MIPS += "\tadd $sp,$sp,%d\n" % offset
						strFileFor_MIPS += "\tmove $s0,$sp\n"
					else:	#function or procedure
						strFileFor_MIPS += "L%d:\tsw $ra,($sp)\n" % ((programKey - 100) / rateOfChange)
				elif (hashMapCommands[programKey][0] == "end_block"):
					strFileFor_MIPS += "L%d:\tlw $ra,($sp)\n" % ((programKey - 100) / rateOfChange)
					strFileFor_MIPS += "\tjr $ra\n"
					break	#break the loop2. find end_block
				elif (hashMapCommands[programKey][0] == "halt" and len(stackListScopes) == 1):
					return
				else:
					strFileFor_MIPS += "L%d:\n" % ((programKey - 100) / rateOfChange)
				programKey += rateOfChange
			break


	


openFile()	#<call openFile> to START.
