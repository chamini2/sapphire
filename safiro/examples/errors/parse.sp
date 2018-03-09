a : Int

#   TypeIdInsteadOfVarId
a = A + 1

#   VarIdInsteadOfTypeId
b : int

#   VariableListComma  
a b : Char

#   FieldListComma    
record A as
    a : Int
    b : Char
end

#   ParameterListComma 
def func : Int a Char b -> Int 
    return a
end

#   AssignmentMissingExpression
a = 

#   AssignmentMissingAccess
= a + 1

#   VariableDefinitionWithoutDataType 
d :

#   TypeDefinitionIdentifier         
union as
    a : Int
    b : Bool
end

#   ArraySize                         
arr : [-1]Int

#   ArrayDataTypeSize                
arr : [a]Int

#   VariableDefinitionMissingColon   
#a Int

#   NoFieldsInType                   
record B as end

#   FunctionDefinitionIdentifier
def : Char c -> Char 
    return c
end

#   EmptyReturn                  -> "return statement must have an expression"
def g : Char c -> Char 
    return 
end

#   NoWhensInCase -> 
caseVar : Int
case caseVar
end
