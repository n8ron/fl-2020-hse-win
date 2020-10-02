import ply.yacc as yacc 

from lex import tokens 

# if 42 then 13 else 0
# if 42 then if 1+2 then 6 else 3*5 

def p_if(p): 
  '''if : IF expression THEN if ELSE if
        | IF expression THEN if 
        | expression '''
  if len(p) == 7: 
    p[0] = p[4] if p[2] == 0 else p[6]
  else: 
    if len(p) == 5: 
      p[0] = p[4] if p[2] == 0 else 42
    else: 
      p[0] = p[1]
  

def p_expression_plus(p): 
  'expression : expression PLUS term'
  p[0] = p[1] + p[3]

def p_expression_term(p): 
  'expression : term' 
  p[0] = p[1]

def p_term_times(p): 
  'term : term MULT factor'
  p[0] = p[1] * p[3]

def p_term_factor(p): 
  'term : factor' 
  p[0] = p[1] 

def p_factor_num(p): 
  'factor : NUM'
  p[0] = p[1]

def p_factor_expr(p): 
  'factor : LBR expression RBR'
  p[0] = p[2]

def p_error(p): 
  print("Syntax error")

parser = yacc.yacc()

while True: 
  try: 
    s = input("calc> ")
  except EOFError: 
    break
  if not s: 
    continue
  result=parser.parse(s)
  print(result) 
