# scanner texto -> tokens
# lee y hace las piezas del rompecabezas
from typing import Any
import ply.lex as lex 

#revisa el orden de los tokens (si hacen oraciones)
import ply.yacc as yacc 

import sys #esto es para leer archivos (como mis pruebas)

#Las modifcaciones que hice al codigo en la entrega 2 las tengo marcadas para yo saber con (entrega #2)

# para correr:
# python3 patito.py pruebas/archivo 

# Cubo semantico: (entrega #2)
cubosemantico = {
    'entero': {
        'entero': {
            '+': 'entero',  '-': 'entero',   '*': 'entero',   '/': 'entero',
            '>': 'bool',    '<': 'bool',      '!=': 'bool',    '==': 'bool'
        },
        'flotante': {
            '+': 'flotante', '-': 'flotante', '*': 'flotante', '/': 'flotante',
            '>': 'bool',     '<': 'bool',     '!=': 'bool',    '==': 'bool'
        }
    },
    'flotante': {
        'entero': {
            '+': 'flotante', '-': 'flotante', '*': 'flotante', '/': 'flotante',
            '>': 'bool',     '<': 'bool',     '!=': 'bool',    '==': 'bool'
        },
        'flotante': {
            '+': 'flotante', '-': 'flotante', '*': 'flotante', '/': 'flotante',
            '>': 'bool',     '<': 'bool',     '!=': 'bool',    '==': 'bool'
        }
    }
}


def tiporesultante(tipo1, op, tipo2):
    try:
        return cubosemantico[tipo1][tipo2][op]
    except KeyError:
        return None

directorioFunciones = {}
ambitoActual        = None
hayErrores          = False
pilaOperandos  = []
pilaTipos      = []
pilaOperadores = []
filaCuadruplos = []
contTemp       = [0]
pilaJumps = [] #(entrega #4)


# (entrega #4) manejo de direcciones virtuales
_contadores_dir = {
    'global':    {'entero': 1000, 'flotante': 1500},
    'constante': {'entero': 2000, 'flotante': 2500},
    'local':     {'entero': 3000, 'flotante': 3500},
    'temporal':  {'entero': 4000, 'flotante': 4500, 'bool': 4900},
}
_limites_dir = {
    'global':    {'entero': 1499, 'flotante': 1999},
    'constante': {'entero': 2499, 'flotante': 2999},
    'local':     {'entero': 3499, 'flotante': 3999},
    'temporal':  {'entero': 4499, 'flotante': 4899, 'bool': 4999},
}
tablaConstantes = {}

def asignarDireccion(segmento, tipo):
    global hayErrores
    cont = _contadores_dir[segmento]
    lim  = _limites_dir[segmento]
    if tipo not in cont:
        print(f"error: tipo '{tipo}' no tiene rango en segmento '{segmento}'")
        hayErrores = True
        return -1
    if cont[tipo] > lim[tipo]:
        print(f"error: memoria '{segmento}/{tipo}' llena (limite {lim[tipo]})")
        hayErrores = True
        return -1
    dir_ = cont[tipo]
    cont[tipo] += 1
    return dir_

def resetDirecciones():
    _contadores_dir['global']    = {'entero': 1000, 'flotante': 1500}
    _contadores_dir['constante'] = {'entero': 2000, 'flotante': 2500}
    _contadores_dir['local']     = {'entero': 3000, 'flotante': 3500}
    _contadores_dir['temporal']  = {'entero': 4000, 'flotante': 4500, 'bool': 4900}
    tablaConstantes.clear()

# directorio de funciones y helpers: (entrega #2)
def registrarVariable(nombre, tipo, linea=0): 
    global hayErrores
    tabla = directorioFunciones[ambitoActual]['tablaVariables']
    if nombre in tabla:
        print(f"error semantico: variable '{nombre}' ya declarada en '{ambitoActual}' (linea {linea})")
        hayErrores = True
    else:
        segmento = 'global' if ambitoActual == 'global' else 'local' #(entrega #4)
        dir_ = asignarDireccion(segmento, tipo)
        if dir_ == -1:
            print(f"error semantico: no hay memoria disponible para '{nombre}' (linea {linea})")
            hayErrores = True
            return
    tabla[nombre] = {'tipo': tipo, 'dir': dir_}

def buscarVariable(nombre): 
    if ambitoActual in directorioFunciones:
        if nombre in directorioFunciones[ambitoActual]['tablaVariables']:
            return directorioFunciones[ambitoActual]['tablaVariables'][nombre]['tipo']
    if 'global' in directorioFunciones:
        if nombre in directorioFunciones['global']['tablaVariables']:
            return directorioFunciones['global']['tablaVariables'][nombre]['tipo']
    return None

#(entrega #4)
def buscarDireccion(nombre):
    if ambitoActual in directorioFunciones:
        if nombre in directorioFunciones[ambitoActual]['tablaVariables']:
            return directorioFunciones[ambitoActual]['tablaVariables'][nombre]['dir']
    if 'global' in directorioFunciones:
        if nombre in directorioFunciones['global']['tablaVariables']:
            return directorioFunciones['global']['tablaVariables'][nombre]['dir']
    return None

def imprimirDirectorio():
    for nombre, info in directorioFunciones.items():
        print(f"\n  [{nombre}]  tipo: {info['tipo']}")
        if info.get('params'):
            print(f"    params : {info['params']}")
        print(f"    variables:")
        if info['tablaVariables']:
            for var, datos in info['tablaVariables'].items():
                dir_str = datos.get('dir', '?') #(entrega #4)
                print(f"      {var:15} -> {datos['tipo']:10} dir: {dir_str:4}") #(entrega #4)
        else:
            print("      (ninguna)")

        


# Primera parte: Mi Scanner 
# Palabras que mi cogido puede recnocer, no son variables pero son parte del lenguaje. 
palabrasReservadas = {
    'programa' : 'PROGRAMA',
    'inicio'   : 'INICIO',
    'fin'      : 'FIN',
    'vars'     : 'VARS',
    'entero'   : 'ENTERO',
    'flotante'  : 'FLOTANTE',
    'si'       : 'SI',
    'sino'     : 'SINO',
    'mientras' : 'MIENTRAS',
    'haz'      : 'HAZ',
    'escribe'  : 'ESCRIBE',
    'nula'     : 'NULA',
}

# Lista de todos los tokens
# en otras palabras es el menu del scanner.
tokens = [
    'ID',
    'CTE_ENT',
    'CTE_FLOT',
    'LETRERO',
    'SUMA',
    'RESTA',
    'MULTIPLICACION',
    'DIVISION',
    'MAYORQUE',
    'MENORQUE',
    'DIFERENTE',
    'IGUALIGUAL',
    'ASIGNACION',
    'PARENTIZQ',
    'PARENTDER',
    'LLAVEIZQ',
    'LLAVEDER',
    'PUNTOCOMA',
    'DOSPUNTOS',
    'COMA',
] + list(palabrasReservadas.values())

# Tokens simples asignaciones de simbolos.
# Si aparece este simbolo, llamalo asi
t_SUMA           = r'\+'
t_RESTA          = r'-'
t_MULTIPLICACION = r'\*'
t_DIVISION       = r'/'
t_MAYORQUE       = r'>'
t_MENORQUE       = r'<'
t_ASIGNACION     = r'='
t_PARENTIZQ      = r'\('
t_PARENTDER      = r'\)'
t_LLAVEIZQ       = r'\{'
t_LLAVEDER       = r'\}'
t_PUNTOCOMA      = r';'
t_DOSPUNTOS      = r':'
t_COMA           = r','

# Tokens con acción 
# CTE_FLOT debe ir ANTES que CTE_ENT
# Si no 3.14 se hace un entero 3 + punto + entero 14

#numeros flotantes 
def t_CTE_FLOT(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

#numeros enteros 
def t_CTE_ENT(t):
    r'\d+'
    t.value = int(t.value)
    return t

#textos entre "" son letreros. 
def t_LETRERO(t):
    r'"[^"\n]*"'
    t.value = t.value[1:-1]   
    return t

# IMPORTANTE: DIFERENTE e IGUALIGUAL antes de ASIGNACION

# para que != y == no se partan en ! + = o = + =
def t_DIFERENTE(t):
    r'!='
    return t

def t_IGUALIGUAL(t):
    r'=='
    return t

# lee nombres y distingue si es nombre normal o palabra especial del lenguaje.
def t_ID(t):
    r'[a-zA-Z][a-zA-Z0-9]*'

    # Si el identificador es palabra reservada, cambia su tipo
    t.type = palabrasReservadas.get(t.value, 'ID')
    return t

# ignora espacios 
t_ignore = ' \t'

# cuenta los saltos de línea (para reportar número de línea en errores)
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Error léxico
def t_error(t):
    print(f"error lexico: caracter no reconocido '{t.value[0]}' en lnea {t.lexer.lineno}")
    t.lexer.skip(1)

# construye el lexer
lexer = lex.lex()


# Segunda parte: El Parser
# Revisa si las piezas van en el orden correcto
def p_programa(p):
    '''programa : programa_inicio vars_opc funcs_opc INICIO cuerpo FIN'''
    p[0] = ('programa', p[1], p[2], p[3], p[5]) #entrega #2

def p_programa_inicio(p):      #entrega #2
    '''programa_inicio : PROGRAMA ID PUNTOCOMA'''
    global ambitoActual
    directorioFunciones['global'] = {'tipo': 'programa', 'tablaVariables': {}}
    ambitoActual = 'global'
    p[0] = p[2]

# Puede tener vars o no tener nada
def p_vars_opc_con(p): # Si hay vars
    '''vars_opc : VARS lista_decl vars_p'''
    p[0] = ('vars', p[2])

def p_vars_opc_vacio(p): # No hay vars
    '''vars_opc : empty'''
    p[0] = None

# Despues de la 1ra lnea de vars, hay más lineas? 
def p_vars_p_con(p):
    '''vars_p : lista_decl vars_p'''
    pass

def p_vars_p_vacio(p): # ya no hay
    '''vars_p : empty'''
    pass

# Primera linea de variables: x, y
def p_lista_decl(p):   
    '''lista_decl : decl lista_decl_p'''
    pass

# Otra declaración más en la misma lista?
def p_lista_decl_p_con(p): 
    '''lista_decl_p : decl lista_decl_p'''
    pass

def p_lista_decl_p_vacio(p): # termina la lista
    '''lista_decl_p : empty'''
    pass

# Una línea de variables: x, y : entero;  (o solo x : entero;)
def p_decl(p): #(entrega #2)
    '''decl : ID lista_ids DOSPUNTOS tipo PUNTOCOMA'''
    todosLosIds = [p[1]] + p[2]
    for nombre in todosLosIds:
        registrarVariable(nombre, p[4], p.lineno(1))

# Más nombres en la misma línea: , y , z
def p_lista_ids_con(p): #(entrega #2)
    '''lista_ids : COMA ID lista_ids'''
    p[0] = [p[2]] + p[3]

def p_lista_ids_vacio(p): #(entrega #2)
    '''lista_ids : empty'''
    p[0] = []

# entero o flotante
def p_tipo(p):
    '''tipo : ENTERO
            | FLOTANTE'''
    p[0] = p[1]


# El programa puede tener funciones o no
def p_funcs_opc_con(p): # si hay funciones
    '''funcs_opc : funcs'''
    p[0] = p[1]

def p_funcs_opc_vacio(p): # no hay funciones
    '''funcs_opc : empty'''
    p[0] = None

def p_funcs_varias(p): # más de una función
    '''funcs : func funcs'''
    pass

def p_funcs_una(p): # solo una función
    '''funcs : func'''
    pass

# Una función: nula nombre
def p_func_inicio(p):      #(entrega #2)
    '''func_inicio : NULA ID PARENTIZQ'''
    global ambitoActual, hayErrores
    nombre = p[2]
    if nombre in directorioFunciones:
        print(f"error semantico: funcion '{nombre}' ya declarada")
        hayErrores = True
    else:
        directorioFunciones[nombre] = {'tipo': 'nula', 'params': [], 'tablaVariables': {}}
    ambitoActual = nombre
    p[0] = nombre

def p_func(p): #(entrega #2)
    '''func : func_inicio params_opc PARENTDER vars_opc cuerpo PUNTOCOMA'''
    global ambitoActual
    generarCuadruplo('ENDFUNC', None, None, None)  # (entrega #4) PN-O
    ambitoActual = 'global'
    p[0] = ('func', p[1])

# Parámetros: entero n  o  entero n, flotante x
def p_params_opc_con(p): #(entrega #2)
    '''params_opc : tipo ID params_p'''
    registrarVariable(p[2], p[1])
    resto = p[3] if p[3] else []
    directorioFunciones[ambitoActual]['params'] = [(p[1], p[2])] + resto

def p_params_p_con(p): #(entrega #2)
    '''params_p : COMA tipo ID params_p'''
    registrarVariable(p[3], p[2])
    resto = p[4] if p[4] else []
    p[0] = [(p[2], p[3])] + resto

def p_params_p_vacio(p): #(entrega #2)
    '''params_p : empty'''
    p[0] = []

# Lo que va entre { y }
def p_cuerpo(p): # hay líneas dentro de { }
    '''cuerpo : LLAVEIZQ estatutos LLAVEDER''' 
    p[0] = ('cuerpo', p[2])

def p_estatutos_con(p):  # hay líneas dentro de { }
    '''estatutos : estatuto estatutos'''
    pass

def p_estatutos_vacio(p): # { } vacío 
    '''estatutos : empty'''
    pass

# Una línea puede ser: asignar, si, mientras, llamar función, o escribe
def p_estatuto(p):
    '''estatuto : asigna
                | condicion
                | ciclo
                | llamada
                | imprime'''
    p[0] = p[1]


# Asignaciones: x = 5;  o  promedio = x + 1;
def p_asigna(p): # (entrega #3)
    '''asigna : ID ASIGNACION expresion PUNTOCOMA'''
    global hayErrores
    nombre_expr, tipo_expr = p[3]
    tipoVar = buscarVariable(p[1])
    if tipoVar is None:
        print(f"error semantico: variable '{p[1]}' no declarada (linea {p.lineno(1)})")
        hayErrores = True
    elif tipo_expr is not None:
        if tipoVar != tipo_expr and not (tipoVar == 'flotante' and tipo_expr == 'entero'):
            print(f"error semantico: no se puede asignar '{tipo_expr}' a '{p[1]}' de tipo '{tipoVar}'")
            hayErrores = True
    dir_dest = buscarDireccion(p[1]) if tipoVar else p[1] #(entrega #4)
    generarCuadruplo('=', nombre_expr, None, dir_dest) #(entrega #4)


# Conddicion si (expresion) { líneas } sino { líneas }
def p_condicion_inicio(p):  # (entrega #4) PN-F
    '''condicion_inicio : SI PARENTIZQ expresion PARENTDER'''
    cond, _ = p[3]
    idx = len(filaCuadruplos)
    generarCuadruplo('GOTOF', cond, None, None)  # destino pendiente
    pilaJumps.append(idx)

def p_condicion(p):  # (entrega #4) PN-G y PN-H
    '''condicion : condicion_inicio cuerpo sino_opc PUNTOCOMA'''
    p[0] = ('condicion',)

def p_sino_opc_con(p):  # (entrega #4) PN-G + PN-H
    '''sino_opc : sino_inicio cuerpo'''
    # PN-H: rellena el GOTO que quedó pendiente
    idx_goto = pilaJumps.pop()
    filaCuadruplos[idx_goto] = (
        filaCuadruplos[idx_goto][0],
        filaCuadruplos[idx_goto][1],
        filaCuadruplos[idx_goto][2],
        len(filaCuadruplos)
    )
    p[0] = ('sino',)

def p_sino_inicio(p):  # (entrega #4) PN-G
    '''sino_inicio : SINO'''
    # rellena el GOTOF pendiente con la dirección actual
    idx_gotof = pilaJumps.pop()
    filaCuadruplos[idx_gotof] = (
        filaCuadruplos[idx_gotof][0],
        filaCuadruplos[idx_gotof][1],
        filaCuadruplos[idx_gotof][2],
        len(filaCuadruplos)
    )
    # genera GOTO para saltar el sino, queda pendiente
    idx_goto = len(filaCuadruplos)
    generarCuadruplo('GOTO', None, None, None)
    pilaJumps.append(idx_goto)

def p_sino_opc_vacio(p):  # (entrega #4) sin sino
    '''sino_opc : empty'''
    # rellena el GOTOF directamente al cuádruplo actual
    idx_gotof = pilaJumps.pop()
    filaCuadruplos[idx_gotof] = (
        filaCuadruplos[idx_gotof][0],
        filaCuadruplos[idx_gotof][1],
        filaCuadruplos[idx_gotof][2],
        len(filaCuadruplos)
    )
    p[0] = None


# Loop
#mientras (algo) haz { ... }
def p_ciclo_inicio(p):  # (entrega #4) PN-I: guarda donde empieza la condición
    '''ciclo_inicio : MIENTRAS'''
    pilaJumps.append(len(filaCuadruplos))  # índice de regreso

def p_ciclo_cond(p):  # (entrega #4) PN-J: evalúa condición y genera GOTOF
    '''ciclo_cond : ciclo_inicio PARENTIZQ expresion PARENTDER'''
    cond, _ = p[3]
    idx = len(filaCuadruplos)
    generarCuadruplo('GOTOF', cond, None, None)  # destino pendiente
    pilaJumps.append(idx)

def p_ciclo(p):  # (entrega #4) PN-K: cierra el ciclo
    '''ciclo : ciclo_cond HAZ cuerpo PUNTOCOMA'''
    # saca el GOTOF pendiente
    idx_gotof = pilaJumps.pop()
    # saca el índice de regreso
    idx_regreso = pilaJumps.pop()
    # genera GOTO de regreso al inicio
    generarCuadruplo('GOTO', None, None, idx_regreso)
    # rellena el GOTOF con el cuádruplo actual (salida del ciclo)
    filaCuadruplos[idx_gotof] = (
        filaCuadruplos[idx_gotof][0],
        filaCuadruplos[idx_gotof][1],
        filaCuadruplos[idx_gotof][2],
        len(filaCuadruplos)
    )
    p[0] = ('ciclo',)

# call funciones 
def p_llamada_inicio(p):  # (entrega #4) PN-L
    '''llamada_inicio : ID PARENTIZQ'''
    global hayErrores
    if p[1] not in directorioFunciones:
        print(f"error semantico: funcion '{p[1]}' no declarada (linea {p.lineno(1)})")
        hayErrores = True
    else:
        generarCuadruplo('ERA', p[1], None, None)  # reserva memoria
    p[0] = p[1]  # guarda el nombre de la función

def p_llamada(p):  # (entrega #4) PN-N
    '''llamada : llamada_inicio args_opc PARENTDER PUNTOCOMA'''
    generarCuadruplo('GOSUB', p[1], None, None)  # llama la función
    p[0] = ('llamada', p[1])

def p_args_opc_con(p):  # con argumentos: f(1, 2)
    '''args_opc : arg args_p'''
    pass

def p_args_opc_vacio(p): # sin argumentos: f()
    '''args_opc : empty'''
    pass

def p_arg(p):  # (entrega #4) PN-M
    '''arg : expresion'''
    val, _ = p[1]
    generarCuadruplo('PARAM', val, None, None)
    p[0] = p[1]

def p_args_p_con(p):  # , otro argumento
    '''args_p : COMA arg args_p'''
    pass

def p_args_p_vacio(p): # ya no hay más argumentos
    '''args_p : empty'''
    pass

def p_args_opc_con(p):
    '''args_opc : arg args_p'''
    pass

def p_args_p_con(p):
    '''args_p : COMA arg args_p'''
    pass

def p_args_opc_vacio(p): # sin argumentos: f()
    '''args_opc : empty'''
    pass

def p_args_p_con(p):  # , otro argumento
    '''args_p : COMA expresion args_p'''
    pass

def p_args_p_vacio(p): # ya no hay más argumentos
    '''args_p : empty'''
    pass

# Imprime cosas
def p_imprime(p): # (entrega #3)
    '''imprime : ESCRIBE PARENTIZQ item_imp items_p PARENTDER PUNTOCOMA'''
    generarCuadruplo('print', p[3], None, None)
    p[0] = ('imprime',)

def p_item_imp_expr(p): # (entrega #3)
    '''item_imp : expresion'''
    p[0] = p[1][0]

def p_item_imp_letrero(p): # (entrega #3)
    '''item_imp : LETRERO'''
    p[0] = f'"{p[1]}"'

def p_items_p_con(p): # (entrega #3)
    '''items_p : COMA item_imp items_p'''
    generarCuadruplo('print', p[2], None, None)

def p_items_p_vacio(p):
    '''items_p : empty'''
    pass


# expresión completa: puede llevar > < == != 
def p_expresion_mayor(p): # (entrega #3)
    '''expresion : exp MAYORQUE exp'''
    temp = nuevoTempTipado('bool')  #(entrega #4)
    generarCuadruplo('>', p[1][0], p[3][0], temp)
    pilaOperandos.append(temp); pilaTipos.append('bool')
    p[0] = (temp, 'bool')

def p_expresion_menor(p): # (entrega #3)
    '''expresion : exp MENORQUE exp'''
    temp = nuevoTempTipado('bool')  #(entrega #4)
    generarCuadruplo('<', p[1][0], p[3][0], temp)
    pilaOperandos.append(temp); pilaTipos.append('bool')
    p[0] = (temp, 'bool')

def p_expresion_diferente(p): # (entrega #3)        
    '''expresion : exp DIFERENTE exp'''
    temp = nuevoTempTipado('bool')  #(entrega #4)
    generarCuadruplo('!=', p[1][0], p[3][0], temp)
    pilaOperandos.append(temp); pilaTipos.append('bool')
    p[0] = (temp, 'bool')

def p_expresion_igual(p): # (entrega #3)
    '''expresion : exp IGUALIGUAL exp'''
    temp = nuevoTempTipado('bool')  #(entrega #4)
    generarCuadruplo('==', p[1][0], p[3][0], temp)
    pilaOperandos.append(temp); pilaTipos.append('bool')
    p[0] = (temp, 'bool')

def p_expresion_exp(p): # (entrega #3)
    '''expresion : exp'''
    p[0] = p[1]

# suma y resta: left-recursive para generar cuádruplos en orden correcto
def p_exp_suma(p): # (entrega #3)
    '''exp : exp SUMA termino'''
    global hayErrores
    tipo = tiporesultante(p[1][1], '+', p[3][1])
    if tipo is None and p[1][1] and p[3][1]:
        print(f"error semantico: '+' invalido entre '{p[1][1]}' y '{p[3][1]}'")
        hayErrores = True
        tipo = p[1][1]
    temp = nuevoTempTipado(tipo) #(entrega #4)
    generarCuadruplo('+', p[1][0], p[3][0], temp)
    pilaOperandos.append(temp); pilaTipos.append(tipo)
    p[0] = (temp, tipo)

def p_exp_resta(p): # (entrega #3)
    '''exp : exp RESTA termino'''
    global hayErrores
    tipo = tiporesultante(p[1][1], '-', p[3][1])
    if tipo is None and p[1][1] and p[3][1]:
        print(f"error semantico: '-' invalido entre '{p[1][1]}' y '{p[3][1]}'")
        hayErrores = True
        tipo = p[1][1]
    temp = nuevoTempTipado(tipo) #(entrega #4)

    generarCuadruplo('-', p[1][0], p[3][0], temp)
    pilaOperandos.append(temp); pilaTipos.append(tipo)
    p[0] = (temp, tipo)

def p_exp_termino(p): # (entrega #3)
    '''exp : termino'''
    p[0] = p[1]

# multiplicación y división: left-recursive
def p_termino_mult(p): # (entrega #3)
    '''termino : termino MULTIPLICACION factor'''
    global hayErrores
    tipo = tiporesultante(p[1][1], '*', p[3][1])
    if tipo is None and p[1][1] and p[3][1]:
        print(f"error semantico: '*' invalido entre '{p[1][1]}' y '{p[3][1]}'")
        hayErrores = True
        tipo = p[1][1]
    temp = nuevoTempTipado(tipo) #(entrega #4)
    generarCuadruplo('*', p[1][0], p[3][0], temp)
    pilaOperandos.append(temp); pilaTipos.append(tipo)
    p[0] = (temp, tipo)

def p_termino_div(p): # (entrega #3)
    '''termino : termino DIVISION factor'''
    global hayErrores
    tipo = tiporesultante(p[1][1], '/', p[3][1])
    if tipo is None and p[1][1] and p[3][1]:
        print(f"error semantico: '/' invalido entre '{p[1][1]}' y '{p[3][1]}'")
        hayErrores = True
        tipo = p[1][1]
    temp = nuevoTempTipado(tipo) #(entrega #4)
    generarCuadruplo('/', p[1][0], p[3][0], temp)
    pilaOperandos.append(temp); pilaTipos.append(tipo)
    p[0] = (temp, tipo)

def p_termino_factor(p): # (entrega #3)
    '''termino : factor'''
    p[0] = p[1]

# uso de parentesis (algo)
def p_factor_parentesis(p): # (entrega #3)
    '''factor : PARENTIZQ expresion PARENTDER'''
    p[0] = p[2]

# operando con signo opcional: -x o +x o x
def p_factor_operando(p): # (entrega #3)
    '''factor : signo_opc operando'''
    nombre, tipo = p[2]
    if p[1] is not None:
        temp = nuevoTemp()
        generarCuadruplo(p[1], nombre, None, temp)
        pilaOperandos.append(temp); pilaTipos.append(tipo)
        p[0] = (temp, tipo)
    else:
        p[0] = p[2]

def p_signo_opc_pos(p): # (entrega #3)
    '''signo_opc : SUMA'''
    p[0] = '+'

def p_signo_opc_neg(p): # (entrega #3)
    '''signo_opc : RESTA'''
    p[0] = '-'

def p_signo_opc_vacio(p): # (entrega #3)
    '''signo_opc : empty'''
    p[0] = None

def p_operando_cte(p): # (entrega #3)
    '''operando : cte'''
    p[0] = p[1]

# busca el tipo de la variable y la pushea a las pilas
def p_operando_id(p): # (entrega #3)
    '''operando : ID'''
    global hayErrores
    tipo = buscarVariable(p[1])
    if tipo is None:
        print(f"error semantico: variable '{p[1]}' no declarada (linea {p.lineno(1)})")
        hayErrores = True
        tipo = 'error'
    dir_ = buscarDireccion(p[1]) if tipo != 'error' else p[1] #(entrega #4)
    pilaOperandos.append(dir_); pilaTipos.append(tipo) #(entrega #4)
    p[0] = (dir_, tipo) #(entrega #4)

# constantes pushean su valor y tipo a las pilas
def p_cte_ent(p): #(entrega #4)
    '''cte : CTE_ENT'''
    clave = str(p[1])
    if clave not in tablaConstantes:
        tablaConstantes[clave] = asignarDireccion('constante', 'entero')
    dir_ = tablaConstantes[clave]
    pilaOperandos.append(dir_); pilaTipos.append('entero')
    p[0] = (dir_, 'entero')


def p_cte_flot(p): #(entrega #4)
    '''cte : CTE_FLOT'''
    clave = str(p[1])
    if clave not in tablaConstantes:
        tablaConstantes[clave] = asignarDireccion('constante', 'flotante')
    dir_ = tablaConstantes[clave]
    pilaOperandos.append(dir_); pilaTipos.append('flotante')
    p[0] = (dir_, 'flotante')

# regla vacía 
def p_empty(p):
    '''empty :'''
    p[0] = None

# error sintáctico
def p_error(p):
    if p:
        print(f"Error sintactico: token inesperado '{p.value}' en linea {p.lineno}")
    else:
        print("Error sintactico: fin de archivo inesperado")

def nuevoTemp(): #entrega #4
    contTemp[0] += 1
    return f"t{contTemp[0]}"

def nuevoTempTipado(tipo): #entrega #4
    contTemp[0] += 1
    tipo_seg = tipo if tipo in ('entero', 'flotante', 'bool') else 'entero'
    dir_ = asignarDireccion('temporal', tipo_seg)
    return dir_

def generarCuadruplo(op, izq, der, res):
    filaCuadruplos.append((
        op,
        izq if izq is not None else '_',
        der if der is not None else '_',
        res if res is not None else '_'
    ))

def imprimirCuadruplos():
    print("=" * 45)
    print(" FILA DE CUADRUPLOS ".center(45))
    print("=" * 45)
    print(f"{'#':<5} {'OP':<10} {'IZQ':<10} {'DER':<10} {'RES':<10}")
    print("-" * 45)
    for i, (op, izq, der, res) in enumerate(filaCuadruplos): #(entrega #4)
        print(f"{i:<5} {str(op):<10} {str(izq):<10} {str(der):<10} {str(res):<10}")
    print("-" * 45)

# Entrega 3 ^^^

def imprimirConstantes():
    if not tablaConstantes:
        return
    print("=" * 45)
    print(" TABLA DE CONSTANTES ".center(35))
    print("=" * 45)
    print(f"{'VALOR':<15} {'DIR':<10}")
    print("-" * 45)
    for val, dir_ in tablaConstantes.items():
        print(f"{val:<15} {dir_:<10}")
    print("-" * 45)

# Construir el parser 
parser = yacc.yacc(errorlog=yacc.NullLogger())



#Tercera parte: Main
if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Uso: python patito.py <archivo.patito>")
        sys.exit(1)

    archivo = sys.argv[1]
    try:
        with open(archivo, 'r') as f:
            codigo = f.read()
    except FileNotFoundError:
        print(f"Error: no se encontro el archivo '{archivo}'")
        sys.exit(1)

    directorioFunciones.clear()
    ambitoActual = None
    hayErrores = False
    filaCuadruplos.clear()
    pilaOperandos.clear()
    pilaTipos.clear()
    pilaOperadores.clear()
    pilaJumps.clear()        # (entrega #4)
    contTemp[0] = 0
    resetDirecciones()       # (entrega #4)

    print(f"\nAnalizando: {archivo}")
    resultado = parser.parse(codigo, lexer=lexer.clone())

    if not hayErrores:
        print("programa valido")
        imprimirDirectorio()
        imprimirConstantes()   # (entrega #4)
        imprimirCuadruplos()
    else:
        print("programa tiene errores")




