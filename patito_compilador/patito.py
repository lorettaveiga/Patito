#para correr:
#python3 patito.py pruebas/archivo 

# ---------------------------------------------------------
# IMPORTS Y LIBRERÍAS

from typing import Any
import ply.lex as lex 
import ply.yacc as yacc 
import sys 
# ----------------------------------------------------


# ---------------------------------------------------------
# CUBO SEMÁNTICO

# Verifica qué operaciones son válidas entre tipos.
# Ejemplo: entero + flotante = flotante
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
} # cierra las reglas del cubo semántico

# Consulta el cubo semántico y regresa el tipo resultante.
def tiporesultante(tipo1, op, tipo2): #tipo1 y tipo2 son los tipos de los operandos, op es el operador.
    try: # intenta buscar el resultado en el cubo semántico pero puede fallar
        return cubosemantico[tipo1][tipo2][op] # devuelve el tipo resultante
    except KeyError: # si no encuentra el resultado
        return None # devuelve none
# ---------------------------------------------------------



# ---------------------------------------------------------
# VARIABLES GLOBALES Y ESTRUCTURAS DEL COMPILADOR

directorioFunciones = {} 
ambitoActual        = None 
hayErrores          = False 
pilaOperandos  = [] 
pilaTipos      = [] 
pilaOperadores = [] 
filaCuadruplos = [] 
contTemp       = [0] 
pilaJumps = [] 


# pila auxiliar para el paso de parametros en las llamadas a funciones
# cada elemento es {'func': nombreFuncion, 'k': numeroDeParametroActual}
pilaParams = [] #agregue esto
# ---------------------------------------------------------



# ---------------------------------------------------------
# MEMORIA VIRTUAL

# Define los rangos de direcciones para variables globales,
# locales, constantes y temporales.
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


# guarda el valor real de cada constante indexado por su direccion virtual
# (ej. 2000 -> 0, 2001 -> 5, 2500 -> 3.14) para que la maquina virtual
# pueda cargar las constantes en memoria de ejecucion
valoresConstantes = {}
# ---------------------------------------------------------



# ---------------------------------------------------------
# ASIGNACIÓN Y REINICIO DE DIRECCIONES VIRTUALES

#recibe informacion del segmento y el tipo de la variable
def asignarDireccion(segmento, tipo):
    global hayErrores
    cont = _contadores_dir[segmento] 
    lim  = _limites_dir[segmento]

    # revisa si el tipo está en el segmento 
    # (ej busca bool en global y si no esta entc manda -1) 
    if tipo not in cont:
        return -1
    
    # revisa si se excede el limite de direcciones en el segmento
    # (ej busca enteros y si ya no hay espacio manda -1) 
    if cont[tipo] > lim[tipo]:
        hayErrores = True
        return -1

    #si si hay espacio y si esta asigna la dirección y actualiza el contador
    dir_ = cont[tipo]
    cont[tipo] += 1
    return dir_
    
# reinicia las direcciones
def resetDirecciones():
    _contadores_dir['global']    = {'entero': 1000, 'flotante': 1500}
    _contadores_dir['constante'] = {'entero': 2000, 'flotante': 2500}
    _contadores_dir['local']     = {'entero': 3000, 'flotante': 3500}
    _contadores_dir['temporal']  = {'entero': 4000, 'flotante': 4500, 'bool': 4900}
    tablaConstantes.clear()


    valoresConstantes.clear()
# ---------------------------------------------------------



# ---------------------------------------------------------
# DIRECTORIO DE FUNCIONES Y TABLA DE VARIABLES

# recibe nombre, tpo de variable y en q linea del codigo esta
def registrarVariable(nombre, tipo, linea=0): 
    global hayErrores

    #abre el directorio de funciones y busca en la tabla
    tabla = directorioFunciones[ambitoActual]['tablaVariables']
    
    # revisa si la variable ya está declarada en el ambito actual
    # si esta, imprime un error
    if nombre in tabla:
        print(f"error semantico: variable '{nombre}' ya declarada en '{ambitoActual}' (linea {linea})")
        hayErrores = True
        return
    
    # si no está declarada, 
    # asigna una dirección y la agrega a la tabla de variables
    else:
        segmento = 'global' if ambitoActual == 'global' else 'local' #(entrega #4)
        dir_ = asignarDireccion(segmento, tipo)
        
        # si no hay memoria disponible, imprime un error
        if dir_ == -1:
            print(f"error semantico: no hay memoria disponible para '{nombre}' (linea {linea})")
            hayErrores = True
            return

# si habia lugar, guarda la variable en tabla de variables
    tabla[nombre] = {'tipo': tipo, 'dir': dir_}

# busca las variables en el directorio de funciones
def buscarVariable(nombre): 

    # entra al ambito local 
    if ambitoActual in directorioFunciones:

        # busca la variable en el ambito local y si esta devuelve el tipo
        if nombre in directorioFunciones[ambitoActual]['tablaVariables']:
            return directorioFunciones[ambitoActual]['tablaVariables'][nombre]['tipo']
    
    # si no estaba, busca la variable en el ambito global y si esta devuelve el tipo
    if 'global' in directorioFunciones:
        if nombre in directorioFunciones['global']['tablaVariables']:
            return directorioFunciones['global']['tablaVariables'][nombre]['tipo']
    
    # si no esta, devuelve none
    return None

# busca direcciones virtuales
def buscarDireccion(nombre):

    # entra al ambito local 
    if ambitoActual in directorioFunciones:

        # busca la variable en el ambito local y si esta devuelve la direccion
        if nombre in directorioFunciones[ambitoActual]['tablaVariables']:
            return directorioFunciones[ambitoActual]['tablaVariables'][nombre]['dir']
    
    # si no estaba, entra al ambito global
    if 'global' in directorioFunciones:

        # busca la variable en el ambito global y si esta devuelve la direccion
        if nombre in directorioFunciones['global']['tablaVariables']:
            return directorioFunciones['global']['tablaVariables'][nombre]['dir']
    
    # si no esta, devuelve none
    return None

#imprime el directorio de funciones
def imprimirDirectorio():

    # recorre el directorio de funciones
    for nombre, info in directorioFunciones.items():
        # imprime el nombre del ambito y el tipo de la funcion
        print(f"\n  [{nombre}]  tipo: {info['tipo']}")
        
        # imprime los parametros de la funcion, si hay
        if info.get('params'):
            print(f"    params : {info['params']}")
        print(f"    variables:")
        
        # saca toda la tabla y la imprme
        if info['tablaVariables']:
            for var, datos in info['tablaVariables'].items():
                dir_str = datos.get('dir', '?') 
                print(f"      {var:15} -> {datos['tipo']:10} dir: {dir_str:4}") 
      
        # si no hay variables, imprime que no hay ninguna
        else:
            print("      (ninguna)")

# ----------------------------------------------------------



# ----------------------------------------------------------
# SCANNER / LEXER

# Convierte el código fuente en tokens.
# Aquí se reconocen palabras reservadas, IDs, números, operadores y símbolos.

# Palabras reservadas del lenguaje Patito
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

# Lista de tokens que el scanner puede reconocer
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

# Tokens simples: operadores y símbolos
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

# Tokens con acción: constantes, letreros, IDs y operadores compuestos

# reconoce cadenas de numeros y las convierte en flotantes
def t_CTE_FLOT(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

# reconoce caenas de numeros y las convierte en enteros
def t_CTE_ENT(t):
    r'\d+'
    t.value = int(t.value)
    return t

# reconoce cadenas de letras y las convierte en strings
def t_LETRERO(t):
    r'"[^"\n]*"'
    t.value = t.value[1:-1]   
    return t

# reconoce != para poder usarlo como operador de diferenca
def t_DIFERENTE(t):
    r'!='
    return t

# reconoce == para poder usarlo como operador de igualdad
def t_IGUALIGUAL(t):
    r'=='
    return t

# busca si es palabra reservada, y asigna totken. Si no, es ID.
def t_ID(t):
    r'[a-zA-Z][a-zA-Z0-9]*'
    t.type = palabrasReservadas.get(t.value, 'ID')
    return t

# ignora espacios y tabs
t_ignore = ' \t'

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# si no reconoce el token, imprime un error
def t_error(t):
    print(f"error lexico: caracter no reconocido '{t.value[0]}' en lnea {t.lexer.lineno}")
    t.lexer.skip(1)

# termina de formar el token
lexer = lex.lex()

# hasta aqui hicimos puras funciones y variables, 
# si fueramos chefs solo hicimos mis and plas de los ingredientes.


# ==========================================================
# ----------------------------------------------------------
# PARSER / GRAMÁTICA
# Revisa que los tokens estén en el orden correcto
# según las reglas del lenguaje.

# ESTRUCTURA PRINCIPAL DEL PROGRAMA

# define las reglas que debe seguir el programa
def p_programa(p):
    # estructura de un codigo en patito


    '''programa : programa_inicio vars_opc funcs_opc main_inicio cuerpo FIN'''
    # guarda los valores de los tokens en el programa
    # las palabras reservadas no son necesarias
    p[0] = ('programa', p[1], p[2], p[3], p[5]) 



# marcador que reconoce 'inicio' y rellena el GOTO inicial con la direccion
# del primer cuadruplo del cuerpo principal (asi el codigo de funciones queda
# arriba y solo se ejecuta cuando se hace GOSUB)
def p_main_inicio(p):
    '''main_inicio : INICIO'''
    filaCuadruplos[0] = ('GOTO', '_', '_', len(filaCuadruplos))

# reconoce la parte de programa: cuadruplo1 o asi. El nombre del archivo.
def p_programa_inicio(p):    
    '''programa_inicio : PROGRAMA ID PUNTOCOMA'''

    # nos avisa que va a modificar el ambito actual
    global ambitoActual

    # crea el directorio de funciones y le asigna el tipo programa
    # y la tabla de variables empieza vacia
    directorioFunciones['global'] = {'tipo': 'programa', 'tablaVariables': {}}

    # asigna el ambito actual a global
    ambitoActual = 'global'



    # genera el GOTO inicial (cuadruplo 0) que salta sobre el codigo de las
    # funciones para llegar directamente al cuerpo principal (main).
    # su destino se rellena cuando se reconoce la palabra reservada inicio.
    generarCuadruplo('GOTO', None, None, None)

    # almacena el nombre del programa
    p[0] = p[2]

# ----------------------------------------------------------
# DECLARACIÓN DE VARIABLES

def p_vars_opc_con(p): 
    #  pone vars (palabra reservada), primer variable (lista_decl) y todas las que sigan (vars_p)
    '''vars_opc : VARS lista_decl vars_p'''
    p[0] = ('vars', p[2])

# si no hay variables (porque era opcional), pone empty 
def p_vars_opc_vacio(p): 
    '''vars_opc : empty'''
    p[0] = None

# se llama asi misma hasta que este vacia para agarrar todas las variables
def p_vars_p_con(p):
    '''vars_p : lista_decl vars_p'''
    pass

# si no hay variables, pone empty y para la recursion
def p_vars_p_vacio(p):
    '''vars_p : empty'''
    pass

def p_lista_decl(p):   
    # nos indica que debe haber al menos una declaración en la lista
    #y despues sigue si hay mas
    '''lista_decl : decl lista_decl_p'''
    pass

# se llama asi misma hasta que este vacia para agarrar todas las declaraciones
def p_lista_decl_p_con(p): 
    '''lista_decl_p : decl lista_decl_p'''
    pass

# si no hay declaraciones, 
# pone empty y para la recursion
def p_lista_decl_p_vacio(p):
    '''lista_decl_p : empty'''
    pass

# declara una variable
def p_decl(p): 
    '''decl : ID lista_ids DOSPUNTOS tipo PUNTOCOMA'''

    # recorre la lista ded todos los ids
    todosLosIds = [p[1]] + p[2]

    # registra cada id
    for nombre in todosLosIds:

        # registra el id con su tipo y la linea ed codigo en la que esta
        # p.lineno(1) es la linea de codigo en la que esta el id
        # lo saca de la funcion t_newline
        registrarVariable(nombre, p[4], p.lineno(1))

# se llama a ella misma para agarrar todos los ids
# lee de izq a derecha las ids y en la coma sabe q queda otra.
def p_lista_ids_con(p):
    '''lista_ids : COMA ID lista_ids'''
    p[0] = [p[2]] + p[3]

# si no hay ids, pone empty y para la recursion
def p_lista_ids_vacio(p): 
    '''lista_ids : empty'''
    p[0] = []

# decllara el tipo de variable que es (entero o flotante)
def p_tipo(p):

    # si es entero o flotante, lo guarda en p[0] 
    # Y lo manda a p[4] para que se guarde en la declaracion de la variable
    '''tipo : ENTERO
            | FLOTANTE'''
    p[0] = p[1]
# ----------------------------------------------------------
# DECLARACIÓN DE FUNCIONES
# Registra funciones nulas, parámetros y variables locales.

# si hay funciones (nula ... (){};), las guarda.
def p_funcs_opc_con(p): 
    '''funcs_opc : funcs'''
    p[0] = p[1]

# si no hay funciones, pone empty
def p_funcs_opc_vacio(p): 
    '''funcs_opc : empty'''
    p[0] = None

# se llama asi misma hasta que este vacia 
# para agarrar todas las funciones
def p_funcs_varias(p): 
    # aqui usa recursion
    '''funcs : func funcs'''
    pass

# si no hay más de una funcion en la pasada
# en esta manda la funcion solita
def p_funcs_una(p): 
    '''funcs : func'''
    pass

# reconoce la primera parte de la funcion
# nula ______ (
def p_func_inicio(p):      
    '''func_inicio : NULA ID PARENTIZQ'''

    # nos avisa que va a modificar el ambito actual y errores
    global ambitoActual, hayErrores

    # guarda el nombre de la funcion
    nombre = p[2]

    # revisa si la funcion ya esta declarada
    # si esta, imprime un error
    if nombre in directorioFunciones:
        print(f"error semantico: funcion '{nombre}' ya declarada")
        hayErrores = True


    # si no esta, la almacena en el directorio de funciones
    # 'inicio' guarda la direccion (indice) del primer cuadruplo del cuerpo de
    # la funcion. como los parametros y las vars no generan cuadruplos, la
    # longitud actual de la fila coincide con ese primer cuadruplo del cuerpo.
    else:
        directorioFunciones[nombre] = {
            'tipo': 'nula',
            'params': [],
            'tablaVariables': {},
            'inicio': len(filaCuadruplos) #agregue esto
        }
    ambitoActual = nombre
    p[0] = nombre

# reconoce la funcion entera
def p_func(p): 

    # ej. (la funcion pasada) (entero a - params) ( ) parentder) (vars) (cuerpo {x=5+3}) ;
    '''func : func_inicio params_opc PARENTDER vars_opc cuerpo PUNTOCOMA'''
    
    global ambitoActual # --> nos avisa q lo vamos a modificar

    # genera el cuadruplo de fin de funcion
    generarCuadruplo('ENDFUNC', None, None, None)  #agregue esto

    # nos regresa al global
    ambitoActual = 'global'
    p[0] = ('func', p[1])

# reconoce los parametros de la funcion
def p_params_opc_con(p): 

    # esto reconoce los parametros de la funcion
    # ej. entero es el tipo, ID la letra, params_p es la lista de parametros
    '''params_opc : tipo ID params_p'''

    # registra el primer parametro en la tabla de variables
    registrarVariable(p[2], p[1])

    # si hay mas parametros, los agrega a la lista de parametros
    resto = p[3] if p[3] else []

    # agrega los parametros a la tabla de variables del directorio de funciones
    directorioFunciones[ambitoActual]['params'] = [(p[1], p[2])] + resto

    # son los parametros que siguen
def p_params_p_con(p): 

    # ej. empieza viendo una coma de que hay mas parametros
    # entero es el tipo, ID la letra, params_p es la lista de parametros
    '''params_p : COMA tipo ID params_p'''

    # registra el parametro en la tabla de variables de la funcion
    registrarVariable(p[3], p[2])

    # agarra el resto de los parametros hasta que este vacia
    resto = p[4] if p[4] else []

    # junta el parametro con todos los extras y 
    # los manda a la funcion anterior en p[3]
    p[0] = [(p[2], p[3])] + resto

# freno de recursion de parametros
def p_params_p_vacio(p):
    # si no hay mas parametros, pone empty
    '''params_p : empty'''
    p[0] = []

# ----------------------------------------------------------
# CUERPO Y ESTATUTOS
# Define qué instrucciones pueden aparecer dentro de { }.
# Usa recursión para aceptar varias instrucciones.

def p_cuerpo(p): 
    '''cuerpo : LLAVEIZQ estatutos LLAVEDER''' 
    p[0] = ('cuerpo', p[2])

def p_estatutos_con(p):
    # aqui usa recursion
    '''estatutos : estatuto estatutos'''
    pass

def p_estatutos_vacio(p): 
    '''estatutos : empty'''
    pass

def p_estatuto(p):
    '''estatuto : asigna
                | condicion
                | ciclo
                | llamada
                | imprime'''
    p[0] = p[1]

# ----------------------------------------------------------
# ASIGNACIONES
# Verifica tipos y genera cuádruplo de asignación.
# Ejemplo: x = a + 5;

def p_asigna(p): 
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
    dir_dest = buscarDireccion(p[1]) if tipoVar else p[1] 
    generarCuadruplo('=', nombre_expr, None, dir_dest)

# ----------------------------------------------------------
# CONDICIONALES IF / ELSE
# Genera GOTOF y GOTO usando la pila de saltos.
# Ejemplo: if (a > 5) { ... } else { ... }

def p_condicion_inicio(p):  
    '''condicion_inicio : SI PARENTIZQ expresion PARENTDER'''
    cond, _ = p[3]
    idx = len(filaCuadruplos)
    generarCuadruplo('GOTOF', cond, None, None)  
    pilaJumps.append(idx)

def p_condicion(p):  
    '''condicion : condicion_inicio cuerpo sino_opc PUNTOCOMA'''
    p[0] = ('condicion',)

def p_sino_opc_con(p):  
    '''sino_opc : sino_inicio cuerpo'''
    idx_goto = pilaJumps.pop()
    filaCuadruplos[idx_goto] = (
        filaCuadruplos[idx_goto][0],
        filaCuadruplos[idx_goto][1],
        filaCuadruplos[idx_goto][2],
        len(filaCuadruplos)
    )
    p[0] = ('sino',)

def p_sino_inicio(p):  
    '''sino_inicio : SINO'''
    idx_gotof = pilaJumps.pop()
    filaCuadruplos[idx_gotof] = (
        filaCuadruplos[idx_gotof][0],
        filaCuadruplos[idx_gotof][1],
        filaCuadruplos[idx_gotof][2],
        len(filaCuadruplos)
    )
    idx_goto = len(filaCuadruplos)
    generarCuadruplo('GOTO', None, None, None)
    pilaJumps.append(idx_goto)

def p_sino_opc_vacio(p):  
    '''sino_opc : empty'''
    idx_gotof = pilaJumps.pop()
    filaCuadruplos[idx_gotof] = (
        filaCuadruplos[idx_gotof][0],
        filaCuadruplos[idx_gotof][1],
        filaCuadruplos[idx_gotof][2],
        len(filaCuadruplos)
    )
    p[0] = None

# ----------------------------------------------------------
# CICLOS WHILE
# Guarda el inicio del ciclo, genera GOTOF
# y al final regresa con GOTO.

def p_ciclo_inicio(p): 
    '''ciclo_inicio : MIENTRAS'''
    pilaJumps.append(len(filaCuadruplos))  

def p_ciclo_cond(p): 
    '''ciclo_cond : ciclo_inicio PARENTIZQ expresion PARENTDER'''
    cond, _ = p[3]
    idx = len(filaCuadruplos)
    generarCuadruplo('GOTOF', cond, None, None) 
    pilaJumps.append(idx)

def p_ciclo(p):  
    '''ciclo : ciclo_cond HAZ cuerpo PUNTOCOMA'''
    idx_gotof = pilaJumps.pop()
    idx_regreso = pilaJumps.pop()
    generarCuadruplo('GOTO', None, None, idx_regreso)
    filaCuadruplos[idx_gotof] = (
        filaCuadruplos[idx_gotof][0],
        filaCuadruplos[idx_gotof][1],
        filaCuadruplos[idx_gotof][2],
        len(filaCuadruplos)
    )
    p[0] = ('ciclo',)

# ----------------------------------------------------------
# LLAMADAS A FUNCIONES
# Genera ERA, PARAM y GOSUB.
# Usa recursión para aceptar varios argumentos.

def p_llamada_inicio(p):  
    '''llamada_inicio : ID PARENTIZQ'''
    global hayErrores
    if p[1] not in directorioFunciones:
        print(f"error semantico: funcion '{p[1]}' no declarada (linea {p.lineno(1)})")
        hayErrores = True
    else:


        # ERA: reserva el espacio (registro de activacion) de la funcion.
        # el destino lleva la direccion de inicio de la funcion para que la
        # maquina virtual sepa cual bloque de codigo va a ejecutar.
        generarCuadruplo('ERA', p[1], None, directorioFunciones[p[1]].get('inicio')) #agregue esto
    # abre un contexto de paso de parametros: lleva la funcion y el contador
    # del parametro que se esta pasando (empieza en 0)
    pilaParams.append({'func': p[1], 'k': 0}) #agregue esto
    p[0] = p[1] 

def p_llamada(p):  
    '''llamada : llamada_inicio args_opc PARENTDER PUNTOCOMA'''
    global hayErrores
    ctx = pilaParams.pop()
    func = p[1]

    # verifica que el numero de argumentos coincida con los parametros
    if func in directorioFunciones:
        n_params = len(directorioFunciones[func].get('params', []))
        if ctx['k'] != n_params:
            print(f"error semantico: la funcion '{func}' espera {n_params} "
                  f"argumento(s) pero recibio {ctx['k']}")
            hayErrores = True

    # GOSUB: salta al cuerpo de la funcion. el destino es la direccion de
    # inicio guardada en el directorio de funciones.
    destino = directorioFunciones[func].get('inicio') if func in directorioFunciones else None #agregue esto
    generarCuadruplo('GOSUB', func, None, destino)  #agregue esto
    p[0] = ('llamada', func)

def p_args_opc_con(p):  
    '''args_opc : arg args_p'''
    pass

def p_args_opc_vacio(p):
    '''args_opc : empty'''
    pass

def p_arg(p): 
    '''arg : expresion'''
    global hayErrores
    val, tipo = p[1]

    # contexto de la llamada actual (que funcion y que numero de parametro)
    ctx = pilaParams[-1]
    func = ctx['func']
    k = ctx['k']

    # calcula la direccion virtual del parametro k de la funcion para mandar
    # el argumento exactamente a ese espacio dentro del registro de activacion
    dir_param = None #agregue esto
    if func in directorioFunciones:
        params = directorioFunciones[func].get('params', [])
        if k < len(params):
            nombre_param = params[k][1]
            dir_param = directorioFunciones[func]['tablaVariables'][nombre_param]['dir'] #agregue esto
        else:
            print(f"error semantico: demasiados argumentos para '{func}'")
            hayErrores = True

    # PARAM: copia el valor del argumento (val) al espacio del parametro.
    # el destino lleva la direccion virtual del parametro destino.
    generarCuadruplo('PARAM', val, None, dir_param) #agregue esto
    ctx['k'] += 1
    p[0] = p[1]

def p_args_p_con(p): 
    # aqui usa recursion para aceptar varios argumentos separados por coma
    '''args_p : COMA arg args_p'''
    pass

def p_args_p_vacio(p): 
    '''args_p : empty'''
    pass

# ----------------------------------------------------------
# IMPRESIÓN
# Maneja escribe(expresion) y escribe("texto").

def p_imprime(p): 
    '''imprime : ESCRIBE PARENTIZQ item_imp items_p PARENTDER PUNTOCOMA'''
    items = [p[3]] + p[4] #agregue esto
    for it in items: #agregue esto
        generarCuadruplo('print', it, None, None) #agregue esto
    p[0] = ('imprime',)

def p_item_imp_expr(p): 
    '''item_imp : expresion'''
    p[0] = p[1][0]

def p_item_imp_letrero(p): 
    '''item_imp : LETRERO'''
    p[0] = f'"{p[1]}"'

def p_items_p_con(p): 
    '''items_p : COMA item_imp items_p'''
    p[0] = [p[2]] + p[3] #agregue esto

def p_items_p_vacio(p):
    '''items_p : empty'''
    p[0] = [] #agregue esto

# ----------------------------------------------------------
# EXPRESIONES RELACIONALES
# Maneja >, <, != y ==.
# Genera temporales booleanos.

def p_expresion_mayor(p): 
    '''expresion : exp MAYORQUE exp'''
    temp = nuevoTempTipado('bool')  
    generarCuadruplo('>', p[1][0], p[3][0], temp)
    pilaOperandos.append(temp); pilaTipos.append('bool')
    p[0] = (temp, 'bool')

def p_expresion_menor(p): 
    '''expresion : exp MENORQUE exp'''
    temp = nuevoTempTipado('bool')  
    generarCuadruplo('<', p[1][0], p[3][0], temp)
    pilaOperandos.append(temp); pilaTipos.append('bool')
    p[0] = (temp, 'bool')

def p_expresion_diferente(p):    
    '''expresion : exp DIFERENTE exp'''
    temp = nuevoTempTipado('bool') 
    generarCuadruplo('!=', p[1][0], p[3][0], temp)
    pilaOperandos.append(temp); pilaTipos.append('bool')
    p[0] = (temp, 'bool')

def p_expresion_igual(p): 
    '''expresion : exp IGUALIGUAL exp'''
    temp = nuevoTempTipado('bool')  
    generarCuadruplo('==', p[1][0], p[3][0], temp)
    pilaOperandos.append(temp); pilaTipos.append('bool')
    p[0] = (temp, 'bool')

def p_expresion_exp(p): 
    '''expresion : exp'''
    p[0] = p[1]

# ----------------------------------------------------------
# EXPRESIONES ARITMÉTICAS
# Aquí está la recursión izquierda para respetar precedencia:
# exp maneja + y -
# termino maneja * y /
# factor maneja paréntesis, signos, IDs y constantes.

def p_exp_suma(p): #(solo maneja +)
    # aqui usa recursion
        #izquierda
    '''exp : exp SUMA termino''' # usa termino, respeta precedencia.
    global hayErrores
    tipo = tiporesultante(p[1][1], '+', p[3][1])
    if tipo is None and p[1][1] and p[3][1]:
        print(f"error semantico: '+' invalido entre '{p[1][1]}' y '{p[3][1]}'")
        hayErrores = True
        tipo = p[1][1]
    temp = nuevoTempTipado(tipo) 
    generarCuadruplo('+', p[1][0], p[3][0], temp)
    pilaOperandos.append(temp); pilaTipos.append(tipo)
    p[0] = (temp, tipo)

def p_exp_resta(p): #(solo maneja -)
    # aqui usa recursion
    '''exp : exp RESTA termino'''
    global hayErrores
    tipo = tiporesultante(p[1][1], '-', p[3][1])
    if tipo is None and p[1][1] and p[3][1]:
        print(f"error semantico: '-' invalido entre '{p[1][1]}' y '{p[3][1]}'")
        hayErrores = True
        tipo = p[1][1]
    temp = nuevoTempTipado(tipo) 

    generarCuadruplo('-', p[1][0], p[3][0], temp)
    pilaOperandos.append(temp); pilaTipos.append(tipo)
    p[0] = (temp, tipo)

#precedencia
def p_exp_termino(p): #avisa resuelve primero termino
    '''exp : termino''' 
    p[0] = p[1]

def p_termino_mult(p): #(solo maneja *)
    # aqui usa recursion
        #izquierda
    '''termino : termino MULTIPLICACION factor'''
    global hayErrores
    tipo = tiporesultante(p[1][1], '*', p[3][1])
    if tipo is None and p[1][1] and p[3][1]:
        print(f"error semantico: '*' invalido entre '{p[1][1]}' y '{p[3][1]}'")
        hayErrores = True
        tipo = p[1][1]
    temp = nuevoTempTipado(tipo) 
    generarCuadruplo('*', p[1][0], p[3][0], temp)
    pilaOperandos.append(temp); pilaTipos.append(tipo)
    p[0] = (temp, tipo)

def p_termino_div(p): #(solo maneja /)
    '''termino : termino DIVISION factor'''
    # aqui usa recursion
    global hayErrores
    tipo = tiporesultante(p[1][1], '/', p[3][1])
    if tipo is None and p[1][1] and p[3][1]:
        print(f"error semantico: '/' invalido entre '{p[1][1]}' y '{p[3][1]}'")
        hayErrores = True
        tipo = p[1][1]
    temp = nuevoTempTipado(tipo) 
    generarCuadruplo('/', p[1][0], p[3][0], temp)
    pilaOperandos.append(temp); pilaTipos.append(tipo)
    p[0] = (temp, tipo)

def p_termino_factor(p): 
#si no hay multiplicación ni división, 
# entonces el término es simplemente un factor.
    '''termino : factor'''
    p[0] = p[1]

# ----------------------------------------------------------
# FACTORES, OPERANDOS Y CONSTANTES
# Maneja paréntesis, signos, variables y constantes.
# También guarda constantes en la tabla de constantes.

def p_factor_parentesis(p): 
    '''factor : PARENTIZQ expresion PARENTDER'''
    p[0] = p[2]

def p_factor_operando(p): 
    '''factor : signo_opc operando'''
    nombre, tipo = p[2]
    if p[1] is not None:


        temp = nuevoTempTipado(tipo)
        generarCuadruplo(p[1], nombre, None, temp)
        pilaOperandos.append(temp); pilaTipos.append(tipo)
        p[0] = (temp, tipo)
    else:
        p[0] = p[2]

def p_signo_opc_pos(p): 
    '''signo_opc : SUMA'''
    p[0] = '+'

def p_signo_opc_neg(p): 
    '''signo_opc : RESTA'''
    p[0] = '-'

def p_signo_opc_vacio(p):
    '''signo_opc : empty'''
    p[0] = None

def p_operando_cte(p): 
    '''operando : cte'''
    p[0] = p[1]

def p_operando_id(p):
    '''operando : ID'''
    global hayErrores
    tipo = buscarVariable(p[1])
    if tipo is None:
        print(f"error semantico: variable '{p[1]}' no declarada (linea {p.lineno(1)})")
        hayErrores = True
        tipo = 'error'
    dir_ = buscarDireccion(p[1]) if tipo != 'error' else p[1]
    pilaOperandos.append(dir_); pilaTipos.append(tipo) 
    p[0] = (dir_, tipo) 

def p_cte_ent(p): 
    '''cte : CTE_ENT'''
    clave = str(p[1])
    if clave not in tablaConstantes:
        tablaConstantes[clave] = asignarDireccion('constante', 'entero')
        valoresConstantes[tablaConstantes[clave]] = p[1]
    dir_ = tablaConstantes[clave]
    pilaOperandos.append(dir_); pilaTipos.append('entero')
    p[0] = (dir_, 'entero')


def p_cte_flot(p): 
    '''cte : CTE_FLOT'''
    clave = str(p[1])
    if clave not in tablaConstantes:
        tablaConstantes[clave] = asignarDireccion('constante', 'flotante')
        valoresConstantes[tablaConstantes[clave]] = p[1]
    dir_ = tablaConstantes[clave]
    pilaOperandos.append(dir_); pilaTipos.append('flotante')
    p[0] = (dir_, 'flotante')

# ----------------------------------------------------------
# REGLAS AUXILIARES Y MANEJO DE ERRORES
# empty permite reglas opcionales.
# p_error reporta errores sintácticos.

def p_empty(p):
    '''empty :'''
    p[0] = None

def p_error(p):
    global hayErrores #agregue esto
    hayErrores = True #agregue esto
    if p:
        print(f"Error sintactico: token inesperado '{p.value}' en linea {p.lineno}")
    else:
        print("Error sintactico: fin de archivo inesperado")

# ----------------------------------------------------------
# ==========================================================



#----------------------------------------------------------
# GENERACIÓN DE TEMPORALES Y CUÁDRUPLOS

# Crea direcciones temporales y guarda operaciones
# en la fila de cuádruplos.
def nuevoTemp(): 
    contTemp[0] += 1
    return f"t{contTemp[0]}"

def nuevoTempTipado(tipo): 
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

# ----------------------------------------------------------



# ----------------------------------------------------------
# FUNCIONES DE IMPRESIÓN / DEBUG

# Imprimen cuádruplos y tabla de constantes.
def imprimirCuadruplos():
    print("=" * 45)
    print(" FILA DE CUADRUPLOS ".center(45))
    print("=" * 45)
    print(f"{'#':<5} {'OP':<10} {'IZQ':<10} {'DER':<10} {'RES':<10}")
    print("-" * 45)
    for i, (op, izq, der, res) in enumerate(filaCuadruplos): #(entrega #4)
        print(f"{i:<5} {str(op):<10} {str(izq):<10} {str(der):<10} {str(res):<10}")
    print("-" * 45)

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
# ----------------------------------------------------------



# ----------------------------------------------------------
# CONSTRUCCIÓN DEL PARSER

parser = yacc.yacc(errorlog=yacc.NullLogger())
# ----------------------------------------------------------



# ----------------------------------------------------------
# FUNCIÓN DE COMPILACIÓN REUTILIZABLE
# Reinicia todas las estructuras, analiza el codigo fuente y, si no hubo
# errores, regresa todo lo que la maquina virtual necesita para ejecutar:
# la fila de cuadruplos, el directorio de funciones, la tabla de constantes
# (direccion -> valor) y el nombre del programa.

def compilar(codigo):
    global ambitoActual, hayErrores

    # reinicia todas las estructuras globales del compilador
    directorioFunciones.clear()
    ambitoActual = None
    hayErrores = False
    filaCuadruplos.clear()
    pilaOperandos.clear()
    pilaTipos.clear()
    pilaOperadores.clear()
    pilaJumps.clear()
    pilaParams.clear()
    contTemp[0] = 0
    resetDirecciones()

    # analiza el codigo y genera el codigo intermedio (cuadruplos)
    parser.parse(codigo, lexer=lexer.clone())

    # arma el paquete de salida para la maquina virtual
    return {
        'ok': not hayErrores,
        'cuadruplos': list(filaCuadruplos),
        'directorio': directorioFunciones,
        'constantes': dict(valoresConstantes),
    }
# ----------------------------------------------------------



# ----------------------------------------------------------
# MAIN

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

    print(f"\nAnalizando: {archivo}")
    compilar(codigo)

    if not hayErrores:
        print("programa valido")
        imprimirDirectorio()
        imprimirConstantes()  
        imprimirCuadruplos()
    else:
        print("programa tiene errores")
# ---------------------------------------------------------

