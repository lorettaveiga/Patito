#para correr:
#python3 maquinavirtual.py pruebas/archivo

import sys
from patito import compilar

RANGO_GLOBAL    = (1000, 1999)
RANGO_CONSTANTE = (2000, 2999)
RANGO_LOCAL     = (3000, 3999)
RANGO_TEMPORAL  = (4000, 4999)

class MaquinaVirtual:
    def __init__(self, paquete):
        self.cuadruplos       = paquete['cuadruplos']
        self.directorio       = paquete['directorio']
        self.memoriaGlobal    = {}
        self.memoriaConstante = dict(paquete['constantes'])
        self.contextoActual   = {'local': {}, 'temporal': {}}
        self.pilaLlamadas     = []
        self.arPendiente      = None
        self.ip               = 0

    def leer(self, dir_):
        d = int(dir_)
        if RANGO_GLOBAL[0] <= d <= RANGO_GLOBAL[1]:
            return self.memoriaGlobal.get(d)
        if RANGO_CONSTANTE[0] <= d <= RANGO_CONSTANTE[1]:
            return self.memoriaConstante.get(d)
        if RANGO_LOCAL[0] <= d <= RANGO_LOCAL[1]:
            return self.contextoActual['local'].get(d)
        if RANGO_TEMPORAL[0] <= d <= RANGO_TEMPORAL[1]:
            return self.contextoActual['temporal'].get(d)
        return None

    def escribir(self, dir_, valor):
        d = int(dir_)
        if RANGO_GLOBAL[0] <= d <= RANGO_GLOBAL[1]:
            self.memoriaGlobal[d] = valor
        elif RANGO_CONSTANTE[0] <= d <= RANGO_CONSTANTE[1]:
            self.memoriaConstante[d] = valor
        elif RANGO_LOCAL[0] <= d <= RANGO_LOCAL[1]:
            self.contextoActual['local'][d] = valor
        elif RANGO_TEMPORAL[0] <= d <= RANGO_TEMPORAL[1]:
            self.contextoActual['temporal'][d] = valor

    def aritmetica(self, op, a, b):
        if op == '+':
            return a + b
        if op == '-':
            return a - b
        if op == '*':
            return a * b
        if op == '/':
            if isinstance(a, int) and isinstance(b, int):
                return int(a / b)
            return a / b

    def relacional(self, op, a, b):
        if op == '>':
            return a > b
        if op == '<':
            return a < b
        if op == '!=':
            return a != b
        if op == '==':
            return a == b

    def ejecutar(self):
        while self.ip < len(self.cuadruplos):
            op, izq, der, res = self.cuadruplos[self.ip]

            if op == 'GOTO':
                self.ip = res
                continue

            elif op == 'GOTOF':
                if not self.leer(izq):
                    self.ip = res
                    continue

            elif op == '=':
                self.escribir(res, self.leer(izq))

            elif op in ('+', '-', '*', '/'):
                if der == '_':
                    valor = self.leer(izq)
                    self.escribir(res, valor if op == '+' else -valor)
                else:
                    self.escribir(res, self.aritmetica(op, self.leer(izq), self.leer(der)))

            elif op in ('>', '<', '!=', '=='):
                self.escribir(res, self.relacional(op, self.leer(izq), self.leer(der)))

            elif op == 'print':
                if isinstance(izq, str) and izq.startswith('"'):
                    print(izq.strip('"'))
                else:
                    print(self.leer(izq))

            elif op == 'ERA':
                self.arPendiente = {'local': {}, 'temporal': {}}

            elif op == 'PARAM':
                self.arPendiente['local'][int(res)] = self.leer(izq)

            elif op == 'GOSUB':
                self.pilaLlamadas.append((self.ip + 1, self.contextoActual))
                self.contextoActual = self.arPendiente
                self.arPendiente = None
                self.ip = res
                continue

            elif op == 'ENDFUNC':
                self.ip, self.contextoActual = self.pilaLlamadas.pop()
                continue

            self.ip += 1

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Uso: python maquinavirtual.py <archivo.patito>")
        sys.exit(1)

    archivo = sys.argv[1]
    try:
        with open(archivo, 'r') as f:
            codigo = f.read()
    except FileNotFoundError:
        print(f"Error: no se encontro el archivo '{archivo}'")
        sys.exit(1)

    paquete = compilar(codigo)

    if not paquete['ok']:
        print("programa tiene errores, no se ejecuta")
        sys.exit(1)

    print("=" * 45)
    print(" EJECUCION (MAQUINA VIRTUAL) ".center(45))
    print("=" * 45)
    vm = MaquinaVirtual(paquete)
    vm.ejecutar()
