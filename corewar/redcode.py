# coding: utf-8

from copy import copy
import re

__all__ = ['parse', 'DAT', 'MOV', 'ADD', 'SUB', 'MUL', 'DIV', 'MOD', 'JMP',
           'JMZ', 'JMN', 'DJN', 'SPL', 'SLT', 'CMP', 'SEQ', 'SNE', 'NOP',
           'M_A', 'M_B', 'M_AB', 'M_BA', 'M_F', 'M_X', 'M_I', 'IMMEDIATE',
           'DIRECT', 'INDIRECT_B', 'PREDEC_B', 'POSTINC_B', 'INDIRECT_A',
           'PREDEC_A', 'POSTINC_A', 'Instruction', 'Warrior']

DAT = 0     # terminate process
MOV = 1     # move from A to B
ADD = 2     # add A to B, store result in B
SUB = 3     # subtract A from B, store result in B
MUL = 4     # multiply A by B, store result in B
DIV = 5     # divide B by A, store result in B if A <> 0, else terminate
MOD = 6     # divide B by A, store remainder in B if A <> 0, else terminate
JMP = 7     # transfer execution to A
JMZ = 8     # transfer execution to A if B is zero
JMN = 9     # transfer execution to A if B is non-zero
DJN = 10    # decrement B, if B is non-zero, transfer execution to A
SPL = 11    # split off process to A
SLT = 12    # skip next instruction if A is less than B
CMP = 13    # same as SEQ
SEQ = 14    # Skip next instruction if A is equal to B
SNE = 15    # Skip next instruction if A is not equal to B
NOP = 16    # No operation

# Instructions read and write A-fields.
M_A = 0

# Instructions read and write B-fields.
M_B = 1

# Instructions read the A-field of the A-instruction and the B-field of the
# B-instruction and write to B-fields.
M_AB = 2

# Instructions read the B-field of the A-instruction and the A-field of the
# B-instruction and write to A-fields.
M_BA = 3

# Instructions read both A- and B-fields of the A and B-instruction and
# write to both A- and B-fields (A to A and B to B).
M_F = 4

# Instructions read both A- and B-fields of the A and B-instruction  and
# write  to both A- and B-fields exchanging fields (A to B and B to A).
M_X = 5

# Instructions read and write entire instructions.
M_I = 6

IMMEDIATE = 0   # immediate
DIRECT = 1      # direct
INDIRECT_B = 2  # indirect using B-field
PREDEC_B  = 3   # predecrement indirect using B-field
POSTINC_B = 4   # postincrement indirect using B-field
INDIRECT_A = 5  # indirect using A-field
PREDEC_A = 6    # predecrement indirect using A-field
POSTINC_A = 7   # postincrement indirect using A-field

INSTRUCTION_REGEX = re.compile(r'([a-z]{3})'  # opcode
                               r'(?:\s*\.\s*([abfxi]{1,2}))?' # optional modifier
                               r'(?:\s*([#\$\*@\{<\}>])?\s*([^,$]+))?' # optional first value
                               r'(?:\s*,\s*([#\$\*@\{<\}>])?\s*(.+))?$', # optional second value
                               re.I)

OPCODES = {'DAT': DAT, 'MOV': MOV, 'ADD': ADD, 'SUB': SUB, 'MUL': MUL,
           'DIV': DIV, 'MOD': MOD, 'JMP': JMP, 'JMZ': JMZ, 'JMN': JMN,
           'DJN': DJN, 'SPL': SPL, 'SLT': SLT, 'CMP': CMP, 'SEQ': SEQ,
           'SNE': SNE, 'NOP': NOP}

reserved_words = {'DAT', 'MOV', 'ADD', 'SUB', 'MUL', 'DIV', 'MOD', 'JMP',
                  'JMZ', 'JMN', 'DJN', 'SPL', 'SLT', 'CMP', 'SEQ', 'SNE',
                  'NOP', 'FOR', 'ROF', 'ORG', 'END'}

MODIFIERS = {'A': M_A, 'B': M_B, 'AB': M_AB, 'BA': M_BA, 'F': M_F, 'X': M_X,
             'I': M_I}

MODES = { '#': IMMEDIATE, '$': DIRECT, '@': INDIRECT_B, '<': PREDEC_B,
          '>': POSTINC_B, '*': INDIRECT_A, '{': PREDEC_A, '}': POSTINC_A }

# ICWS'88 to ICWS'94 Conversion
# The default modifier for ICWS'88 emulation is determined according to the
# table below.
#        Opcode                             A-mode    B-mode    modifier
DEFAULT_MODIFIERS = {
        ('DAT', 'NOP')                 : {('#$@<>{}', '#$@<>{}'): 'F'},
        ('MOV','CMP')                  : {('#'     , '#$@<>'): 'AB',
                                          ('$@<>' , '#'    ): 'B' ,
                                          ('$@<>*{}' , '$@<>*{}' ): 'I'},
        ('ADD','SUB','MUL','DIV','MOD'): {('#'    , '#$@<>'): 'AB',
                                          ('$@<>' , '#'    ): 'B' ,
                                          ('$@<>' , '$@<>*{}' ): 'F'},
        ('SLT', 'SEQ', 'SNE')          : {('#'    , '#$@<>'): 'AB',
                                          ('$@<>' , '#$@<>'): 'B'},
        ('JMP','JMZ','JMN','DJN','SPL'): {('#$@<>{}', '#$@<>{}'): 'B'}
    }

# Transform the readable form above, into the internal representation
DEFAULT_MODIFIERS = dict((tuple(OPCODES[opcode] for opcode in opcodes),
                         dict(((tuple(MODES[a] for a in ab_modes[0]),
                                tuple(MODES[b] for b in ab_modes[1])),
                               MODIFIERS[modifier]) for ab_modes, modifier in ab_modes_modifiers.items()))
                         for opcodes, ab_modes_modifiers in DEFAULT_MODIFIERS.items())

class Warrior(object):
    "An encapsulation of a Redcode Warrior, with instructions and meta-data"

    def __init__(self, name='Unnamed', author='Anonymous', date=None,
                 version=None, strategy=None, start=0, instructions=[]):
        self.name = name
        self.author = author
        self.date = date
        self.version = version
        self.strategy = strategy
        self.start = start
        self.instructions = instructions

    def __iter__(self):
        return iter(self.instructions)

    def __len__(self):
        return len(self.instructions)

    def __repr__(self):
        return "<Warrior name=%s %d instructions>" % (self.name, len(self.instructions))
    
    def decompile(self):
        code = ';redcode\n' + 'ORG %d' % self.start + '\n'
        for instruction in self.instructions:
            code = code + str(instruction) + '\n'
        return code

class Instruction(object):
    "An encapsulation of a Redcode instruction."

    def __init__(self, opcode, modifier=None, a_mode=None, a_number=0,
                 b_mode=None, b_number=0):
        self.opcode = OPCODES[opcode.upper()] if isinstance(opcode, str) else opcode
        if a_mode is not None:
            self.a_mode = MODES[a_mode] if isinstance(a_mode, str) else a_mode
        else:
            self.a_mode = DIRECT
        if b_mode is not None:
            self.b_mode = MODES[b_mode] if isinstance(b_mode, str) else b_mode
        else:
            self.b_mode = IMMEDIATE if self.opcode == DAT and a_number != None else DIRECT
        self._a_number = a_number if a_number else 0
        self._b_number = b_number if b_number else 0

        # this should be last, to decide on the default modifier
        if modifier is not None:
            self.modifier = MODIFIERS[modifier.upper()] if isinstance(modifier, str) else modifier
        else:
            self.modifier = self.default_modifier()

        self.core = None

    def core_binded(self, core):
        """Return a copy of this instruction binded to a Core.
        """
        instruction = copy(self)
        instruction.core = core
        return instruction

    def default_modifier(self):
        for opcodes, modes_modifiers in DEFAULT_MODIFIERS.items():
            if self.opcode in opcodes:
                for ab_modes, modifier in modes_modifiers.items():
                    a_modes, b_modes = ab_modes
                    if self.a_mode in a_modes and self.b_mode in b_modes:
                        return modifier
        raise ValueError(
            "Error getting default modifier for opcode %s with A mode %s and B mode %s" % (self.opcode, self.a_mode, self.b_mode))

    @property
    def a_number(self):
        return self._a_number

    @property
    def b_number(self):
        return self._b_number

    @a_number.setter
    def a_number(self, number):
        self._a_number = self.core.trim_signed(number) if self.core else number

    @b_number.setter
    def b_number(self, number):
        self._b_number = self.core.trim_signed(number) if self.core else number

    def __eq__(self, other):
        return (self.opcode == other.opcode and self.modifier == other.modifier and
                self.a_mode == other.a_mode and self.a_number == other.a_number and
                self.b_mode == other.b_mode and self.b_number == other.b_number)

    def __ne__(self, other):
        return not self == other

    def __str__(self):
        # inverse lookup the instruction values
        opcode   = next(key for key,value in OPCODES.items() if value==self.opcode)
        modifier = next(key for key,value in MODIFIERS.items() if value==self.modifier)
        a_mode   = next(key for key,value in MODES.items() if value==self.a_mode)
        b_mode   = next(key for key,value in MODES.items() if value==self.b_mode)

        return "%s.%s %s %s, %s %s" % (opcode,
                                       modifier.ljust(2),
                                       a_mode,
                                       str(self.a_number).rjust(5),
                                       b_mode,
                                       str(self.b_number).rjust(5))

    def __repr__(self):
        return "<%s>" % self

# Context and management class for parsing a warrior's code
class Parser(object):
    # A bunch of global regexes
    blank_line_m = re.compile(r'^\s*$')
    start_comment_m = re.compile(r'^;.+$')
    inline_comment_m = re.compile(r'^([^;]*)\s*;')
    redcode_m = re.compile(r'^;redcode\w*$', re.I)
    name_m = re.compile(r'^;name\s+(.+)$', re.I)
    author_m = re.compile(r'^;author\s+(.+)$', re.I)
    date_m = re.compile(r'^;date\s+(.+)$', re.I)
    version_m = re.compile(r'^;version\s+(.+)$', re.I)
    strategy_m = re.compile(r'^;strat(?:egy)?\s+(.+)$', re.I)
    assert_m = re.compile(r'^;assert\s+(.+)$', re.I)
    c_and_m = re.compile(r'(&&)')
    c_or_m = re.compile(r'(\|\|)')
    equ_m = re.compile(r'^(\w+)\s+EQU\s+(.+)\s*$', re.I)
    label_m = re.compile(r'^(\w+)(.*)$')
    for_m = re.compile(r'^FOR(?:\s+([^\s]+))?$', re.I)
    rof_m = re.compile(r'^ROF\s*$', re.I)
    leading_zeroes_m = re.compile(r'^0+(\d+)$')
    
    def __init__(self, environment, equ_defs = {}):
        # use a clone of environment because we're going to add names to it
        self.environment = copy(environment)
        # Loose strategy notes - will be concatenated later
        self.strategy = []
        self.processing_complete = False
        self.processing_started = False
        self.instructions = []
        self.labels = {}
        self.equ_defs = equ_defs
        
        self.start = 0
        self.name='Unnamed'
        self.author='Anonymous'
    
    def process_directives(self, line, line_num):
        m = Parser.redcode_m.match(line)
        if m:
            if self.processing_started:
                # stop reading, found second ;redcode
                self.processing_complete = True
            else:
                # first ;redcode, so start doing stuff
                self.processing_started = True
            return

        m = Parser.name_m.match(line)
        if m:
            self.name = m.group(1).strip()
            return

        m = Parser.author_m.match(line)
        if m:
            self.author = m.group(1).strip()
            return

        m = Parser.date_m.match(line)
        if m:
            self.date = m.group(1).strip()
            return

        m = Parser.version_m.match(line)
        if m:
            self.version = m.group(1).strip()
            return

        m = Parser.strategy_m.match(line)
        if m:
            self.strategy.append(m.group(1).strip())
            return

        # Test if assert expression evaluates to true
        m = Parser.assert_m.match(line)
        if m:
            assertion = m.group(1)
            assertion = Parser.c_and_m.sub(' and ', assertion)
            assertion = Parser.c_or_m.sub(' or ', assertion)
            if not eval(assertion, self.environment):
                raise AssertionError("Assertion failed: %s, line %d" % (line, line_num))
            return

    def strip_ignored(self, line):
        # ignore comments
        m = Parser.start_comment_m.match(line)
        if m:
            # strip comment from the line
            line = m.group(1)
        return line.strip()

    def process_labels(self, line):
        # Keep matching the first word until it's no label anymore
        # TODO: handle labels on lines which do not have code
        #print(line)
        while True:
            m = Parser.label_m.match(line)
            if m:
                label_candidate = m.group(1)
                #print('label candidate: %s' % label_candidate)
                if label_candidate.upper() not in reserved_words:
                    self.labels[label_candidate] = len(self.instructions)
                    #print('line %d' % self.labels[label_candidate])

                    # strip label off and keep looking
                    line = m.group(2)
                    continue
            # its an instruction, not label. proceed OR no match, probably
            # a all-value-omitted instruction.
            return line.strip()

    def process_equ(self, line):
        # Match EQU defines
        m = Parser.equ_m.match(line)
        if m:
            name, value = m.groups()
            # Some competitors like to use forward declarations or labels
            # in their EQU statements, so we're going to need to save
            # definitions we can't evaluate until later.
            # Evaluate all the ones we can, in the meantime.
            try:
                self.environment[name] = eval(value, self.environment)
                #print('%s: %d' % (name, self.environment[name]))
            except (SyntaxError, NameError) as err:
                #print('Could not evaluate rewrite rule: %s' % line)
                self.equ_defs[name] = value
            return True
        else:
            return False

    def extract_instruction(self, line, line_num):
        # Finally, look for instructions
        m = INSTRUCTION_REGEX.match(line)
        if not m:
            raise ValueError('Error at line %d: expected instruction in expression: "%s"' %
                             (line_num, line))
        else:
            opcode, modifier, a_mode, a_number, b_mode, b_number = m.groups()

            if opcode.upper() not in OPCODES:
                raise ValueError('Invalid opcode: %s in line %d: "%s"' %
                                 (opcode, line_num, line))
            if modifier is not None and modifier.upper() not in MODIFIERS:
                raise ValueError('Invalid modifier: %s in line %d: "%s"' %
                                 (modifier, line_num, line))

            # add parts of instruction read. the fields should be parsed
            # as an expression in the second pass, to expand labels
            self.instructions.append(Instruction(opcode, modifier,
                                                    a_mode, a_number,
                                                    b_mode, b_number))

    def parse(self, code):
        in_for_loop = False
        # first pass
        for n, line in enumerate(code):
            line = line.strip()
            # process info comments
            if Parser.start_comment_m.match(line):
                self.process_directives(line, n)
                if self.processing_complete:
                    return
                continue

            #print(line)
            #line = self.strip_ignored(line)
            # ignore other comments
            m = re.match(r'^([^;]*)\s*;', line)
            if m:
                # strip comment from the line
                line = m.group(1)
            
            if Parser.blank_line_m.match(line):
                continue

            # Match ORG
            m = re.match(r'^ORG\s+(.+)\s*$', line, re.I)
            if m:
                self.start = m.group(1)
                continue

            # Match END
            m = re.match(r'^END(?:\s+([^\s]+))?$', line, re.I)
            if m:
                if m.group(1):
                    self.start = m.group(1)
                return # stop processing (end of redcode)

            if self.process_equ(line):
                continue

            #TODO: This has a loop with a boundary condition, which we want to change.
            line = self.process_labels(line)
            
            # Match FOR pseudo-opcode
            # "for" is a PITA to implement when iterating over lines like this.
            # Lots of human code uses it, though, so we have to do it.
            m = Parser.for_m.match(line)
            if m:
                if m.group(1):
                    #print('entering for expansion')
                    for_copy_expression = m.group(1)
                    in_for_loop = True
                    for_loop_lines = []
                    continue

            if in_for_loop:
                m = Parser.rof_m.match(line)
                if m:
                    #print('end of for expansion')
                    sub_parser = Parser(self.environment, equ_defs=self.equ_defs)
                    sub_parser.parse(for_loop_lines)
                    ops = sub_parser.instructions
                    expand_count = eval(for_copy_expression, self.environment, {'CURLINE': len(self.instructions)})
                    #print('expanding for loop %d times' % expand_count)
                    self.instructions.extend(ops * expand_count)
                    in_for_loop = False
                else:
                    for_loop_lines.append(line)
                continue
            
            if len(line) > 0:
                self.extract_instruction(line, n)

        # evaluate start expression
        if isinstance(self.start, str):
            self.start = eval(self.start, self.environment, self.labels)

        # second pass
        for n, instruction in enumerate(self.instructions):
            try:
                # create a dictionary of relative labels addresses to be used as a local
                # eval environment
                relative_labels = dict((name, address-n) for name, address in self.labels.items())
                relative_labels['CURLINE'] = n
                # Expand EQU statements which use labels
                for identifier, rule in self.equ_defs.items():
                    relative_labels[identifier] = eval(rule, self.environment, relative_labels)

                # evaluate instruction fields using global environment and labels
                if isinstance(instruction.a_number, str):
                    instruction.a_number = self.canonicalize(instruction.a_number, relative_labels)
                if isinstance(instruction.b_number, str):
                    instruction.b_number = self.canonicalize(instruction.b_number, relative_labels)
            except (SyntaxError, NameError) as err:
                print(err)
                raise ValueError('Error while evaluating instruction %s' % instruction)
    
    # This function exists because some clowns prefix zeroes onto their numbers.
    def canonicalize(self, expression, relative_labels):
        m = Parser.leading_zeroes_m.match(expression)
        if m:
            return int(m.group(1))
        else:
            return eval(expression, self.environment, relative_labels)

    def create_warrior(self):
        #TODO: is stub
        start = self.start
        if isinstance(start, str):
            start = eval(start, self.environment, self.labels)

        return Warrior(name=self.name, author=self.author, strategy='\n'.join(self.strategy), start=start, instructions=self.instructions)

def parse(input, env={}):
    parser = Parser(env)
    parser.parse(input)
    return parser.create_warrior()
