'use strict;'
export const Unlambda = class {
    parse (source) {
        let ast = new UnlambdaAST ();
        let node_stack = [ast.top];
        const READ = 0;
        const CHAR = 1;
        const CMMT = 2;
        let state = READ;
        let node = new UnlambdaASTNode ();
        for (let c of source) {
            if (state == READ && c == '#') {
                state = CMMT;
            } else if (state == READ && c.match(/\S/)) {
                const char_map = {
                    '`': UnlambdaASTApply,
                    'e': UnlambdaASTExit,
                    's': UnlambdaASTSubstitute,
                    'k': UnlambdaASTConstant,
                    'i': UnlambdaASTIdentity,
                    'v': UnlambdaASTVoid,
                    'd': UnlambdaASTDelay,
                    'c': UnlambdaASTCallCC,
                    '@': UnlambdaASTRead,
                    '|': UnlambdaASTReprint,
                };
                let type = char_map[c];
                if (type === undefined) {
                    if (c == '.') {
                        node = new UnlambdaASTChar ();
                        state = CHAR;
                    } else if (c == 'r') {
                        node = new UnlambdaASTChar ();
                        node.value = "\n";
                    } else if (c == '?') {
                        node = new UnlambdaASTCompare ();
                        state = CHAR;
                    } else {
                        throw 'Parse error: Unrecognized character '+c+'.'
                    }
                } else {
                    node = new char_map[c] ();
                }
                if (node.id != '!') {
                    let next = node_stack.pop().add(node);
                    if (next !== undefined) node_stack.push(...next);
                }
            } else if (state == CHAR) {
                node.value = c;
                state = READ;
            } else if (state == CMMT) {
                if (c == "\n") state = READ;
            }
        }
        return ast;
    }
}

export const UnlambdaCompiler = class {
    states;
    inits;
    constructor (ast) {
        this.compile(ast);
    }
    compile (ast) {
        this.states = new StateData ();
        this.inits = [];
        for (let node of ast.top.list) {
            if (node instanceof UnlambdaASTApply) {
                this.inits.push(this.compile_node(node));
            }
        }
    }
    compile_node (node) {
        let ptr = this.states.new_call();
        let init = void 8;
        let node_stack = [[node, ptr, void 8, void 8]];
        let left_ptr; let right_ptr;
        while (node_stack.length > 0) {
            [node, ptr, left_ptr, right_ptr] = node_stack.pop();
            let left_valid = node.left.id != '!';
            let right_valid = node.right.id != '!';
            let left_apply = node.left.id == '`';
            let right_apply = node.right.id == '`';
            if (!init && left_valid && right_valid) {
                init = ptr;
            }
            if (left_apply && left_ptr === undefined){
                let next_i = this.states.new_call();
                node_stack.push([node, ptr, next_i, void 8]);
                node_stack.push([node.left, next_i, void 8, void 8]);
                node = node.left;
            } else if (right_apply && right_ptr === undefined){
                let next_i = this.states.new_call();
                node_stack.push([node, ptr, left_ptr, next_i]);
                node_stack.push([node.right, next_i, void 8, void 8]);
                node = node.right;
            } else if (left_valid && right_valid) {
                let convert = (node, ptr) => {
                    if (node.id == '`') {
                        if (node.left.id == '!'|| node.right === '!') {
                            throw 'Application has too few arguments.'
                        }
                        return PointerFn.Call(ptr)
                    } else {
                        let ec = AST2Exec[node.id];
                        if (node.id == '.' || node.id == '?') {
                            return ec(node.value);
                        } else if (node.id == 'e') {
                            return ec(EXIT);
                        } else {
                            return ec;
                        }
                    }
                }
                let left_v = convert(node.left, left_ptr);
                let right_v = convert(node.right, right_ptr);
                this.states.set(ptr, left_v, right_v);
            }
        }
        return init;
    }
    run (input, output) {
        this.inits.map((init)=>this.run_s(input, output, init));
    }
    run_s (input, output, init) {
        this.state_machine = new StateMachine(this.states.clone(), init);
        this.state_machine.run(input, output);
    }
    exit () {
        this.state_machine.exit = true;
    }
    toString () {
        return this.states+'';
    }
}
const UnlambdaAST = class {
    top;
    constructor () {
        this.top = new UnlambdaASTTop ();
    }
    toString () {
        return ''+this.top;
    }
    compile () {
        return new UnlambdaCompiler (this);
    }
}
const UnlambdaASTNode = class {
    get name () { return 'empty' }
    get data () { return false }
    get id () { return '!' }
    static toString () {
        return '[empty]';
    }
    toString () {
        return '['+this.name+']';
    }
}
const UnlambdaASTTop = class {
    get name () { return 'top' }
    get data () { return true }
    list = [];
    add (node) {
        this.list.push(node);
        if (node.data) {
            return [this, node];
        } else {
            return [this];
        }
    }
    toString () {
        return 'AST(' + this.list.join(', ') + ')';
    }
}
const UnlambdaASTApply = class extends UnlambdaASTNode {
    get name () { return 'apply' }
    get data () { return true }
    get id () { return '`' }
    left = new UnlambdaASTNode ();
    right = new UnlambdaASTNode ();
    add (node) {
        if (this.left.id == '!') {
            this.left = node;
            if (node.data) {
                return [this, node];
            } else {
                return [this];
            }
        } else if (this.right.id == '!') {
            this.right = node;
            if (node.data) {
                return [node];
            } else {
                return [];
            }
        } else {
            throw 'Internal parser error: Full apply added to.';
        }
    }
    toString () {
        //return '`(' + this.left + ' ' + this.right + ')';
        let str = '`(';
        let rights = ['', this.right];
        let v = this.left;
        while (rights.length > 0) {
            if (v.id == '`') {
                str += '`('
                if (v.left.id == '`') {
                    rights.push(v.right);
                    v = v.left;
                } else {
                    str += v.left + ', ';
                    if (v.right.id == '`') {
                        rights.push('');
                        v = v.right;
                    } else {
                        str += v.right + ')';
                        v = rights.pop();
                    }
                }
            } else {
                str += ', '+v+')';
                v = rights.pop();
            }
        }
        str += ')';
        return str;
    }
}
const UnlambdaASTConstant = class extends UnlambdaASTNode {
    get name () { return 'const' }
    get id () { return 'k' }
}
const UnlambdaASTSubstitute = class extends UnlambdaASTNode {
    get name () { return 'sub' }
    get id () { return 's' }
}
const UnlambdaASTIdentity = class extends UnlambdaASTNode {
    get name () { return 'id' }
    get id () { return 'i' }
}
const UnlambdaASTVoid = class extends UnlambdaASTNode {
    get name () { return 'void' }
    get id () { return 'v' }
}
const UnlambdaASTCallCC = class extends UnlambdaASTNode {
    get name () { return 'call/cc' }
    get id () { return 'c' }
}
const UnlambdaASTDelay = class extends UnlambdaASTNode {
    get name () { return 'delay' }
    get id () { return 'd' }
}
const UnlambdaASTExit = class extends UnlambdaASTNode {
    get name () { return 'exit' }
    get id () { return 'e' }
}
const UnlambdaASTChar = class extends UnlambdaASTNode {
    get name () { return 'char' }
    get id () { return '.' }
    value = '';
    toString () {
        return '[char ' + this.value + ']';
    }
}
const UnlambdaASTRead = class extends UnlambdaASTNode {
    get name () { return 'read' }
    get id () { return '@' }
}
const UnlambdaASTCompare = class extends UnlambdaASTNode {
    get name () { return 'cmp' }
    get id () { return '?' }
    value = '';
    toString () {
        return '[cmp ' + this.value + ']';
    }
}
const UnlambdaASTReprint = class extends UnlambdaASTNode {
    get name () { return 'reprint' }
    get id () { return '|' }
}

// okay here's where its less obvious whats going on...
// we compile to a state machine and each state is
// a function application called ExecData.
// we very frequently add states.
// there are only a few types of function (ExecNode):
// one of the symbol-functions,
// one of the character-functions,
// the special Variable function,
// or one of the 4 pointer-functions,
// Literal, Call, Continuation, and Chain.
// i believe this is subtly distinct from the canonical C and Java
// interpreters of the language. those interpreters
// build up an explicit tree of continuation/task objects,
// and traverses the tree. however we make no inherent distinction
// between continuations and tasks. a continuation just changes
// our tape head, a "chain" changes our return path, and a "task"
// (here a Call) changes our tape head to an expression that
// it is dependent on, and changes our return path to an
// evaluation on the "last return value". idk i think this is
// different from the canonical approach but im sure.

// as far as the states go,
// we say an ExecNode "resolves" to an immediately callable function,
// and an ExecData "resolves" by producing an ExecData
// that is an immediately callable function.
// a Call pointer-function cannot be resolved, that is,
// we define an ExecData to be unresolved if contains a Call,
// unless it is some `dP, where P is an Call.
// however, an unresolved Data will be used to generate resolved Data
// using the other 4 functions.
// the Variable special function resolves to the value of 
// the last function application. it is the access point for
// our single internal variable other than "current character".
// however, this variable never stores anything other than
// the pointer to some valued ExecData.
// a Literal pointer-function resolves to its target.
// that is, its value is the resolved ExecData that it pointer addresses.
// although a function with Literals is resolved, the resulting value
// may have to be generated as an additional state.
// an Continuation pointer-function encodes a continuation.
// a continuation is identity-valued but causes the state machine
// to move to a particular state.
// an Chain pointer-function encodes the callstack. it calls
// its argument on the ExecVariable, and then calls a continuation
// on that result, as long as its argument isn't Exit.
// Exit is actually just any continuation that addresses
// the special state, `eX (exit with current value).
// this is all in theory. in actuality, some of the possible states
// do not represent valid Unlambda programs, or a valid sequence
// of calculations, so they are considered errors.
// If any of these non-valid states are found by the state machine,
// the resulting execution is considered to be undefined behavior.
// Note: it seems really weird to me that "current character" is
// a "global" variable. i feel like reseting the callstack should
// also reset the "current character" value. but we follow the
// other implementations.
const StateType = {
    Exit: 'E',
    Call: 'C',
    Resolved: 'O',
    Chain: '&',
    Identity: 'I',
    Constant: 'K',
    Substitute: 'S',
    Substitute2: 'Z',
    Promise: 'D',
}
const FunctionId = {
    Unknown: '!',
    Variable: 'X',
    Identity: 'i',
    Void: 'v',
    Delay: 'd',
    Constant: 'k',
    Substitute: 's',
    CallCC: 'c',
    Read: '@',
    Reprint: '|',
    Literal: 'V',
    Call: 'P',
    Continuation: 'Q',
    Chain: 'N',
    Print: '.',
    Compare: '?',
}

const ExecNode = class {
    constructor (id = FunctionId.Unknown) { this.id = id }
    toString () { return this.id }
}
const SymbolFn = {
    Variable: new ExecNode(FunctionId.Variable),
    Identity: new ExecNode(FunctionId.Identity),
    Void: new ExecNode(FunctionId.Void),
    Delay: new ExecNode(FunctionId.Delay),
    Constant: new ExecNode(FunctionId.Constant),
    Substitute: new ExecNode(FunctionId.Substitute),
    CallCC: new ExecNode(FunctionId.CallCC),
    Read: new ExecNode(FunctionId.Read),
    Reprint: new ExecNode(FunctionId.Reprint),
}
// our storage system StateData is typed,
// so our pointers are typed as well.
const StatePtr = class {
    type; addr;
    constructor (type, addr) {
        this.type = type;
        this.addr = addr;
    }
    toString () {
        return '['+this.type+this.addr+']'
    }
}
const EXIT = new StatePtr(StateType.Exit,0);
const ExecPtrNode = class extends ExecNode {
    ptr; is_exit;
    constructor (id, ptr) {
        super(id);
        this.ptr = ptr;
        this.is_exit = ptr.id == EXIT;
    }
    toString () { return this.id + this.ptr.toString() }
}
const PointerFnBase = function (id) {
    return (ptr) => {
        return new ExecPtrNode(id, ptr)
    }
}
const PointerFn = {
    Literal: PointerFnBase(FunctionId.Literal),
    Call: PointerFnBase(FunctionId.Call),
    Continuation: PointerFnBase(FunctionId.Continuation),
    Chain: PointerFnBase(FunctionId.Chain),
}
const ExecChar = class extends ExecNode {
    constructor (id, value) { super(id); this.value = value }
    toString () { return "'"+this.value+"'" }
}
const CharFn = {
    Print: (value) => new ExecChar(FunctionId.Print,value),
    Compare: (value) => new ExecChar(FunctionId.Compare,value),
}
const AST2Exec = {
    'k': SymbolFn.Constant,
    'i': SymbolFn.Identity,
    'v': SymbolFn.Void,
    'd': SymbolFn.Delay,
    'c': SymbolFn.CallCC,
    's': SymbolFn.Substitute,
    '@': SymbolFn.Read,
    '|': SymbolFn.Reprint,
    'e': PointerFn.Continuation,
    '.': CharFn.Print,
    '?': CharFn.Compare,
};

const State = class {
    left; right;
    toString () {
        return this.right ? this.left ? '<'+this.left+' '+this.right+'>'
            : '<'+this.right+'>' : '<>'
    }
}

const StateData = class {
    call_states = [];
    resolved_states = [];
    // note: in chain_states we only store pointers
    // since we always know the structure
    chain_states = [];
    identity_states = [];
    simple_states = [];
    substitute_states = [];
    size () {
        return this.call_states.length/2 + this.resolved_states.length/2
            + this.chain_states.length/2 + this.identity_states.length
            + this.simple_states.length + this.substitute_states.length/2
    }
    clone () {
        let sd = new StateData();
        sd.call_states = this.call_states;
        sd.resolved_states = this.resolved_states;
        sd.chain_states = this.chain_states;
        sd.identity_states = this.identity_states;
        sd.simple_states = this.simple_states;
        sd.substitute_states = this.substitute_states;
        return sd;
    }
    set (a, x, y) { /// hopefully going away soon
        this.call_states[a.addr] = x;
        this.call_states[a.addr+1] = y;
    }
    new_call (a, b) { /// also temporary
        return new StatePtr(StateType.Call, this.call_states.push(void 8, void 8)-2);
    }
    add_call (x, y) {
        let i = this.call_states.push(x, y)-2;
        return new StatePtr(StateType.Call, i);
    }
    get_call ({addr}) {
        return {left: this.call_states[addr], right: this.call_states[addr+1]}
    }
    add_chain (x, y) {
        let i = this.chain_states.push(x, y)-2;
        return new StatePtr(StateType.Chain, i);
    }
    get_chain ({addr}) {
        return {left: this.chain_states[addr], right: this.chain_states[addr+1]}
    }
    add_resolved (x, y) {
        let i = this.resolved_states.push(x, y)-2;
        return new StatePtr(StateType.Resolved, i);
    }
    get_resolved ({addr}) {
        return {left: this.resolved_states[addr], right: this.resolved_states[addr+1]}
    }
    add_simple (t, x) {
        let i = this.simple_states.push(x)-1;
        return new StatePtr(t, i)
    }
    add_constant (x) {
        return this.add_simple(StateType.Constant, x);
    }
    get_constant ({addr}) {
        return this.simple_states[addr]
    }
    add_identity (x) {
        return this.add_simple(StateType.Identity, x);
    }
    get_identity ({addr}) {
        return this.simple_states[addr]
    }
    add_promise (x) {
        return this.add_simple(StateType.Promise, x);
    }
    get_promise ({addr}) {
        return this.simple_states[addr]
    }
    add_substitute (x) {
        return this.add_simple(StateType.Substitute, x);
    }
    get_substitute ({addr}) {
        return this.simple_states[addr]
    }
    add_substitute2 (x, y) {
        let i = this.substitute_states.push(x, y)-2;
        return new StatePtr(StateType.Substitute2, i);
    }
    get_substitute2 ({addr}) {
        return {left: this.substitute_states[addr], right: this.substitute_states[addr+1]}
    }
    toString () {
        return [...this.states.values()].join(' ');
    }
}

const StateMachine = class {
    variable = PointerFn.Continuation(EXIT);
    current_char = SymbolFn.Void;
    trail = EXIT;
    ptr; states;
    exit = false;
    constructor (states, init) {
        this.states = states;
        this.ptr = init;
    }
    run (input, output) {
        let states = this.states;
        while (this.ptr.type != StateType.Exit && !this.exit) {
            switch (this.ptr.type) {
                case StateType.Call:
                let call = states.get_call(this.ptr);
                //console.log(''+this.ptr+' <'+call.left+' '+call.right+'> <- '+this.variable);
                if (call.left.id == FunctionId.Variable) { call.left = this.variable }
                else if (call.right.id == FunctionId.Variable) { call.right = this.variable }
                if (call.left.id == FunctionId.Call) {
                    // adding a chain ensures we come back to <replacement>
                    // after we resolve left.ptr
                    let replacement;
                    if (call.right.id == FunctionId.Call) {
                        replacement = states.add_call(SymbolFn.Variable, call.right);
                    } else {
                        replacement = states.add_resolved(SymbolFn.Variable, call.right);
                    }
                    this.trail = states.add_chain(replacement, this.trail);
                    this.ptr = call.left.ptr;
                } else if (call.left.id == FunctionId.Delay) {
                    this.variable = PointerFn.Literal(states.add_promise(call.right));
                    this.ptr = this.trail;
                } else if (call.right.id == FunctionId.Call) {
                    let replacement = states.add_resolved(call.left, SymbolFn.Variable);
                    this.trail = states.add_chain(replacement, this.trail);
                    this.ptr = call.right.ptr;
                } else {
                    this.ptr = states.add_resolved(call.left, call.right);
                }
                break;
                case StateType.Resolved:
                let rsv = states.get_resolved(this.ptr);
                //console.log(''+this.ptr+' ['+rsv.left+' '+rsv.right+'] <- '+this.variable);
                if (rsv.left.id == FunctionId.Variable) { rsv.left = this.variable }
                else if (rsv.right.id == FunctionId.Variable) { rsv.right = this.variable }
                // the function determines what we do
                switch (rsv.left.id) {
                    // tentatively putting void first since it
                    // should tend to propagate
                    case FunctionId.Void:
                    this.variable = SymbolFn.Void;
                    this.ptr = this.trail;
                    break;
                    // in a resolved state, print delay & identity
                    // have a similar effect
                    case FunctionId.Print:
                    output.push(rsv.left.value);
                    case FunctionId.Delay:
                    case FunctionId.Identity:
                    this.variable = rsv.right;
                    this.ptr = this.trail;
                    break;
                    case FunctionId.Continuation:
                    this.variable = rsv.right;
                    this.ptr = rsv.left.ptr;
                    break;
                    case FunctionId.CallCC:
                    let continuation = PointerFn.Continuation(this.trail);
                    this.ptr = states.add_resolved(rsv.right, continuation);
                    break;
                    case FunctionId.Constant:
                    this.variable = PointerFn.Literal(states.add_constant(rsv.right));
                    this.ptr = this.trail;
                    break;
                    case FunctionId.Substitute:
                    this.variable = PointerFn.Literal(states.add_substitute(rsv.right));
                    this.ptr = this.trail;
                    break;
                    case FunctionId.Read:
                    let m = input.next();
                    if (!m.done) {
                        this.current_char = CharFn.Print(m.value);
                        this.ptr = states.add_resolved(rsv.right, SymbolFn.Identity);
                    } else if (input.eof) {
                        this.ptr = states.add_resolved(rsv.right, SymbolFn.Void);
                    } else {
                        input.restart(this);
                        this.exit = true;
                    }
                    break;
                    case FunctionId.Compare:
                    let compare_result;
                    if (!this.current_char || this.current_char.value != rsv.left.value) {
                        compare_result = SymbolFn.Void;
                    } else {
                        compare_result = SymbolFn.Identity;
                    }
                    this.ptr = states.add_resolved(rsv.right, compare_result);
                    break;
                    case FunctionId.Reprint:
                    this.ptr = states.add_resolved(rsv.right, this.current_char);
                    break;
                    // our function is the result of some resolved function
                    case FunctionId.Literal:
                    switch (rsv.left.ptr.type) {
                        case StateType.Identity:
                        let value = states.get_identity(rsv.left.ptr);
                        this.ptr = states.add_resolved(value, rsv.right);
                        break;
                        case StateType.Promise:
                        let promise = states.get_promise(rsv.left.ptr);
                        this.ptr = states.add_call(promise, rsv.right);
                        break;
                        case StateType.Constant:
                        let constant = states.get_constant(rsv.left.ptr);
                        this.variable = constant;
                        this.ptr = this.trail;
                        break;
                        case StateType.Substitute:
                        let subst = states.get_substitute(rsv.left.ptr);
                        let z = states.add_substitute2(subst, rsv.right);
                        this.variable = PointerFn.Literal(z);
                        this.ptr = this.trail;
                        break;
                        case StateType.Substitute2:
                        let subst2 = states.get_substitute2(rsv.left.ptr);
                        let left_ptr = states.add_resolved(subst2.left, rsv.right);
                        let right_ptr = states.add_resolved(subst2.right, rsv.right);
                        let left_fn = PointerFn.Call(left_ptr);
                        let right_fn = PointerFn.Call(right_ptr);
                        let replacement = states.add_call(left_fn, right_fn);
                        this.ptr = replacement;
                        break;
                        case StateType.Resolved:
                        throw 'Cannot double resolve.';
                        break;
                        case StateType.Call:
                        throw 'A Call cannot be resolved.';
                        case StateType.Chain:
                        throw 'A Chain should not be referenced.'
                        default:
                        throw "Unknown state type in "+left.ptr;
                    }
                    break;
                    default:
                    throw 'Internal runtime error: '+rsv.left+' is not recognized.'
                }
                break;
                case StateType.Chain:
                let chain = states.get_chain(this.ptr);
                //console.log(''+this.ptr+' '+chain.left+' -> '+chain.right);
                this.trail = chain.right;
                this.ptr = chain.left;
                break;
                case StateType.Identity:
                case StateType.Substitute2:
                case StateType.Constant:
                case StateType.Substitute:
                case StateType.Promise:
                throw "The state "+ptr.type+" cannot be evaluated.";
                break;
                default:
                throw "Unknown state type in "+ptr;
            }
        }
        if (this.ptr.type == StateType.Exit) {
            output.done();
        }
    }
}
